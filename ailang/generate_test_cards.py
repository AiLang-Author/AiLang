#!/usr/bin/env python3
"""
COBOL Punch Card Generator for EXEC85 Testing

Converts COBOL source files (.cbl) into simulated "punch card" records
and loads them into PostgreSQL so EXEC85 can process them.

Usage:
    python3 generate_test_cards.py <cobol_file.cbl>
    python3 generate_test_cards.py --directory cobol_frontend/tests/
    python3 generate_test_cards.py --control-cards-only  # Just generate control cards

What it does:
1. Reads COBOL source files (80-column fixed format)
2. Converts each line to a "card" record
3. Generates EXEC85 control cards (*MONITOR, *START, etc.)
4. Inserts everything into CONTROL_CARD_FILE_seq table
5. Ensures all data is clean UTF-8 (no 0xAF garbage!)
"""

import sys
import os
import psycopg2
from pathlib import Path
import re

# PostgreSQL connection settings
DB_CONFIG = {
    'host': 'localhost',
    'port': 5432,
    'database': 'testdb',
    'user': 'testuser',
    'password': 'testpass'
}

def sanitize_utf8(line: str) -> str:
    """
    Ensure line is valid UTF-8 by replacing invalid bytes.
    
    COBOL source is usually ASCII (0x00-0x7F), but might have:
    - EBCDIC remnants
    - Binary data in comments
    - High-bit characters
    
    Replace anything suspicious with spaces.
    """
    # Encode as UTF-8, replacing errors
    clean_bytes = line.encode('utf-8', errors='replace')
    clean_str = clean_bytes.decode('utf-8')
    
    # Replace any remaining non-ASCII printable chars with spaces
    # Keep: space (0x20) through tilde (0x7E), plus tab (0x09) and newline (0x0A)
    result = []
    for char in clean_str:
        code = ord(char)
        if (0x20 <= code <= 0x7E) or code in (0x09, 0x0A):
            result.append(char)
        else:
            result.append(' ')  # Replace garbage with space
    
    return ''.join(result)


def read_cobol_fixed_format(filepath: Path) -> list[str]:
    """
    Read COBOL source file in fixed format (columns 1-80).
    
    COBOL Fixed Format:
    - Columns 1-6: Sequence number (ignored)
    - Column 7: Indicator (*, -, D, or space)
    - Columns 8-72: Area A (8-11) and Area B (12-72)
    - Columns 73-80: Identification (ignored)
    
    We keep the full 80 columns for authenticity.
    
    SPECIAL HANDLING: NIST test files (newcob.cbl) already contain
    control cards (*HEADER, *END-OF) embedded in the source!
    """
    cards = []
    
    with open(filepath, 'r', encoding='utf-8', errors='replace') as f:
        for line_num, line in enumerate(f, 1):
            # Remove newline but preserve spaces
            line = line.rstrip('\n\r')
            
            # Pad or truncate to exactly 80 columns
            if len(line) < 80:
                line = line.ljust(80)
            elif len(line) > 80:
                line = line[:80]
            
            # Sanitize to ensure valid UTF-8
            line = sanitize_utf8(line)
            
            cards.append(line)
            
            # Progress indicator for large files
            if line_num % 10000 == 0:
                print(f"    📖 Read {line_num:,} lines...")
    
    return cards


def generate_control_cards_simple() -> list[str]:
    """
    Generate minimal EXEC85 control cards for testing.
    
    These are the "punch cards" that tell EXEC85 what to do:
    - *MONITOR: Start of control section
    - *EXTRACT-ALL: Extract all programs
    - *LIST PROGRAMS: List what we found
    - *END-MONITOR: End of control section
    
    This is the BARE MINIMUM to get EXEC85 running without crashing.
    """
    cards = [
        "*HEADER,COBOL,TEST-RUN".ljust(80),
        "*MONITOR".ljust(80),
        "*EXTRACT-ALL".ljust(80),
        "*LIST PROGRAMS".ljust(80),
        "*END-MONITOR".ljust(80),
    ]
    
    return cards


def generate_control_cards_for_program(program_name: str) -> list[str]:
    """
    Generate EXEC85 control cards for a specific COBOL program.
    
    Pattern:
    *MONITOR
    *EXTRACT-ALL
    *START <program-name>
    <program source cards go here>
    *END-OF,<program-name>
    *END-MONITOR
    """
    cards = [
        "*MONITOR".ljust(80),
        "*EXTRACT-ALL".ljust(80),
        "*LIST PROGRAMS".ljust(80),
        f"*START {program_name}".ljust(80),
    ]
    
    return cards


def generate_end_cards(program_name: str) -> list[str]:
    """Generate end-of-program control cards"""
    return [
        f"*END-OF,{program_name}".ljust(80),
        "*END-MONITOR".ljust(80),
    ]


def load_cards_to_database(cards: list[str], clear_existing: bool = True):
    """
    Load card records into PostgreSQL CONTROL_CARD_FILE_seq table.
    
    Args:
        cards: List of 80-character card images
        clear_existing: If True, truncate table first
    """
    try:
        conn = psycopg2.connect(**DB_CONFIG)
        cur = conn.cursor()
        
        # Create table if it doesn't exist
        cur.execute("""
            CREATE TABLE IF NOT EXISTS CONTROL_CARD_FILE_seq (
                seq_id SERIAL PRIMARY KEY,
                record_data VARCHAR(4096),
                created_at TIMESTAMP DEFAULT NOW()
            )
        """)
        
        if clear_existing:
            print("🗑️  Clearing existing card data...")
            cur.execute("TRUNCATE TABLE CONTROL_CARD_FILE_seq")
        
        # Insert cards in sequence
        print(f"📇  Loading {len(cards)} cards into database...")
        for i, card in enumerate(cards, 1):
            cur.execute(
                "INSERT INTO CONTROL_CARD_FILE_seq (record_data) VALUES (%s)",
                (card,)
            )
            
            if i % 100 == 0:
                print(f"    Loaded {i}/{len(cards)} cards...")
        
        conn.commit()
        print(f"✅  Successfully loaded {len(cards)} cards!")
        
        # Verify
        cur.execute("SELECT COUNT(*) FROM CONTROL_CARD_FILE_seq")
        count = cur.fetchone()[0]
        print(f"📊  Database now contains {count} total cards")
        
        cur.close()
        conn.close()
        
    except psycopg2.Error as e:
        print(f"❌  Database error: {e}")
        sys.exit(1)


def extract_program_name(filepath: Path) -> str:
    """
    Extract PROGRAM-ID from COBOL source file.
    
    Looks for: PROGRAM-ID. <name>
    """
    with open(filepath, 'r', encoding='utf-8', errors='replace') as f:
        for line in f:
            # Look for PROGRAM-ID
            match = re.search(r'PROGRAM-ID\.\s+(\S+)', line, re.IGNORECASE)
            if match:
                return match.group(1).strip('.')
    
    # Fallback: use filename without extension
    return filepath.stem


def main():
    import argparse
    
    parser = argparse.ArgumentParser(description='Generate COBOL punch cards for EXEC85 testing')
    parser.add_argument('input', nargs='?', help='COBOL source file or directory')
    parser.add_argument('--control-cards-only', action='store_true',
                       help='Generate only control cards (no source)')
    parser.add_argument('--keep-existing', action='store_true',
                       help='Append to existing cards instead of clearing')
    parser.add_argument('--directory', '-d', help='Process all .cbl files in directory')
    
    args = parser.parse_args()
    
    all_cards = []
    
    if args.control_cards_only:
        print("📋  Generating control cards only (minimal test data)...")
        all_cards = generate_control_cards_simple()
        
    elif args.directory:
        print(f"📂  Processing all .cbl files in: {args.directory}")
        path = Path(args.directory)
        
        if not path.is_dir():
            print(f"❌  Not a directory: {args.directory}")
            sys.exit(1)
        
        cobol_files = sorted(path.glob('*.cbl'))
        
        if not cobol_files:
            print(f"❌  No .cbl files found in {args.directory}")
            sys.exit(1)
        
        print(f"📚  Found {len(cobol_files)} COBOL files")
        
        # Start with monitor card
        all_cards.append("*MONITOR".ljust(80))
        all_cards.append("*EXTRACT-ALL".ljust(80))
        
        for cbl_file in cobol_files:
            print(f"📄  Reading: {cbl_file.name}")
            program_name = extract_program_name(cbl_file)
            
            # Start card for this program
            all_cards.append(f"*START {program_name}".ljust(80))
            
            # Read source cards
            source_cards = read_cobol_fixed_format(cbl_file)
            all_cards.extend(source_cards)
            
            # End card for this program
            all_cards.append(f"*END-OF,{program_name}".ljust(80))
            
            print(f"    ✓ {len(source_cards)} source lines + 2 control cards")
        
        # End monitor
        all_cards.append("*END-MONITOR".ljust(80))
        
    elif args.input:
        input_path = Path(args.input)
        
        if not input_path.exists():
            print(f"❌  File not found: {args.input}")
            sys.exit(1)
        
        if input_path.is_file():
            print(f"📄  Reading COBOL source: {input_path.name}")
            
            # Check if this is the NIST test suite
            is_nist_suite = 'newcob' in input_path.name.lower() or input_path.stat().st_size > 1_000_000
            
            if is_nist_suite:
                print("🎯  Detected NIST COBOL-85 Test Suite (newcob.cbl)")
                print("    This file contains embedded control cards (*HEADER, *END-OF)")
                print("    Loading AS-IS without generating additional control cards...")
                
                # Read source cards (which already include control cards)
                source_cards = read_cobol_fixed_format(input_path)
                all_cards.extend(source_cards)
                
                print(f"✅  Loaded {len(source_cards):,} total cards (including embedded control cards)")
            else:
                # Regular single-program file
                program_name = extract_program_name(input_path)
                
                # Generate control cards
                all_cards.extend(generate_control_cards_for_program(program_name))
                
                # Read source
                source_cards = read_cobol_fixed_format(input_path)
                all_cards.extend(source_cards)
                
                # End cards
                all_cards.extend(generate_end_cards(program_name))
                
                print(f"✅  Generated {len(all_cards)} total cards")
        else:
            print(f"❌  Not a file: {args.input}")
            sys.exit(1)
    else:
        parser.print_help()
        sys.exit(1)
    
    # Load to database
    load_cards_to_database(all_cards, clear_existing=not args.keep_existing)
    
    print("\n🎉  Card generation complete!")
    print("💡  Run your transpiled EXEC85 program now:")
    print("    ./EXEC85")


if __name__ == '__main__':
    main()