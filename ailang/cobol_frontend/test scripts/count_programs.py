#!/usr/bin/env python3
"""
Count COBOL Programs in a File
Shows how many programs exist and where they are
Recognizes NIST test suite format with *HEADER and *END-OF markers
"""

import sys
import re

def count_programs(filename):
    """Count all programs in a COBOL file using NIST markers and PROGRAM-ID"""
    
    print("="*80)
    print(f"Analyzing COBOL file: {filename}")
    print("="*80)
    
    try:
        with open(filename, 'r', encoding='utf-8', errors='ignore') as f:
            lines = f.readlines()
    except FileNotFoundError:
        print(f"Error: File '{filename}' not found")
        return
    
    total_lines = len(lines)
    print(f"\nTotal lines in file: {total_lines:,}")
    
    # Find all *HEADER markers (NIST format)
    headers = []
    for i, line in enumerate(lines, 1):
        # Check columns 1-7 for *HEADER
        if line.startswith('*HEADER'):
            # Extract program name from *HEADER,COBOL,PROGNAME
            match = re.search(r'\*HEADER\s*,\s*COBOL\s*,\s*([A-Z0-9\-]+)', line)
            if match:
                prog_name = match.group(1)
                headers.append((i, prog_name, 'HEADER'))
    
    print(f"\nFound {len(headers)} *HEADER markers")
    
    # Find all *END-OF markers (NIST format)
    end_markers = []
    for i, line in enumerate(lines, 1):
        if line.startswith('*END-OF'):
            match = re.search(r'\*END-OF\s*,\s*([A-Z0-9\-]+)', line)
            if match:
                prog_name = match.group(1)
                end_markers.append((i, prog_name, 'END-OF'))
    
    print(f"Found {len(end_markers)} *END-OF markers")
    
    # Find all PROGRAM-ID declarations
    program_ids = []
    i = 0
    while i < len(lines):
        line = lines[i]
        
        # Strip sequence numbers (columns 73-80)
        code_line = line[:72] if len(line) >= 73 else line
        
        # Skip comment lines (column 7 is *)
        if len(code_line) > 6 and code_line[6] == '*':
            i += 1
            continue
        
        # Look for PROGRAM-ID
        upper_line = code_line.upper()
        if 'PROGRAM' in upper_line and 'ID' in upper_line:
            prog_name = None
            
            # Try to find name on same line
            match = re.search(r'PROGRAM[\s\-]*ID\s*\.?\s*([A-Z0-9\-]+)', upper_line)
            if match:
                prog_name = match.group(1)
            else:
                # Look in next few lines for the program name
                for j in range(1, 10):
                    if i + j >= len(lines):
                        break
                    next_line = lines[i + j]
                    code_next = next_line[:72] if len(next_line) >= 73 else next_line
                    
                    # Skip comment lines
                    if len(code_next) > 6 and code_next[6] == '*':
                        continue
                    
                    # Look for identifier (starts in column 8-72)
                    stripped = code_next[7:].strip() if len(code_next) > 7 else code_next.strip()
                    if stripped and not stripped.startswith('*'):
                        match = re.match(r'([A-Z0-9\-]+)\s*\.', stripped)
                        if match:
                            prog_name = match.group(1)
                            break
            
            if prog_name and prog_name not in ['DIVISION', 'SECTION', 'IDENTIFICATION', 'DATA', 'PROCEDURE']:
                program_ids.append((i + 1, prog_name, 'PROGRAM-ID'))
        
        i += 1
    
    print(f"Found {len(program_ids)} PROGRAM-ID declarations\n")
    
    # Combine all program markers
    all_programs = headers + program_ids
    all_programs.sort(key=lambda x: x[0])  # Sort by line number
    
    # Remove duplicates (same program name close together)
    unique_programs = []
    last_name = None
    last_line = 0
    for line_num, name, marker_type in all_programs:
        # If same name within 100 lines, skip (it's the same program)
        if name == last_name and (line_num - last_line) < 100:
            continue
        unique_programs.append((line_num, name, marker_type))
        last_name = name
        last_line = line_num
    
    # Report findings
    print(f"{'='*80}")
    print(f"FOUND {len(unique_programs)} UNIQUE PROGRAMS:")
    print(f"{'='*80}\n")
    
    for idx, (line_num, name, marker_type) in enumerate(unique_programs, 1):
        pct = (line_num / total_lines) * 100
        marker = "📍" if marker_type == 'HEADER' else "🔹"
        print(f"  {idx:3}. {marker} Line {line_num:7,} ({pct:5.1f}%): {name:15} [{marker_type}]")
    
    # Show progress every 10 programs
    if len(unique_programs) > 20:
        print(f"\n{'='*80}")
        print("PROGRESS MARKERS (every 10 programs):")
        print(f"{'='*80}\n")
        for i in range(0, len(unique_programs), 10):
            line_num, name, _ = unique_programs[i]
            pct = (line_num / total_lines) * 100
            print(f"  Program {i+1:3}: {name:15} at line {line_num:7,} ({pct:5.1f}%)")
    
    # Find END PROGRAM statements
    end_programs = []
    for i, line in enumerate(lines, 1):
        code_line = line[:72] if len(line) >= 73 else line
        if len(code_line) > 6 and code_line[6] == '*':
            continue
        
        upper_line = code_line.upper()
        if 'END' in upper_line and 'PROGRAM' in upper_line:
            match = re.search(r'END[\s\-]+PROGRAM\s+([A-Z0-9\-]+)', upper_line)
            if match:
                end_programs.append((i, match.group(1)))
    
    print(f"\n{'='*80}")
    print(f"FOUND {len(end_programs)} END PROGRAM STATEMENTS")
    print(f"{'='*80}\n")
    
    # Validation
    program_names = set(name for _, name, _ in unique_programs)
    end_names = set(name for _, name in end_programs)
    
    missing_ends = program_names - end_names
    extra_ends = end_names - program_names
    
    print(f"{'='*80}")
    print("VALIDATION:")
    print(f"{'='*80}")
    
    if missing_ends:
        print(f"⚠️  Programs WITHOUT END PROGRAM: {len(missing_ends)}")
        for name in sorted(list(missing_ends)[:10]):
            print(f"    - {name}")
        if len(missing_ends) > 10:
            print(f"    ... and {len(missing_ends) - 10} more")
    else:
        print(f"✓ All programs have END PROGRAM statements")
    
    if extra_ends:
        print(f"\n⚠️  END PROGRAM without HEADER/PROGRAM-ID: {len(extra_ends)}")
        for name in sorted(list(extra_ends)[:10]):
            print(f"    - {name}")
        if len(extra_ends) > 10:
            print(f"    ... and {len(extra_ends) - 10} more")
    
    print(f"\n{'='*80}")
    print("SUMMARY:")
    print(f"{'='*80}")
    print(f"  Total programs found:        {len(unique_programs)}")
    print(f"  Total END PROGRAM found:     {len(end_programs)}")
    print(f"  Total lines in file:         {total_lines:,}")
    if unique_programs:
        print(f"  Avg lines/program:           {total_lines // len(unique_programs):,}")
        print(f"  First program:               {unique_programs[0][1]} (line {unique_programs[0][0]:,})")
        print(f"  Last program:                {unique_programs[-1][1]} (line {unique_programs[-1][0]:,})")
    print(f"{'='*80}\n")


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: python3 count_programs.py <cobol_file>")
        print("\nExample:")
        print("  python3 count_programs.py cobol_frontend/tests/bigtest.cbl")
        sys.exit(1)
    
    count_programs(sys.argv[1])