#!/usr/bin/env python3
"""
Copybook Detection Diagnostic Tool

Analyzes NIST test files to understand why copybooks aren't being detected.
Run this BEFORE transpilation to diagnose the issue.

Usage:
    python3 diagnose_copybooks.py cobol_frontend/tests/newcob.cbl
"""

import sys
import re
from pathlib import Path
from typing import List, Tuple, Dict

def analyze_nist_file(filepath: Path) -> Dict:
    """Analyze a NIST file for program and copybook markers"""
    
    print(f"\n{'='*80}")
    print(f"ANALYZING: {filepath}")
    print(f"{'='*80}\n")
    
    with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
        lines = f.readlines()
    
    stats = {
        'total_lines': len(lines),
        'programs': [],
        'copybooks': [],
        'data_sections': [],
        'unknown_headers': []
    }
    
    print(f"Total lines: {len(lines):,}\n")
    
    # First pass: Identify which lines are in which program
    print("Mapping programs and copybooks...")
    print("-" * 80)
    
    line_to_section = {}  # line_num -> ('PROGRAM' or 'COPYBOOK', name)
    current_section_type = None
    current_section_name = None
    
    for i, line in enumerate(lines, 1):
        if line.strip().startswith('*HEADER'):
            parts = line.strip().split(',')
            if len(parts) >= 3:
                marker_type = parts[1].strip().upper()
                name = parts[2].strip()
                current_section_type = marker_type
                current_section_name = name
        elif line.strip().startswith('*END-OF'):
            current_section_type = None
            current_section_name = None
        elif current_section_type and current_section_name:
            line_to_section[i] = (current_section_type, current_section_name)
    
    # Scan for all *HEADER markers
    print("\nScanning for *HEADER markers...")
    print("-" * 80)
    
    for i, line in enumerate(lines, 1):
        if line.strip().startswith('*HEADER'):
            # Parse the header
            parts = line.strip().split(',')
            
            if len(parts) >= 3:
                marker_type = parts[1].strip().upper()
                name = parts[2].strip()
                
                if marker_type == 'COBOL':
                    stats['programs'].append((i, name))
                    print(f"Line {i:6d}: PROGRAM  -> {name}")
                    
                elif marker_type == 'CLBRY':
                    stats['copybooks'].append((i, name))
                    print(f"Line {i:6d}: COPYBOOK -> {name}")
                    
                elif marker_type.startswith('DATA'):
                    stats['data_sections'].append((i, name))
                    print(f"Line {i:6d}: DATA     -> {name}")
                    
                else:
                    stats['unknown_headers'].append((i, marker_type, name))
                    print(f"Line {i:6d}: UNKNOWN  -> {marker_type}, {name}")
    
    # Summary
    print("\n" + "="*80)
    print("SUMMARY")
    print("="*80)
    print(f"Programs found:    {len(stats['programs']):4d}")
    print(f"Copybooks found:   {len(stats['copybooks']):4d}")
    print(f"Data sections:     {len(stats['data_sections']):4d}")
    print(f"Unknown headers:   {len(stats['unknown_headers']):4d}")
    
    # Show first few copybooks
    if stats['copybooks']:
        print("\nFirst 10 copybooks:")
        for line_num, name in stats['copybooks'][:10]:
            print(f"  Line {line_num:6d}: {name}")
    
    # Show where copybooks are used (IMPROVED DETECTION)
    print("\nScanning for COPY statements in programs...")
    print("-" * 80)
    
    copy_statements = []
    for i, line in enumerate(lines, 1):
        # Skip if not in a program section
        if i not in line_to_section:
            continue
        
        section_type, section_name = line_to_section[i]
        if section_type != 'COBOL':  # Only look in programs, not copybooks
            continue
        
        # Skip comment lines (start with * in column 7)
        stripped = line.strip()
        if stripped.startswith('*'):
            continue
        
        # Check if line has actual COBOL code (columns 8-72 typically)
        # Must have at least 7 chars to have code
        if len(line) < 7:
            continue
        
        # Extract code area (skip sequence area and indicator)
        # Columns 1-6: sequence, 7: indicator, 8-72: code
        code_area = line[7:72] if len(line) >= 72 else line[7:]
        
        # Skip if this is in a quoted string
        # Simple check: if COPY appears between quotes, skip
        # Match: COPY <name> with optional period
        match = re.search(r'\bCOPY\s+([A-Z0-9\-]+)\.?', code_area, re.IGNORECASE)
        if match:
            copybook_name = match.group(1)
            
            # Filter out obvious false positives
            # Real copybooks are typically alphanumeric with hyphens
            # Not single words like "NOT", "AND", "PIC"
            if len(copybook_name) < 2:
                continue
            if copybook_name.upper() in {'NOT', 'AND', 'OR', 'TO', 'OF', 'IN', 
                                         'PIC', 'FILE', 'SD', 'FD', 'ENV',
                                         'TEXT', 'REPLACING', 'BY'}:
                continue
            
            # Check if this looks like a real copybook name
            # Format: K####, KP###, etc. (letter + numbers + optional letter)
            if re.match(r'^[A-Z]+\d+[A-Z]*$', copybook_name, re.IGNORECASE) or \
               re.match(r'^[A-Z]{2,}[A-Z0-9]*$', copybook_name, re.IGNORECASE):
                copy_statements.append((i, copybook_name, section_name, code_area.strip()[:60]))
    
    print(f"Found {len(copy_statements)} real COPY statements in programs")
    
    if copy_statements:
        print("\nFirst 20 COPY statements:")
        for line_num, name, program, context in copy_statements[:20]:
            print(f"  Line {line_num:6d}: COPY {name}")
            print(f"            Context: {context}")
        
        # Group by program
        by_program = {}
        for line_num, name, program, context in copy_statements:
            if program not in by_program:
                by_program[program] = []
            by_program[program].append((line_num, name, context))
        
        # Show first 20 programs that use copybooks
        for prog_name in sorted(list(by_program.keys())[:20]):
            copies = by_program[prog_name]
            print(f"\n  Program {prog_name} uses {len(copies)} copybook(s):")
            for line_num, name, context in copies[:5]:  # Show first 5 per program
                print(f"    Line {line_num:6d}: COPY {name}")
            if len(copies) > 5:
                print(f"    ... and {len(copies) - 5} more")
    
    
    # Check if copybooks are actually defined
    if copy_statements and stats['copybooks']:
        print("\n" + "="*80)
        print("COPYBOOK RESOLUTION CHECK")
        print("="*80)
        
        copybook_names = {name for _, name in stats['copybooks']}
        referenced_names = {name for _, name, _, _ in copy_statements}
        
        print(f"\nUnique copybooks defined: {len(copybook_names)}")
        print(f"Unique copybooks referenced: {len(referenced_names)}")
        
        # Check for references to undefined copybooks
        undefined = referenced_names - copybook_names
        if undefined:
            print(f"\n⚠️  WARNING: {len(undefined)} copybooks are referenced but NOT defined:")
            for name in sorted(list(undefined)[:20]):
                print(f"  - {name}")
        else:
            print("\n✓ All referenced copybooks are defined!")
        
        # Check for unused copybooks
        unused = copybook_names - referenced_names
        if unused:
            print(f"\nℹ️  INFO: {len(unused)} copybooks are defined but NOT used:")
            for name in sorted(list(unused)[:10]):
                print(f"  - {name}")
    
    return stats


def extract_copybook_content(filepath: Path, copybook_name: str) -> str:
    """Extract a specific copybook's content from NIST file"""
    
    with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
        lines = f.readlines()
    
    # Find the copybook
    in_copybook = False
    content_lines = []
    
    for line in lines:
        if line.strip().startswith('*HEADER,CLBRY,' + copybook_name):
            in_copybook = True
            continue
        
        if in_copybook:
            if line.strip().startswith('*END-OF,' + copybook_name):
                break
            content_lines.append(line)
    
    return ''.join(content_lines)


def main():
    if len(sys.argv) < 2:
        print("Usage: python3 diagnose_copybooks.py <nist_file.cbl>")
        print("\nExample:")
        print("  python3 diagnose_copybooks.py cobol_frontend/tests/newcob.cbl")
        sys.exit(1)
    
    filepath = Path(sys.argv[1])
    
    if not filepath.exists():
        print(f"❌ ERROR: File not found: {filepath}")
        sys.exit(1)
    
    stats = analyze_nist_file(filepath)
    
    # Show sample copybook content
    if stats['copybooks']:
        print("\n" + "="*80)
        print("SAMPLE COPYBOOK CONTENT")
        print("="*80)
        
        first_copybook = stats['copybooks'][0][1]
        content = extract_copybook_content(filepath, first_copybook)
        
        print(f"\nCopybook: {first_copybook}")
        print("-" * 80)
        print(content[:500])
        if len(content) > 500:
            print(f"\n... ({len(content) - 500} more characters)")
    
    print("\n" + "="*80)
    print("DIAGNOSTIC COMPLETE")
    print("="*80)



def extract_copybook_content(filepath: Path, copybook_name: str) -> str:
    """Extract a specific copybook's content from NIST file"""
    
    with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
        lines = f.readlines()
    
    # Find the copybook
    in_copybook = False
    content_lines = []
    
    for line in lines:
        if line.strip().startswith('*HEADER,CLBRY,' + copybook_name):
            in_copybook = True
            continue
        
        if in_copybook:
            if line.strip().startswith('*END-OF,' + copybook_name):
                break
            content_lines.append(line)
    
    return ''.join(content_lines)


def main():
    if len(sys.argv) < 2:
        print("Usage: python3 diagnose_copybooks.py <nist_file.cbl>")
        print("\nExample:")
        print("  python3 diagnose_copybooks.py cobol_frontend/tests/newcob.cbl")
        sys.exit(1)
    
    filepath = Path(sys.argv[1])
    
    if not filepath.exists():
        print(f"❌ ERROR: File not found: {filepath}")
        sys.exit(1)
    
    stats = analyze_nist_file(filepath)
    
    # Show sample copybook content
    if stats['copybooks']:
        print("\n" + "="*80)
        print("SAMPLE COPYBOOK CONTENT")
        print("="*80)
        
        first_copybook = stats['copybooks'][0][1]
        content = extract_copybook_content(filepath, first_copybook)
        
        print(f"\nCopybook: {first_copybook}")
        print("-" * 80)
        print(content[:500])
        if len(content) > 500:
            print(f"\n... ({len(content) - 500} more characters)")
    
    print("\n" + "="*80)
    print("DIAGNOSTIC COMPLETE")
    print("="*80)


if __name__ == '__main__':
    main()