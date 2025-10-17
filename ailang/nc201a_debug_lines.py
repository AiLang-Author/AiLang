#!/usr/bin/env python3
"""
Debug script for NC201A-NC209A.cbl parser errors
Shows actual COBOL source at each error line
"""

import sys
import os

# Error lines from the test output
ERROR_LINES = [
    (110, "Expected PERIOD, got IS"),
    (2346, "Expected variable name after level 1"),
    (4385, "Expected variable name after level 77"),
    (6135, "Expected variable name after level 1"),
    (7284, "Expected variable name after level 77"),
    (8483, "Expected TO, got IN"),
    (10262, "Unexpected token in expression: IN"),
    (12663, "Expected variable name after level 1"),
    (13635, "Expected PERIOD after RENAMES clause"),
]

def show_context(filename, line_num, error_msg, context_lines=5):
    """Show source code context around an error line"""
    try:
        with open(filename, 'r') as f:
            lines = f.readlines()
        
        print(f"\n{'='*80}")
        print(f"ERROR AT LINE {line_num}: {error_msg}")
        print('='*80)
        
        start = max(0, line_num - context_lines - 1)
        end = min(len(lines), line_num + context_lines)
        
        for i in range(start, end):
            marker = ">>> " if i == line_num - 1 else "    "
            print(f"{marker}{i+1:5d}: {lines[i]}", end='')
            
        print()
        
    except FileNotFoundError:
        print(f"ERROR: Could not find file {filename}")
        return None
    except Exception as e:
        print(f"ERROR: {e}")
        return None

def main():
    if len(sys.argv) < 2:
        print("Usage: python3 nc201a_debug_lines.py <cobol_file>")
        sys.exit(1)
    
    filename = sys.argv[1]
    
    if not os.path.exists(filename):
        print(f"ERROR: File not found: {filename}")
        sys.exit(1)
    
    print("="*80)
    print(f"DEBUGGING: {filename}")
    print(f"Total errors to examine: {len(ERROR_LINES)}")
    print("="*80)
    
    for line_num, error_msg in ERROR_LINES:
        show_context(filename, line_num, error_msg)
    
    print("\n" + "="*80)
    print("ANALYSIS COMPLETE")
    print("="*80)

if __name__ == "__main__":
    main()