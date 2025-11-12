#!/usr/bin/env python3
"""
Examine the context around line 231149 in newval.cbl
"""

def examine_line_context(filename, target_line, context_lines=20):
    """Show lines around target line"""
    print(f"="*80)
    print(f"EXAMINING {filename} around line {target_line}")
    print(f"="*80)
    
    with open(filename, 'r') as f:
        lines = f.readlines()
    
    start = max(0, target_line - context_lines - 1)
    end = min(len(lines), target_line + context_lines)
    
    print(f"\nShowing lines {start+1} to {end}")
    print("-"*80)
    
    for i in range(start, end):
        line_num = i + 1
        line = lines[i].rstrip('\n')
        
        # Highlight the target line
        marker = ">>>" if line_num == target_line else "   "
        
        # Show both raw and what columns 7-72 would be
        if len(line) >= 7:
            col7_indicator = line[6] if len(line) > 6 else ' '
            code_area = line[6:72] if len(line) >= 72 else line[6:]
            print(f"{marker} {line_num:6d} [{col7_indicator}] {repr(line)}")
            if line_num == target_line:
                print(f"           Code area (7-72): {repr(code_area)}")
        else:
            print(f"{marker} {line_num:6d} [ ] {repr(line)}")
    
    print("-"*80)
    
    # Look backwards to find what metadata paragraph this is part of
    print("\nSearching backwards for metadata keyword...")
    for i in range(target_line - 2, max(0, target_line - 100), -1):
        line = lines[i].strip().upper()
        metadata_keywords = ['SECURITY.', 'REMARKS.', 'AUTHOR.', 'INSTALLATION.', 
                            'DATE-WRITTEN.', 'DATE-COMPILED.']
        for keyword in metadata_keywords:
            if keyword in line:
                print(f"  Found {keyword} at line {i+1}")
                print(f"  Content: {repr(lines[i].rstrip())}")
                return
    
    print("  No metadata keyword found in previous 100 lines")

def check_tokenization_state():
    """Check if lexer is tracking metadata state"""
    print("\n" + "="*80)
    print("CHECKING LEXER METADATA STATE TRACKING")
    print("="*80)
    
    import sys
    sys.path.insert(0, '.')
    from cobol_frontend.cobol_lexer import COBOLLexer
    
    # Check if lexer has any state tracking for IDENTIFICATION DIVISION
    print("\nLexer class attributes:")
    lexer = COBOLLexer("")
    
    # Look for division tracking
    attrs = [attr for attr in dir(lexer) if not attr.startswith('_')]
    print(f"  Public attributes: {attrs}")
    
    # Check for anything related to division or metadata
    division_related = [attr for attr in dir(lexer) if 'division' in attr.lower() or 'metadata' in attr.lower()]
    print(f"  Division/metadata related: {division_related}")
    
    if not division_related:
        print("\n  ❌ PROBLEM: Lexer has NO state tracking for divisions or metadata")
        print("     This means it cannot know when it's in IDENTIFICATION DIVISION")
        print("     and should treat bare quotes as non-code text")

if __name__ == '__main__':
    import sys
    
    filename = 'cobol_frontend/tests/newval.cbl'
    target_line = 231149
    
    if len(sys.argv) > 1:
        target_line = int(sys.argv[1])
    
    try:
        examine_line_context(filename, target_line)
        check_tokenization_state()
    except FileNotFoundError:
        print(f"Error: {filename} not found")
        print("Please run from the project root directory")