#!/usr/bin/env python3
"""
Trace the exact failure point
"""

import sys
sys.path.insert(0, '.')

from cobol_frontend.cobol_lexer import COBOLLexer

def test_installation_lines():
    """Test the exact sequence that's failing"""
    print("="*80)
    print("TESTING INSTALLATION PARAGRAPH LINES")
    print("="*80)
    
    # The exact sequence from preprocessing
    test_source = """ INSTALLATION.                                                    
     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".
     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".
 ENVIRONMENT DIVISION."""
    
    print(f"Test source:")
    for i, line in enumerate(test_source.split('\n'), 1):
        print(f"  {i}: {repr(line)}")
    
    try:
        lexer = COBOLLexer(test_source)
        tokens = lexer.tokenize()
        
        print(f"\n✅ SUCCESS!")
        print(f"Tokens:")
        for token in tokens:
            print(f"  {token.type.name:20s} = {repr(token.value)}")
    
    except Exception as e:
        print(f"\n❌ FAILED: {e}")
        print(f"\nLexer state:")
        print(f"  Position: {lexer.position}")
        print(f"  Line: {lexer.line}")
        print(f"  Column: {lexer.column}")

def test_with_handle_special():
    """Test if _handle_special_lines catches the metadata line"""
    print("\n" + "="*80)
    print("TESTING _handle_special_lines")
    print("="*80)
    
    from cobol_frontend.cobol_lexer import COBOLLexer
    
    lexer = COBOLLexer("")
    
    test_lines = [
        (" INSTALLATION.", "Metadata keyword"),
        ('     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".', "Quoted continuation"),
        (' ENVIRONMENT DIVISION.', "Normal division"),
    ]
    
    for line, description in test_lines:
        result = lexer._handle_special_lines(line, 1)
        print(f"\n{description}:")
        print(f"  Line: {repr(line)}")
        print(f"  _handle_special_lines returned: {result}")
        print(f"  Stripped starts with INSTALLATION: {line.strip().startswith('INSTALLATION')}")

if __name__ == '__main__':
    test_installation_lines()
    test_with_handle_special()