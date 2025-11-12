#!/usr/bin/env python3
"""
Diagnostic script to examine lexer behavior on problematic lines
"""

import sys
sys.path.insert(0, '.')

from cobol_frontend.cobol_lexer import COBOLLexer, COBOLTokenType

def test_problematic_line():
    """Test the exact line that's failing"""
    
    # Line 231149 from briefing document
    test_line = '     THIS PROGRAM CHECKS THE COMPILER"S ABILITY'
    
    print("="*80)
    print("TESTING PROBLEMATIC LINE 231149")
    print("="*80)
    print(f"Input: {repr(test_line)}")
    print()
    
    lexer = COBOLLexer(test_line)
    
    try:
        tokens = lexer.tokenize()
        print(f"✅ SUCCESS: Tokenized into {len(tokens)} tokens")
        print()
        print("Tokens produced:")
        for i, token in enumerate(tokens):
            print(f"  {i}: {token.type.name:20s} = {repr(token.value)}")
    except Exception as e:
        print(f"❌ FAILED: {type(e).__name__}")
        print(f"   Message: {e}")
        print()
        print(f"   Lexer state at failure:")
        print(f"     Position: {lexer.position}")
        print(f"     Line: {lexer.line}")
        print(f"     Column: {lexer.column}")
        if lexer.position < len(lexer.source):
            print(f"     Current char: {repr(lexer.source[lexer.position])}")
            print(f"     Context: {repr(lexer.source[max(0, lexer.position-10):lexer.position+10])}")

def test_variations():
    """Test variations to understand the quote handling"""
    
    print("\n" + "="*80)
    print("TESTING VARIATIONS")
    print("="*80)
    
    test_cases = [
        ('No quotes', 'THIS PROGRAM CHECKS THE COMPILER ABILITY'),
        ('Single quote', "THIS PROGRAM CHECKS THE COMPILER'S ABILITY"),
        ('Double quote (problematic)', 'THIS PROGRAM CHECKS THE COMPILER"S ABILITY'),
        ('Escaped double quote', 'THIS PROGRAM CHECKS THE COMPILER""S ABILITY'),
        ('Quote at end', 'THIS PROGRAM CHECKS"'),
        ('Quote at start', '"THIS PROGRAM CHECKS'),
        ('Multiple unmatched', 'FIRST"SECOND"THIRD'),
    ]
    
    for name, test_line in test_cases:
        print(f"\n{name}:")
        print(f"  Input: {repr(test_line)}")
        
        lexer = COBOLLexer(test_line)
        try:
            tokens = lexer.tokenize()
            print(f"  ✅ SUCCESS ({len(tokens)} tokens)")
            # Show just token types for brevity
            token_summary = [t.type.name for t in tokens]
            print(f"     Types: {', '.join(token_summary)}")
        except Exception as e:
            print(f"  ❌ FAILED: {e}")

def test_metadata_context():
    """Test how lexer handles IDENTIFICATION DIVISION metadata"""
    
    print("\n" + "="*80)
    print("TESTING METADATA CONTEXT")
    print("="*80)
    
    # Simulate what happens in actual COBOL
    test_cases = [
        ("SECURITY paragraph", "     SECURITY."),
        ("SECURITY with text", "     SECURITY. THIS IS CLASSIFIED"),
        ("SECURITY with quote", "     SECURITY. THIS PROGRAM CHECKS THE COMPILER\"S ABILITY"),
        ("REMARKS paragraph", "     REMARKS."),
        ("REMARKS with text", "     REMARKS. THIS IS A TEST PROGRAM"),
        ("AUTHOR with quote", "     AUTHOR. BOB'S BANK & TRUST"),
    ]
    
    for name, test_line in test_cases:
        print(f"\n{name}:")
        print(f"  Input: {repr(test_line)}")
        
        lexer = COBOLLexer(test_line)
        try:
            tokens = lexer.tokenize()
            print(f"  ✅ Tokens: {len(tokens)}")
            for token in tokens:
                print(f"     {token.type.name:20s} = {repr(token.value)}")
        except Exception as e:
            print(f"  ❌ FAILED: {e}")

def examine_skip_special_line():
    """Examine the _skip_special_line logic"""
    
    print("\n" + "="*80)
    print("EXAMINING _skip_special_line LOGIC")
    print("="*80)
    
    # Look at what the lexer considers "special"
    test_cases = [
        ("Comment line", "      * THIS IS A COMMENT"),
        ("AUTHOR metadata", "     AUTHOR. JOHN DOE"),
        ("SECURITY metadata", "     SECURITY. CLASSIFIED"),
        ("REMARKS metadata", "     REMARKS. TEST PROGRAM"),
        ("Normal code", "     MOVE X TO Y"),
    ]
    
    for name, test_line in test_cases:
        print(f"\n{name}:")
        print(f"  Input: {repr(test_line)}")
        
        lexer = COBOLLexer(test_line)
        
        # Call the _skip_special_line method directly
        is_special = lexer._skip_special_line(test_line, 1)
        
        print(f"  _skip_special_line returned: {is_special}")
        
        if is_special:
            print(f"  Tokens created: {len(lexer.tokens)}")
            for token in lexer.tokens:
                print(f"    {token.type.name:20s} = {repr(token.value)}")

def main():
    """Run all diagnostics"""
    test_problematic_line()
    test_variations()
    test_metadata_context()
    examine_skip_special_line()
    
    print("\n" + "="*80)
    print("DIAGNOSTIC COMPLETE")
    print("="*80)

if __name__ == '__main__':
    main()