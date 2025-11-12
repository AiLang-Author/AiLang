#!/usr/bin/env python3
"""
Test case for hex literal tokenization fix
This will verify the lexer correctly handles X'09' as a single token
"""

import sys
sys.path.insert(0, '/mnt/c/Users/Sean/Documents/AiLang/ailang')

from cobol_frontend.cobol_lexer import COBOLLexer

def test_hex_literal():
    """Test that X'09' is tokenized as a single STRING_LITERAL token"""
    
    print("=" * 60)
    print("TEST: Hex Literal Tokenization")
    print("=" * 60)
    
    # Test case 1: Simple hex literal
    source1 = "IF A = X'09'"
    print(f"\nTest 1: {source1}")
    print("-" * 40)
    
    lexer1 = COBOLLexer(source1)
    tokens1 = lexer1.tokenize()
    
    print("Tokens:")
    for i, token in enumerate(tokens1):
        print(f"  {i}: {token.type.name:20s} = '{token.value}'")
    
    # Check for phantom X
    x_tokens = [t for t in tokens1 if t.value == 'X' and t.type.name == 'IDENTIFIER']
    hex_tokens = [t for t in tokens1 if 'X\'09\'' in str(t.value)]
    
    if x_tokens:
        print("\n❌ FAIL: Found phantom X identifier token!")
        print(f"   Token: {x_tokens[0]}")
    elif hex_tokens:
        print(f"\n✅ PASS: X'09' tokenized as single token: {hex_tokens[0].value}")
    else:
        print("\n❓ UNEXPECTED: No X or X'09' found")
    
    # Test case 2: Abbreviated condition with hex literal
    source2 = "IF WS-CHAR = ' ' OR ':' OR X'09'"
    print(f"\n\nTest 2: {source2}")
    print("-" * 40)
    
    lexer2 = COBOLLexer(source2)
    tokens2 = lexer2.tokenize()
    
    print("Tokens:")
    for i, token in enumerate(tokens2):
        print(f"  {i}: {token.type.name:20s} = '{token.value}'")
    
    # Check for phantom X
    x_tokens2 = [t for t in tokens2 if t.value == 'X' and t.type.name == 'IDENTIFIER']
    hex_tokens2 = [t for t in tokens2 if 'X\'09\'' in str(t.value) or "X'09'" in str(t.value)]
    
    if x_tokens2:
        print("\n❌ FAIL: Found phantom X identifier token!")
        print(f"   Token: {x_tokens2[0]}")
    elif hex_tokens2:
        print(f"\n✅ PASS: X'09' tokenized as single token: {hex_tokens2[0].value}")
    else:
        print("\n❓ UNEXPECTED: No X or X'09' found")
    
    # Test case 3: Normal X identifier (should still work)
    source3 = "MOVE X TO Y"
    print(f"\n\nTest 3: {source3}")
    print("-" * 40)
    
    lexer3 = COBOLLexer(source3)
    tokens3 = lexer3.tokenize()
    
    print("Tokens:")
    for i, token in enumerate(tokens3):
        print(f"  {i}: {token.type.name:20s} = '{token.value}'")
    
    # Should have X as identifier
    x_tokens3 = [t for t in tokens3 if t.value == 'X' and t.type.name == 'IDENTIFIER']
    
    if x_tokens3:
        print(f"\n✅ PASS: X tokenized as identifier (correct for standalone X)")
    else:
        print("\n❌ FAIL: X should be an identifier when not followed by quote")
    
    print("\n" + "=" * 60)

if __name__ == '__main__':
    try:
        test_hex_literal()
    except Exception as e:
        print(f"\n❌ ERROR: {e}")
        import traceback
        traceback.print_exc()