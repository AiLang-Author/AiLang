#!/usr/bin/env python3
"""
Test context-aware keyword recognition for COBOL parser
"""
import sys
sys.path.insert(0, '.')

from cobol_frontend.cobol_lexer import COBOLLexer

# Test cases that are currently failing
test_cases = [
    # Should work: keywords as variable names
    "77 PROCEDURE PIC X(10).",
    "77 COPY PIC X(5).",
    "01 DIVISION PIC 9(3).",
    "05 COMPUTE PIC S9(5).",
    
    # Should fail: keywords followed by keywords (not variable names)
    "77 PROCEDURE DIVISION.",
    "PROCEDURE DIVISION.",
]

print("="*80)
print("Testing Keyword Tokenization in Data Division Context")
print("="*80)

for i, code in enumerate(test_cases, 1):
    print(f"\n[Test {i}] Input: {code}")
    
    lexer = COBOLLexer(code)
    tokens = lexer.tokenize()
    
    print(f"  Tokens: ", end="")
    for tok in tokens:
        if tok.type.name not in ['NEWLINE', 'EOF']:
            print(f"{tok.type.name}('{tok.value}') ", end="")
    print()
    
    # Check if the second token (after LEVEL_NUMBER) is correct
    if len(tokens) >= 3:
        level_tok = tokens[0]
        name_tok = tokens[1]
        next_tok = tokens[2]
        
        print(f"  Analysis:")
        print(f"    Level: {level_tok.type.name} = {level_tok.value}")
        print(f"    Name:  {name_tok.type.name} = {name_tok.value}")
        print(f"    Next:  {next_tok.type.name} = {next_tok.value}")
        
        # Check if name token is a keyword that should be an identifier
        if name_tok.type.name in ['PROCEDURE', 'COPY', 'DIVISION', 'COMPUTE']:
            if next_tok.type.name in ['PIC', 'PICTURE']:
                print(f"  ✓ CORRECT: {name_tok.value} treated as keyword but followed by PIC")
            else:
                print(f"  ✗ ISSUE: {name_tok.value} is keyword token, should be IDENTIFIER")
                print(f"           Parser will fail: 'Expected PIC clause for {name_tok.value}'")