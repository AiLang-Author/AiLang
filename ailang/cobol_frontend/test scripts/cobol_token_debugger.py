#!/usr/bin/env python3
"""
COBOL Token Debugger
Tokenizes a COBOL file and prints all tokens for debugging
"""

import sys
sys.path.insert(0, '.')

from cobol_frontend.cobol_lexer import COBOLLexer

# Test specifically the problematic UNSTRING line
test_code = '''
       UNSTRING INPUT-1 DELIMITED BY "," OR ALL " "                VALU1 
            INTO INTEREST-IN NO-OF-PERIODS-IN.
'''

print("=" * 80)
print("TOKENIZING TEST CODE:")
print(test_code)
print("=" * 80)

lexer = COBOLLexer(test_code)
tokens = lexer.tokenize()

print(f"\nGenerated {len(tokens)} tokens:\n")

for i, token in enumerate(tokens):
    # Show string literals with special markers for empty/whitespace
    value_display = token.value
    if token.type.name == 'STRING_LITERAL':
        if value_display == '':
            value_display = '<EMPTY STRING>'
        elif value_display == ' ':
            value_display = '<SPACE>'
        elif value_display.strip() == '':
            value_display = f'<WHITESPACE: {repr(value_display)}>'
        else:
            value_display = f'"{value_display}"'
    
    print(f"{i:3d}. Line {token.line:2d} Col {token.column:3d}: "
          f"{token.type.name:20s} = {value_display}")

print("\n" + "=" * 80)

# Now test with the actual COBVALU file
print("\nNow testing with actual COBVALU.cbl file...")
try:
    with open('cobol_frontend/tests/cabval.cbl', 'r') as f:
        content = f.read()
    
    # Find the UNSTRING statement
    lines = content.split('\n')
    for i, line in enumerate(lines, 1):
        if 'UNSTRING INPUT-1' in line:
            print(f"\nFound UNSTRING at line {i}:")
            print(f"  Line {i}: {line}")
            if i < len(lines):
                print(f"  Line {i+1}: {lines[i]}")
            
            # Tokenize just this section
            section = '\n'.join(lines[max(0, i-2):min(len(lines), i+3)])
            print(f"\nTokenizing section:")
            print(section)
            print("\nTokens:")
            
            lexer2 = COBOLLexer(section)
            tokens2 = lexer2.tokenize()
            
            for j, token in enumerate(tokens2):
                if 'UNSTRING' in str(token.value) or j > tokens2.index(next(t for t in tokens2 if 'UNSTRING' in str(t.value))):
                    value_display = token.value
                    if token.type.name == 'STRING_LITERAL':
                        if value_display == '':
                            value_display = '<EMPTY STRING>'
                        elif value_display == ' ':
                            value_display = '<SPACE>'
                        elif value_display.strip() == '':
                            value_display = f'<WHITESPACE: {repr(value_display)}>'
                        else:
                            value_display = f'"{value_display}"'
                    
                    print(f"  {j:3d}. {token.type.name:20s} = {value_display}")
                    
                    if token.type.name == 'PERIOD':
                        break
            break
    
except FileNotFoundError:
    print("Could not find cabval.cbl - skipping file test")

print("\nDone!")