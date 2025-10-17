#!/usr/bin/env python3
"""Test VALUE ALL QUOTE parsing"""

import sys
sys.path.insert(0, '.')

from cobol_frontend.cobol_lexer import COBOLLexer
from cobol_frontend.parser import COBOLMultiProgramParser

# Test code with VALUE ALL QUOTE
test_cobol = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-VALUE-ALL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  TEST1 PIC X(10) VALUE ALL QUOTE.
       77  TEST2 PIC X(10) VALUE ALL "-".
       77  TEST3 PIC X(5) VALUE ALL SPACE.
       77  TEST4 PIC X(8) VALUE ALL ZEROS.
       PROCEDURE DIVISION.
           STOP RUN.
"""

print("Tokenizing...")
lexer = COBOLLexer(test_cobol)
tokens = lexer.tokenize()

print(f"Generated {len(tokens)} tokens")

# Find VALUE tokens and show what follows
print("\nVALUE clauses:")
for i, token in enumerate(tokens):
    if token.type.name == 'VALUE':
        print(f"\nLine {token.line}: VALUE")
        for j in range(1, 5):
            if i + j < len(tokens):
                next_tok = tokens[i + j]
                print(f"  +{j}: {next_tok.type.name:20} = {repr(next_tok.value)}")

print("\n" + "="*80)
print("Parsing...")
try:
    parser = COBOLMultiProgramParser(tokens)
    ast = parser.parse_all_programs()
    print(f"✓ Successfully parsed!")
    
    if ast.programs:
        prog = ast.programs[0]
        if prog.data_division and prog.data_division.working_storage:
            print(f"\nWorking Storage Variables:")
            for var in prog.data_division.working_storage:
                print(f"  {var.name:10} = {var.value}")
except Exception as e:
    print(f"✗ Parse error: {e}")
    import traceback
    traceback.print_exc()