#!/usr/bin/env python3
"""
Test if parser can handle level 88 condition names after level 77
"""

import sys
sys.path.insert(0, '.')

from cobol_frontend.cobol_lexer import COBOLLexer
from cobol_frontend.parser import COBOLMultiProgramParser

# Minimal test case
test_source = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST88.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  IF-D32 PICTURE S9(4)V99.
           88  A  VALUE 1.
           88  B VALUES ARE 2 THRU 4.
           88  C VALUE IS ZERO.
       PROCEDURE DIVISION.
           STOP RUN.
"""

print("Testing: Level 88 condition names after level 77")
print("="*80)

try:
    # Tokenize
    print("\n[1] Tokenizing...")
    lexer = COBOLLexer(test_source)
    tokens = lexer.tokenize()
    print(f"   ✓ {len(tokens)} tokens")
    
    # Show tokens around the 88 levels
    print("\n[2] Tokens around level 88:")
    for i, tok in enumerate(tokens):
        if tok.type.name == 'LEVEL_NUMBER' and tok.value in ['77', '88']:
            print(f"   [{i:3d}] {tok.type.name:20s} = {tok.value:10s} (line {tok.line})")
            # Show next 5 tokens
            for j in range(i+1, min(i+6, len(tokens))):
                print(f"   [{j:3d}]   → {tokens[j].type.name:20s} = {tokens[j].value}")
            print()
    
    # Parse
    print("\n[3] Parsing...")
    parser = COBOLMultiProgramParser(tokens, debug=True)
    ast = parser.parse_all_programs()
    
    print(f"\n   ✓ SUCCESS: Parsed {len(ast.programs)} program(s)")
    
    # Check if we got the 88 levels
    prog = ast.programs[0]
    ws = prog.data_division.working_storage
    
    print(f"\n[4] Checking parsed structure:")
    # ws is a list of variables, not an object with .variables
    for var in ws:
        print(f"   Level {var.level:2d}: {var.name:20s} PIC={var.pic_clause}")
        if var.children:
            for child in var.children:
                print(f"      ↳ Level {child.level}: {child.name} VALUE={child.value}")
    
except Exception as e:
    print(f"\n   ✗ FAILED: {e}")
    import traceback
    traceback.print_exc()