#!/usr/bin/env python3
"""
Direct check: Compare what parser produces vs what converter sees
"""
import sys
import os
sys.path.insert(0, '/mnt/c/Users/Sean/Documents/AiLang/ailang')

from cobol_frontend import COBOLLexer, COBOLParser

# Minimal test case
test_cobol = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTMIN.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  PPS-RTC     PIC 9(02) VALUE 0.
       
       PROCEDURE DIVISION.
       
       0000-MAINLINE-CONTROL.
           PERFORM 0100-INITIAL-ROUTINE.
           GOBACK.
       
       0100-INITIAL-ROUTINE.
           MOVE ZEROS TO PPS-RTC.
       
       1000-EDIT-THE-BILL-INFO.
           IF PPS-RTC = 00
              MOVE 53 TO PPS-RTC
           END-IF.
"""

print("="*80)
print("PARSING...")
print("="*80)

lexer = COBOLLexer(test_cobol)
tokens = lexer.tokenize()
parser = COBOLParser(tokens, debug=False)
ast = parser.parse_all_programs()

program = ast.programs[0]

print(f"\nProgram: {program.program_id}")
print(f"Paragraphs in PROCEDURE DIVISION: {len(program.procedure_division.paragraphs)}")

for idx, para in enumerate(program.procedure_division.paragraphs):
    print(f"\n  [{idx}] {para.name}")
    print(f"      statements: {len(para.statements) if hasattr(para, 'statements') else 'NO ATTR'}")
    if hasattr(para, 'statements') and para.statements:
        for stmt_idx, stmt in enumerate(para.statements):
            print(f"        {stmt_idx}: {type(stmt).__name__}")

print("\n" + "="*80)
print("NOW CHECK CONVERTER")
print("="*80)

from cobol_frontend.converter import COBOLToAilangMultiProgramConverter

converter = COBOLToAilangMultiProgramConverter(debug=False)

# Step through the converter manually
print("\nCalling _collect_paragraphs...")
converter.current_program_name = "TESTMIN"
converter.paragraphs = {}
converter._collect_paragraphs(program)

print(f"\nConverter's self.paragraphs has {len(converter.paragraphs)} entries:")
for name, para in converter.paragraphs.items():
    print(f"\n  {name}:")
    print(f"    type: {type(para)}")
    print(f"    has statements attr: {hasattr(para, 'statements')}")
    if hasattr(para, 'statements'):
        print(f"    statements count: {len(para.statements)}")
        if para.statements:
            print(f"    first statement: {type(para.statements[0]).__name__}")
    else:
        print(f"    attributes: {[a for a in dir(para) if not a.startswith('_')]}")

print("\n" + "="*80)
print("DONE")
print("="*80)

print("\n" + "="*80)
print("TEST create_paragraph_subroutine")
print("="*80)

# Set up converter state
converter.variables = {'PPS_RTC': {'type': 'Integer'}}
converter.current_pool_name = "ESCAL056_VARS"
converter.current_program_name = "TESTMIN"

# Get one paragraph
para = converter.paragraphs['0100_INITIAL_ROUTINE']
print(f"\nParagraph: {para.name}")
print(f"  Statements before conversion: {len(para.statements)}")

# Call create_paragraph_subroutine
subroutine = converter.create_paragraph_subroutine(
    para, 
    converter.variables,
    "TESTMIN",
    "TESTMIN_VARS"
)

print(f"\nSubroutine: {subroutine.name}")
print(f"  Body statements: {len(subroutine.body)}")
for idx, stmt in enumerate(subroutine.body):
    print(f"    [{idx}] {type(stmt).__name__}")

print("\n" + "="*80)