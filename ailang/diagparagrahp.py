#!/usr/bin/env python3
"""
Diagnostic script to trace paragraph parsing in ESCAL056
Run this to see what's actually happening during paragraph parsing.
"""

import sys
import os

# Get the actual project path
project_root = '/home/barberb/COBOL-to-Ailang'
sys.path.insert(0, project_root)

from cobol_frontend.cobol_lexer import COBOLLexer
from cobol_frontend.parser import COBOLMultiProgramParser
from pathlib import Path

print("="*70)
print("ESCAL056 PARAGRAPH PARSING DIAGNOSTICS")
print("="*70)

# Try to find ESCAL056 file
possible_paths = [
    Path(project_root) / 'NIST_Dataset' / 'ESCAL056',
    Path(project_root) / 'cobol_frontend' / 'tests' / 'medicare' / 'ESCAL056',
    Path('/mnt/c/Users/Sean/Documents/AiLang/ailang/cobol_frontend/tests/medicare/ESCAL056'),
    Path('cobol_frontend/tests/medicare/ESCAL056'),
]

escal_path = None
for path in possible_paths:
    if path.exists():
        escal_path = path
        break

if not escal_path:
    print(f"\n✗ ERROR: Cannot find ESCAL056 in any of these locations:")
    for p in possible_paths:
        print(f"    - {p}")
    print("\nPlease provide the path as argument:")
    print("  python3 diagparagrahp.py <path-to-ESCAL056>")
    if len(sys.argv) > 1:
        escal_path = Path(sys.argv[1])
        if not escal_path.exists():
            print(f"\n✗ Provided path does not exist: {escal_path}")
            sys.exit(1)
    else:
        sys.exit(1)

with open(escal_path, 'r') as f:
    cobol_source = f.read()

print(f"\n✓ Loaded {escal_path.name}")
print(f"  Size: {len(cobol_source):,} bytes")

# Tokenize
print("\n" + "="*70)
print("TOKENIZING...")
print("="*70)

lexer = COBOLLexer(cobol_source)
tokens = lexer.tokenize()

print(f"\n✓ Tokenized {len(tokens)} tokens")

# Find PROCEDURE DIVISION tokens
proc_idx = None
for i, tok in enumerate(tokens):
    if tok.type.name == 'PROCEDURE':
        proc_idx = i
        break

if proc_idx:
    print(f"\nTokens around PROCEDURE DIVISION (indices {proc_idx} to {proc_idx+50}):")
    for i in range(proc_idx, min(proc_idx+50, len(tokens))):
        tok = tokens[i]
        print(f"  [{i:4d}] {tok.type.name:20s} = '{tok.value}'  (line {tok.line})")

# Parse with debug enabled
print("\n" + "="*70)
print("PARSING...")
print("="*70)

parser = COBOLMultiProgramParser(tokens, debug=True, source_file=escal_path)

try:
    programs = parser.parse_all_programs()
    
    print(f"\n✓ Parsed {len(programs)} program(s)")
    
    for program in programs:
        print(f"\n{'='*70}")
        print(f"Program: {program.program_id}")
        print(f"{'='*70}")
        
        if program.procedure_division:
            print(f"\n  Procedure Division:")
            sections = program.procedure_division.sections if hasattr(program.procedure_division, 'sections') else []
            paragraphs = program.procedure_division.paragraphs
            
            print(f"    Sections: {len(sections)}")
            print(f"    Paragraphs: {len(paragraphs)}")
            
            if sections:
                print(f"\n  Detected Sections:")
                for sec in sections:
                    print(f"    - {sec.name} ({len(sec.paragraphs) if hasattr(sec, 'paragraphs') else 0} paragraphs)")
            
            if paragraphs:
                print(f"\n  Detected Paragraphs:")
                for para in paragraphs:
                    if para.name:
                        print(f"    - {para.name:30s} ({len(para.statements):3d} statements)")
                    else:
                        print(f"    - <unnamed>                    ({len(para.statements):3d} statements)")
            
            if not sections and not paragraphs:
                print(f"\n  ⚠ WARNING: NO SECTIONS OR PARAGRAPHS FOUND!")
        else:
            print(f"  ⚠ NO PROCEDURE DIVISION")

except Exception as e:
    print(f"\n✗ PARSING FAILED!")
    print(f"  Error: {e}")
    import traceback
    traceback.print_exc()

print("\n" + "="*70)
print("DIAGNOSTIC COMPLETE")
print("="*70)