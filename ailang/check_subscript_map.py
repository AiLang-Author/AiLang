#!/usr/bin/env python3
import sys
sys.path.insert(0, '/mnt/c/Users/Sean/Documents/AiLang/ailang')

from cobol_frontend.parser.parser_core import COBOLMultiProgramParser
from cobol_frontend.cobol_lexer import COBOLLexer
from cobol_frontend.converter.converter_core import COBOLToAilangMultiProgramConverter

with open('cobol_frontend/tests/EXEC85.cbl', 'r') as f:
    source = f.read()

lexer = COBOLLexer(source)
tokens = lexer.tokenize()
parser = COBOLMultiProgramParser(tokens, debug=False)
ast = parser.parse_all_programs()  # FIX: Changed from parse() to parse_all_programs()

converter = COBOLToAilangMultiProgramConverter(debug=False)
converter.convert(ast)

print("Checking subscript_parent_map:")
if 'WG_MODULE_SELECTED' in converter.subscript_parent_map:
    parent = converter.subscript_parent_map['WG_MODULE_SELECTED']
    print(f"  WG_MODULE_SELECTED -> {parent}")
else:
    print("  WG_MODULE_SELECTED not in map")

print("\nChecking variables dict:")
if 'WG_MODULE_SELECTED' in converter.variables:
    print(f"  WG_MODULE_SELECTED in variables: {converter.variables['WG_MODULE_SELECTED']}")
else:
    print("  WG_MODULE_SELECTED NOT in variables")

if 'FILLER' in converter.variables:
    print(f"  FILLER in variables: occurs={converter.variables['FILLER'].get('occurs')}")
else:
    print("  FILLER NOT in variables")