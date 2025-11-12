#!/bin/bash
# Run transpiler with full debug to see what redefines_handler detects

echo "========================================================================"
echo "Running transpiler with debug to see REDEFINES analysis..."
echo "========================================================================"

python3 cobol_frontend/cobol_integration.py --debug cobol_frontend/tests/TEST_REDEFINES.cbl 2>&1 | grep -E "(REDEFINES|BASE|OVERLAY|SKIP|analyze_variable_structure|FIRST_3|BUFFER_80|print_debug_info)" | head -100

echo ""
echo "========================================================================"
echo "Checking if FIRST_3 is detected as overlay..."
echo "========================================================================"

# Add temporary debug to converter_core.py
echo "Creating temp debug file..."
cat > /tmp/test_redefines_check.py << 'EOF'
import sys
sys.path.insert(0, '/mnt/c/Users/Sean/Documents/AiLang/ailang')

from cobol_frontend.parser.parser_core import COBOLMultiProgramParser
from cobol_frontend.cobol_lexer import COBOLLexer
from cobol_frontend.converter.converter_core import COBOLToAilangMultiProgramConverter

# Read test file
with open('cobol_frontend/tests/TEST_REDEFINES.cbl', 'r') as f:
    source = f.read()

# Lex and parse
lexer = COBOLLexer(source)
tokens = lexer.tokenize()
parser = COBOLMultiProgramParser(tokens, debug=False)
ast = parser.parse()

# Convert with debug
converter = COBOLToAilangMultiProgramConverter(debug=True)
converter.convert(ast)

# Check what's in redefines_overlays
print("\n" + "="*70)
print("REDEFINES HANDLER STATE:")
print("="*70)
print(f"Base fields: {list(converter.redefines_handler.redefines_bases.keys())}")
print(f"Overlay fields: {list(converter.redefines_handler.redefines_overlays.keys())}")
print(f"\nIs FIRST_3 an overlay? {converter.redefines_handler.is_overlay_field('FIRST_3')}")
print(f"Is BUFFER_80 a base? {converter.redefines_handler.is_base_field('BUFFER_80')}")

if 'FIRST_3' in converter.redefines_overlays:
    overlay = converter.redefines_overlays['FIRST_3']
    print(f"\nFIRST_3 overlay details:")
    print(f"  Base: {overlay.base_field}")
    print(f"  Offset: {overlay.start_offset}")
    print(f"  Length: {overlay.length}")
EOF

python3 /tmp/test_redefines_check.py