#!/bin/bash
# Find where [RSP] is being written with RAX=0

echo "=== Finding RSP Corruption Bug ==="
echo ""

echo "1. Search for raw MOV [RSP], RAX bytes (0x48, 0x89, 0x04, 0x24):"
grep -rn "0x48.*0x89.*0x04.*0x24" ailang_compiler/modules/ | grep -v ".pyc" | grep -v "__pycache__"
echo ""

echo "2. Search for any emit_bytes writing to [RSP]:"
grep -rn "emit_bytes.*0x04.*0x24" ailang_compiler/modules/ | grep -v ".pyc" | grep -v "__pycache__"
echo ""

echo "3. Check NumberToString for suspicious stack writes:"
grep -A50 "def compile_number_to_string" ailang_compiler/modules/string_ops.py | grep -E "emit_bytes.*RSP|emit_mov.*RSP|0x24"
echo ""

echo "4. Check StringConcat for suspicious stack writes:"
grep -A100 "def compile_string_concat\(" ailang_compiler/modules/string_ops.py | grep -E "emit_bytes.*RSP|emit_mov.*RSP|0x24"
echo ""

echo "5. Check if there's stack manipulation in zero case of NumberToString:"
grep -B5 -A20 "zero_case\|JZ.*zero\|TEST RAX, RAX" ailang_compiler/modules/string_ops.py | head -60
echo ""

echo "6. Look for any place that does MOV [RSP+offset], RAX where offset might be 0:"
grep -rn "MOV \[RSP" ailang_compiler/modules/ | grep -v ".pyc" | grep -v "__pycache__" | grep -v "RSP+"
echo ""

echo "=== CRITICAL: Check for conditional writes based on RAX value ==="
echo "7. Search for patterns like: TEST RAX, RAX followed by conditional store:"
grep -B2 -A5 "TEST RAX, RAX\|emit_test_rax_rax" ailang_compiler/modules/string_ops.py | grep -A5 "emit_bytes.*0x89"
echo ""

echo "=== Analysis Complete ==="
echo "The bug is likely in:"
echo "- NumberToString when converting 0"
echo "- StringConcat when one string is empty/zero"
echo "- Some conditional that only executes when RAX=0"