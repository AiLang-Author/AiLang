#!/bin/bash

# DIAGNOSTIC SCRIPT - Check ESCAL056 main subroutine generation
# Run this to see what the converter is actually generating

echo "═══════════════════════════════════════════════════════════════════"
echo "DIAGNOSTIC: Check ESCAL056 transpiler output"
echo "═══════════════════════════════════════════════════════════════════"

# Step 1: Run the transpiler with FULL debug output
echo ""
echo "Step 1: Running transpiler with --debug..."
python3 cobol_frontend/cobol_integration.py \
    cobol_frontend/tests/medicare/ESCAL056 \
    --output /tmp/escal056_test \
    --ailang-only \
    --debug 2>&1 | tee /tmp/escal056_full_debug.log

echo ""
echo "═══════════════════════════════════════════════════════════════════"
echo "Step 2: Check what subroutines were generated..."
echo "═══════════════════════════════════════════════════════════════════"

# Extract subroutine generation messages
echo ""
echo "--- Subroutine Creation Messages ---"
grep -E "Creating main subroutine|Entry point|Generating.*subroutines" /tmp/escal056_full_debug.log

echo ""
echo "--- List of all generated subroutines ---"
grep "^SubRoutine\." /tmp/escal056_test_ESCAL056.ailang || echo "ERROR: No AiLang file generated!"

echo ""
echo "═══════════════════════════════════════════════════════════════════"
echo "Step 3: Check the ESCAL056 main subroutine content..."
echo "═══════════════════════════════════════════════════════════════════"

# Show the ESCAL056 subroutine (not Main)
echo ""
echo "--- SubRoutine.ESCAL056 content (should call first paragraph) ---"
sed -n '/^SubRoutine\.ESCAL056 {/,/^}/p' /tmp/escal056_test_ESCAL056.ailang | head -30

echo ""
echo "═══════════════════════════════════════════════════════════════════"
echo "Step 4: Check if Main() subroutine exists and what it calls..."
echo "═══════════════════════════════════════════════════════════════════"

echo ""
echo "--- SubRoutine.Main content ---"
sed -n '/^SubRoutine\.Main {/,/^}/p' /tmp/escal056_test_ESCAL056.ailang | head -20

echo ""
echo "═══════════════════════════════════════════════════════════════════"
echo "Step 5: Check paragraph generation..."
echo "═══════════════════════════════════════════════════════════════════"

echo ""
echo "--- Paragraph subroutines (should show 7 paragraphs) ---"
grep "^SubRoutine\.ESCAL056_" /tmp/escal056_test_ESCAL056.ailang | head -10

echo ""
echo "═══════════════════════════════════════════════════════════════════"
echo "Step 6: Check converter debug output for entry point detection..."
echo "═══════════════════════════════════════════════════════════════════"

echo ""
echo "--- Entry point detection messages ---"
grep -A 5 "Strategy 1:\|Strategy 2:\|Strategy 3:\|Entry point:" /tmp/escal056_full_debug.log

echo ""
echo "--- Paragraph collection messages ---"
grep -E "PARAGRAPHS|paragraphs|Found.*paragraph" /tmp/escal056_full_debug.log | grep -v "^SubRoutine"

echo ""
echo "═══════════════════════════════════════════════════════════════════"
echo "SUMMARY"
echo "═══════════════════════════════════════════════════════════════════"

echo ""
echo "Expected structure:"
echo "  1. SubRoutine.Main { ... }           - Calls ESCAL056"
echo "  2. SubRoutine.ESCAL056 { ... }       - Calls first paragraph (0000_MAINLINE_CONTROL)"
echo "  3. SubRoutine.ESCAL056_0000_MAINLINE_CONTROL { ... }"
echo "  4. SubRoutine.ESCAL056_0100_INITIAL_ROUTINE { ... }"
echo "  5-9. Other paragraph subroutines..."

echo ""
echo "Actual structure (from generated file):"
if [ -f /tmp/escal056_test_ESCAL056.ailang ]; then
    grep "^SubRoutine\." /tmp/escal056_test_ESCAL056.ailang | nl
else
    echo "  ERROR: No output file generated!"
fi

echo ""
echo "═══════════════════════════════════════════════════════════════════"
echo "Files for manual inspection:"
echo "  Full debug log: /tmp/escal056_full_debug.log"
echo "  Generated code: /tmp/escal056_test_ESCAL056.ailang"
echo "═══════════════════════════════════════════════════════════════════"