#!/bin/bash
# Search for OLD-TEST-CASE and related patterns in medicare folder

echo "========================================"
echo "SEARCHING MEDICARE FOLDER FOR OLD-TEST"
echo "========================================"
echo ""

# Search for OLD-TEST-CASE (case insensitive)
echo "📂 Searching for 'OLD-TEST-CASE' (case insensitive)..."
grep -rin "old-test-case" cobol_frontend/tests/medicare/ 2>/dev/null
echo ""

# Search for OLD TEST (without hyphen)
echo "📂 Searching for 'OLD TEST' (case insensitive)..."
grep -rin "old.*test" cobol_frontend/tests/medicare/ 2>/dev/null | grep -i "old.*test" | head -20
echo ""

# Search for any 88 level TEST variables
echo "📂 Searching for 88-level TEST variables..."
grep -rin "88.*test" cobol_frontend/tests/medicare/ 2>/dev/null | head -20
echo ""

# Search for TEST-CASE anywhere
echo "📂 Searching for 'TEST-CASE'..."
grep -rin "test-case" cobol_frontend/tests/medicare/ 2>/dev/null
echo ""

# List all files in medicare directory
echo "📂 All files in medicare directory:"
ls -lh cobol_frontend/tests/medicare/
echo ""

# Show first 20 lines of BILLCPY
echo "📂 First 30 lines of BILLCPY (if exists):"
if [ -f "cobol_frontend/tests/medicare/BILLCPY" ]; then
    head -30 cobol_frontend/tests/medicare/BILLCPY
else
    echo "   BILLCPY not found"
fi
echo ""

# Show first 20 lines of WAGECPY
echo "📂 First 30 lines of WAGECPY (if exists):"
if [ -f "cobol_frontend/tests/medicare/WAGECPY" ]; then
    head -30 cobol_frontend/tests/medicare/WAGECPY
else
    echo "   WAGECPY not found"
fi