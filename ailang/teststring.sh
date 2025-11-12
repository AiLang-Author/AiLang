#!/bin/bash
#
# COBOL String Library Test Script
# Tests the library standalone before transpiler integration
#

set -e  # Exit on error

echo "========================================================================"
echo "  COBOL String Library - Test Suite"
echo "========================================================================"
echo ""

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Check if library exists
if [ ! -f "Library/COBOL_String.ailang" ]; then
    echo -e "${RED}❌ ERROR: Library/COBOL_String.ailang not found${NC}"
    echo "Please copy the library file first:"
    echo "  cp Library.COBOL_String.ailang Library/"
    exit 1
fi

echo -e "${GREEN}✅ Library file found${NC}"
echo ""

# Check if test file exists
if [ ! -f "test_cobol_string.ailang" ]; then
    echo -e "${RED}❌ ERROR: test_cobol_string.ailang not found${NC}"
    echo "Please create the test file first"
    exit 1
fi

echo -e "${GREEN}✅ Test file found${NC}"
echo ""

# Compile the test
echo "========================================================================"
echo "  Step 1: Compiling Test Suite"
echo "========================================================================"
echo ""

python3 main.py -D3 -P test_cobol_string.ailang

if [ $? -ne 0 ]; then
    echo ""
    echo -e "${RED}❌ COMPILATION FAILED${NC}"
    echo "Fix compilation errors before running tests"
    exit 1
fi

echo ""
echo -e "${GREEN}✅ Compilation successful${NC}"
echo ""

# Run the test
echo "========================================================================"
echo "  Step 2: Running Tests"
echo "========================================================================"
echo ""

./test_cobol_string

TEST_RESULT=$?

echo ""
echo "========================================================================"
echo "  Step 3: Results"
echo "========================================================================"
echo ""

if [ $TEST_RESULT -eq 0 ]; then
    echo -e "${GREEN}✅ ALL TESTS PASSED!${NC}"
    echo ""
    echo "The library is working correctly and ready for integration."
    echo ""
    echo "Next steps:"
    echo "  1. Add string_optimizer.py to cobol_frontend/converter/"
    echo "  2. Integrate optimizer into converter_core.py"
    echo "  3. Add 'COBOL_String' to required libraries list"
    echo "  4. Retranspile ESCAL056"
    echo ""
    exit 0
else
    echo -e "${RED}❌ SOME TESTS FAILED${NC}"
    echo ""
    echo "Fix the failing tests before integration."
    echo "Check the test output above for details."
    echo ""
    exit 1
fi