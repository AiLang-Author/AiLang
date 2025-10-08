#!/bin/bash
################################################################################
# AILANG COMPILER TEST SUITE RUNNER (Simplified)
################################################################################

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Results tracking
PASSED=0
FAILED=0
SYNTAX_ERROR=0
TOTAL=0
declare -a FAILED_TESTS
declare -a SYNTAX_ERROR_TESTS

TIMESTAMP=$(date '+%Y-%m-%d_%H-%M-%S')
REPORT_FILE="test_report_${TIMESTAMP}.txt"

################################################################################
# Functions
################################################################################

run_test() {
    local test_file="$1"
    
    echo -n "Testing: $test_file ... "
    
    # Try to compile with 10 second timeout
    if timeout 10s python3 main.py "$test_file" > /tmp/ailang_test_$.txt 2>&1; then
        echo -e "${GREEN}PASS${NC}"
        ((PASSED++))
        echo "PASS: $test_file" >> "$REPORT_FILE"
        return 0
    else
        EXIT_CODE=$?
        
        # Check if timeout
        if [ $EXIT_CODE -eq 124 ]; then
            echo -e "${YELLOW}TIMEOUT${NC}"
            ((FAILED++))
            FAILED_TESTS+=("$test_file (TIMEOUT)")
            echo "TIMEOUT: $test_file" >> "$REPORT_FILE"
            return 1
        fi
        
        # Get error output
        ERROR_OUTPUT=$(cat /tmp/ailang_test_$.txt)
        
        # Check if syntax error
        if echo "$ERROR_OUTPUT" | grep -qi "syntax\|parse\|unexpected\|invalid syntax"; then
            echo -e "${RED}SYNTAX ERROR${NC}"
            ((SYNTAX_ERROR++))
            SYNTAX_ERROR_TESTS+=("$test_file")
            echo "SYNTAX_ERROR: $test_file" >> "$REPORT_FILE"
        else
            echo -e "${RED}FAIL${NC}"
            ((FAILED++))
            FAILED_TESTS+=("$test_file")
            echo "FAIL: $test_file" >> "$REPORT_FILE"
        fi
        
        # Save first line of error
        echo "  $(echo "$ERROR_OUTPUT" | head -1)" >> "$REPORT_FILE"
        return 1
    fi
}

################################################################################
# Main
################################################################################

echo "============================================================================="
echo "AILANG COMPILER TEST SUITE"
echo "============================================================================="
echo "Started: $(date)"
echo "Report: $REPORT_FILE"
echo ""

# Initialize report
echo "AILANG Compiler Test Report" > "$REPORT_FILE"
echo "Generated: $(date)" >> "$REPORT_FILE"
echo "========================================" >> "$REPORT_FILE"
echo "" >> "$REPORT_FILE"

# Test directories
TEST_DIRS=(
    "Unit Tests Example Code"
    "compiler dev tests"
)

# Process each directory
for test_dir in "${TEST_DIRS[@]}"; do
    if [ ! -d "$test_dir" ]; then
        echo "Warning: Directory '$test_dir' not found"
        continue
    fi
    
    echo "============================================================================="
    echo "Testing: $test_dir"
    echo "============================================================================="
    
    # Find all .ailang files (use process substitution to preserve counters)
    while IFS= read -r test_file; do
        ((TOTAL++))
        run_test "$test_file"
    done < <(find "$test_dir" -name "*.ailang" -type f | sort)
    
    echo ""
done

# Generate summary
echo "============================================================================="
echo "SUMMARY"
echo "============================================================================="
echo ""
echo "Total Tests:     $TOTAL"
echo -e "${GREEN}Passed:          $PASSED${NC}"
echo -e "${RED}Failed:          $FAILED${NC}"
echo -e "${RED}Syntax Errors:   $SYNTAX_ERROR${NC}"
echo ""

if [ $TOTAL -gt 0 ]; then
    SUCCESS_RATE=$(awk "BEGIN {printf \"%.1f\", ($PASSED * 100) / $TOTAL}")
    echo "Success Rate:    ${SUCCESS_RATE}%"
fi

# Write summary to report
echo "" >> "$REPORT_FILE"
echo "========================================" >> "$REPORT_FILE"
echo "SUMMARY" >> "$REPORT_FILE"
echo "========================================" >> "$REPORT_FILE"
echo "Total Tests:     $TOTAL" >> "$REPORT_FILE"
echo "Passed:          $PASSED" >> "$REPORT_FILE"
echo "Failed:          $FAILED" >> "$REPORT_FILE"
echo "Syntax Errors:   $SYNTAX_ERROR" >> "$REPORT_FILE"

if [ $TOTAL -gt 0 ]; then
    echo "Success Rate:    ${SUCCESS_RATE}%" >> "$REPORT_FILE"
fi

# List problematic tests
if [ ${#FAILED_TESTS[@]} -gt 0 ]; then
    echo ""
    echo "Failed Tests:"
    for test in "${FAILED_TESTS[@]}"; do
        echo "  - $test"
    done
fi

if [ ${#SYNTAX_ERROR_TESTS[@]} -gt 0 ]; then
    echo ""
    echo "Syntax Error Tests:"
    for test in "${SYNTAX_ERROR_TESTS[@]}"; do
        echo "  - $test"
    done
fi

echo ""
echo "Full report saved to: $REPORT_FILE"
echo ""

# Cleanup
rm -f /tmp/ailang_test_$$.txt

# Exit code
if [ $FAILED -gt 0 ] || [ $SYNTAX_ERROR -gt 0 ]; then
    exit 1
else
    exit 0
fi