#!/bin/bash
################################################################################
# COMPARE TEST RUNS
# Compares two test reports to detect regressions or improvements
################################################################################

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <baseline_report.txt> <new_report.txt>"
    echo ""
    echo "Example:"
    echo "  # Run tests before changes"
    echo "  ./run_all_tests.sh"
    echo "  mv test_report_*.txt baseline_report.txt"
    echo ""
    echo "  # Make your changes to compiler"
    echo ""
    echo "  # Run tests after changes"
    echo "  ./run_all_tests.sh"
    echo ""
    echo "  # Compare"
    echo "  ./compare_test_runs.sh baseline_report.txt test_report_*.txt"
    exit 1
fi

BASELINE="$1"
NEW_REPORT="$2"

if [ ! -f "$BASELINE" ]; then
    echo -e "${RED}Error: Baseline report not found: $BASELINE${NC}"
    exit 1
fi

if [ ! -f "$NEW_REPORT" ]; then
    echo -e "${RED}Error: New report not found: $NEW_REPORT${NC}"
    exit 1
fi

echo -e "${BLUE}=============================================================================${NC}"
echo -e "${BLUE}COMPARING TEST RUNS${NC}"
echo -e "${BLUE}=============================================================================${NC}"
echo ""
echo "Baseline:    $BASELINE"
echo "New Report:  $NEW_REPORT"
echo ""

# Extract statistics
get_stat() {
    local file=$1
    local stat=$2
    grep "^$stat:" "$file" | awk '{print $2}'
}

BASELINE_PASSED=$(get_stat "$BASELINE" "Passed")
BASELINE_FAILED=$(get_stat "$BASELINE" "Failed")
BASELINE_SYNTAX=$(get_stat "$BASELINE" "Syntax Errors")
BASELINE_TOTAL=$(get_stat "$BASELINE" "Total Tests")

NEW_PASSED=$(get_stat "$NEW_REPORT" "Passed")
NEW_FAILED=$(get_stat "$NEW_REPORT" "Failed")
NEW_SYNTAX=$(get_stat "$NEW_REPORT" "Syntax Errors")
NEW_TOTAL=$(get_stat "$NEW_REPORT" "Total Tests")

# Calculate differences
DIFF_PASSED=$((NEW_PASSED - BASELINE_PASSED))
DIFF_FAILED=$((NEW_FAILED - BASELINE_FAILED))
DIFF_SYNTAX=$((NEW_SYNTAX - BASELINE_SYNTAX))

echo -e "${BLUE}Statistics:${NC}"
echo ""
printf "%-20s %10s %10s %10s\n" "Metric" "Baseline" "New" "Diff"
echo "------------------------------------------------------------"
printf "%-20s %10s %10s " "Total Tests" "$BASELINE_TOTAL" "$NEW_TOTAL"
if [ "$NEW_TOTAL" -eq "$BASELINE_TOTAL" ]; then
    echo -e "${GREEN}(same)${NC}"
else
    echo -e "${YELLOW}($((NEW_TOTAL - BASELINE_TOTAL)))${NC}"
fi

printf "%-20s %10s %10s " "Passed" "$BASELINE_PASSED" "$NEW_PASSED"
if [ $DIFF_PASSED -gt 0 ]; then
    echo -e "${GREEN}(+$DIFF_PASSED)${NC}"
elif [ $DIFF_PASSED -lt 0 ]; then
    echo -e "${RED}($DIFF_PASSED)${NC}"
else
    echo -e "${GREEN}(same)${NC}"
fi

printf "%-20s %10s %10s " "Failed" "$BASELINE_FAILED" "$NEW_FAILED"
if [ $DIFF_FAILED -gt 0 ]; then
    echo -e "${RED}(+$DIFF_FAILED)${NC}"
elif [ $DIFF_FAILED -lt 0 ]; then
    echo -e "${GREEN}($DIFF_FAILED)${NC}"
else
    echo -e "${GREEN}(same)${NC}"
fi

printf "%-20s %10s %10s " "Syntax Errors" "$BASELINE_SYNTAX" "$NEW_SYNTAX"
if [ $DIFF_SYNTAX -gt 0 ]; then
    echo -e "${RED}(+$DIFF_SYNTAX)${NC}"
elif [ $DIFF_SYNTAX -lt 0 ]; then
    echo -e "${GREEN}($DIFF_SYNTAX)${NC}"
else
    echo -e "${GREEN}(same)${NC}"
fi

echo ""

# Find newly failing tests (regressions)
BASELINE_PASSED_TESTS=$(grep "^PASS:" "$BASELINE" | cut -d' ' -f2- | sort)
NEW_FAILED_TESTS=$(grep "^FAIL:" "$NEW_REPORT" | cut -d' ' -f2- | sort)

REGRESSIONS=$(comm -12 <(echo "$BASELINE_PASSED_TESTS") <(echo "$NEW_FAILED_TESTS"))

if [ -n "$REGRESSIONS" ]; then
    echo -e "${RED}⚠ REGRESSIONS DETECTED (previously passing tests now fail):${NC}"
    echo "$REGRESSIONS" | while read -r test; do
        echo -e "${RED}  - $test${NC}"
    done
    echo ""
    exit 1
else
    echo -e "${GREEN}✓ No regressions detected${NC}"
fi

# Find newly passing tests (improvements)
BASELINE_FAILED_TESTS=$(grep "^FAIL:" "$BASELINE" | cut -d' ' -f2- | sort)
NEW_PASSED_TESTS=$(grep "^PASS:" "$NEW_REPORT" | cut -d' ' -f2- | sort)

IMPROVEMENTS=$(comm -12 <(echo "$BASELINE_FAILED_TESTS") <(echo "$NEW_PASSED_TESTS"))

if [ -n "$IMPROVEMENTS" ]; then
    echo -e "${GREEN}✓ IMPROVEMENTS (previously failing tests now pass):${NC}"
    echo "$IMPROVEMENTS" | while read -r test; do
        echo -e "${GREEN}  + $test${NC}"
    done
    echo ""
fi

echo ""
if [ $DIFF_PASSED -gt 0 ] && [ -z "$REGRESSIONS" ]; then
    echo -e "${GREEN}=============================================================================${NC}"
    echo -e "${GREEN}✓ TEST SUITE IMPROVED! (+$DIFF_PASSED passing, no regressions)${NC}"
    echo -e "${GREEN}=============================================================================${NC}"
    exit 0
elif [ $DIFF_PASSED -eq 0 ] && [ $DIFF_FAILED -eq 0 ] && [ -z "$REGRESSIONS" ]; then
    echo -e "${GREEN}=============================================================================${NC}"
    echo -e "${GREEN}✓ TEST SUITE STABLE (no changes)${NC}"
    echo -e "${GREEN}=============================================================================${NC}"
    exit 0
else
    echo -e "${RED}=============================================================================${NC}"
    echo -e "${RED}⚠ CHANGES DETECTED - REVIEW REQUIRED${NC}"
    echo -e "${RED}=============================================================================${NC}"
    exit 1
fi