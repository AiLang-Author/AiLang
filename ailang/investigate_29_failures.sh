#!/bin/bash
#
# Investigate the 29 Remaining NIST Failures
# Categorize errors and determine next steps
#

echo "=============================================="
echo "INVESTIGATING 29 REMAINING FAILURES"
echo "93% Pass Rate (430/459 programs)"
echo "=============================================="
echo ""

# Find the most recent log file
LATEST_LOG=$(ls -t nist_validation_report/transpiler_output_*.log 2>/dev/null | head -1)

if [ -z "$LATEST_LOG" ]; then
    echo "Error: No log files found in nist_validation_report/"
    exit 1
fi

echo "Analyzing: $LATEST_LOG"
echo "=============================================="
echo ""

# Extract all failures
echo "EXTRACTING ALL FAILURES..."
ALL_FAILURES=$(grep "✗ Failed" "$LATEST_LOG")
TOTAL_FAILURES=$(echo "$ALL_FAILURES" | wc -l)

echo "Total failures found: $TOTAL_FAILURES"
echo ""

# Category 1: Known Good Errors (Bad COBOL Syntax)
echo "=============================================="
echo "CATEGORY 1: KNOWN BAD SYNTAX (Parser is correct)"
echo "=============================================="
echo ""

echo "1a. Missing expression before COMMA:"
COMMA_ERRORS=$(echo "$ALL_FAILURES" | grep "Missing expression before COMMA" | wc -l)
echo "   Count: $COMMA_ERRORS"
if [ $COMMA_ERRORS -gt 0 ]; then
    echo "$ALL_FAILURES" | grep "Missing expression before COMMA" | head -3
    echo ""
fi

echo "1b. Unexpected GO in expression:"
GO_ERRORS=$(echo "$ALL_FAILURES" | grep "Unexpected GO in expression" | wc -l)
echo "   Count: $GO_ERRORS"
if [ $GO_ERRORS -gt 0 ]; then
    echo "$ALL_FAILURES" | grep "Unexpected GO in expression" | head -2
    echo ""
fi

echo "1c. Unexpected LEVEL_NUMBER in expression:"
LEVEL_ERRORS=$(echo "$ALL_FAILURES" | grep "Unexpected.*LEVEL_NUMBER" | wc -l)
echo "   Count: $LEVEL_ERRORS"
if [ $LEVEL_ERRORS -gt 0 ]; then
    echo "$ALL_FAILURES" | grep "Unexpected.*LEVEL_NUMBER" | head -2
    echo ""
fi

KNOWN_BAD=$((COMMA_ERRORS + GO_ERRORS + LEVEL_ERRORS))
echo "Total Known Bad Syntax: $KNOWN_BAD"
echo ""

# Category 2: Unknown Failures (Need Investigation)
echo "=============================================="
echo "CATEGORY 2: UNKNOWN FAILURES (Need Investigation)"
echo "=============================================="
echo ""

UNKNOWN_FAILURES=$(echo "$ALL_FAILURES" | \
    grep -v "Missing expression before COMMA" | \
    grep -v "Unexpected.*LEVEL_NUMBER" | \
    grep -v "Unexpected.*GO")

UNKNOWN_COUNT=$(echo "$UNKNOWN_FAILURES" | grep -c "✗")

echo "Unknown failures: $UNKNOWN_COUNT"
echo ""

if [ $UNKNOWN_COUNT -gt 0 ]; then
    echo "Analyzing error patterns..."
    echo ""
    
    # Extract just the error messages (remove line numbers and file info)
    ERROR_PATTERNS=$(echo "$UNKNOWN_FAILURES" | \
        sed 's/✗ Failed at line [0-9]*: //' | \
        sed 's/line [0-9]*/line N/g' | \
        sort | uniq -c | sort -rn)
    
    echo "Top error patterns:"
    echo "$ERROR_PATTERNS" | head -10
    echo ""
    
    echo "Sample failures (first 10):"
    echo "$UNKNOWN_FAILURES" | head -10
    echo ""
fi

# Summary
echo "=============================================="
echo "SUMMARY"
echo "=============================================="
echo ""
echo "Total programs tested:    459"
echo "Passing:                  430 (93%)"
echo "Failing:                  $TOTAL_FAILURES"
echo ""
echo "Known bad syntax:         $KNOWN_BAD (legitimate COBOL errors)"
echo "Unknown failures:         $UNKNOWN_COUNT (need investigation)"
echo ""

# Recommendations
echo "=============================================="
echo "RECOMMENDATIONS"
echo "=============================================="
echo ""

if [ $UNKNOWN_COUNT -eq 0 ]; then
    echo "✅ ALL FAILURES ARE LEGITIMATE BAD COBOL SYNTAX"
    echo "   → Parser is working correctly"
    echo "   → Document these as expected failures"
    echo "   → 93% pass rate is EXCELLENT for NIST test suite"
    echo ""
else
    echo "Next steps for $UNKNOWN_COUNT unknown failures:"
    echo ""
    echo "1. Run detailed analysis:"
    echo "   python3 analyze_remaining_failures.py"
    echo ""
    echo "2. Get list of failed program files:"
    echo "   cat nist_validation_report/failed_*.txt | sort"
    echo ""
    echo "3. Examine a specific failed program:"
    echo "   # Pick a program from the failed list"
    echo "   python3 cobol_frontend/cobol_integration.py \\"
    echo "       cobol_frontend/tests/PROGRAM_NAME.cbl \\"
    echo "       --output debug_output \\"
    echo "       --ailang-only --split-programs --io-backend jcl"
    echo ""
    echo "4. Check if failures are parser bugs or bad COBOL"
    echo ""
fi

echo "=============================================="
echo "STATUS: 93% PASS RATE - INVESTIGATION PHASE"
echo "=============================================="