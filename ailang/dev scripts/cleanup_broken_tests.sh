#!/bin/bash
################################################################################
# CLEANUP BROKEN TESTS
# Identifies and optionally deletes tests with syntax errors
################################################################################

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Find the most recent test report
LATEST_REPORT=$(ls -t test_report_*.txt 2>/dev/null | head -1)

if [ -z "$LATEST_REPORT" ]; then
    echo -e "${RED}Error: No test report found. Run ./run_all_tests.sh first${NC}"
    exit 1
fi

echo -e "${BLUE}Using report: $LATEST_REPORT${NC}"
echo ""

# Extract syntax error tests
SYNTAX_ERRORS=$(grep "^SYNTAX_ERROR:" "$LATEST_REPORT" | cut -d' ' -f2-)

if [ -z "$SYNTAX_ERRORS" ]; then
    echo -e "${GREEN}No syntax error tests found! All tests are valid.${NC}"
    exit 0
fi

# Count syntax errors
SYNTAX_COUNT=$(echo "$SYNTAX_ERRORS" | wc -l)

echo -e "${YELLOW}Found $SYNTAX_COUNT test(s) with syntax errors:${NC}"
echo ""
echo "$SYNTAX_ERRORS"
echo ""

# Ask user what to do
echo -e "${YELLOW}What would you like to do?${NC}"
echo "1) Delete all syntax error tests (PERMANENT)"
echo "2) Move them to a 'broken_tests' folder (safe)"
echo "3) Just show me the list (no action)"
echo "4) Cancel"
echo ""
read -p "Choice [1-4]: " choice

case $choice in
    1)
        echo ""
        echo -e "${RED}WARNING: This will permanently delete $SYNTAX_COUNT test files!${NC}"
        read -p "Are you sure? Type 'yes' to confirm: " confirm
        if [ "$confirm" = "yes" ]; then
            while IFS= read -r test_file; do
                if [ -f "$test_file" ]; then
                    echo "Deleting: $test_file"
                    rm "$test_file"
                fi
            done <<< "$SYNTAX_ERRORS"
            echo -e "${GREEN}Deleted $SYNTAX_COUNT test files${NC}"
        else
            echo "Cancelled."
        fi
        ;;
    2)
        mkdir -p broken_tests
        echo ""
        while IFS= read -r test_file; do
            if [ -f "$test_file" ]; then
                test_name=$(basename "$test_file")
                echo "Moving: $test_file -> broken_tests/$test_name"
                mv "$test_file" "broken_tests/$test_name"
            fi
        done <<< "$SYNTAX_ERRORS"
        echo -e "${GREEN}Moved $SYNTAX_COUNT test files to broken_tests/${NC}"
        ;;
    3)
        echo "No action taken."
        ;;
    4)
        echo "Cancelled."
        ;;
    *)
        echo -e "${RED}Invalid choice${NC}"
        exit 1
        ;;
esac