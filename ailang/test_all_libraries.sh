#!/bin/bash

# test_all_libraries.sh
# Tests all AILANG libraries for compilation errors

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Results tracking
TOTAL=0
PASSED=0
FAILED=0
RESULTS_FILE="library_test_results.txt"

# Clear previous results
> "$RESULTS_FILE"

echo "========================================" | tee -a "$RESULTS_FILE"
echo "AILANG LIBRARY COMPILATION TEST" | tee -a "$RESULTS_FILE"
echo "$(date)" | tee -a "$RESULTS_FILE"
echo "========================================" | tee -a "$RESULTS_FILE"
echo "" | tee -a "$RESULTS_FILE"

# Change to the Librarys directory
cd Librarys || { echo "Error: Librarys directory not found"; exit 1; }

# Test each library file
for lib_file in Library.*.ailang; do
    if [ -f "$lib_file" ]; then
        TOTAL=$((TOTAL + 1))
        lib_name="${lib_file#Library.}"
        lib_name="${lib_name%.ailang}"
        
        echo -e "${BLUE}[$TOTAL] Testing:${NC} $lib_name" | tee -a "$RESULTS_FILE"
        
        # Try to compile the library
        output=$(python3 ../main.py "$lib_file" 2>&1)
        exit_code=$?
        
        if [ $exit_code -eq 0 ]; then
            # Check if compilation actually succeeded
            if echo "$output" | grep -q "SUCCESS!"; then
                echo -e "  ${GREEN}✓ PASSED${NC}" | tee -a "$RESULTS_FILE"
                PASSED=$((PASSED + 1))
                
                # Clean up the executable
                exec_file="${lib_file%.ailang}_exec"
                rm -f "$exec_file"
            else
                echo -e "  ${YELLOW}⚠ PARTIAL${NC}" | tee -a "$RESULTS_FILE"
                echo "  Output: $output" >> "$RESULTS_FILE"
                FAILED=$((FAILED + 1))
            fi
        else
            echo -e "  ${RED}✗ FAILED${NC}" | tee -a "$RESULTS_FILE"
            FAILED=$((FAILED + 1))
            
            # Extract and log the error
            error_msg=$(echo "$output" | grep -E "ERROR:|Parse error|AttributeError|ValueError" | head -1)
            if [ -n "$error_msg" ]; then
                echo "  Error: $error_msg" | tee -a "$RESULTS_FILE"
            fi
            
            # Check for specific syntax issues
            if echo "$output" | grep -q "Parse error"; then
                parse_error=$(echo "$output" | grep "Parse error" | head -1)
                echo "  Parse: $parse_error" >> "$RESULTS_FILE"
            fi
            
            if echo "$output" | grep -q "Unexpected token"; then
                token_error=$(echo "$output" | grep "Unexpected token" | head -1)
                echo "  Token: $token_error" >> "$RESULTS_FILE"
            fi
        fi
        echo "" | tee -a "$RESULTS_FILE"
    fi
done

# Summary
echo "========================================" | tee -a "$RESULTS_FILE"
echo "SUMMARY" | tee -a "$RESULTS_FILE"
echo "========================================" | tee -a "$RESULTS_FILE"
echo "Total Libraries: $TOTAL" | tee -a "$RESULTS_FILE"
echo -e "${GREEN}Passed: $PASSED${NC}" | tee -a "$RESULTS_FILE"
echo -e "${RED}Failed: $FAILED${NC}" | tee -a "$RESULTS_FILE"

if [ $TOTAL -gt 0 ]; then
    success_rate=$((PASSED * 100 / TOTAL))
    echo "Success Rate: ${success_rate}%" | tee -a "$RESULTS_FILE"
fi

echo "" | tee -a "$RESULTS_FILE"
echo "Detailed results saved to: library_test_results.txt" | tee -a "$RESULTS_FILE"

# List failed libraries for quick reference
if [ $FAILED -gt 0 ]; then
    echo "" | tee -a "$RESULTS_FILE"
    echo "Failed Libraries:" | tee -a "$RESULTS_FILE"
    echo "----------------" | tee -a "$RESULTS_FILE"
    
    for lib_file in Library.*.ailang; do
        if [ -f "$lib_file" ]; then
            output=$(python3 ../main.py "$lib_file" 2>&1)
            if [ $? -ne 0 ] || ! echo "$output" | grep -q "SUCCESS!"; then
                lib_name="${lib_file#Library.}"
                lib_name="${lib_name%.ailang}"
                echo "- $lib_name" | tee -a "$RESULTS_FILE"
            fi
        fi
    done
fi

exit $FAILED