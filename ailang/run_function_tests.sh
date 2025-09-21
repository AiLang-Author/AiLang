#!/bin/bash

# run_function_tests.sh - Run all function tests and track results
# Usage: ./run_function_tests.sh [test_name]
# If test_name provided, runs only that test

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Test files in order of complexity
TESTS=(
    "test_minimal"
    "test_simplest_function"
    "test_param_incremental"
    "test_params"
    "test_returns"
    "test_edge_cases"
    "test_functions_basic"
    "test_resp_simple"
    "test_library_pattern"
    "test_func_integration"
    "test_functions_comprehensive"
    #"calculator_app"
    "test_pools_comprehensive"
    "test_loops_comprehensive"
    "test_arrays_safe"
    "test_initialize_syntax"
    "test_function_features"
    "test_basic_ops"
    "test_control_flow"
    "test_edge_cases_extended"
    "test_string_ops"
    "test_bitwise_ops"
    "test_complex_expressions"
    "test_loop_patterns"
    "test_error_conditions"
    "test_function_limits"
    "test_mixed_operations"
    "test_performance"
    "calculator_simple"
    "calculator_app"
    "test_debug"
    #"test_string_complete"
    "test_pool_overflow"
    "test_pool_tracking"
    "test_concat_broken"
    "test_user_functions_basic"
    "test_user_functions_advanced"
    "test_user_functions_edge"
    "test_strings_comprehensive"
    "test_fileio_comprehensive"
    "test_fileio_minimal"
    "test_strings_missing"


    # Basic WhileLoop tests (should work)
    "test_loops_comprehensive"
    "test_loop_patterns"
    "test_loop_simple"
    
    # New structure tests
    "test_loopmain_structure"
    "test_subroutine_basic"
    "test_loopactor_basic"
    "test_loop_special"
    "test_loop_structures_full"
    #"test_loop_advanced"
    "test_runtask_comprehensive"
    "test_loop_syntax_forms"
    "test_loop_edge_cases"
)

# Additional test files that might exist
OPTIONAL_TESTS=(
    "test_with_func"
    "test_arrays_basic"
    "test_arrays_simple"
    "test_arrays"
)

# Results tracking
PASSED=0
FAILED=0
SKIPPED=0
RESULTS=()

# Create results directory
mkdir -p test_results

# Function to run a single test
run_test() {
    local test_name=$1
    local test_file="${test_name}.ailang"
    local exec_file="${test_name}_exec"
    local log_file="test_results/${test_name}.log"
    
    echo -e "${BLUE}[TEST]${NC} Running $test_name..."
    
    # Check if test file exists
    if [ ! -f "$test_file" ]; then
        echo -e "${YELLOW}[SKIP]${NC} $test_file not found"
        SKIPPED=$((SKIPPED + 1))
        RESULTS+=("${test_name}: SKIPPED (file not found)")
        return
    fi
    
    # Clean up previous executable
    rm -f "$exec_file"
    
    # Compile
    echo "  Compiling..."
    python3 main.py "$test_file" > "$log_file" 2>&1
    compile_result=$?
    
    if [ $compile_result -ne 0 ]; then
        echo -e "${RED}[FAIL]${NC} $test_name - Compilation failed"
        echo "  See $log_file for details"
        FAILED=$((FAILED + 1))
        RESULTS+=("${test_name}: FAILED (compilation)")
        
        # Show last few lines of error
        echo "  Error snippet:"
        tail -n 5 "$log_file" | sed 's/^/    /'
        return
    fi
    
    # Check if executable was created
    if [ ! -f "$exec_file" ]; then
        echo -e "${RED}[FAIL]${NC} $test_name - No executable produced"
        FAILED=$((FAILED + 1))
        RESULTS+=("${test_name}: FAILED (no executable)")
        return
    fi
    
    # Make executable
    chmod +x "$exec_file"
    
    # Run the test
    echo "  Executing..."
    ./"$exec_file" >> "$log_file" 2>&1
    exec_result=$?
    
    if [ $exec_result -ne 0 ]; then
        # Check for segfault
        if [ $exec_result -eq 139 ]; then
            echo -e "${RED}[FAIL]${NC} $test_name - Segmentation fault"
            RESULTS+=("${test_name}: FAILED (segfault)")
        else
            echo -e "${RED}[FAIL]${NC} $test_name - Runtime error (exit code: $exec_result)"
            RESULTS+=("${test_name}: FAILED (runtime)")
        fi
        echo "  See $log_file for output"
        FAILED=$((FAILED + 1))
        
        # Show last output
        echo "  Last output:"
        tail -n 5 "$log_file" | sed 's/^/    /'
    else
        echo -e "${GREEN}[PASS]${NC} $test_name"
        PASSED=$((PASSED + 1))
        RESULTS+=("${test_name}: PASSED")
        
        # Optionally show output for passed tests
        if [ "$VERBOSE" = "1" ]; then
            echo "  Output:"
            tail -n 10 "$log_file" | sed 's/^/    /'
        fi
    fi
}

# Function to run all tests
run_all_tests() {
    echo -e "${BLUE}===== Function Test Suite =====${NC}"
    echo "Running ${#TESTS[@]} core tests..."
    echo ""
    
    for test in "${TESTS[@]}"; do
        run_test "$test"
        echo ""
    done
    
    # Check for optional tests
    echo -e "${BLUE}Checking optional tests...${NC}"
    for test in "${OPTIONAL_TESTS[@]}"; do
        if [ -f "${test}.ailang" ]; then
            run_test "$test"
            echo ""
        fi
    done
}

# Function to show summary
show_summary() {
    echo -e "${BLUE}===== Test Summary =====${NC}"
    echo -e "Passed:  ${GREEN}$PASSED${NC}"
    echo -e "Failed:  ${RED}$FAILED${NC}"
    echo -e "Skipped: ${YELLOW}$SKIPPED${NC}"
    echo ""
    
    if [ ${#RESULTS[@]} -gt 0 ]; then
        echo "Results:"
        for result in "${RESULTS[@]}"; do
            if [[ $result == *"PASSED"* ]]; then
                echo -e "  ${GREEN}✓${NC} $result"
            elif [[ $result == *"FAILED"* ]]; then
                echo -e "  ${RED}✗${NC} $result"
            else
                echo -e "  ${YELLOW}○${NC} $result"
            fi
        done
    fi
    
    echo ""
    if [ $FAILED -eq 0 ] && [ $PASSED -gt 0 ]; then
        echo -e "${GREEN}All tests passed!${NC}"
        exit 0
    else
        echo -e "${RED}Some tests failed. Check test_results/ for logs.${NC}"
        exit 1
    fi
}

# Function to clean up test artifacts
clean_tests() {
    echo "Cleaning test artifacts..."
    for test in "${TESTS[@]}"; do
        rm -f "${test}_exec"
    done
    for test in "${OPTIONAL_TESTS[@]}"; do
        rm -f "${test}_exec"
    done
    rm -rf test_results/
    echo "Clean complete."
}

# Main script logic
case "$1" in
    clean)
        clean_tests
        exit 0
        ;;
    -v|--verbose)
        VERBOSE=1
        shift
        ;;
    -h|--help)
        echo "Usage: $0 [options] [test_name]"
        echo ""
        echo "Options:"
        echo "  -v, --verbose   Show output for passing tests"
        echo "  clean           Remove all test artifacts"
        echo "  -h, --help      Show this help"
        echo ""
        echo "If test_name is provided, runs only that test."
        echo "Otherwise runs all tests in sequence."
        echo ""
        echo "Available tests:"
        for test in "${TESTS[@]}"; do
            echo "  - $test"
        done
        exit 0
        ;;
    "")
        # No arguments - run all tests
        run_all_tests
        show_summary
        ;;
    *)
        # Specific test name provided
        test_name=$1
        # Remove .ailang extension if provided
        test_name=${test_name%.ailang}
        
        echo -e "${BLUE}Running single test: $test_name${NC}"
        echo ""
        run_test "$test_name"
        echo ""
        
        # Simple summary for single test
        if [ $PASSED -eq 1 ]; then
            echo -e "${GREEN}Test passed!${NC}"
            exit 0
        elif [ $FAILED -eq 1 ]; then
            echo -e "${RED}Test failed!${NC}"
            exit 1
        else
            echo -e "${YELLOW}Test skipped.${NC}"
            exit 2
        fi
        ;;
esac