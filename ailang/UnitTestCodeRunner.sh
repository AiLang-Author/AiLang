#!/bin/bash

echo "=================================="
echo "AILANG COMPREHENSIVE TEST SUITE"
echo "=================================="
echo ""

test_dir="UnitTestCode"
compile_pass=0
compile_fail=0
run_pass=0
run_fail=0
run_crash=0

# Step 1: Compile all tests
echo "========================================="
echo "STEP 1: COMPILING ALL TESTS"
echo "========================================="
echo ""

for file in "$test_dir"/*.ailang; do
    if [ ! -f "$file" ]; then
        continue
    fi
    
    basename=$(basename "$file" .ailang)
    echo "Compiling: $basename"
    
    # Compile
    python3 main.py "$file" > /tmp/compile_${basename}.log 2>&1
    compile_status=$?
    
    if [ $compile_status -ne 0 ]; then
        echo "  ❌ COMPILE FAILED"
        echo "  Last 10 lines:"
        tail -10 /tmp/compile_${basename}.log | sed 's/^/    /'
        compile_fail=$((compile_fail + 1))
    else
        echo "  ✅ COMPILED"
        compile_pass=$((compile_pass + 1))
    fi
    echo ""
done

echo "========================================="
echo "COMPILATION SUMMARY"
echo "========================================="
echo "✅ Compiled: $compile_pass"
echo "❌ Failed:   $compile_fail"
echo "Total:       $((compile_pass + compile_fail))"
echo ""
echo ""

# Step 2: Run all compiled tests
echo "========================================="
echo "STEP 2: RUNNING ALL COMPILED TESTS"
echo "========================================="
echo ""

cd "$test_dir"

for exec in *_exec; do
    if [ ! -f "$exec" ]; then
        continue
    fi
    
    basename="${exec%_exec}"
    echo "Running: $basename"
    
    # Run with timeout
    timeout 5s ./"$exec" > /tmp/run_${basename}.log 2>&1
    run_status=$?
    
    if [ $run_status -eq 139 ]; then
        echo "  💥 SEGFAULT"
        run_crash=$((run_crash + 1))
        echo "  Last 20 lines:"
        tail -20 /tmp/run_${basename}.log | sed 's/^/    /'
    elif [ $run_status -eq 124 ]; then
        echo "  ⏱️  TIMEOUT"
        run_crash=$((run_crash + 1))
        echo "  Last 20 lines:"
        tail -20 /tmp/run_${basename}.log | sed 's/^/    /'
    elif [ $run_status -ne 0 ]; then
        echo "  ❌ RUNTIME ERROR (exit code: $run_status)"
        run_fail=$((run_fail + 1))
        echo "  Last 10 lines:"
        tail -10 /tmp/run_${basename}.log | sed 's/^/    /'
    else
        echo "  ✅ PASSED"
        run_pass=$((run_pass + 1))
    fi
    echo ""
done

cd ..

# Step 3: Final Summary
echo "========================================="
echo "FINAL TEST SUMMARY"
echo "========================================="
echo ""
echo "COMPILATION:"
echo "  ✅ Passed:  $compile_pass"
echo "  ❌ Failed:  $compile_fail"
echo ""
echo "EXECUTION:"
echo "  ✅ Passed:  $run_pass"
echo "  ❌ Failed:  $run_fail"
echo "  💥 Crashed: $run_crash"
echo ""
echo "OVERALL:"
total_tests=$((compile_pass + compile_fail))
successful_runs=$run_pass
echo "  Tests:      $total_tests"
echo "  Success:    $successful_runs"

# Calculate success rate
if [ $total_tests -gt 0 ]; then
    success_rate=$((successful_runs * 100 / total_tests))
    echo "  Rate:       ${success_rate}%"
fi

echo "========================================="

# Exit with error if any tests failed
if [ $compile_fail -gt 0 ] || [ $run_fail -gt 0 ] || [ $run_crash -gt 0 ]; then
    echo ""
    echo "⚠️  ISSUES DETECTED:"
    [ $compile_fail -gt 0 ] && echo "  - $compile_fail compilation failures"
    [ $run_fail -gt 0 ] && echo "  - $run_fail runtime failures"
    [ $run_crash -gt 0 ] && echo "  - $run_crash crashes/timeouts"
    exit 1
else
    echo ""
    echo "🎉 ALL TESTS PASSED!"
    exit 0
fi