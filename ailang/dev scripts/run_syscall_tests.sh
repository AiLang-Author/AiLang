#!/bin/bash
# Comprehensive syscall test runner

cd "$(dirname "$0")/tests/syscalls"

echo "🔨 Compiling all syscall tests..."
echo ""

# Compile all test files
for test in test_*.ailang; do
    basename="${test%.ailang}"
    echo "  📝 Compiling $test..."
    python3 ../../main.py "$test" 2>&1 | grep -E "(SUCCESS|FAILED|ERROR)" || echo "    ✓ Compiled"
done

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "🚀 Running all syscall tests..."
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

# Run all compiled tests
for exec in test_*_exec; do
    if [ -f "$exec" ] && [ -x "$exec" ]; then
        echo "┌────────────────────────────────────────┐"
        echo "│  Running: $exec"
        echo "└────────────────────────────────────────┘"
        ./"$exec"
        exit_code=$?
        if [ $exit_code -eq 0 ]; then
            echo "✅ Exit code: $exit_code"
        else
            echo "⚠️  Exit code: $exit_code"
        fi
        echo ""
    fi
done

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "✅ All tests complete!"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"