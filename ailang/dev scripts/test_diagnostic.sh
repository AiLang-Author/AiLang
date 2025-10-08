#!/bin/bash
################################################################################
# DIAGNOSTIC - Check test setup
################################################################################

echo "=== AILANG Test Suite Diagnostic ==="
echo ""
echo "Current directory: $(pwd)"
echo ""

echo "Checking for test directories:"
echo ""

TEST_DIRS=(
    "Unit Tests Example Code"
    "compiler dev tests"
)

for dir in "${TEST_DIRS[@]}"; do
    if [ -d "$dir" ]; then
        count=$(find "$dir" -name "*.ailang" -type f 2>/dev/null | wc -l)
        echo "✓ Found: $dir ($count .ailang files)"
        
        # Show first 5 files as examples
        if [ $count -gt 0 ]; then
            echo "  Sample files:"
            find "$dir" -name "*.ailang" -type f | head -5 | while read f; do
                echo "    - $f"
            done
        fi
    else
        echo "✗ NOT FOUND: $dir"
    fi
    echo ""
done

echo "Checking for main.py:"
if [ -f "main.py" ]; then
    echo "✓ Found main.py"
else
    echo "✗ NOT FOUND: main.py"
    echo "  (Make sure you're running from the ailang project root)"
fi
echo ""

echo "Total .ailang files in current directory and subdirectories:"
find . -name "*.ailang" -type f 2>/dev/null | wc -l
echo ""

echo "Directory structure:"
ls -la | head -20