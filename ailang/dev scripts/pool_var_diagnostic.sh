#!/bin/bash

# Pool Variable Diagnostic Script
# Tracks the exact issue with pool variable compilation

echo "========================================"
echo "POOL VARIABLE DIAGNOSTIC"
echo "========================================"

AILANG_DIR="/mnt/c/Users/Sean/Documents/Ailang/ailang"
cd "$AILANG_DIR"

# Step 1: Create minimal test
echo -e "\n[STEP 1] Creating minimal test case..."
cat > /tmp/minimal_pool_test.ailang << 'EOF'
// Absolute minimal pool test
FixedPool.TestPool {
    "test_var": Initialize=0
}

SubRoutine.Main {
    // Simple store
    TestPool.test_var = 42
    
    // Simple read
    value = TestPool.test_var
    PrintNumber(value)
}

RunTask(Main)
EOF

echo "Test case created: /tmp/minimal_pool_test.ailang"

# Step 2: Try to compile with verbose output
echo -e "\n[STEP 2] Compiling with maximum debug output..."
python3 ailang_compiler/ailang_compiler.py /tmp/minimal_pool_test.ailang 2>&1 | tee /tmp/compile_verbose.log

# Step 3: Check if binary was created
echo -e "\n[STEP 3] Checking for output binary..."
if [ -f "/tmp/minimal_pool_test" ]; then
    echo "Binary created successfully!"
    
    # Step 4: Run and check output
    echo -e "\n[STEP 4] Running binary..."
    /tmp/minimal_pool_test
    echo -e "\nExit code: $?"
    
    # Step 5: Check binary size and basic structure
    echo -e "\n[STEP 5] Binary analysis..."
    ls -la /tmp/minimal_pool_test
    file /tmp/minimal_pool_test
    
    # Step 6: Look for pool-related strings in binary
    echo -e "\n[STEP 6] Searching for pool markers in binary..."
    strings /tmp/minimal_pool_test | grep -i "pool\|test_var" || echo "No pool strings found"
    
    # Step 7: Check for StoreValue pattern in hex
    echo -e "\n[STEP 7] Looking for MOV patterns..."
    xxd /tmp/minimal_pool_test | grep "48 89" | head -5
    
else
    echo "ERROR: Binary not created!"
    echo -e "\n[ERROR DETAILS] Last 20 lines of compile output:"
    tail -20 /tmp/compile_verbose.log
fi

# Step 8: Search compiler source for pool handling
echo -e "\n[STEP 8] Checking compiler pool handling..."
echo "Looking for pool variable handling in compiler..."

# Check if pool variables are being discovered
grep -n "discover_pool_variables\|pool_var" ailang_compiler/modules/memory_manager.py | head -5

# Check for pool table allocation
grep -n "allocate_pool_table\|pool_table" ailang_compiler/modules/memory_manager.py | head -5

# Step 9: Create an even simpler test without pools
echo -e "\n[STEP 9] Testing without pool variables..."
cat > /tmp/no_pool_test.ailang << 'EOF'
// Test without pools
SubRoutine.Main {
    x = 42
    PrintNumber(x)
}
RunTask(Main)
EOF

python3 ailang_compiler/ailang_compiler.py /tmp/no_pool_test.ailang 2>&1 | tail -5

if [ -f "/tmp/no_pool_test" ]; then
    echo "Non-pool test compiled successfully"
    /tmp/no_pool_test
else
    echo "ERROR: Even non-pool test failed to compile!"
fi

echo -e "\n========================================"
echo "DIAGNOSTIC COMPLETE"
echo "Check /tmp/compile_verbose.log for full output"
echo "========================================"