#!/bin/bash
# trace_import_bug.sh - Trace exactly where the import bug happens

echo "========================================"
echo "  Tracing Import/Alias Bug"
echo "========================================"
echo ""

# 1. Show the exact error
echo "1. THE BUG:"
echo "-----------"
echo "Error: ValueError: Unknown function: NSUHTDNJ_XArray.XCreate"
echo ""
echo "This means:"
echo "  ✓ Import resolver created alias 'NSUHTDNJ_XArray'"
echo "  ✓ Preprocessor replaced 'XArray' with 'NSUHTDNJ_XArray'"
echo "  ✗ Compiler doesn't know what 'NSUHTDNJ_XArray' is"
echo ""

# 2. Find where the alias is created
echo "2. FINDING ALIAS CREATION:"
echo "--------------------------"
echo "Looking for where 'NSUHTDNJ' is generated..."

# Search for random string generation in Python files
grep -r "random" --include="*.py" | grep -i "import\|alias" | head -5

# Search for the specific prefix pattern
grep -r "NSUHTDNJ" . 2>/dev/null | head -5

echo ""

# 3. Check the compilation command flow
echo "3. COMPILATION FLOW:"
echo "--------------------"
echo "When you run: python3 main.py redis_server.ailang"
echo ""

# Look at main.py to understand the flow
if [ -f "main.py" ]; then
    echo "main.py flow (import-related lines):"
    grep -n "import\|resolve\|preprocess\|compile" main.py | head -10
fi

echo ""

# 4. Create a minimal reproducer
echo "4. MINIMAL BUG REPRODUCER:"
echo "--------------------------"

cat > bug_test.ailang << 'EOF'
Import XArrays from "./Library.XArrays.ailang"

Program BugTest {
    Main: {
        // This line causes: "Unknown function: XXXXX_XArray.XCreate"
        arr = XArrays.XArray.XCreate(10)
        ReturnValue(0)
    }
}
EOF

echo "Created bug_test.ailang"
echo ""
echo "Test with: python3 main.py bug_test.ailang 2>&1 | grep -A2 'Unknown function'"
echo ""

# 5. The fix location
echo "5. WHERE TO FIX:"
echo "----------------"
echo "The bug is in one of these places:"
echo ""
echo "A. import_resolver.py (or similar):"
echo "   - Generates 'NSUHTDNJ' prefix"
echo "   - Should save mapping: NSUHTDNJ_XArray -> XArray"
echo "   - Should pass mapping to compiler"
echo ""
echo "B. ailang_compiler.py compile_function_call():"
echo "   - Gets 'NSUHTDNJ_XArray.XCreate'"
echo "   - Should check alias mappings"
echo "   - Currently just says 'Unknown function'"
echo ""
echo "C. The connection between them:"
echo "   - Mappings might not be passed from resolver to compiler"
echo ""

# 6. Suggest the fix
echo "6. THE FIX (probably):"
echo "----------------------"
cat << 'FIXCODE'
# In compile_function_call() before raising "Unknown function":

# Current code (probably):
if function_name not in known_functions:
    raise ValueError(f"Unknown function: {function_name}")

# Fixed code:
if function_name not in known_functions:
    # Check if it's an aliased module function
    if '.' in function_name:
        parts = function_name.split('.')
        if len(parts) >= 2:
            module_part = parts[0]
            # Check if this is an alias (has random prefix)
            if '_' in module_part:
                # Try without the prefix
                _, original_module = module_part.split('_', 1)
                original_function = f"{original_module}.{'.'.join(parts[1:])}"
                if original_function in known_functions:
                    function_name = original_function
                else:
                    raise ValueError(f"Unknown function: {function_name}")
            else:
                raise ValueError(f"Unknown function: {function_name}")
    else:
        raise ValueError(f"Unknown function: {function_name}")
FIXCODE

echo ""
echo "========================================"
echo "  Quick Test"
echo "========================================"
echo ""
echo "Run these commands:"
echo "1. python3 debug_import_system.py"
echo "2. python3 main.py bug_test.ailang"
echo "3. Look for 'Unknown function' error"
echo ""
echo "The error message tells us EXACTLY what needs fixing!"
echo "The Redis server exposed this perfectly!"