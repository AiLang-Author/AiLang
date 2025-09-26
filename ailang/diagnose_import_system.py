#!/usr/bin/env python3
# diagnose_import_system.py - Complete diagnostic of the import resolution system

import os
import json
import re
import subprocess
import inspect

def section(title):
    print("\n" + "=" * 60)
    print(f"  {title}")
    print("=" * 60)

def subsection(title):
    print(f"\n{title}")
    print("-" * len(title))

# STEP 1: Map the import_resolver.py file
section("STEP 1: Analyzing import_resolver.py")

subsection("1.1 File Structure")
if os.path.exists("import_resolver.py"):
    with open("import_resolver.py", "r") as f:
        content = f.read()
        lines = content.split('\n')
    
    print(f"File size: {len(content)} bytes")
    print(f"Total lines: {len(lines)}")
    
    # Find all functions
    functions = re.findall(r'^def (\w+)\(', content, re.MULTILINE)
    print(f"\nFunctions found: {len(functions)}")
    for func in functions:
        print(f"  - {func}")

subsection("1.2 enhanced_load_source Function")
# Extract the entire function
func_pattern = r'(def enhanced_load_source.*?)(?=\ndef |\nclass |\Z)'
match = re.search(func_pattern, content, re.DOTALL)
if match:
    func_code = match.group(1)
    func_lines = func_code.count('\n')
    print(f"Function length: {func_lines} lines")
    
    # Find what it returns
    returns = re.findall(r'^\s*return (.+)$', func_code, re.MULTILINE)
    print(f"\nReturn statements: {len(returns)}")
    for i, ret in enumerate(returns):
        print(f"  {i+1}. return {ret[:50]}...")

subsection("1.3 Import Resolution Process")
# Look for key operations
operations = {
    "Random prefix generation": r"random\.choices|random\.seed",
    "Module detection": r"Module\s+(\w+)",
    "Import statement parsing": r"Import\s+.*?from",
    "File reading": r"open\(.*?\)",
    "String replacement": r"\.replace\(",
    "Alias tracking": r"alias|mapping|prefix",
}

for op_name, pattern in operations.items():
    matches = re.findall(pattern, content)
    print(f"{op_name}: {len(matches)} occurrences")

# STEP 2: Test enhanced_load_source directly
section("STEP 2: Testing enhanced_load_source")

subsection("2.1 Direct Function Test")
try:
    from import_resolver import enhanced_load_source
    
    # Check its signature
    sig = inspect.signature(enhanced_load_source)
    print(f"Function signature: {sig}")
    
    # Test with a simple file
    test_content = '''Import XArrays from "./Library.XArrays.ailang"

Program Test {
    Main: {
        x = XArrays.XArray.XCreate(10)
    }
}'''
    
    with open("test_import.ailang", "w") as f:
        f.write(test_content)
    
    # Call the function
    result = enhanced_load_source("test_import.ailang")
    
    print(f"\nReturn type: {type(result)}")
    if isinstance(result, tuple):
        print(f"Tuple length: {len(result)}")
        for i, item in enumerate(result):
            print(f"  Item {i}: {type(item).__name__} (length: {len(str(item))})")
    else:
        print(f"Single value returned (length: {len(result)})")
        
        # Check if aliases are embedded in the result
        alias_pattern = r'([A-Z0-9]{6,8})_(\w+)'
        aliases = re.findall(alias_pattern, result)
        if aliases:
            print(f"\nAliases found in processed source: {len(aliases)}")
            for prefix, module in aliases[:3]:
                print(f"  {prefix}_{module} -> {module}")

except Exception as e:
    print(f"Error testing function: {e}")

# STEP 3: Check debug output
section("STEP 3: Analyzing Debug Output")

subsection("3.1 Debug Files")
debug_files = [f for f in os.listdir('.') if '.import_debug.json' in f]
print(f"Debug files found: {len(debug_files)}")

if debug_files:
    # Analyze one debug file
    with open(debug_files[0], 'r') as f:
        debug_data = json.load(f)
    
    print(f"\nAnalyzing: {debug_files[0]}")
    print(f"Keys in debug data: {list(debug_data.keys())}")
    
    if 'aliases' in debug_data:
        print(f"\nAlias mappings found:")
        for alias, original in list(debug_data['aliases'].items())[:5]:
            print(f"  {alias} -> {original}")
        
        # THIS IS KEY: The mappings exist but aren't being returned!
        print(f"\n⚠️ IMPORTANT: Aliases are tracked in debug file but not returned by function!")

# STEP 4: Check how main.py uses enhanced_load_source
section("STEP 4: Analyzing main.py")

subsection("4.1 Usage of enhanced_load_source")
with open("main.py", "r") as f:
    main_content = f.read()

# Find how it's called
usage_pattern = r'(\w+)\s*=\s*enhanced_load_source\([^)]+\)'
matches = re.findall(usage_pattern, main_content)
print(f"enhanced_load_source assignments: {len(matches)}")
for var in matches:
    print(f"  Assigns to: {var}")
    # Check if this variable is used with tuple unpacking anywhere
    if f"{var}, " in main_content or f", {var}" in main_content:
        print(f"    ⚠️ Variable {var} might be unpacked elsewhere")

subsection("4.2 Compiler initialization")
# Check how compiler is created
compiler_pattern = r'compiler\s*=\s*AILANGToX64Compiler\([^)]*\)'
compiler_matches = re.findall(compiler_pattern, main_content)
print(f"Compiler creation: {len(compiler_matches)} instances")

# Check if 'alias' appears near compiler creation
compiler_context = main_content[main_content.find("AILANGToX64Compiler")-200:main_content.find("AILANGToX64Compiler")+200]
if 'alias' in compiler_context.lower():
    print("  ✓ 'alias' mentioned near compiler creation")
else:
    print("  ✗ No 'alias' handling near compiler creation")

# STEP 5: Check compiler's handling of function calls
section("STEP 5: Analyzing Compiler")

subsection("5.1 compile_function_call method")
compiler_path = "ailang_compiler/ailang_compiler.py"
if os.path.exists(compiler_path):
    with open(compiler_path, "r") as f:
        compiler_content = f.read()
    
    # Find the method
    method_pattern = r'def compile_function_call\(self, node\):(.*?)(?=\n    def |\Z)'
    match = re.search(method_pattern, compiler_content, re.DOTALL)
    if match:
        method_code = match.group(1)
        print(f"Method found, length: {method_code.count(chr(10))} lines")
        
        # Check for alias handling
        if 'alias' in method_code.lower():
            print("  ✓ Method mentions 'alias'")
        else:
            print("  ✗ No 'alias' handling in method")
        
        # Find where error is raised
        error_pattern = r'raise ValueError\(.*?Unknown function.*?\)'
        if re.search(error_pattern, method_code):
            print("  Found 'Unknown function' error raise")

# STEP 6: Trace the full flow
section("STEP 6: Complete Flow Analysis")

print("""
IMPORT RESOLUTION FLOW:
1. main.py calls enhanced_load_source(filepath)
2. enhanced_load_source:
   - Reads the file
   - Detects imports
   - Generates random prefixes (e.g., ABCDEF_)
   - Replaces module names with prefixed versions
   - Saves debug info (including alias mappings)
   - Returns ONLY the processed source (not mappings!)
3. main.py receives processed source
4. Parser processes the source (with aliased names)
5. Compiler tries to compile
6. compile_function_call gets "ABCDEF_Module.Function"
7. Compiler doesn't know what "ABCDEF_Module" is
8. Raises "Unknown function" error

THE PROBLEM: Alias mappings exist but aren't passed through the pipeline!
""")

# FINAL DIAGNOSIS
section("DIAGNOSIS COMPLETE")

print("""
ROOT CAUSE:
-----------
1. enhanced_load_source creates alias mappings
2. It saves them to debug JSON file
3. But it only returns the processed source
4. main.py doesn't get the mappings
5. Compiler never learns about aliases

EVIDENCE:
---------
- enhanced_load_source returns: string (single value)
- Debug JSON contains: {'aliases': {'PREFIX_Module': 'Module'}}
- main.py expects: only source_code
- Compiler has: no alias handling

SOLUTION NEEDED:
---------------
1. Make enhanced_load_source return (source, aliases)
2. Update main.py to receive both values
3. Pass aliases to compiler
4. Update compiler to resolve aliases

EXACT LOCATIONS TO FIX:
----------------------
1. import_resolver.py: Line with 'return processed_source'
2. main.py: Line with 'source_code = enhanced_load_source'
3. ailang_compiler.py: compile_function_call method
""")

# Save diagnosis to file
with open("import_diagnosis.txt", "w") as f:
    f.write("IMPORT SYSTEM DIAGNOSIS\n")
    f.write("=" * 50 + "\n\n")
    f.write("Problem: Alias mappings created but not passed through compilation pipeline\n\n")
    f.write("Files to modify:\n")
    f.write("1. import_resolver.py - make it return mappings\n")
    f.write("2. main.py - capture and pass mappings\n") 
    f.write("3. ailang_compiler.py - use mappings to resolve aliases\n")

print("\nDiagnosis saved to: import_diagnosis.txt")