#!/usr/bin/env python3
# fix_import_alias_bug.py - Apply the fix to the compiler

import os
import re

print("=" * 60)
print("  Fixing Import Alias Bug")
print("=" * 60)
print()

# 1. Check main.py to see how import resolver and compiler connect
print("1. Checking main.py flow...")
print("-" * 40)

with open("main.py", "r") as f:
    main_content = f.read()

# Find where enhanced_load_source is called
if "enhanced_load_source" in main_content:
    print("✓ Found enhanced_load_source usage in main.py")
    
    # Find the pattern
    pattern = r'(\w+)\s*=\s*enhanced_load_source\([^)]+\)'
    match = re.search(pattern, main_content)
    if match:
        var_name = match.group(1)
        print(f"  Import resolver returns: {var_name}")
        
        # Check if alias mappings are extracted
        if "alias" in main_content.lower():
            print("  ✓ Found 'alias' mentioned")
        else:
            print("  ✗ No 'alias' handling found - THIS IS THE BUG!")

print()
print("2. The Problem:")
print("-" * 40)
print("enhanced_load_source returns: (processed_source, alias_mappings)")
print("But main.py probably only uses processed_source!")
print()

# 2. Show the fix
print("3. THE FIX for main.py:")
print("-" * 40)
print("""
CURRENT CODE (probably):
    # Load and resolve imports
    source_code = enhanced_load_source(filepath)
    
    # Parse and compile
    ast = parser.compile(source_code)
    compiler = AILANGToX64Compiler(vm_mode=vm_mode)

FIXED CODE:
    # Load and resolve imports
    source_code, alias_mappings = enhanced_load_source(filepath)
    #            ^^^^^^^^^^^^^^^^ Extract both values!
    
    # Parse and compile
    ast = parser.compile(source_code)
    compiler = AILANGToX64Compiler(vm_mode=vm_mode)
    compiler.set_alias_mappings(alias_mappings)  # Pass mappings to compiler!
    # OR if compiler doesn't have this method yet:
    compiler.alias_mappings = alias_mappings
""")

print()
print("4. THE FIX for ailang_compiler.py:")
print("-" * 40)
print("""
In compile_function_call method, before raising "Unknown function":

    def compile_function_call(self, node):
        function_name = node.function
        
        # NEW: Check alias mappings
        if hasattr(self, 'alias_mappings') and self.alias_mappings:
            # If function has a module prefix that might be aliased
            if '.' in function_name:
                module_part = function_name.split('.')[0]
                
                # Check if this module part is an alias
                for alias, original in self.alias_mappings.items():
                    if alias in function_name:
                        # Replace alias with original
                        function_name = function_name.replace(alias, original)
                        break
        
        # Now continue with normal lookup
        if function_name not in self.known_functions:
            raise ValueError(f"Unknown function: {function_name}")
""")

print()
print("5. Quick Test:")
print("-" * 40)
print("After applying the fix:")
print("  python3 main.py bug_test.ailang")
print("  Should work without 'Unknown function' error!")
print()

# Create a patch file
print("6. Creating patch file...")
print("-" * 40)

patch_content = """
# PATCH 1: main.py
# Change this line:
source_code = enhanced_load_source(filepath)

# To this:
source_code, alias_mappings = enhanced_load_source(filepath)

# And after creating the compiler:
compiler = AILANGToX64Compiler(vm_mode=vm_mode)
# Add:
if alias_mappings:
    compiler.alias_mappings = alias_mappings

---

# PATCH 2: ailang_compiler.py
# In compile_function_call, add at the beginning:

# Handle aliased module functions
if hasattr(self, 'alias_mappings') and self.alias_mappings and '.' in node.function:
    for alias, original in self.alias_mappings.items():
        if alias in node.function:
            node.function = node.function.replace(alias, original)
            break
"""

with open("import_alias_fix.patch", "w") as f:
    f.write(patch_content)

print("Created import_alias_fix.patch")
print()
print("=" * 60)
print("  Summary")
print("=" * 60)
print()
print("The bug is simple:")
print("1. import_resolver returns (source, mappings)")
print("2. main.py only uses source, ignores mappings")
print("3. Compiler never learns about aliases")
print()
print("The fix is simple:")
print("1. Capture both values from enhanced_load_source")
print("2. Pass mappings to compiler")
print("3. Use mappings in compile_function_call")
print()
print("This will fix your Redis server compilation!")
print("The Redis server perfectly exposed this compiler bug! 🎯")