#!/usr/bin/env python3
"""
DIAGNOSTIC: Test if library prefix pollution is causing the compilation bug
Based on COMPILATION BUG INVESTIGATION CONTINUATION SHEET
"""

import sys
sys.path.insert(0, '/mnt/c/Users/Sean/Documents/AiLang/ailang')

from ailang_parser.compiler import AILANGCompiler
from ailang_compiler.ailang_compiler import AILANGToX64Compiler

print("=" * 80)
print("🔍 LIBRARY PREFIX POLLUTION DIAGNOSTIC")
print("=" * 80)

# Parse the ESCAL056 file
parser = AILANGCompiler()
print("\n[Step 1] Parsing ESCAL056.ailang...")
ast = parser.compile_file('ESCAL056.ailang')
print(f"✅ AST parsed successfully")

# Create compiler
compiler = AILANGToX64Compiler()
print("\n[Step 2] Compiler instance created")
print(f"  Initial current_library_prefix: {compiler.current_library_prefix}")

# Phase 0: Register everything
print("\n[Step 3] Phase 0: Registering declarations...")
for i, decl in enumerate(ast.declarations):
    node_type = type(decl).__name__
    
    if node_type in ('Function', 'FunctionDefinition'):
        print(f"  [{i}] Registering Function: {decl.name}")
        compiler.user_functions.register_function(decl)
        
    elif node_type == 'Library':
        print(f"\n  [{i}] Loading Library: {decl.name}")
        print(f"      BEFORE: current_library_prefix = '{compiler.current_library_prefix}'")
        
        try:
            compiler.compile_library(decl)
            print(f"      AFTER:  current_library_prefix = '{compiler.current_library_prefix}'")
            
            # Check if prefix was cleared
            if compiler.current_library_prefix is not None:
                print(f"      ⚠️  WARNING: Library prefix NOT cleared! Still: '{compiler.current_library_prefix}'")
            else:
                print(f"      ✅ Library prefix properly cleared")
                
        except Exception as e:
            print(f"      ❌ Library load failed: {e}")
            
    elif node_type == 'SubRoutine':
        print(f"  [{i}] SubRoutine: {decl.name}")
    elif node_type == 'LinkagePool':
        print(f"  [{i}] LinkagePool: {decl.name}")
    else:
        print(f"  [{i}] {node_type}")

# Check if ESCAL function is registered
print("\n[Step 4] Checking ESCAL056_0000_MAINLINE_CONTROL registration...")
func_name = 'ESCAL056_0000_MAINLINE_CONTROL'

if func_name in compiler.user_functions.user_functions:
    print(f"✅ Function IS registered in user_functions")
    func_info = compiler.user_functions.user_functions[func_name]
    print(f"   - Label: {func_info['label']}")
    print(f"   - Params: {func_info['params']}")
    print(f"   - Compiled: {func_info['compiled']}")
    print(f"   - Node exists: {func_info['node'] is not None}")
    if func_info['node']:
        print(f"   - Node type: {type(func_info['node']).__name__}")
        print(f"   - Node name: {func_info['node'].name}")
else:
    print(f"❌ Function NOT registered!")

# Check all registered functions
print("\n[Step 5] All registered user functions:")
for name, info in compiler.user_functions.user_functions.items():
    compiled_str = "✓" if info['compiled'] else "✗"
    print(f"  [{compiled_str}] {name}")

# Check if any functions have library prefixes that shouldn't
print("\n[Step 6] Checking for incorrectly prefixed user functions...")
suspicious_found = False
for name in compiler.user_functions.user_functions.keys():
    # Check if name has a dot but isn't a library function
    if '.' in name and not name.startswith(('Cobol.', 'XArrays.')):
        # Might be incorrectly prefixed
        if name.startswith('ESCAL'):
            continue  # ESCAL functions can have dots
        print(f"  ⚠️  Suspicious: {name}")
        suspicious_found = True

if not suspicious_found:
    print("  ✅ No suspicious prefixing detected")

# Final check: Try to find the function in compile_function_call
print("\n[Step 7] Testing compile_function_call lookup...")
print(f"  Current library prefix: {compiler.current_library_prefix}")

# Simulate what happens during compilation
test_result = compiler.user_functions.is_function_registered(func_name)
print(f"  is_function_registered('{func_name}'): {test_result}")

print("\n" + "=" * 80)
print("🎯 DIAGNOSTIC COMPLETE")
print("=" * 80)

# Summary
print("\n📊 SUMMARY:")
print(f"  - Library prefix after loading: {compiler.current_library_prefix}")
print(f"  - ESCAL function registered: {func_name in compiler.user_functions.user_functions}")
print(f"  - Total functions registered: {len(compiler.user_functions.user_functions)}")

if compiler.current_library_prefix is not None:
    print(f"\n❌ BUG CONFIRMED: Library prefix not cleared!")
    print(f"   This could cause user functions to be incorrectly prefixed.")
else:
    print(f"\n✅ Library prefix is properly None after loading")
    print(f"   The bug must be elsewhere in the compilation flow.")