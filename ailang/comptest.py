#!/usr/bin/env python3
"""
Debug script to trace compilation of parameter passing
"""

import sys
import os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from ailang_parser.compiler import AILANGCompiler
from ailang_compiler.ailang_compiler import AILANGToX64Compiler

# Minimal test with detailed output
test_code = """
Function.TestSimple {
    Input: a: Integer
    Input: b: Integer
    Output: Integer
    
    PrintMessage("Inside TestSimple")
    PrintMessage("Param a:")
    PrintNumber(a)
    PrintMessage("Param b:")
    PrintNumber(b)
    
    sum = Add(a, b)
    PrintMessage("Sum:")
    PrintNumber(sum)
    
    ReturnValue(sum)
}

PrintMessage("Calling TestSimple(5, 10)")
result = TestSimple(5, 10)
PrintMessage("Result:")
PrintNumber(result)
"""

print("=== COMPILATION DEBUG ===\n")

try:
    # Parse
    print("1. Parsing...")
    parser = AILANGCompiler()
    ast = parser.compile(test_code)
    print("   ✓ Parsed successfully\n")
    
    # Check what parser found
    for decl in ast.declarations:
        if hasattr(decl, 'name') and 'TestSimple' in str(decl.name):
            print(f"   Function found: {decl.name}")
            if hasattr(decl, 'input_params'):
                print(f"   Parameters: {[p[0] for p in decl.input_params]}")
            print()
    
    # Compile with debug output
    print("2. Compiling to x64...")
    compiler = AILANGToX64Compiler()
    
    # Enable maximum debug output
    import logging
    logging.basicConfig(level=logging.DEBUG)
    
    executable = compiler.compile(ast)
    print(f"   ✓ Compiled to {len(executable)} bytes\n")
    
    # Save executable
    with open('test_simple_exec', 'wb') as f:
        f.write(executable)
    os.chmod('test_simple_exec', 0o755)
    print("3. Saved as test_simple_exec")
    print("   Run: ./test_simple_exec\n")
    
except Exception as e:
    print(f"ERROR: {e}")
    import traceback
    traceback.print_exc()

# Now test 7 params with detailed tracing
print("\n=== Testing 7-Parameter Compilation ===\n")

test_7param = """
Function.Test7 {
    Input: p1: Integer
    Input: p2: Integer  
    Input: p3: Integer
    Input: p4: Integer
    Input: p5: Integer
    Input: p6: Integer
    Input: p7: Integer
    Output: Integer
    
    PrintMessage("Test7 called")
    PrintMessage("p1:") 
    PrintNumber(p1)
    PrintMessage("p7:")
    PrintNumber(p7)
    
    ReturnValue(p7)
}

PrintMessage("Calling Test7")
r = Test7(1, 2, 3, 4, 5, 6, 7)
PrintMessage("Result (should be 7):")
PrintNumber(r)
"""

try:
    ast = parser.compile(test_7param)
    
    # Check what parser sees
    for decl in ast.declarations:
        if hasattr(decl, 'name') and 'Test7' in str(decl.name):
            print(f"Function: {decl.name}")
            print(f"Params found: {len(decl.input_params)}")
            
    executable = compiler.compile(ast)
    
    with open('test_7param_exec', 'wb') as f:
        f.write(executable)
    os.chmod('test_7param_exec', 0o755)
    print("Saved as test_7param_exec")
    
except Exception as e:
    print(f"ERROR: {e}")
    import traceback
    traceback.print_exc()

print("\n=== END COMPILATION DEBUG ===")