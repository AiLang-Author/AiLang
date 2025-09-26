#!/usr/bin/env python3
"""
Debug script to check what the parser sees for multi-parameter functions
"""

import sys
import os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from ailang_parser.compiler import AILANGCompiler

# Test AILANG code with 7 parameters
test_code = """
Function.Test7Params {
    Input: p1: Integer
    Input: p2: Integer
    Input: p3: Integer
    Input: p4: Integer
    Input: p5: Integer
    Input: p6: Integer
    Input: p7: Integer
    Output: Integer
    
    sum = Add(p1, p2)
    sum = Add(sum, p3)
    sum = Add(sum, p4)
    sum = Add(sum, p5)
    sum = Add(sum, p6)
    sum = Add(sum, p7)
    
    ReturnValue(sum)
}

// Call the function
result = Test7Params(10, 20, 30, 40, 50, 60, 70)
PrintNumber(result)
"""

print("=== PARSER DEBUG ===\n")
print("Parsing AILANG code with 7-parameter function...\n")

try:
    parser = AILANGCompiler()
    ast = parser.compile(test_code)
    
    print("AST parsed successfully!\n")
    
    # Find the function definition in the AST
    for decl in ast.declarations:
        if hasattr(decl, 'name') and 'Test7Params' in str(decl.name):
            print(f"Found function: {decl.name}")
            
            # Check input_params
            if hasattr(decl, 'input_params'):
                print(f"Number of input_params: {len(decl.input_params)}")
                print("Parameters:")
                for i, param in enumerate(decl.input_params):
                    print(f"  Param {i+1}: {param}")
            else:
                print("No input_params attribute found!")
            
            # Check body
            if hasattr(decl, 'body'):
                print(f"Function has body with {len(decl.body)} statements")
            
            print()
    
    # Find the function call in the AST
    for decl in ast.declarations:
        if hasattr(decl, 'target') and decl.target == 'result':
            print("Found function call assignment")
            if hasattr(decl, 'value'):
                call = decl.value
                if hasattr(call, 'function'):
                    print(f"Calling function: {call.function}")
                if hasattr(call, 'arguments'):
                    print(f"Number of arguments: {len(call.arguments)}")
                    print("Arguments:")
                    for i, arg in enumerate(call.arguments):
                        print(f"  Arg {i+1}: {arg}")
            print()
    
except Exception as e:
    print(f"ERROR: {e}")
    import traceback
    traceback.print_exc()

print("\n=== Testing with 10 parameters ===\n")

test_code_10 = """
Function.Test10 {
    Input: p1: Integer
    Input: p2: Integer
    Input: p3: Integer
    Input: p4: Integer
    Input: p5: Integer
    Input: p6: Integer
    Input: p7: Integer
    Input: p8: Integer
    Input: p9: Integer
    Input: p10: Integer
    Output: Integer
    
    ReturnValue(p10)
}

result = Test10(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
"""

try:
    ast = parser.compile(test_code_10)
    
    for decl in ast.declarations:
        if hasattr(decl, 'name') and 'Test10' in str(decl.name):
            print(f"Function: {decl.name}")
            if hasattr(decl, 'input_params'):
                print(f"Parser found {len(decl.input_params)} parameters")
                for i, param in enumerate(decl.input_params):
                    print(f"  Param {i+1}: {param[0] if isinstance(param, tuple) else param}")
            print()
            
except Exception as e:
    print(f"ERROR: {e}")
    import traceback
    traceback.print_exc()

print("=== END PARSER DEBUG ===")