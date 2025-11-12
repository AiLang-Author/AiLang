import sys
sys.path.insert(0, '/mnt/c/Users/Sean/Documents/AiLang/ailang')

from ailang_parser.compiler import AILANGCompiler
from ailang_compiler.ailang_compiler import AILANGToX64Compiler
from ailang_compiler.modules.user_functions import UserFunctions

# Patch UserFunctions.compile_function_call with detailed tracing
original_user_fc = UserFunctions.compile_function_call

def traced_user_fc(self, node):
    fname = node.function
    original_fname = fname
    
    if 'ESCAL' in fname or '0000' in fname or '0100' in fname:
        print(f"\n>>> ENTER UserFunctions.compile_function_call: '{fname}'")
        print(f"    Checking if in user_functions dict...")
        
        if hasattr(self, 'user_functions'):
            if fname in self.user_functions:
                print(f"    ✓ Found in user_functions")
            else:
                print(f"    ✗ NOT FOUND in user_functions!")
                print(f"    Available keys: {list(self.user_functions.keys())[:5]}")
    
    try:
        result = original_user_fc(self, node)
        
        if node.function != original_fname:
            print(f"    ❌ NAME CHANGED: '{original_fname}' -> '{node.function}'")
        
        if 'ESCAL' in original_fname or '0000' in original_fname:
            print(f"<<< EXIT UserFunctions.compile_function_call: '{original_fname}'")
        
        return result
    except Exception as e:
        if 'ESCAL' in original_fname or '0000' in original_fname:
            print(f"    ❌ ERROR: {e}")
        raise

UserFunctions.compile_function_call = traced_user_fc

parser = AILANGCompiler()
ast = parser.compile_file('ESCAL056.ailang')

compiler = AILANGToX64Compiler()
try:
    binary = compiler.compile(ast)
    print("\n✅ SUCCESS!")
except Exception as e:
    print(f"\n❌ FAILED")
