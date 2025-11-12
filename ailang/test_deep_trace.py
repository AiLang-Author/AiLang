from ailang_parser.parser import Parser
from ailang_parser.lexer_modules.lexer_class import Lexer  
from ailang_compiler.ailang_compiler import AILANGToX64Compiler

code = open('test_escal_with_imports.ailang').read()
lexer = Lexer(code)
tokens = lexer.tokenize()
parser = Parser(tokens)
ast = parser.parse()

compiler = AILANGToX64Compiler()

# Patch compile_function_call to see what's happening
original_cfc = AILANGToX64Compiler.compile_function_call
call_count = [0]

def traced_cfc(self, node):
    call_count[0] += 1
    print(f"\n{'='*70}")
    print(f"CALL #{call_count[0]}: compile_function_call")
    print(f"  node.function = '{node.function}'")
    print(f"  loaded_libraries = {self.loaded_libraries if hasattr(self, 'loaded_libraries') else 'NONE'}")
    print(f"  current_library_prefix = {self.current_library_prefix if hasattr(self, 'current_library_prefix') else 'NONE'}")
    
    # Check the "Search through imported libraries" section
    if hasattr(self, 'loaded_libraries') and self.loaded_libraries:
        print(f"  Testing library prefixes...")
        for lib_name in self.loaded_libraries:
            lib_prefix = lib_name.split('.')[-1]
            prefixed_name = f"{lib_prefix}.{node.function}"
            print(f"    {lib_name} -> trying '{prefixed_name}'")
    
    try:
        return original_cfc(self, node)
    except Exception as e:
        print(f"  ❌ EXCEPTION: {e}")
        raise

AILANGToX64Compiler.compile_function_call = traced_cfc

try:
    compiler.compile(ast)
    print("\n✓ SUCCESS")
except Exception as e:
    print(f"\n✗ FAILED: {e}")
