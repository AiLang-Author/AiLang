from ailang_parser.parser import Parser
from ailang_parser.lexer_modules.lexer_class import Lexer  
from ailang_compiler.ailang_compiler import AILANGToX64Compiler

code = open('test_nested_calls.ailang').read()
lexer = Lexer(code)
tokens = lexer.tokenize()
parser = Parser(tokens)
ast = parser.parse()

compiler = AILANGToX64Compiler()

original_cfc = AILANGToX64Compiler.compile_function_call

def traced_cfc(self, node):
    original_name = str(node.function)
    print(f"\n>>> ENTER compile_function_call: '{original_name}'")
    
    try:
        result = original_cfc(self, node)
        after_name = str(node.function)
        if original_name != after_name:
            print(f">>> EXIT: CORRUPTED '{original_name}' -> '{after_name}'")
        else:
            print(f">>> EXIT: OK ('{original_name}')")
        return result
    except Exception as e:
        after_name = str(node.function)
        if original_name != after_name:
            print(f">>> EXCEPTION: '{original_name}' was corrupted to '{after_name}' before error")
        print(f">>> EXCEPTION: {e}")
        raise

AILANGToX64Compiler.compile_function_call = traced_cfc

try:
    compiler.compile(ast)
except:
    pass
