from ailang_parser.parser import Parser
from ailang_parser.lexer_modules.lexer_class import Lexer
from ailang_compiler.ailang_compiler import AILANGToX64Compiler

code = open('test_escal_minimal.ailang').read()

lexer = Lexer(code)
tokens = lexer.tokenize()
parser = Parser(tokens)
ast = parser.parse()

# Patch compile_function_call to show what's happening
original = AILANGToX64Compiler.compile_function_call

def traced(self, node):
    import traceback
    print(f"\n{'='*70}")
    print(f"compile_function_call TRACE")
    print(f"{'='*70}")
    print(f"node.function = '{node.function}'")
    print(f"type(node.function) = {type(node.function)}")
    
    # Show the call stack to see WHO called compile_function_call
    print(f"\nCall stack:")
    for line in traceback.format_stack()[:-1]:
        if 'ailang_compiler' in line or 'compile' in line.lower():
            print(line.strip())
    
    try:
        return original(self, node)
    except ValueError as e:
        print(f"\n❌ EXCEPTION: {e}")
        print(f"node.function became: '{node.function}'")
        raise

AILANGToX64Compiler.compile_function_call = traced

compiler = AILANGToX64Compiler()
try:
    binary = compiler.compile(ast)
except Exception as e:
    print(f"\nFinal error: {e}")
