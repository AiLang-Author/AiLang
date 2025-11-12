from ailang_parser.parser import Parser
from ailang_parser.lexer_modules.lexer_class import Lexer  
from ailang_compiler.ailang_compiler import AILANGToX64Compiler

code = open('test_nested_calls.ailang').read()
lexer = Lexer(code)
tokens = lexer.tokenize()
parser = Parser(tokens)
ast = parser.parse()

compiler = AILANGToX64Compiler()

# Patch compile_function_call to catch the corruption
original_cfc = AILANGToX64Compiler.compile_function_call

def traced_cfc(self, node):
    original_name = str(node.function)
    
    # Patch all compile_operation calls to check for corruption
    for module_name in ['arithmetic', 'fileio', 'strings', 'lowlevel', 'hash_ops', 'network_ops', 'virtual_memory', 'atomics']:
        if hasattr(self, module_name):
            module = getattr(self, module_name)
            if hasattr(module, 'compile_operation'):
                orig_op = module.compile_operation
                
                def make_traced_op(mod_name, orig):
                    def traced_op(n):
                        before = str(n.function) if hasattr(n, 'function') else 'N/A'
                        result = orig(n)
                        after = str(n.function) if hasattr(n, 'function') else 'N/A'
                        if before != after:
                            print(f"🔴 {mod_name} CORRUPTED: '{before}' -> '{after}'")
                        return result
                    return traced_op
                
                module.compile_operation = make_traced_op(module_name, orig_op)
    
    try:
        result = original_cfc(self, node)
        if str(node.function) != original_name:
            print(f"🔴 CORRUPTION DETECTED: '{original_name}' became '{node.function}'")
        return result
    except Exception as e:
        if str(node.function) != original_name:
            print(f"🔴 CORRUPTION BEFORE ERROR: '{original_name}' became '{node.function}'")
        raise

AILANGToX64Compiler.compile_function_call = traced_cfc

try:
    compiler.compile(ast)
except:
    pass
