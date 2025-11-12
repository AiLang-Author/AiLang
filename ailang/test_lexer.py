# Focused debug - ONLY show function call info
# Save as test_function_call_debug.py

import sys
import io
from contextlib import redirect_stdout

from ailang_parser.parser import Parser
from ailang_parser.lexer_modules.lexer_class import Lexer
from ailang_compiler.ailang_compiler import AILANGToX64Compiler

test_code = """
Function.ESCAL056_0000_MAINLINE_CONTROL {
    Input: linkage: Integer
    Body: { ReturnValue(0) }
}

SubRoutine.Main {
    ESCAL056_0000_MAINLINE_CONTROL(123)
}
"""

print("="*70)
print("FOCUSED FUNCTION CALL DEBUG")
print("="*70)

# Parse
lexer = Lexer(test_code)
tokens = lexer.tokenize()
parser = Parser(tokens)
ast = parser.parse()

# Monkey-patch to capture ONLY function call info
call_log = []

original_compile_function_call = AILANGToX64Compiler.compile_function_call

def debug_compile_function_call(self, node):
    entry = {
        'function_name': str(node.function),
        'acronym_table': dict(self.acronym_table) if hasattr(self, 'acronym_table') else {},
        'registered_functions': list(self.user_functions.user_functions.keys()) if hasattr(self, 'user_functions') else []
    }
    
    # Test resolve_acronym_identifier if it exists
    if hasattr(self, 'resolve_acronym_identifier'):
        resolved = self.resolve_acronym_identifier(node.function)
        entry['resolved_name'] = resolved
        entry['was_corrupted'] = (resolved != node.function)
    
    call_log.append(entry)
    
    # Call original
    try:
        return original_compile_function_call(self, node)
    except Exception as e:
        entry['exception'] = f"{type(e).__name__}: {e}"
        raise

AILANGToX64Compiler.compile_function_call = debug_compile_function_call

# Compile with output suppressed
print("\nCompiling (output suppressed)...\n")
compiler = AILANGToX64Compiler()

# Suppress all output during compilation
f = io.StringIO()
try:
    with redirect_stdout(f):
        binary = compiler.compile(ast)
    print("✓ Compilation succeeded")
except Exception as e:
    print(f"✗ Compilation failed: {e}")

# Show function call log
print("\n" + "="*70)
print(f"FUNCTION CALLS DETECTED: {len(call_log)}")
print("="*70)

for i, entry in enumerate(call_log):
    print(f"\nCall #{i+1}:")
    print(f"  Function name: '{entry['function_name']}'")
    
    if 'resolved_name' in entry:
        print(f"  After resolve_acronym_identifier: '{entry['resolved_name']}'")
        if entry.get('was_corrupted'):
            print(f"  ⚠️  CORRUPTION DETECTED! ⚠️")
            print(f"     Original: '{entry['function_name']}'")
            print(f"     Became:   '{entry['resolved_name']}'")
    
    if entry['acronym_table']:
        print(f"  Acronym table: {entry['acronym_table']}")
    
    print(f"  Registered functions: {entry['registered_functions']}")
    
    if 'exception' in entry:
        print(f"  Exception: {entry['exception']}")

print("\n" + "="*70)