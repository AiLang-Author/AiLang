#!/usr/bin/env python3
"""
Debug version of the integration test with timeout and intermediate output
"""

import subprocess
import sys
import os
import tempfile

# Setup paths
frontend_dir = os.path.dirname(os.path.abspath(__file__))
tests_dir = os.path.join(frontend_dir, 'tests')
input_file = os.path.join(tests_dir, 'hello.aim')
output_file = os.path.join(tests_dir, 'hello_exec')
ailang_file = os.path.join(tests_dir, 'hello_exec.ailang')

os.makedirs(tests_dir, exist_ok=True)

# Create test source file
hello_aim_content = """
# A simple "Hello, World!" program in AIMacro
def main():
    message = "Hello from AIMacro!"
    x = 10 + 32
    print(message)
    print("The answer is:")
    print(x)
end;
"""

print("=== Creating hello.aim ===")
with open(input_file, 'w', encoding='utf-8') as f:
    f.write(hello_aim_content)
print(f"Written to: {input_file}")

# Add parent to path for imports
project_root = os.path.abspath(os.path.join(frontend_dir, '..'))
sys.path.insert(0, project_root)

print("\n=== Compiling AIMacro to AILang ===")

# Import and run the frontend compiler stages
from aimacro_frontend.lexer import AIMacroLexer
from aimacro_frontend.parser import AIMacroParser
from aimacro_frontend.ast_converter import AIMacroToAILangConverter, AILangASTSerializer

with open(input_file, 'r') as f:
    source_code = f.read()

# Lex
print("Stage 1: Lexing...")
lexer = AIMacroLexer(source_code)
tokens = lexer.tokenize()

# Parse  
print("Stage 2: Parsing...")
parser = AIMacroParser(tokens)
aimacro_ast = parser.parse()

# Convert
print("Stage 3: Converting to AILang AST...")
converter = AIMacroToAILangConverter()
ailang_ast = converter.convert(aimacro_ast)

# Serialize
print("Stage 4: Serializing AILang...")
serializer = AILangASTSerializer()
ailang_source = serializer.serialize(ailang_ast)

print("\n=== Generated AILang Code ===")
print(ailang_source)
print("="*40)

# Save AILang file
with open(ailang_file, 'w') as f:
    f.write(ailang_source)
print(f"\nSaved to: {ailang_file}")

# Now compile with main.py
print(f"\n=== Invoking AILang Backend Compiler ===")
backend_main = os.path.join(project_root, 'main.py')
compile_command = [sys.executable, backend_main, ailang_file, '-o', output_file]

print(f"Command: {' '.join(compile_command)}")
print(f"Working directory: {project_root}")

try:
    # Run with timeout to avoid hanging
    result = subprocess.run(
        compile_command, 
        cwd=project_root,
        capture_output=True,
        text=True,
        timeout=10  # 10 second timeout
    )
    
    print(f"\nReturn code: {result.returncode}")
    if result.stdout:
        print("STDOUT:")
        print(result.stdout)
    if result.stderr:
        print("STDERR:")
        print(result.stderr)
        
    if result.returncode == 0:
        print("\n✅ Compilation successful!")
        if os.path.exists(output_file):
            print(f"Binary created: {output_file}")
            
            # Try to run it
            print("\n=== Running compiled binary ===")
            if sys.platform != "win32":
                os.chmod(output_file, 0o755)
            
            run_result = subprocess.run(
                output_file,
                capture_output=True,
                text=True,
                timeout=5
            )
            print(f"Output:\n{run_result.stdout}")
            if run_result.stderr:
                print(f"Errors:\n{run_result.stderr}")
    else:
        print("\n❌ Compilation failed!")
        
except subprocess.TimeoutExpired:
    print("\n❌ Compilation timed out after 10 seconds!")
    print("The compiler appears to be stuck.")
    print("Check the generated AILang code above for issues.")
except Exception as e:
    print(f"\n❌ Error: {e}")
    import traceback
    traceback.print_exc()

