# main.py

import sys
import os
from ailang_parser.compiler import AILANGCompiler
from ailang_compiler.ailang_compiler import AILANGToX64Compiler

def compile_ailang_to_executable(source_code, output_file):
    """Compiles a single AILANG source string to an executable file."""
    try:
        print(f"🔨 Compiling AILANG source...")
        parser = AILANGCompiler()
        ast = parser.compile(source_code)
        
        compiler = AILANGToX64Compiler(vm_mode="user")
        executable = compiler.compile(ast)
        
        full_path = os.path.abspath(output_file)
        print(f"📁 Writing to: {full_path}")
        with open(output_file, 'wb') as f:
            f.write(executable)
        print(f"✅ Wrote {len(executable)} bytes")
        
        os.chmod(output_file, 0o755)
        print(f"✅ Made executable")
        
        print(f"✅ Compiled to {output_file} ({len(executable)} bytes)")
        return True

    except Exception as e:
        print(f"❌ ERROR: {e}")
        import traceback
        traceback.print_exc()
        return False

def main():
    if len(sys.argv) < 2:
        print("Usage: python3 main.py <source_file_1.ailang> [source_file_2.ailang] ...")
        sys.exit(1)

    source_files = [arg for arg in sys.argv[1:] if arg.endswith('.ailang')]
    if not source_files:
        print("Error: No .ailang source files provided.")
        sys.exit(1)

    all_successful = True
    for source_file in source_files:
        if not os.path.exists(source_file):
            print(f"❌ ERROR: Source file not found: {source_file}")
            all_successful = False
            continue

        output_file = source_file.replace('.ailang', '_exec')
        
        print("\n" + "="*50)
        print(f"🚀 Starting compilation for: {source_file} -> {output_file}")
        print("="*50)
        
        with open(source_file, 'r') as f:
            source_code = f.read()

        success = compile_ailang_to_executable(source_code, output_file)
        if success:
            print(f"🎉 SUCCESS! Compilation of {source_file} completed successfully!")
        else:
            print(f"🔥 FAILED! Compilation of {source_file} encountered an error.")
            all_successful = False
    
    if not all_successful:
        print("\n❌ One or more files failed to compile.")
        sys.exit(1)

if __name__ == "__main__":
    main()