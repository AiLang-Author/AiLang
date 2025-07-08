#!/usr/bin/env python3
"""
Main AILANG Compiler Interface
Compiles AILANG source to x86-64 executables
"""

import os
import sys
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', 'Ailang', 'ailang_parser')))
from compiler import AILANGCompiler
from ailang.compiler import AILANGToX64Compiler

def compile_ailang_to_executable(source_code: str, output_file: str = "program", vm_mode: str = "user"):
    """Compile AILANG source directly to executable with VM mode selection"""
    
    # Debug: Show current working directory and VM mode
    cwd = os.getcwd()
    print(f"📂 Current directory: {cwd}")
    print(f"🔧 VM Mode: {vm_mode.upper()}")
    
    # Parse AILANG
    parser = AILANGCompiler()
    ast = parser.compile(source_code)
    
    # Compile to machine code with specified VM mode
    compiler = AILANGToX64Compiler(vm_mode=vm_mode)
    executable = compiler.compile(ast)
    
    # Use absolute path to ensure we know where it goes
    if not os.path.isabs(output_file):
        output_file = os.path.join(cwd, output_file)
    
    # Write executable
    print(f"📁 Writing to: {output_file}")
    
    try:
        with open(output_file, 'wb') as f:
            f.write(executable)
        print(f"✅ Wrote {len(executable)} bytes")
    except Exception as e:
        print(f"❌ Error writing file: {e}")
        return False
    
    # Make executable
    try:
        os.chmod(output_file, 0o755)
        print(f"✅ Made executable")
    except Exception as e:
        print(f"⚠️  Warning: Could not set executable permission: {e}")
    
    # Verify file exists
    if os.path.exists(output_file):
        file_size = os.path.getsize(output_file)
        print(f"✅ Compiled to {output_file} ({file_size} bytes)")
        print(f"📍 Full path: {output_file}")
        print(f"🚀 Run with: {output_file}")
    else:
        print(f"❌ ERROR: File {output_file} was not created!")
    
    return True

def main():
    """Command-line interface for AILANG compiler"""
    
    # Check command-line arguments
    if len(sys.argv) < 2:
        print("Usage: python3 main.py <source_file> [output_file]")
        print("Example: python3 main.py test.ailang test_executable")
        sys.exit(1)
    
    source_file = sys.argv[1]
    output_file = sys.argv[2] if len(sys.argv) > 2 else "program"
    
    # Check if source file exists
    if not os.path.exists(source_file):
        print(f"❌ ERROR: Source file '{source_file}' not found!")
        sys.exit(1)
    
    print(f"🔨 Compiling AILANG source: {source_file}")
    print(f"📤 Output executable: {output_file}")
    
    try:
        # Read source code
        with open(source_file, 'r') as f:
            source_code = f.read()
        
        print(f"📄 Source code ({len(source_code)} characters):")
        print("-" * 50)
        print(source_code)
        print("-" * 50)
        
        # Compile to executable
        success = compile_ailang_to_executable(source_code, output_file)
        
        if success:
            print(f"🎉 SUCCESS! Compilation completed successfully!")
            print(f"🚀 Run your program with: ./{output_file}")
            sys.exit(0)
        else:
            print(f"❌ FAILED! Compilation failed!")
            sys.exit(1)
            
    except Exception as e:
        print(f"❌ ERROR: {str(e)}")
        import traceback
        traceback.print_exc()
        sys.exit(1)

if __name__ == "__main__":
    main()