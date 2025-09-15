# main.py

import sys
import os
import argparse
from ailang_parser.compiler import AILANGCompiler
from ailang_compiler.ailang_compiler import AILANGToX64Compiler

def compile_ailang_to_executable(source_code, output_file, debug_level=0, perf_enabled=False, vm_mode="user"):
    """Compiles a single AILANG source string to an executable file."""
    try:
        print(f"🔨 Compiling AILANG source...")
        parser = AILANGCompiler()
        ast = parser.compile(source_code)
        
        # Create compiler with debug settings
        compiler = AILANGToX64Compiler(vm_mode=vm_mode)
        
        # Pass debug settings to the debug module if it exists
        if hasattr(compiler, 'debug_ops') and (debug_level > 0 or perf_enabled):
            debug_flags = set()
            if debug_level > 0:
                debug_flags.add('all')
            if perf_enabled:
                debug_flags.add('perf')
                
            compiler.debug_ops.set_debug_options({
                'debug': True,
                'debug_level': debug_level if debug_level > 0 else 1,  # Enable at least level 1 for perf
                'debug_flags': debug_flags
            })
            
            if debug_level > 0:
                print(f"DEBUG: Enabled at level {debug_level}")
            if perf_enabled:
                print(f"PERF: Performance profiling enabled")
        
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
    # Set up argument parser
    parser = argparse.ArgumentParser(
        description='AILANG Compiler - The cache-aware systems programming language',
        usage='python3 main.py [options] source_file.ailang ...'
    )
    
    # Add debug options
    parser.add_argument('-D', '--debug', action='store_const', const=1, dest='debug_level', default=0,
                        help='Enable debug level 1')
    parser.add_argument('-D2', '--debug2', action='store_const', const=2, dest='debug_level',
                        help='Enable debug level 2')
    parser.add_argument('-D3', '--debug3', action='store_const', const=3, dest='debug_level',
                        help='Enable debug level 3 (all debug output)')
    
    # Add performance profiling option
    parser.add_argument('-P', '--profile', action='store_true', dest='perf_enabled',
                        help='Enable performance profiling with RDTSC')
    parser.add_argument('-P:cache', '--profile-cache', action='store_const', const='cache', dest='perf_mode',
                        help='Enable cache profiling')
    parser.add_argument('-P:branch', '--profile-branch', action='store_const', const='branch', dest='perf_mode',
                        help='Enable branch prediction profiling')
    parser.add_argument('-P:all', '--profile-all', action='store_const', const='all', dest='perf_mode',
                        help='Enable all performance counters')
    
    # Add VM mode option
    parser.add_argument('--vm-mode', choices=['user', 'kernel'], default='user',
                        help='VM operation mode (default: user)')
    
    # Add output option
    parser.add_argument('-o', '--output', help='Output filename (default: input_exec)')
    
    # Source files (positional arguments)
    parser.add_argument('source_files', nargs='+', help='AILANG source files to compile')
    
    # Parse arguments
    args = parser.parse_args()
    
    # Set perf_enabled if any perf mode is selected
    if hasattr(args, 'perf_mode') and args.perf_mode:
        args.perf_enabled = True
    
    # Filter for .ailang files
    source_files = [f for f in args.source_files if f.endswith('.ailang')]
    if not source_files:
        print("Error: No .ailang source files provided.")
        sys.exit(1)
    
    all_successful = True
    for source_file in source_files:
        if not os.path.exists(source_file):
            print(f"❌ ERROR: Source file not found: {source_file}")
            all_successful = False
            continue
        
        # Determine output file
        if args.output and len(source_files) == 1:
            output_file = args.output
        else:
            output_file = source_file.replace('.ailang', '_exec')
        
        print("\n" + "="*50)
        print(f"🚀 Starting compilation for: {source_file} -> {output_file}")
        if args.debug_level > 0:
            print(f"🔍 Debug level: {args.debug_level}")
        if args.perf_enabled:
            print(f"⏱️ Performance profiling: ENABLED")
            if hasattr(args, 'perf_mode') and args.perf_mode:
                print(f"   Mode: {args.perf_mode}")
        print("="*50)
        
        with open(source_file, 'r') as f:
            source_code = f.read()

        success = compile_ailang_to_executable(
            source_code, 
            output_file,
            debug_level=args.debug_level,
            perf_enabled=args.perf_enabled,
            vm_mode=args.vm_mode
        )
        
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