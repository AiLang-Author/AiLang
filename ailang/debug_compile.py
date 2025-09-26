#!/usr/bin/env python3
"""
Debug script to trace function compilation and calling
"""

import sys
import os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from ailang_parser.compiler import AILANGCompiler
from ailang_compiler.ailang_compiler import AILANGToX64Compiler

# Simple test
test_code = """
Function.TestFunc {
    Output: Integer
    
    PrintMessage(">>> FUNCTION EXECUTED <<<")
    ReturnValue(42)
}

PrintMessage("Before call")
result = TestFunc()
PrintMessage("After call")
PrintNumber(result)
"""

print("=== COMPILATION TRACE ===\n")

try:
    # Parse
    print("1. Parsing...")
    parser = AILANGCompiler()
    ast = parser.compile(test_code)
    print("   ✓ Parsed\n")
    
    # Compile with debug
    print("2. Compiling...")
    compiler = AILANGToX64Compiler()
    
    # Enable debug
    import logging
    logging.basicConfig(level=logging.DEBUG)
    
    # Compile
    executable = compiler.compile(ast)
    
    print(f"\n3. Executable size: {len(executable)} bytes")
    
    # Check if functions were registered
    if hasattr(compiler, 'user_functions'):
        print(f"\n4. User functions registered:")
        for name, info in compiler.user_functions.user_functions.items():
            print(f"   - {name}: label={info.get('label')}, compiled={info.get('compiled')}")
    
    # Check if labels were created
    if hasattr(compiler.asm, 'labels'):
        print(f"\n5. Labels in asm:")
        for label, pos in compiler.asm.labels.items():
            print(f"   - {label}: position {pos}")
    
    # Check if there are pending fixups
    if hasattr(compiler.asm, 'pending_calls'):
        print(f"\n6. Pending calls: {len(compiler.asm.pending_calls)}")
        for call_pos, label in compiler.asm.pending_calls:
            print(f"   - Position {call_pos} -> {label}")
    
    if hasattr(compiler, 'user_function_fixups'):
        print(f"\n7. User function fixups: {len(compiler.user_function_fixups)}")
        for name, pos in compiler.user_function_fixups:
            print(f"   - {name} at position {pos}")
    
    # Check the actual bytes around the CALL
    print(f"\n8. Looking for CALL instructions (0xE8):")
    for i, byte in enumerate(executable):
        if byte == 0xE8:  # CALL opcode
            # Get the next 4 bytes (offset)
            if i + 4 < len(executable):
                import struct
                offset = struct.unpack('<i', executable[i+1:i+5])[0]
                print(f"   - CALL at {i}: offset = {offset} ({hex(offset)})")
                if offset == 0:
                    print(f"     WARNING: Zero offset - forward reference not resolved!")
    
    # Save executable
    with open('debug_test_exec', 'wb') as f:
        f.write(executable)
    os.chmod('debug_test_exec', 0o755)
    
    print("\n9. Saved as debug_test_exec")
    print("   Run: ./debug_test_exec")
    
except Exception as e:
    print(f"ERROR: {e}")
    import traceback
    traceback.print_exc()

print("\n=== END TRACE ===")