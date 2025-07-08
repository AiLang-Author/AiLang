#!/usr/bin/env python3
"""
Code Generator Module for AILANG Compiler
Handles low-level code emission helpers
"""

import struct
import sys
import os
from ...parser.ailang_ast import *

class CodeGenerator:
    """Handles low-level code generation"""
    
    def __init__(self, compiler_context):
        self.compiler = compiler_context
        self.asm = compiler_context.asm
    
    def emit_stack_frame_prologue(self, stack_size):
        """Emit stack frame setup code"""
        try:
            aligned_size = (stack_size + 15) & ~15
            self.asm.emit_push_rbp()
            self.asm.emit_bytes(0x48, 0x89, 0xE5)
            if aligned_size > 0:
                if aligned_size <= 2147483647:
                    self.asm.emit_bytes(0x48, 0x81, 0xEC)
                    self.asm.emit_bytes(*struct.pack('<I', aligned_size))
                else:
                    self.asm.emit_mov_rax_imm64(aligned_size)
                    self.asm.emit_bytes(0x48, 0x29, 0xC4)
            print(f"DEBUG: Emitted stack frame prologue, allocated {aligned_size} bytes")
        except Exception as e:
            print(f"ERROR: Stack frame prologue emission failed: {str(e)}")
            raise
    
    def emit_stack_frame_epilogue(self):
        """Emit stack frame cleanup code"""
        try:
            self.asm.emit_bytes(0x48, 0x89, 0xEC)
            self.asm.emit_pop_rbp()
            print("DEBUG: Emitted stack frame epilogue")
        except Exception as e:
            print(f"ERROR: Stack frame epilogue emission failed: {str(e)}")
            raise
    
    def emit_conditional_jump(self, target_label, jump_type="JE"):
        """Emit a conditional jump instruction"""
        try:
            self.asm.emit_bytes(0x0F, 0x84 if jump_type == "JE" else 0x85, 0x00, 0x00, 0x00, 0x00)
            jump_pos = len(self.asm.code) - 4
            self.asm.labels[target_label] = jump_pos  # Will be fixed up later
            print(f"DEBUG: Emitted {jump_type} to {target_label} at position {jump_pos}")
            return jump_pos
        except Exception as e:
            print(f"ERROR: Conditional jump emission failed: {str(e)}")
            raise