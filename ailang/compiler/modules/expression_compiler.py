#!/usr/bin/env python3
"""
Expression Compiler Module for AILANG Compiler
Handles expression evaluation
"""

import sys
import os
import struct
from ailang_parser.ailang_ast import *

class ExpressionCompiler:
    """Handles expression compilation"""
    
    def __init__(self, compiler_context):
        self.compiler = compiler_context
        self.asm = compiler_context.asm
    
    def compile_expression(self, expr):
        try:
            if isinstance(expr, Number):
                self.asm.emit_mov_rax_imm64(int(float(expr.value)))
            elif isinstance(expr, Identifier):
                resolved_name = self.compiler.resolve_acronym_identifier(expr.name)
                if resolved_name in self.compiler.variables:
                    offset = self.compiler.variables[resolved_name]
                    self.asm.emit_bytes(0x48, 0x8b, 0x85)
                    self.asm.emit_bytes(*struct.pack('<i', -offset))
                    
                else:
                    raise ValueError(f"Undefined variable: '{resolved_name}'")
            elif isinstance(expr, String):
                string_offset = self.asm.add_string(expr.value)
                self.asm.emit_load_data_address('rax', string_offset)
                print(f"DEBUG: Loaded string literal at data offset {string_offset}")
            elif isinstance(expr, FunctionCall):
                self.compiler.compile_function_call(expr)
            elif isinstance(expr, Dereference):
                print("DEBUG: Compiling low-level operation: Dereference")
                self.compiler.lowlevel.compile_operation(expr)
            else:
                raise ValueError(f"Unsupported expression type: {type(expr)}")
        except Exception as e:
            print(f"ERROR: Expression compilation failed: {str(e)}")
            raise