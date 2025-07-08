#!/usr/bin/env python3
"""
Expression Compiler Module for AILANG Compiler
Handles expression evaluation
"""

import sys
import os
import struct
from ...parser.ailang_ast import *

class ExpressionCompiler:
    """Handles expression compilation"""
    
    def __init__(self, compiler_context):
        self.compiler = compiler_context
        self.asm = compiler_context.asm
    
    def compile_expression(self, expr):
        """Compile an expression and leave result in RAX"""
        try:
            if isinstance(expr, Number):
                self.asm.emit_mov_rax_imm64(int(expr.value))
                print(f"DEBUG: Loaded number {expr.value}")
                
            elif isinstance(expr, Identifier):
                # Resolve any acronyms before processing
                resolved_name = self.compiler.resolve_acronym_identifier(expr.name)
                
                if resolved_name in self.compiler.variables:
                    offset = self.compiler.variables[resolved_name]
                    print(f"DEBUG: Loading variable {resolved_name} from [RBP - {offset}]")
                    # MOV RAX, [RBP - offset]
                    if offset <= 127:
                        self.asm.emit_bytes(0x48, 0x8B, 0x45, 256 - offset)
                    else:
                        self.asm.emit_bytes(0x48, 0x8B, 0x85)
                        self.asm.emit_bytes(*struct.pack('<i', -offset))
                else:
                    raise ValueError(f"Undefined variable: {resolved_name}")
                    
            elif isinstance(expr, FunctionCall):
                self.compiler.compile_function_call(expr)
                
            else:
                raise ValueError(f"Unsupported expression type: {type(expr)}")
                
        except Exception as e:
            print(f"ERROR: Expression compilation failed: {str(e)}")
            raise