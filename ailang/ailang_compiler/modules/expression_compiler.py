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
                value_str = str(expr.value)
                if value_str.startswith('0x') or value_str.startswith('0X'):
                    self.asm.emit_mov_rax_imm64(int(value_str, 16))
                elif value_str.startswith('0b') or value_str.startswith('0B'):
                    self.asm.emit_mov_rax_imm64(int(value_str, 2))
                elif '.' in value_str or 'e' in value_str.lower():
                    self.asm.emit_mov_rax_imm64(int(float(value_str)))
                else:
                    self.asm.emit_mov_rax_imm64(int(value_str))
                    
            elif isinstance(expr, Identifier):
                resolved_name = self.compiler.resolve_acronym_identifier(expr.name)
                
                # Try to find the variable
                if resolved_name not in self.compiler.variables:
                    # Try with pool type prefixes
                    pool_types = ['FixedPool', 'DynamicPool', 'TemporalPool', 
                                'NeuralPool', 'KernelPool', 'ActorPool', 
                                'SecurityPool', 'ConstrainedPool', 'FilePool']
                    
                    for pool_type in pool_types:
                        prefixed_name = f"{pool_type}.{resolved_name}"
                        if prefixed_name in self.compiler.variables:
                            resolved_name = prefixed_name
                            print(f"DEBUG: Resolved pool variable {expr.name} -> {prefixed_name}")
                            break
                
                if resolved_name in self.compiler.variables:
                    value = self.compiler.variables[resolved_name]
                    
                    # Check if this is a pool variable (high bit set)
                    if value & 0x80000000:  # Pool variable marker
                        pool_index = value & 0x7FFFFFFF  # Get actual index
                        print(f"DEBUG: Loading pool var {resolved_name} from pool[{pool_index}]")
                        # MOV RAX, [R15 + pool_index*8]
                        self.asm.emit_bytes(0x49, 0x8B, 0x87)  # MOV RAX, [R15 + disp32]
                        self.asm.emit_bytes(*struct.pack('<i', pool_index * 8))
                    else:
                        # Stack-relative access for local variables
                        offset = value
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
            elif type(expr).__name__ == 'AddressOf':
                print("DEBUG: Compiling low-level operation: AddressOf")
                self.compiler.lowlevel.compile_operation(expr)
            elif type(expr).__name__ == 'SizeOf':
                print("DEBUG: Compiling low-level operation: SizeOf")
                self.compiler.lowlevel.compile_operation(expr)
            else:
                raise ValueError(f"Unsupported expression type: {type(expr)}")
        except Exception as e:
            print(f"ERROR: Expression compilation failed: {str(e)}")
            raise