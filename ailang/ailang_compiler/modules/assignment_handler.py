#!/usr/bin/env python3

# Copyright (c) 2025 Sean Collins, 2 Paws Machine and Engineering. All rights reserved.
#
# Licensed under the Sean Collins Software License (SCSL). See the LICENSE file in the root directory of this project
# for the full terms and conditions, including restrictions on forking, corporate use, and permissions for private/teaching purposes.

"""
Assignment Handler Module for AILANG Compiler
Handles compilation of variable assignment statements.
"""

import struct
import sys
import os
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', 'ailang_parser')))
from ailang_ast import *
from .symbol_table import SymbolType

class AssignmentHandler:
    """Handles compilation of assignment statements."""

    def __init__(self, compiler_context):
        self.compiler = compiler_context
        self.asm = compiler_context.asm

    def compile_assignment(self, node):
        """Compile variable assignment with pool-aware addressing"""
        try:
            print(f"DEBUG: Assignment to {node.target}, value: {node.value}, type: {type(node.value)}")
            
            # Compile the value expression first (we need it in RAX for any assignment)
            if isinstance(node.value, (Number, String, Identifier, FunctionCall)):
                self.compiler.compile_expression(node.value)
            elif type(node.value).__name__ == 'RunTask':
                self.compiler.compile_node(node.value)
            elif isinstance(node.value, Boolean):
                print(f"DEBUG: Compiling boolean literal: {node.value.value}")
                value_to_emit = 1 if node.value.value else 0
                self.asm.emit_mov_rax_imm64(value_to_emit)
            else:
                value_type = type(node.value).__name__
                print(f"DEBUG: Attempting to compile {value_type} as expression")
                self.compiler.compile_expression(node.value)
            
            # Now handle the assignment with value in RAX
            # Use the symbol table for lookup
            symbol = self.compiler.symbol_table.lookup(node.target)
            if not symbol:
                raise ValueError(f"Undeclared variable '{node.target}' encountered during assignment. Semantic analysis failed.")
            
            # Check for DynamicPool member assignment
            if symbol.type == SymbolType.VARIABLE and '.' in symbol.name:
                parts = symbol.name.split('.')
                if len(parts) == 3 and parts[0] == 'DynamicPool':
                    pool_name = f"{parts[0]}.{parts[1]}"
                    member_key = parts[2]
                    pool_symbol = self.compiler.symbol_table.lookup(pool_name)
                    if pool_symbol and pool_symbol.metadata and pool_symbol.metadata.get('pool_type') == 'Dynamic':
                        print(f"DEBUG: Storing to dynamic pool var {symbol.name}")
                        self.asm.emit_push_rax() # Save value
                        member_offset = pool_symbol.metadata['members'][member_key]
                        # This needs to call the helper in PoolManager
                        self.compiler.pool_manager.emit_dynamic_pool_store(pool_symbol.offset, member_offset)
                        print(f"DEBUG: Assignment to DynamicPool member {symbol.name} completed")
                        return
            
            # Check if it's a pool variable (FixedPool)
            if symbol.offset & self.compiler.symbol_table.POOL_MARKER:
                pool_index = symbol.offset & ~self.compiler.symbol_table.POOL_MARKER
                print(f"DEBUG: Storing to pool var {symbol.name} at pool[{pool_index}]")
                self.asm.emit_bytes(0x49, 0x89, 0x87)  # MOV [R15 + disp32], RAX
                self.asm.emit_bytes(*struct.pack('<i', pool_index * 8))
            else:
                # Regular stack variable storage
                offset = symbol.offset
                print(f"DEBUG: Storing to stack var {symbol.name} at [RBP-{offset}]")
                
                # MOV [RBP - offset], RAX
                if offset <= 127:
                    # Small offset: use 8-bit displacement
                    self.asm.emit_bytes(0x48, 0x89, 0x45, (256 - offset) & 0xFF)  # MOV [RBP-offset], RAX
                else:
                    # Large offset: use 32-bit displacement
                    self.asm.emit_bytes(0x48, 0x89, 0x85)  # MOV [RBP-offset], RAX
                    self.asm.emit_bytes(*struct.pack('<i', -offset))
                
                print(f"DEBUG: Stored value to {symbol.name} at [RBP-{offset}]")
            
            print(f"DEBUG: Assignment to {node.target} completed")
            
        except Exception as e:
            print(f"ERROR: Assignment compilation failed: {str(e)}")
            raise