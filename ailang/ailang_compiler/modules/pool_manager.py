#!/usr/bin/env python3
"""
Pool Management Module for AILANG Compiler
Handles compilation of FixedPool and DynamicPool operations.
"""

import struct
import sys
import os
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', 'ailang_parser')))
from ailang_ast import *
from .symbol_table import SymbolType

class PoolManager:
    """Handles compilation of pool-related operations."""

    def __init__(self, compiler_context):
        self.compiler = compiler_context
        self.asm = compiler_context.asm
        # These were previously in MemoryManager, now they belong here
        self.pool_variables = {} # For FixedPools, mapping var_name to pool_index
        self.dynamic_pool_metadata = {} # For DynamicPools, mapping pool_name to {'members': {}}

    def emit_dynamic_pool_store(self, pool_stack_offset, member_offset):
        """Helper to emit code for storing a value in a dynamic pool member."""
        # Assumes value to store is on the stack
        # Load pool base pointer from stack into RCX
        self.asm.emit_bytes(0x48, 0x8B, 0x8D, *struct.pack('<i', -pool_stack_offset)) # MOV RCX, [RBP - offset]
        # Add member offset to base pointer
        self.asm.emit_bytes(0x48, 0x81, 0xC1, *struct.pack('<i', member_offset)) # ADD RCX, member_offset
        # Pop value from stack into RAX and store it
        self.asm.emit_pop_rax()
        self.asm.emit_bytes(0x48, 0x89, 0x01) # MOV [RCX], RAX

    def compile_pool_resize(self, node):
        """Compile PoolResize(pool_name, new_size) - resize a memory pool"""
        try:
            print("DEBUG: Compiling PoolResize")
            
            self.compiler.compile_expression(node.arguments[1])
            self.asm.emit_push_rax()
            
            if type(node.arguments[0]).__name__ == 'Identifier':
                pool_name = node.arguments[0].name
                print(f"DEBUG: Resizing pool {pool_name}")
            
            self.asm.emit_pop_rsi()
            self.asm.emit_mov_rax_imm64(9)
            self.asm.emit_mov_rdi_imm64(0)
            self.asm.emit_mov_rdx_imm64(3)
            self.asm.emit_mov_r10_imm64(0x22)
            self.asm.emit_mov_r8_imm64(0xFFFFFFFFFFFFFFFF)
            self.asm.emit_mov_r9_imm64(0)
            self.asm.emit_syscall()
            
            error_label = self.asm.create_label()
            success_label = self.asm.create_label()
            
            self.asm.emit_bytes(0x48, 0x83, 0xF8, 0x00)
            self.asm.emit_jump_to_label(error_label, "JL")
            self.asm.emit_jump_to_label(success_label, "JMP")
            
            self.asm.mark_label(error_label)
            self.asm.emit_mov_rax_imm64(0)
            
            self.asm.mark_label(success_label)
            print("DEBUG: PoolResize completed")
            return True
            
        except Exception as e:
            print(f"ERROR: PoolResize compilation failed: {str(e)}")
            raise

    def compile_pool_move(self, node):
        """Compile PoolMove(data, from_pool, to_pool) - uses size from header"""
        try:
            print("DEBUG: Compiling PoolMove with dynamic size")
            
            self.compiler.compile_expression(node.arguments[0])
            self.asm.emit_push_rax()
            
            self.asm.emit_bytes(0x48, 0x8B, 0x48, 0xF8)
            self.asm.emit_push_rcx()
            
            self.asm.emit_bytes(0x48, 0x83, 0xC1, 0x08)
            self.asm.emit_mov_rsi_rcx()
            
            self.asm.emit_mov_rax_imm64(9)
            self.asm.emit_mov_rdi_imm64(0)
            self.asm.emit_mov_rdx_imm64(3)
            self.asm.emit_mov_r10_imm64(0x22)
            self.asm.emit_mov_r8_imm64(0xFFFFFFFFFFFFFFFF)
            self.asm.emit_mov_r9_imm64(0)
            self.asm.emit_syscall()
            
            error_label = self.asm.create_label()
            success_label = self.asm.create_label()
            
            self.asm.emit_bytes(0x48, 0x85, 0xC0)
            self.asm.emit_jump_to_label(error_label, "JZ")
            
            self.asm.emit_mov_rdi_rax()
            
            self.asm.emit_pop_rcx()
            self.asm.emit_push_rcx()
            self.asm.emit_bytes(0x48, 0x89, 0x08)
            
            self.asm.emit_bytes(0x48, 0x83, 0xC7, 0x08)
            self.asm.emit_push_rdi()
            
            # This needs to be updated to use symbol table lookup for stack offset
            # self.asm.emit_mov_rsi_from_stack_offset(16) 
            
            self.asm.emit_pop_rax()
            self.asm.emit_pop_rcx()
            self.asm.emit_push_rax()
            
            self.asm.emit_bytes(0xF3, 0xA4)
            
            self.asm.emit_pop_rax()
            self.asm.emit_add_rsp_imm8(8)
            self.asm.emit_jump_to_label(success_label, "JMP")
            
            self.asm.mark_label(error_label)
            self.asm.emit_mov_rax_imm64(0)
            self.asm.emit_add_rsp_imm8(16)
            
            self.asm.mark_label(success_label)
            print("DEBUG: PoolMove with dynamic size completed")
            return True
            
        except Exception as e:
            print(f"ERROR: PoolMove compilation failed: {str(e)}")
            raise
    
    def compile_pool_compact(self, node):
        """Compile PoolCompact(pool_name) - defragment a memory pool"""
        try:
            print("DEBUG: Compiling PoolCompact")
            
            self.asm.emit_mov_rcx_imm64(0)
            self.asm.emit_mov_rdx_imm64(0)
            
            scan_loop = self.asm.create_label()
            scan_end = self.asm.create_label()
            
            self.asm.mark_label(scan_loop)
            
            self.asm.emit_bytes(0x48, 0x81, 0xF9)
            self.asm.emit_bytes(0x00, 0x10, 0x00, 0x00)
            self.asm.emit_jump_to_label(scan_end, "JGE")
            
            self.asm.emit_bytes(0x48, 0x83, 0xC1, 0x10)
            self.asm.emit_jump_to_label(scan_loop, "JMP")
            
            self.asm.mark_label(scan_end)
            
            self.asm.emit_mov_rax_rcx()
            
            print("DEBUG: PoolCompact completed")
            return True
            
        except Exception as e:
            print(f"ERROR: PoolCompact compilation failed: {str(e)}")
            raise

    def compile_pool_allocate(self, node):
        """Compile PoolAllocate(pool_name, size)"""
        try:
            print("DEBUG: Compiling PoolAllocate")
            pool_name = node.arguments[0].value if isinstance(node.arguments[0], String) else node.arguments[0].name
            size_node = node.arguments[1]
            size = int(size_node.value) if isinstance(size_node, Number) else 8
            if size <= 0:
                size = 8
            size += 8
            
            self.compiler.compile_expression(size_node)
            self.asm.emit_mov_rsi_rax()  # Size in RSI
            
            self.asm.emit_mov_rax_imm64(9)  # mmap syscall
            self.asm.emit_mov_rdi_imm64(0)  # addr = NULL
            self.asm.emit_mov_rdx_imm64(3)  # PROT_READ | PROT_WRITE
            self.asm.emit_mov_r10_imm64(0x22)  # MAP_PRIVATE | MAP_ANONYMOUS
            self.asm.emit_mov_r8_imm64(0xFFFFFFFFFFFFFFFF)  # fd = -1
            self.asm.emit_mov_r9_imm64(0)  # offset = 0
            self.asm.emit_syscall()
            
            error_label = self.asm.create_label()
            success_label = self.asm.create_label()
            
            self.asm.emit_bytes(0x48, 0x85, 0xC0)  # TEST RAX, RAX
            self.asm.emit_jump_to_label(error_label, "JZ")
            
            # Store mmap address immediately
            var_name = node.variable if hasattr(node, 'variable') else pool_name
            # This needs to use the new symbol table for lookup
            # resolved_name = self.compiler.resolve_acronym_identifier(var_name)
            # if resolved_name not in self.compiler.variables:
            #     offset = self.compiler.stack_size
            #     self.compiler.variables[resolved_name] = offset
            #     self.compiler.stack_size += 8
            #     print(f"DEBUG: Allocated {resolved_name} at [rbp - {offset}]")
            # offset = self.compiler.variables[resolved_name]
            # if offset <= 0 or offset > self.compiler.stack_size:
            #     print(f"ERROR: Invalid stack offset {offset} for {resolved_name}")
            #     raise ValueError(f"Invalid stack offset {offset} for {resolved_name}")
            # self.asm.emit_bytes(0x48, 0x89, 0x85)
            # self.asm.emit_bytes(*struct.pack('<i', -offset))
            # self.asm.emit_push_rax()
            # self.asm.emit_print_number()  # Log mmap address
            # self.asm.emit_pop_rax()
            # print(f"DEBUG: Stored mmap address in {resolved_name} at [rbp - {offset}]")
            
            self.asm.emit_mov_rbx_rax()  # Save address in RBX
            self.asm.emit_mov_rax_rsi()  # Restore size in RAX
            self.asm.emit_bytes(0x48, 0x89, 0x03)  # Store size at [RBX]
            self.asm.emit_lea_rax_rbx(8)  # RAX = RBX + 8
            
            self.asm.emit_jump_to_label(success_label, "JMP")
            
            self.asm.mark_label(error_label)
            self.asm.emit_mov_rax_imm64(0)
            print(f"DEBUG: mmap allocation failed for {pool_name}")
            
            self.asm.mark_label(success_label)
            print("DEBUG: PoolAllocate with size header completed")
            return True
        except Exception as e:
            print(f"ERROR: PoolAllocate compilation failed: {str(e)}")
            raise

    def compile_pool_free(self, node):
        """Compile PoolFree(pool_name, address) - return memory to pool"""
        try:
            print("DEBUG: Compiling PoolFree")
            
            self.compiler.compile_expression(node.arguments[1])
            
            self.asm.emit_mov_rax_imm64(1)
            
            print("DEBUG: PoolFree completed")
            return True
            
        except Exception as e:
            print(f"ERROR: PoolFree compilation failed: {str(e)}")
            raise 
        
    def compile_pool_get_size(self, node):
        """Get allocated size from pool allocation header"""
        try:
            print("DEBUG: Compiling PoolGetSize")
            
            self.compiler.compile_expression(node.arguments[0])
            
            self.asm.emit_bytes(0x48, 0x8B, 0x40, 0xF8)
            
            print("DEBUG: PoolGetSize completed")
            return True
            
        except Exception as e:
            print(f"ERROR: PoolGetSize compilation failed: {str(e)}")
            raise