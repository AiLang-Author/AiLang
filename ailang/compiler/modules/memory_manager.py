#!/usr/bin/env python3
"""
Memory Management Module for AILANG Compiler
Handles stack allocation, variable management, and pool operations
"""

import struct
import sys
import os
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', 'ailang_parser')))
from ailang_ast import *

class MemoryManager:
    """Handles memory-related operations"""
    
    def __init__(self, compiler_context):
        self.compiler = compiler_context
        self.asm = compiler_context.asm
        self.pool_metadata = {}
        self.allocation_headers = {}

    # In memory_manager.py
    def compile_program(self, node):
        """Compile Program with stack management"""
        try:
            print("DEBUG: Starting program compilation")
            self.calculate_stack_size(node)
            
            print_buffer_size = 64
            temp_space = 128
            red_zone = 128
            allocate_space = self.scan_allocate_sizes(node)
            print(f"DEBUG: Total Allocate space needed: {allocate_space}")
            total_space = self.compiler.stack_size + print_buffer_size + temp_space + red_zone + allocate_space
            print(f"DEBUG: Stack calculation - {len(self.compiler.variables)} vars = {self.compiler.stack_size} bytes, total allocated: {total_space}")
            self.compiler.codegen.emit_stack_frame_prologue(total_space)
            
            # Zero-initialize all stack variables
            for var_name, offset in self.compiler.variables.items():
                self.asm.emit_mov_rax_imm64(0)
                self.asm.emit_bytes(0x48, 0x89, 0x85)
                self.asm.emit_bytes(*struct.pack('<i', -offset))
                print(f"DEBUG: Zero-initialized {var_name} at [rbp - {offset}]")
            
            self.asm.emit_push_rbx()
            self.asm.emit_push_r12()
            self.asm.emit_push_r13()
            self.asm.emit_push_r14()
            self.asm.emit_push_r15()
            
            for decl in node.declarations:
                self.compiler.compile_node(decl)
            
            self.asm.emit_pop_r15()
            self.asm.emit_pop_r14()
            self.asm.emit_pop_r13()
            self.asm.emit_pop_r12()
            self.asm.emit_pop_rbx()
            
            self.compiler.codegen.emit_stack_frame_epilogue()
            
            self.asm.emit_mov_rax_imm64(60)
            self.asm.emit_mov_rdi_imm64(0)
            self.asm.emit_syscall()
            
            print("DEBUG: Program compilation completed")
        except Exception as e:
            print(f"ERROR: Program compilation failed: {str(e)}")
            self.compiler.codegen.emit_stack_frame_epilogue()
            raise
    
    def calculate_stack_size(self, node, depth=0):
        """Calculate stack size for variables"""
        if depth > 100:
            print(f"WARNING: Deep recursion detected at depth {depth}")
            return
        
        self.compiler.max_depth = max(self.compiler.max_depth, depth)
        
        node_type = type(node).__name__
        
        if hasattr(node, 'declarations'):  # Program node
            for decl in node.declarations:
                self.calculate_stack_size(decl, depth + 1)
        
        elif node_type == 'Assignment' or (hasattr(node, 'target') and hasattr(node, 'value')):
            if node.target not in self.compiler.variables:
                self.compiler.stack_size += 16
                self.compiler.variables[node.target] = self.compiler.stack_size
                print(f"DEBUG: Allocated {node.target} at stack offset {self.compiler.stack_size}")
            
            if hasattr(node.value, '__class__'):
                self.calculate_stack_size(node.value, depth + 1)
        
        elif node_type == 'While':
            for stmt in node.body:
                self.calculate_stack_size(stmt, depth + 1)
        
        elif node_type == 'Loop':
            for stmt in node.body:
                self.calculate_stack_size(stmt, depth + 1)
        
        elif node_type == 'If':
            for stmt in node.then_body:
                self.calculate_stack_size(stmt, depth + 1)
            if node.else_body:
                for stmt in node.else_body:
                    self.calculate_stack_size(stmt, depth + 1)
        
        elif node_type == 'FunctionCall':
            if hasattr(node, 'arguments'):
                for arg in node.arguments:
                    self.calculate_stack_size(arg, depth + 1)
        
        elif hasattr(node, 'pool_type'):  # Pool node
            self.compile_pool_allocation(node)
            for item in node.body:
                self.calculate_stack_size(item, depth + 1)
        
        elif hasattr(node, 'body') and isinstance(node.body, list):
            for stmt in node.body:
                self.calculate_stack_size(stmt, depth + 1)
        
        elif hasattr(node, 'message'):
            self.calculate_stack_size(node.message, depth + 1)
    

    def scan_allocate_sizes(self, node, depth=0):
        """Scan AST for Allocate calls and sum their sizes"""
        total_alloc = 0
        
        if hasattr(node, '__dict__'):
            # Check if this is an Allocate call
            if hasattr(node, 'function') and node.function == 'Allocate':
                if hasattr(node, 'arguments') and len(node.arguments) > 0:
                    arg = node.arguments[0]
                    if hasattr(arg, 'value'):
                        try:
                            size = int(arg.value)
                            total_alloc += size
                            print(f"DEBUG: Found Allocate({size}) at depth {depth}")
                        except:
                            # If it's not a constant, reserve 1KB as safety
                            total_alloc += 1024
                            print(f"DEBUG: Found dynamic Allocate, reserving 1024")
            
            # Recursively scan all attributes
            for attr_name in dir(node):
                if not attr_name.startswith('_'):
                    attr = getattr(node, attr_name)
                    if isinstance(attr, list):
                        for item in attr:
                            total_alloc += self.scan_allocate_sizes(item, depth + 1)
                    elif hasattr(attr, '__dict__'):
                        total_alloc += self.scan_allocate_sizes(attr, depth + 1)
        
        return total_alloc

    def compile_assignment(self, node):
        """Compile variable assignment with enhanced type support"""
        try:
            print(f"DEBUG: Assignment to {node.target}, value: {node.value}, type: {type(node.value)}")
            
            # Compile the value expression first
            if isinstance(node.value, (Number, String, Identifier, FunctionCall)):
                # These are handled by compile_expression
                self.compiler.compile_expression(node.value)
            elif type(node.value).__name__ == 'RunTask':
                # Compile RunTask - it should leave result in RAX
                self.compiler.compile_node(node.value)
            else:
                # Try to compile as expression for other types
                value_type = type(node.value).__name__
                print(f"DEBUG: Attempting to compile {value_type} as expression")
                self.compiler.compile_expression(node.value)
            
            # Result is now in RAX, store it in the variable
            resolved_name = self.compiler.resolve_acronym_identifier(node.target)
            
            if resolved_name not in self.compiler.variables:
                # Allocate space for new variable
                self.compiler.stack_size += 16
                offset = self.compiler.stack_size
                self.compiler.variables[resolved_name] = offset
                print(f"DEBUG: Allocated {resolved_name} at stack offset {offset}")
            else:
                offset = self.compiler.variables[resolved_name]
            
            # Store RAX to [RBP - offset]
            self.compiler.asm.emit_bytes(0x48, 0x89, 0x85)
            self.compiler.asm.emit_bytes(*struct.pack('<i', -offset))
            
            print(f"DEBUG: Assignment to {node.target} completed")
            
        except Exception as e:
            print(f"ERROR: Assignment compilation failed: {str(e)}")
            raise
    
    def compile_resource_item(self, item, pool):
        """Compile a resource item"""
        try:
            pool_name = f"{pool.pool_type}.{pool.name}"
            var_name = f"{pool_name}.{item.key}"
            
            print(f"DEBUG: Compiling resource item {var_name}")
            
            if item.value is not None:
                if var_name not in self.compiler.variables:
                    self.compiler.stack_size += 16
                    self.compiler.variables[var_name] = self.compiler.stack_size
                    print(f"DEBUG: Allocated pool variable {var_name} at stack offset {self.compiler.stack_size}")
                
                if hasattr(item.value, 'value'):
                    item_type = type(item.value).__name__
                    if item_type == 'String':
                        string_value = item.value.value
                        print(f"DEBUG: Initializing {var_name} = '{string_value}' (string)")
                        string_offset = self.asm.add_string(string_value)
                        value = 0x402000 + string_offset
                        print(f"DEBUG: String stored at address {value}")
                    else:
                        value = int(item.value.value)
                        print(f"DEBUG: Initializing {var_name} = {value} (number)")
                    
                    self.asm.emit_mov_rax_imm64(value)
                    offset = self.compiler.variables[var_name]
                    if offset <= 127:
                        self.asm.emit_bytes(0x48, 0x89, 0x45, 256 - offset)
                    else:
                        self.asm.emit_bytes(0x48, 0x89, 0x85)
                        self.asm.emit_bytes(*struct.pack('<i', -offset))
                    
                    print(f"DEBUG: Initialized {var_name} with value {value}")
            
        except Exception as e:
            print(f"ERROR: Resource item compilation failed: {str(e)}")
            raise
    
    def compile_pool(self, node):
        """Compile Pool declaration"""
        try:
            pool_name = f"{node.pool_type}.{node.name}"
            print(f"DEBUG: Compiling pool {pool_name}")
            
            for item in node.body:
                if hasattr(item, 'key'):
                    self.compile_resource_item(item, node)
            
            print(f"DEBUG: Pool {pool_name} compilation completed")
            return True
            
        except Exception as e:
            print(f"ERROR: Pool compilation failed: {str(e)}")
            raise
    
    def compile_pool_allocation(self, node):
        """Pre-allocate pool variables"""
        try:
            if hasattr(node, 'pool_type') and hasattr(node, 'name'):
                pool_name = f"{node.pool_type}.{node.name}"
                print(f"DEBUG: Pre-allocating pool {pool_name}")
                
                for item in node.body:
                    if hasattr(item, 'key'):
                        var_name = f"{pool_name}.{item.key}"
                        if var_name not in self.compiler.variables:
                            self.compiler.stack_size += 16
                            self.compiler.variables[var_name] = self.compiler.stack_size
                            print(f"DEBUG: Pre-allocated pool variable {var_name} at offset {self.compiler.stack_size}")
            
        except Exception as e:
            print(f"ERROR: Pool allocation failed: {str(e)}")
            raise
    
    def compile_acronym_definitions(self, node):
        """Compile acronym definitions"""
        try:
            print(f"DEBUG: Processing {len(node.definitions)} acronym definitions")
            
            for acronym, full_name in node.definitions.items():
                if acronym in self.compiler.acronym_table:
                    print(f"WARNING: Redefining acronym {acronym}")
                
                self.compiler.acronym_table[acronym] = full_name
                print(f"DEBUG: Mapped {acronym} -> {full_name}")
                
            print(f"DEBUG: Acronym table now contains: {self.compiler.acronym_table}")
            
        except Exception as e:
            print(f"ERROR: Failed to compile acronym definitions: {str(e)}")
            raise
    
    def resolve_acronym_identifier(self, identifier_name):
        """Resolve acronym.variable syntax"""
        try:
            parts = identifier_name.split('.')
            
            if len(parts) == 2:
                acronym, variable = parts
                
                if acronym in self.compiler.acronym_table:
                    full_namespace = self.compiler.acronym_table[acronym]
                    
                    candidates = [
                        f"{full_namespace}.{variable}",
                        f"FixedPool.{full_namespace}.{variable}",
                        f"DynamicPool.{full_namespace}.{variable}",
                        f"TemporalPool.{full_namespace}.{variable}",
                        f"NeuralPool.{full_namespace}.{variable}",
                        f"KernelPool.{full_namespace}.{variable}",
                        f"ActorPool.{full_namespace}.{variable}",
                        f"SecurityPool.{full_namespace}.{variable}",
                        f"ConstrainedPool.{full_namespace}.{variable}"
                    ]
                    
                    for candidate in candidates:
                        if candidate in self.compiler.variables:
                            print(f"DEBUG: Resolved {identifier_name} -> {candidate}")
                            return candidate
                    
                    basic_resolution = f"{full_namespace}.{variable}"
                    print(f"DEBUG: Basic resolution {identifier_name} -> {basic_resolution}")
                    return basic_resolution
            
            return identifier_name
            
        except Exception as e:
            print(f"ERROR: Failed to resolve acronym {identifier_name}: {str(e)}")
            return identifier_name
    
    # All the pool operation methods remain the same
    def compile_pool_resize(self, node):
        """Compile PoolResize(pool_name, new_size) - resize a memory pool"""
        try:
            print("DEBUG: Compiling PoolResize")
            
            self.compiler.compile_expression(node.arguments[1])
            self.asm.emit_push_rax()
            
            if type(node.arguments[0]).__name__ == 'Identifier':
                pool_name = node.arguments[0].name
                resolved_pool = self.resolve_acronym_identifier(pool_name)
                print(f"DEBUG: Resizing pool {resolved_pool}")
            
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
            
            self.asm.emit_mov_rsi_from_stack_offset(16)
            
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

       
    # In memory_manager.py
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
            resolved_name = self.compiler.resolve_acronym_identifier(var_name)
            if resolved_name not in self.compiler.variables:
                offset = self.compiler.stack_size
                self.compiler.variables[resolved_name] = offset
                self.compiler.stack_size += 8
                print(f"DEBUG: Allocated {resolved_name} at [rbp - {offset}]")
            offset = self.compiler.variables[resolved_name]
            if offset <= 0 or offset > self.compiler.stack_size:
                print(f"ERROR: Invalid stack offset {offset} for {resolved_name}")
                raise ValueError(f"Invalid stack offset {offset} for {resolved_name}")
            self.asm.emit_bytes(0x48, 0x89, 0x85)
            self.asm.emit_bytes(*struct.pack('<i', -offset))
            self.asm.emit_push_rax()
            self.asm.emit_print_number()  # Log mmap address
            self.asm.emit_pop_rax()
            print(f"DEBUG: Stored mmap address in {resolved_name} at [rbp - {offset}]")
            
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