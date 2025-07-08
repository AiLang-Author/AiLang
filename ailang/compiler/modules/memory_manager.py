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
    
    def compile_program(self, node):
        """Compile Program with stack management"""
        try:
            print("DEBUG: Starting program compilation")
            
            self.calculate_stack_size(node)
            
            print_buffer_size = 64
            temp_space = 128
            red_zone = 128
            total_space = self.compiler.stack_size + print_buffer_size + temp_space + red_zone
            
            self.compiler.codegen.emit_stack_frame_prologue(total_space)
            
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
            try:
                self.compiler.codegen.emit_stack_frame_epilogue()
                self.asm.emit_mov_rax_imm64(60)
                self.asm.emit_mov_rdi_imm64(1)
                self.asm.emit_syscall()
            except:
                pass
            raise
    
    def calculate_stack_size(self, node, depth=0):
        """Calculate stack size for variables"""
        if depth > 100:
            print(f"WARNING: Deep recursion detected at depth {depth}")
            return
        
        self.compiler.max_depth = max(self.compiler.max_depth, depth)
        
        if isinstance(node, Program):
            print(f"DEBUG: Calculating stack size for Program (depth {depth})")
            for decl in node.declarations:
                self.calculate_stack_size(decl, depth + 1)
                
        elif isinstance(node, Assignment):
            if node.target not in self.compiler.variables:
                self.compiler.stack_size += 16
                self.compiler.variables[node.target] = self.compiler.stack_size
                print(f"DEBUG: Allocated {node.target} at stack offset {self.compiler.stack_size}")
            
            if hasattr(node.value, '__class__'):
                self.calculate_stack_size(node.value, depth + 1)
                
        elif isinstance(node, While):
            print(f"DEBUG: Scanning While loop body for variables (depth {depth})")
            for stmt in node.body:
                self.calculate_stack_size(stmt, depth + 1)
                
        elif isinstance(node, If):
            print(f"DEBUG: Scanning If statement bodies for variables (depth {depth})")
            for stmt in node.then_body:
                self.calculate_stack_size(stmt, depth + 1)
            if node.else_body:
                for stmt in node.else_body:
                    self.calculate_stack_size(stmt, depth + 1)
                    
        elif isinstance(node, FunctionCall):
            print(f"DEBUG: Scanning FunctionCall arguments for variables (depth {depth})")
            for arg in node.arguments:
                self.calculate_stack_size(arg, depth + 1)
                
        elif hasattr(node, 'pool_type'):
            print(f"DEBUG: Scanning Pool {node.pool_type}.{node.name} for variables (depth {depth})")
            self.compile_pool_allocation(node)
            for item in node.body:
                self.calculate_stack_size(item, depth + 1)
                
        elif hasattr(node, 'body') and isinstance(node.body, list):
            print(f"DEBUG: Scanning generic body for variables (depth {depth})")
            for stmt in node.body:
                self.calculate_stack_size(stmt, depth + 1)
                
        elif hasattr(node, 'message'):
            self.calculate_stack_size(node.message, depth + 1)
    
    def compile_assignment(self, node):
        """Compile Assignment"""
        try:
            print(f"DEBUG: Assignment to {node.target}")
        
            if node.target not in self.compiler.variables:
                raise ValueError(f"Variable {node.target} not pre-allocated")
        
            if isinstance(node.value, Number):
                self.asm.emit_mov_rax_imm64(int(node.value.value))
            elif isinstance(node.value, String):
                string_offset = self.asm.add_string(node.value.value)
                string_addr = 0x402000 + string_offset
                self.asm.emit_mov_rax_imm64(string_addr)
            elif isinstance(node.value, Boolean):
                if node.value.value:  # True
                    self.asm.emit_mov_rax_imm64(1)
                    print(f"DEBUG: Loading boolean True as 1")
                else:  # False
                    self.asm.emit_mov_rax_imm64(0)
                    print(f"DEBUG: Loading boolean False as 0")
            elif isinstance(node.value, Identifier):
                resolved_name = self.compiler.resolve_acronym_identifier(node.value.name)
                if resolved_name in self.compiler.variables:
                    offset = self.compiler.variables[resolved_name]
                    self.asm.emit_bytes(0x48, 0x8B, 0x45)
                    self.asm.emit_bytes(256 - offset if offset <= 127 else 0)
                    if offset > 127:
                        self.asm.emit_bytes(0x48, 0x8B, 0x85)
                        self.asm.emit_bytes(*struct.pack('<i', -offset))
                else:
                    raise ValueError(f"Undefined variable: {resolved_name}")
            elif isinstance(node.value, FunctionCall):
                self.compiler.compile_function_call(node.value)
            elif hasattr(node.value, '__class__') and node.value.__class__.__name__ in ['AddressOf', 'Dereference', 'SizeOf']:
                # Handle low-level operations (AddressOf, Dereference, SizeOf)
                print(f"DEBUG: Compiling low-level operation: {node.value.__class__.__name__}")
                if hasattr(self.compiler, 'lowlevel') and self.compiler.lowlevel:
                    # Use the low-level operations module
                    self.compiler.lowlevel.compile_operation(node.value)
                else:
                    # Fallback: handle as assignment error
                    print(f"DEBUG: No lowlevel module available")
                    raise ValueError(f"Low-level operation {node.value.__class__.__name__} requires lowlevel module")
            else:
                raise ValueError(f"Unsupported assignment value: {type(node.value)}")
        
            # Store RAX value to variable
            offset = self.compiler.variables[node.target]
            if offset <= 127:
                self.asm.emit_bytes(0x48, 0x89, 0x45)
                self.asm.emit_bytes(256 - offset)
            else:
                self.asm.emit_bytes(0x48, 0x89, 0x85)
                self.asm.emit_bytes(*struct.pack('<i', -offset))
        
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
                    if hasattr(item.value, '__class__') and item.value.__class__.__name__ == 'String':
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
            print(f"ERROR: Failed to resolve acronym {identifier_name}: {str(e)}")            return identifier_name
