#!/usr/bin/env python3

# Copyright (c) 2025 Sean Collins, 2 Paws Machine and Engineering. All rights reserved.
#
# Licensed under the Sean Collins Software License (SCSL). See the LICENSE file in the root directory of this project
# for the full terms and conditions, including restrictions on forking, corporate use, and permissions for private/teaching purposes.

"""
User-Defined Functions Module for AILANG Compiler
Implements support for Function.Category.Name definitions
"""

import struct
from ailang_parser.ailang_ast import *

class UserFunctions:
    """Handles user-defined function compilation and calls"""
    
    def __init__(self, compiler_context):
        self.compiler = compiler_context
        self.asm = compiler_context.asm
        
        # Track user-defined functions
        # Key: "Category.Name", Value: {'label': label, 'params': [...], 'node': ast_node}
        self.user_functions = {}
        
        # Track current function context for returns
        self.current_function = None

    def is_function_registered(self, func_name: str) -> bool:
        """Checks if a function name has been registered."""
        # This is the method that ailang_compiler.py expects to exist.
        return func_name in self.user_functions
        
    def register_function(self, node):
        """Register a user-defined function during first pass"""
        try:
            # Extract function name from node
            func_name = node.name
            
            # CRITICAL FIX: Strip "Function." prefix if present
            if func_name.startswith("Function."):
                func_name = func_name[9:]  # Remove "Function." prefix
            
            # Generate unique label for this function
            label = self.asm.create_label()
            
            # Extract parameter names from input_params list of tuples
            params = []
            if hasattr(node, 'input_params') and node.input_params:
                params = [param[0] for param in node.input_params]  # Get just the names
            
            # Store function info
            self.user_functions[func_name] = {
                'label': label,
                'params': params,
                'node': node,
                'compiled': False
            }
            
            print(f"DEBUG: Registered user function: {func_name} with params {params} at label {label}")
            return label
            
        except Exception as e:
            print(f"ERROR: Failed to register function: {str(e)}")
            raise
    
    def compile_function_definition(self, node):
        """Compile a function definition"""
        try:
            func_name = node.name
            
            # CRITICAL FIX: Strip "Function." prefix if present
            if func_name.startswith("Function."):
                func_name = func_name[9:]
            
            if func_name not in self.user_functions:
                # Register it first (will also strip prefix)
                self.register_function(node)
            
            func_info = self.user_functions[func_name]
            
            # Skip if already compiled
            if func_info['compiled']:
                return
            
            print(f"DEBUG: Compiling function {func_name}")
            
            # Save current function context
            old_function = self.current_function
            self.current_function = func_name
            
            # CRITICAL FIX: Save global variable scope
            saved_variables = self.compiler.variables.copy()
            saved_stack_size = self.compiler.stack_size
            
            # Create fresh local scope but PRESERVE POOL VARIABLES
            self.compiler.variables = {}
            self.compiler.stack_size = 0
            
            # Copy pool variables (high bit set) to local scope
            # Pool variables are global and should be accessible everywhere
            for var_name, var_value in saved_variables.items():
                if var_value & 0x80000000:  # Pool variable marker
                    self.compiler.variables[var_name] = var_value
                    print(f"DEBUG: Preserved pool variable {var_name} in local scope")
            
            # Push function scope for parameter tracking
            if hasattr(self.compiler, 'scope_mgr'):
                self.compiler.scope_mgr.push_function(func_name)
            
            # CRITICAL FIX: Jump over function definition during normal execution
            skip_label = self.asm.create_label()
            self.asm.emit_jump_to_label(skip_label, "JMP")
            
            # Mark function entry point
            label = func_info['label']
            self.asm.mark_label(label)
            
            # Function prologue
            self.asm.emit_push_rbp()
            self.asm.emit_mov_rbp_rsp()

            # --- FIX: Save all callee-saved registers ---
            self.asm.emit_push_rbx()
            self.asm.emit_push_r12()
            self.asm.emit_push_r13()
            self.asm.emit_push_r14()
            
            # --- FIX: Dynamically calculate local variable space ---
            # Pre-scan function body to calculate space needed for locals.
            known_vars = set(self.compiler.variables.keys()) # Params + pool vars
            local_vars_needed = self.compiler.memory.scan_for_locals(node.body, known_vars)
            
            param_count = len(func_info['params'])
            local_space = len(local_vars_needed) * 16 # 16 bytes per local for safety
            print(f"DEBUG: Function '{func_name}' requires space for {param_count} params and {len(local_vars_needed)} locals ({local_space} bytes).")
            if param_count > 0 or local_space > 0:
                # Allocate space for parameters AND calculated local variables
                stack_space = ((param_count * 8 + local_space + 15) // 16) * 16
                
                # FIX: Use proper instruction encoding for stack allocation
                if stack_space < 128:
                    # Small immediate: SUB RSP, imm8
                    self.asm.emit_bytes(0x48, 0x83, 0xEC, stack_space)
                else:
                    # Large immediate: SUB RSP, imm32
                    self.asm.emit_bytes(0x48, 0x81, 0xEC)
                    self.asm.emit_bytes(*struct.pack('<I', stack_space))
                
                print(f"DEBUG: Allocated {stack_space} bytes for function locals")
            
            # Register parameters in LOCAL scope
            for i, param_name in enumerate(func_info['params']):
                self.compiler.stack_size += 8
                offset = self.compiler.stack_size
                self.compiler.variables[param_name] = offset
                print(f"DEBUG: Param {param_name} at local offset {offset}")
                
                # Register parameter in scope manager - THIS IS THE KEY FIX
                if hasattr(self.compiler, 'scope_mgr'):
                    self.compiler.scope_mgr.add_parameter(param_name, offset)
                    print(f"DEBUG: Registered param {param_name} with scope manager at offset {offset}")
                
                # Move from register to stack
                if i == 0:
                    self.asm.emit_mov_rax_rdi()
                elif i == 1:
                    self.asm.emit_mov_rax_rsi()
                elif i == 2:
                    self.asm.emit_bytes(0x48, 0x89, 0xD0)  # MOV RAX, RDX
                elif i == 3:
                    self.asm.emit_mov_rax_rcx()
                elif i == 4:
                    self.asm.emit_bytes(0x4C, 0x89, 0xC0)  # MOV RAX, R8
                elif i == 5:
                    self.asm.emit_bytes(0x4C, 0x89, 0xC8)  # MOV RAX, R9
                
                # Store to LOCAL stack frame
                self.asm.emit_bytes(0x48, 0x89, 0x45)  # MOV [RBP-offset], RAX
                self.asm.emit_bytes(256 - offset)
                print(f"DEBUG: Param {param_name} stored at [RBP-{offset}]")
            
            # Compile function body with local scope
            if hasattr(node, 'body'):
                for stmt in node.body:
                    self.compiler.compile_node(stmt)
            
            # Default return value if no explicit return
            if not self._has_return_statement(node):
                self.compile_return(None)
            
            # Mark as compiled
            func_info['compiled'] = True
            
            # CRITICAL FIX: Mark the skip label to continue normal execution
            self.asm.mark_label(skip_label)
            
            # Pop function scope
            if hasattr(self.compiler, 'scope_mgr'):
                self.compiler.scope_mgr.pop()
            
            # Restore function context
            self.current_function = old_function
            
            # CRITICAL FIX: Restore global variable scope
            self.compiler.variables = saved_variables
            self.compiler.stack_size = saved_stack_size
            
            print(f"DEBUG: Function {func_name} compiled successfully")
            
        except Exception as e:
            print(f"ERROR: Failed to compile function {func_name}: {str(e)}")
            raise
            
    def _count_local_variables(self, body):
        """Count local variables that will be created in function body"""
        # Simple heuristic - count assignments
        count = 0
        for stmt in body:
            if hasattr(stmt, 'target'):  # Assignment
                count += 1
        return count + 10  # Add some extra space for temporaries
    
    def compile_function_call(self, node):
        """Compile a call to a user-defined function"""
        try:
            func_name = node.function
            
            # Check if it's a user-defined function
            if func_name not in self.user_functions:
                return False  # Not a user function
            
            func_info = self.user_functions[func_name]
            
            print(f"DEBUG: Calling user function {func_name}")
            
            # --- REWRITE: Argument passing logic for correctness ---
            # This new logic prevents register clobbering during argument evaluation.
            # 1. Evaluate all arguments and push their results onto the stack.
            #    We do this in reverse order so they can be popped in the correct order.
            if hasattr(node, 'arguments') and node.arguments:
                num_args = len(node.arguments[:6])
                for i in range(num_args - 1, -1, -1):
                    arg = node.arguments[i]
                    self.compiler.compile_expression(arg)
                    self.asm.emit_push_rax()
            
            # 2. Pop the results from the stack into the correct parameter registers.
            if hasattr(node, 'arguments') and node.arguments:
                num_args = len(node.arguments[:6])
                for i in range(num_args):
                    if i == 0: self.asm.emit_pop_rdi()
                    elif i == 1: self.asm.emit_pop_rsi()
                    elif i == 2: self.asm.emit_pop_rdx()
                    elif i == 3: self.asm.emit_pop_rcx()
                    elif i == 4: self.asm.emit_bytes(0x41, 0x58) # POP R8
                    elif i == 5: self.asm.emit_bytes(0x41, 0x59) # POP R9
            # --- END REWRITE ---

            # Align stack before CALL (must be 16-byte aligned)
            # This is a common requirement and good practice.
            
            # Emit CALL instruction
            label = func_info['label']
            current_pos = len(self.asm.code)
            self.asm.emit_bytes(0xE8)  # CALL opcode
            
            if label in self.asm.labels:
                # Label position is known
                target_pos = self.asm.labels[label]
                offset = target_pos - (current_pos + 5)
                self.asm.emit_bytes(*struct.pack('<i', offset))
                print(f"DEBUG: CALL offset: {offset} ({hex(offset)})")
            else:
                # Forward reference - will be fixed later
                self.asm.emit_bytes(0x00, 0x00, 0x00, 0x00)
                # Add to fixup list
                if not hasattr(self.compiler, 'user_function_fixups'):
                    self.compiler.user_function_fixups = []
                self.compiler.user_function_fixups.append((func_name, current_pos))
                print(f"DEBUG: Forward reference added")
            
            # Result is in RAX
            print(f"DEBUG: User function {func_name} called")
            return True
            
        except Exception as e:
            print(f"ERROR: Failed to call function {func_name}: {str(e)}")
            raise
    
    def compile_return(self, value_node):
        """Compile ReturnValue statement"""
        try:
            print(f"DEBUG: Compiling ReturnValue")
            
            # Compile return value to RAX if provided
            if value_node:
                self.compiler.compile_expression(value_node)
            else:
                # Default return 0
                self.asm.emit_mov_rax_imm64(0)

            # Check if we're in a SubRoutine context
            if hasattr(self.compiler, 'compiling_subroutine') and self.compiler.compiling_subroutine:
                print("DEBUG: ReturnValue in SubRoutine - jumping to return label")
                # Jump to the subroutine's return label
                if hasattr(self.compiler, 'subroutine_return_label'):
                    self.asm.emit_jump_to_label(self.compiler.subroutine_return_label, "JMP")
                return
            
            # In a user Function context - do full epilogue
            if hasattr(self, 'current_function') and self.current_function:
                print("DEBUG: ReturnValue in Function - full epilogue")
                # Restore callee-saved registers
                self.asm.emit_pop_r14()
                self.asm.emit_pop_r13()
                self.asm.emit_pop_r12()
                self.asm.emit_pop_rbx()
                
                # Function epilogue
                self.asm.emit_mov_rsp_rbp()
                self.asm.emit_pop_rbp()
                self.asm.emit_ret()
            else:
                # Not in any special context
                print("DEBUG: ReturnValue in unknown context - value in RAX only")
            
            print(f"DEBUG: ReturnValue compiled")
            
        except Exception as e:
            print(f"ERROR: Failed to compile return: {str(e)}")
            raise
    
    def _has_return_statement(self, node):
        """Check if function has explicit return statement"""
        if hasattr(node, 'body'):
            for stmt in node.body:
                if hasattr(stmt, 'type') and stmt.type == 'ReturnValue':
                    return True
                # Could be FunctionCall to ReturnValue
                if isinstance(stmt, FunctionCall) and stmt.function == 'ReturnValue':
                    return True
        return False
    
    def process_all_functions(self, ast):
        """First pass: find and register all function definitions"""
        try:
            self._find_functions_in_node(ast)
            print(f"DEBUG: Found {len(self.user_functions)} user-defined functions")
        except Exception as e:
            print(f"ERROR: Failed to process functions: {str(e)}")
            raise
    
    def _find_functions_in_node(self, node):
        """Recursively find function definitions"""
        from ailang_parser.ailang_ast import Function
        
        # Check if it's a Function node by class
        if isinstance(node, Function):
            self.register_function(node)
        
        # Check for nested structures
        if hasattr(node, 'body'):
            if isinstance(node.body, list):
                for item in node.body:
                    self._find_functions_in_node(item)
        
        if hasattr(node, 'declarations'):
            for decl in node.declarations:
                self._find_functions_in_node(decl)