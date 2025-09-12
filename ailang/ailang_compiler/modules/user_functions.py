#!/usr/bin/env python3
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
            
            # CRITICAL FIX: Jump over function definition during normal execution
            skip_label = self.asm.create_label()
            self.asm.emit_jump_to_label(skip_label, "JMP")
            
            # Mark function entry point
            label = func_info['label']
            self.asm.mark_label(label)
            
            # Function prologue
            self.asm.emit_push_rbp()
            self.asm.emit_mov_rbp_rsp()
            
            # Allocate space for local variables if we have parameters
            param_count = len(func_info['params'])
            if param_count > 0:
                # Allocate 8 bytes per parameter for storage
                stack_space = ((param_count * 8 + 15) // 16) * 16  # Align to 16 bytes
                self.asm.emit_bytes(0x48, 0x83, 0xEC, stack_space)  # SUB RSP, stack_space
                print(f"DEBUG: Allocated {stack_space} bytes for {param_count} parameters")
            
            # CRITICAL FIX: Register parameters in variables BEFORE compiling body
            # First, register all parameters as variables so they can be referenced
            for i, param_name in enumerate(func_info['params'][:6]):
                offset = (i + 1) * 8
                # Register variable
                self.compiler.variables[param_name] = offset
                print(f"DEBUG: Registered param {param_name} at offset {offset}")
                
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
                
                # Store to stack
                self.asm.emit_bytes(0x48, 0x89, 0x45)  # MOV [RBP-offset], RAX
                self.asm.emit_bytes(256 - offset)
                print(f"DEBUG: Param {param_name} moved to [RBP-{offset}]")
            
            # Compile function body
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
            
            # Restore function context
            self.current_function = old_function
            
            # Clean up parameter variables
            for param_name in func_info['params']:
                if param_name in self.compiler.variables:
                    del self.compiler.variables[param_name]
            
            print(f"DEBUG: Function {func_name} compiled successfully")
            
        except Exception as e:
            print(f"ERROR: Failed to compile function {func_name}: {str(e)}")
            raise
    
    def compile_function_call(self, node):
        """Compile a call to a user-defined function"""
        try:
            func_name = node.function
            
            # Check if it's a user-defined function
            if func_name not in self.user_functions:
                return False  # Not a user function
            
            func_info = self.user_functions[func_name]
            
            print(f"DEBUG: Calling user function {func_name}")
            
            # Compile arguments and pass in registers
            if hasattr(node, 'arguments') and node.arguments:
                for i, arg in enumerate(node.arguments[:6]):
                    # Compile argument to RAX
                    self.compiler.compile_expression(arg)
                    
                    # Move to appropriate parameter register
                    if i == 0:
                        self.asm.emit_mov_rdi_rax()
                    elif i == 1:
                        self.asm.emit_mov_rsi_rax()
                    elif i == 2:
                        self.asm.emit_mov_rdx_rax()
                    elif i == 3:
                        self.asm.emit_mov_rcx_rax()
                    elif i == 4:
                        self.asm.emit_bytes(0x49, 0x89, 0xC0)  # MOV R8, RAX
                    elif i == 5:
                        self.asm.emit_bytes(0x49, 0x89, 0xC1)  # MOV R9, RAX
            
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
            
            # Function epilogue - restore stack and frame
            self.asm.emit_mov_rsp_rbp()
            self.asm.emit_pop_rbp()
            self.asm.emit_ret()
            
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