#!/usr/bin/env python3
"""
Arithmetic Operations Module
Handles all arithmetic and comparison operations for AILANG compiler
"""

import sys
import os

from ..ailang_ast import *

class ArithmeticOps:
    """Module for handling arithmetic and comparison operations"""
    
    def __init__(self, compiler_context):
        """Initialize with reference to main compiler context"""
        self.compiler = compiler_context
        self.asm = compiler_context.asm
    
    def compile_operation(self, node):
        """
        Compile arithmetic operations (Add, Subtract, Multiply, Divide)
        """
        try:
            if node.function == 'Add' and len(node.arguments) == 2:
                print(f"DEBUG: Compiling Add({self._get_arg_name(node.arguments[0])}, {self._get_arg_name(node.arguments[1])})")
                
                # Evaluate first argument
                self.compiler.compile_expression(node.arguments[0])
                self.asm.emit_push_rax()
                print("DEBUG: Pushed first argument")
                
                # Evaluate second argument
                self.compiler.compile_expression(node.arguments[1])
                
                # Perform addition
                self.asm.emit_mov_rbx_rax()
                self.asm.emit_pop_rax()
                self.asm.emit_add_rax_rbx()
                print("DEBUG: Add operation completed")
                return True
            
            elif node.function == 'Subtract' and len(node.arguments) == 2:
                print(f"DEBUG: Compiling Subtract({self._get_arg_name(node.arguments[0])}, {self._get_arg_name(node.arguments[1])})")
                
                # Evaluate first argument
                self.compiler.compile_expression(node.arguments[0])
                self.asm.emit_push_rax()
                print("DEBUG: Pushed first argument")
                
                # Evaluate second argument
                self.compiler.compile_expression(node.arguments[1])
                
                # Perform subtraction
                self.asm.emit_mov_rbx_rax()
                self.asm.emit_pop_rax()
                self.asm.emit_sub_rax_rbx()
                print("DEBUG: Subtract operation completed")
                return True
            
            elif node.function == 'Multiply' and len(node.arguments) == 2:
                print(f"DEBUG: Compiling Multiply({self._get_arg_name(node.arguments[0])}, {self._get_arg_name(node.arguments[1])})")
                
                # Evaluate first argument
                self.compiler.compile_expression(node.arguments[0])
                self.asm.emit_push_rax()
                print("DEBUG: Pushed first argument")
                
                # Evaluate second argument
                self.compiler.compile_expression(node.arguments[1])
                
                # Perform multiplication
                self.asm.emit_mov_rbx_rax()
                self.asm.emit_pop_rax()
                self.asm.emit_imul_rax_rbx()
                print("DEBUG: Multiply operation completed")
                return True
            
            elif node.function == 'Divide' and len(node.arguments) == 2:
                print(f"DEBUG: Compiling Divide({self._get_arg_name(node.arguments[0])}, {self._get_arg_name(node.arguments[1])})")
                
                # Evaluate first argument
                self.compiler.compile_expression(node.arguments[0])
                self.asm.emit_push_rax()
                print("DEBUG: Pushed first argument")
                
                # Evaluate second argument
                self.compiler.compile_expression(node.arguments[1])
                
                # Perform division
                self.asm.emit_mov_rbx_rax()
                self.asm.emit_pop_rax()
                self.asm.emit_xor_rdx_rdx()  # Clear RDX for division
                self.asm.emit_bytes(0x48, 0xF7, 0xF3)  # DIV RBX
                print("DEBUG: Divide operation completed")
                return True
            
            # Not an arithmetic operation
            return False
            
        except Exception as e:
            print(f"ERROR: Arithmetic operation compilation failed: {str(e)}")
            raise
    
    def compile_comparison(self, node):
        """
        Compile comparison operations (LessThan, GreaterThan, EqualTo)
        """
        try:
            if node.function in ['LessThan', 'GreaterThan', 'EqualTo'] and len(node.arguments) == 2:
                print(f"DEBUG: Compiling {node.function}({self._get_arg_name(node.arguments[0])}, {self._get_arg_name(node.arguments[1])})")
                
                # Evaluate first argument
                self.compiler.compile_expression(node.arguments[0])
                self.asm.emit_push_rax()
                print("DEBUG: Pushed first argument")
                
                # Evaluate second argument
                self.compiler.compile_expression(node.arguments[1])
                
                # Perform comparison
                self.asm.emit_mov_rbx_rax()
                self.asm.emit_pop_rax()
                self.asm.emit_bytes(0x48, 0x39, 0xD8)  # CMP RAX, RBX
                
                # Set result based on comparison
                if node.function == 'LessThan':
                    self.asm.emit_bytes(0x0F, 0x9C, 0xC0)  # SETL AL
                elif node.function == 'GreaterThan':
                    self.asm.emit_bytes(0x0F, 0x9F, 0xC0)  # SETG AL
                elif node.function == 'EqualTo':
                    self.asm.emit_bytes(0x0F, 0x94, 0xC0)  # SETE AL
                
                # Zero extend AL to RAX
                self.asm.emit_bytes(0x48, 0x0F, 0xB6, 0xC0)  # MOVZX RAX, AL
                print(f"DEBUG: {node.function} operation completed")
                return True
            
            # Not a comparison operation
            return False
            
        except Exception as e:
            print(f"ERROR: Comparison operation compilation failed: {str(e)}")
            raise
    
    def _get_arg_name(self, arg):
        """Helper to get argument name for debugging"""
        if hasattr(arg, 'name'):  # Identifier
            return arg.name
        elif hasattr(arg, 'value'):  # Number or String
            return str(arg.value)
        elif hasattr(arg, 'function'):  # FunctionCall
            return f"{arg.function}(...)"
        else:
            return type(arg).__name__