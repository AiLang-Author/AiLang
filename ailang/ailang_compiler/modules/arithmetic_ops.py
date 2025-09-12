#!/usr/bin/env python3
"""
Arithmetic Operations Module
Handles all arithmetic and comparison operations for AILANG compiler
"""

import sys
import os
import struct
from ailang_parser.ailang_ast import *

class ArithmeticOps:
    """Module for handling arithmetic and comparison operations"""
    
    def __init__(self, compiler_context):
        """Initialize with reference to main compiler context"""
        self.compiler = compiler_context
        self.asm = compiler_context.asm
    
    def compile_operation(self, node):
        """
        Compile arithmetic operations (Add, Subtract, Multiply, Divide, BitwiseAnd, BitwiseOr)
        """
        try:
            if node.function == 'Add' and len(node.arguments) == 2:
                print(f"DEBUG: Compiling Add({self._get_arg_name(node.arguments[0])}, {self._get_arg_name(node.arguments[1])})")
                self.compiler.compile_expression(node.arguments[0])
                self.asm.emit_push_rax()
                print("DEBUG: Pushed first argument")
                self.compiler.compile_expression(node.arguments[1])
                self.asm.emit_mov_rbx_rax()
                self.asm.emit_pop_rax()
                self.asm.emit_add_rax_rbx()
                print("DEBUG: Add operation completed")
                return True
            
            elif node.function == 'Subtract' and len(node.arguments) == 2:
                print(f"DEBUG: Compiling Subtract({self._get_arg_name(node.arguments[0])}, {self._get_arg_name(node.arguments[1])})")
                self.compiler.compile_expression(node.arguments[0])
                self.asm.emit_push_rax()
                print("DEBUG: Pushed first argument")
                self.compiler.compile_expression(node.arguments[1])
                self.asm.emit_mov_rbx_rax()
                self.asm.emit_pop_rax()
                self.asm.emit_sub_rax_rbx()
                print("DEBUG: Subtract operation completed")
                return True
            
            elif node.function == 'Multiply' and len(node.arguments) == 2:
                print(f"DEBUG: Compiling Multiply({self._get_arg_name(node.arguments[0])}, {self._get_arg_name(node.arguments[1])})")
                self.compiler.compile_expression(node.arguments[0])
                self.asm.emit_push_rax()
                print("DEBUG: Pushed first argument")
                self.compiler.compile_expression(node.arguments[1])
                self.asm.emit_mov_rbx_rax()
                self.asm.emit_pop_rax()
                self.asm.emit_imul_rax_rbx()
                print("DEBUG: Multiply operation completed")
                return True
            
            elif node.function == 'Divide' and len(node.arguments) == 2:
               # Force signed division path (CQO + IDIV)
                return self.compile_divide(node)

            # Optional: expose Modulo if your language supports it
            elif node.function == 'Modulo' and len(node.arguments) == 2:
                return self.compile_modulo(node)
            
            
            
            elif node.function == 'BitwiseAnd' and len(node.arguments) == 2:
                print(f"DEBUG: Compiling BitwiseAnd({self._get_arg_name(node.arguments[0])}, {self._get_arg_name(node.arguments[1])})")
                self.compiler.compile_expression(node.arguments[0])
                self.asm.emit_push_rax()
                print("DEBUG: Pushed first argument")
                self.compiler.compile_expression(node.arguments[1])
                self.asm.emit_mov_rbx_rax()
                self.asm.emit_pop_rax()
                self.asm.emit_bytes(0x48, 0x21, 0xD8)  # AND RAX, RBX
                print("DEBUG: BitwiseAnd operation completed")
                return True
            
            elif node.function == 'BitwiseOr' and len(node.arguments) == 2:
                print(f"DEBUG: Compiling BitwiseOr({self._get_arg_name(node.arguments[0])}, {self._get_arg_name(node.arguments[1])})")
                self.compiler.compile_expression(node.arguments[0])
                self.asm.emit_push_rax()
                print("DEBUG: Pushed first argument")
                self.compiler.compile_expression(node.arguments[1])
                self.asm.emit_mov_rbx_rax()
                self.asm.emit_pop_rax()
                self.asm.emit_bytes(0x48, 0x09, 0xD8)  # OR RAX, RBX
                print("DEBUG: BitwiseOr operation completed")
                return True
            
            
            elif node.function == 'BitwiseXor' and len(node.arguments) == 2:
                print(f"DEBUG: Compiling BitwiseXor({self._get_arg_name(node.arguments[0])}, {self._get_arg_name(node.arguments[1])})")
                self.compiler.compile_expression(node.arguments[0])
                self.asm.emit_push_rax()
                print("DEBUG: Pushed first argument")
                self.compiler.compile_expression(node.arguments[1])
                self.asm.emit_mov_rbx_rax()
                self.asm.emit_pop_rax()
                self.asm.emit_bytes(0x48, 0x31, 0xD8)  # XOR RAX, RBX
                print("DEBUG: BitwiseXor operation completed")
                return True
            
            
            elif node.function == 'LeftShift' and len(node.arguments) == 2:
                print(f"DEBUG: Compiling LeftShift({self._get_arg_name(node.arguments[0])}, {self._get_arg_name(node.arguments[1])})")
                self.compiler.compile_expression(node.arguments[0])
                self.asm.emit_push_rax()
                print("DEBUG: Pushed first argument")
                self.compiler.compile_expression(node.arguments[1])
                self.asm.emit_mov_rcx_rax()  # Shift amount goes in CL (low byte of RCX)
                self.asm.emit_pop_rax()
                self.asm.emit_bytes(0x48, 0xD3, 0xE0)  # SHL RAX, CL
                print("DEBUG: LeftShift operation completed")
                return True

            elif node.function == 'RightShift' and len(node.arguments) == 2:
                print(f"DEBUG: Compiling RightShift({self._get_arg_name(node.arguments[0])}, {self._get_arg_name(node.arguments[1])})")
                self.compiler.compile_expression(node.arguments[0])
                self.asm.emit_push_rax()
                print("DEBUG: Pushed first argument")
                self.compiler.compile_expression(node.arguments[1])
                self.asm.emit_mov_rcx_rax()  # Shift amount goes in CL (low byte of RCX)
                self.asm.emit_pop_rax()
                self.asm.emit_bytes(0x48, 0xD3, 0xE8)  # SHR RAX, CL
                print("DEBUG: RightShift operation completed")
                return True
            
            
            elif node.function in ['LessThan', 'LessEqual', 'GreaterThan', 'GreaterEqual', 'EqualTo']:
                return self.compile_comparison(node)
        
            elif node.function == 'NotEqual':
                return self.compile_not_equal(node)
            
            return False
            
        except Exception as e:
            print(f"ERROR: Arithmetic operation compilation failed: {str(e)}")
            raise
    
    def compile_comparison(self, node):
        """
        Compile comparison operations (LessThan, GreaterThan, EqualTo)
        """
        try:
            if node.function in ['LessThan', 'LessEqual', 'GreaterThan', 'GreaterEqual', 'EqualTo'] and len(node.arguments) == 2:
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
                elif node.function == 'LessEqual':
                    self.asm.emit_bytes(0x0F, 0x9E, 0xC0)  # SETLE AL
                elif node.function == 'GreaterThan':
                    self.asm.emit_bytes(0x0F, 0x9F, 0xC0)  # SETG AL
                elif node.function == 'GreaterEqual':
                    self.asm.emit_bytes(0x0F, 0x9D, 0xC0)  # SETGE AL
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

    def compile_not_equal(self, node):
        """Compile NotEqual(a, b) - returns 1 if not equal, 0 if equal"""
        if len(node.arguments) < 2:
            raise ValueError("NotEqual requires 2 arguments")
        
        print("Compiling NotEqual operation")
        
        # Compile first argument
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_push_rax()
        
        # Compile second argument  
        self.compiler.compile_expression(node.arguments[1])
        self.asm.emit_mov_rbx_rax()
        
        # Pop first argument
        self.asm.emit_pop_rax()
        
        # Compare RAX and RBX
        self.asm.emit_bytes(0x48, 0x39, 0xD8)  # CMP RAX, RBX
        
        # Set AL to 1 if not equal, 0 if equal (SETNE)
        self.asm.emit_bytes(0x0F, 0x95, 0xC0)  # SETNE AL
        
        # Zero-extend AL to RAX
        self.asm.emit_bytes(0x48, 0x0F, 0xB6, 0xC0)  # MOVZX RAX, AL
        
        print("NotEqual operation completed")
        return True
    
    def compile_bitwise_and(self, node):
        """Compile BitwiseAnd(a, b)"""
        if len(node.arguments) != 2:
            raise ValueError("BitwiseAnd requires two arguments")
        self.compile_expression(node.arguments[0])
        self.asm.emit_push_rax()
        self.compile_expression(node.arguments[1])
        self.asm.emit_pop_rbx()
        self.asm.emit_bytes(0x48, 0x21, 0xD8)  # AND RAX, RBX
        print("DEBUG: Emitted BitwiseAnd operation")

    def compile_bitwise_or(self, node):
        """Compile BitwiseOr(a, b)"""
        if len(node.arguments) != 2:
            raise ValueError("BitwiseOr requires two arguments")
        self.compile_expression(node.arguments[0])
        self.asm.emit_push_rax()
        self.compile_expression(node.arguments[1])
        self.asm.emit_pop_rbx()
        self.asm.emit_bytes(0x48, 0x09, 0xD8)  # OR RAX, RBX
        print("DEBUG: Emitted BitwiseOr operation")

    def compile_left_shift(self, node):
        """Compile LeftShift(a, b)"""
        if len(node.arguments) != 2:
            raise ValueError("LeftShift requires two arguments")
        self.compile_expression(node.arguments[0])
        self.asm.emit_push_rax()
        self.compile_expression(node.arguments[1])
        self.asm.emit_mov_rcx_rax()  # Shift amount in CL
        self.asm.emit_pop_rax()
        self.asm.emit_bytes(0x48, 0xD3, 0xE0)  # SHL RAX, CL
        print("DEBUG: Emitted LeftShift operation")

    def compile_right_shift(self, node):
        """Compile RightShift(a, b)"""
        if len(node.arguments) != 2:
            raise ValueError("RightShift requires two arguments")
        self.compile_expression(node.arguments[0])
        self.asm.emit_push_rax()
        self.compile_expression(node.arguments[1])
        self.asm.emit_mov_rcx_rax()  # Shift amount in CL
        self.asm.emit_pop_rax()
        self.asm.emit_bytes(0x48, 0xD3, 0xE8)  # SHR RAX, CL
        print("DEBUG: Emitted RightShift operation")
        
    def compile_divide(self, node):
        """
        Signed integer division (authoritative override).
        RAX := arg0 / arg1  using CQO + IDIV
        """
        # Sanity guard
        if not (hasattr(node, "arguments") and len(node.arguments) == 2):
            raise ValueError("compile_divide expects exactly 2 arguments")

        print("=== AILANG DIVISION: USING SIGNED CQO+IDIV PATH (arithmetic_ops.py) ===")

        # RAX := arg0
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_push_rax()                 # save arg0

        # RAX := arg1
        self.compiler.compile_expression(node.arguments[1])
        self.asm.emit_mov_rbx_rax()              # RBX := arg1
        self.asm.emit_pop_rax()                  # RAX := arg0

        # Sign extend RAX -> RDX:RAX, then IDIV by RBX
        if hasattr(self.asm, "emit_cqo"):
            self.asm.emit_cqo()                  # CQO
        else:
            self.asm.emit_bytes(0x48, 0x99)      # CQO

        # IDIV r/m64 (RBX):  RAX <- quotient, RDX <- remainder
        self.asm.emit_bytes(0x48, 0xF7, 0xFB)    # IDIV RBX
        return True


    def compile_modulo(self, node):
        """
        Signed integer modulo (authoritative override).
        RAX := arg0 % arg1  using CQO + IDIV (remainder in RDX -> move to RAX)
        """
        if not (hasattr(node, "arguments") and len(node.arguments) == 2):
            raise ValueError("compile_modulo expects exactly 2 arguments")

        print("=== AILANG MODULO: USING SIGNED CQO+IDIV PATH (arithmetic_ops.py) ===")

        # RAX := arg0
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_push_rax()

        # RAX := arg1
        self.compiler.compile_expression(node.arguments[1])
        self.asm.emit_mov_rbx_rax()
        self.asm.emit_pop_rax()                  # RAX := arg0

        # Signed divide to produce remainder in RDX
        if hasattr(self.asm, "emit_cqo"):
            self.asm.emit_cqo()
        else:
            self.asm.emit_bytes(0x48, 0x99)

        self.asm.emit_bytes(0x48, 0xF7, 0xFB)    # IDIV RBX

        # Move remainder (RDX) -> RAX
        if hasattr(self.asm, "emit_mov_rax_rdx"):
            self.asm.emit_mov_rax_rdx()
        else:
            self.asm.emit_bytes(0x48, 0x89, 0xD0)  # MOV RAX, RDX

        return True
