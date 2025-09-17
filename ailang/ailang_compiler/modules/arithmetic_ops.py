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
            
            elif node.function == 'AbsoluteValue' and len(node.arguments) == 1:
                print(f"DEBUG: Compiling AbsoluteValue({self._get_arg_name(node.arguments[0])})")
                
                # Compile the argument
                self.compiler.compile_expression(node.arguments[0])
                
                # Absolute value implementation:
                # If negative (sign bit set), negate; otherwise keep as is
                # Using the bit-twiddling trick: (x ^ (x >> 63)) - (x >> 63)
                
                # Save original in RBX
                self.asm.emit_mov_rbx_rax()  # MOV RBX, RAX
                
                # Get sign bit (arithmetic shift right by 63)
                self.asm.emit_bytes(0x48, 0xC1, 0xF8, 0x3F)  # SAR RAX, 63
                
                # XOR original with sign mask
                self.asm.emit_bytes(0x48, 0x31, 0xC3)  # XOR RBX, RAX
                
                # Subtract sign mask (converts to positive)
                self.asm.emit_bytes(0x48, 0x29, 0xC3)  # SUB RBX, RAX
                
                # Move result back to RAX
                self.asm.emit_mov_rax_rbx()  # MOV RAX, RBX
                
                print("DEBUG: AbsoluteValue operation completed")
                return True
            
            
            elif node.function == 'And' and len(node.arguments) == 2:
                return self.compile_and(node)
                
            elif node.function == 'Or' and len(node.arguments) == 2:
                return self.compile_or(node)
                
            elif node.function == 'Not' and len(node.arguments) == 1:
                return self.compile_not(node) 
            
            elif node.function == 'Power' and len(node.arguments) == 2:
                return self.compile_power(node) 
            
            elif node.function == 'BitwiseNot' and len(node.arguments) == 1:
                return self.compile_bitwise_not(node)     
            
            
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
                self.asm.emit_bytes(0x48, 0xD3, 0xF8)  # SAR RAX, CL (arithmetic)
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
           if node.function in ['LessThan', 'LessEqual', 'GreaterThan', 'GreaterEqual', 'Equal', 'EqualTo'] and len(node.arguments) == 2:
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
                elif node.function == 'Equal':
                    self.asm.emit_bytes(0x0F, 0x94, 0xC0)  # SETE AL
                elif node.function == 'EqualTo':
                    self.asm.emit_bytes(0x0F, 0x94, 0xC0)  # SETE AL
                
                # Zero extend AL to RAX
                self.asm.emit_bytes(0x48, 0x0F, 0xB6, 0xC0)  # MOVZX RAX, AL
                print(f"DEBUG: {node.function} operation completed")
                return True
            
                        
        except Exception as e:
            print(f"ERROR: Comparison operation compilation failed: {str(e)}")
            raise
        
    # In arithmetic_ops.py, add these methods:

    def compile_and(self, node):
        """Compile logical And(a, b)"""
        if len(node.arguments) != 2:
            raise ValueError("And requires two arguments")
        
        # Compile first argument
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_push_rax()
        
        # Compile second argument
        self.compiler.compile_expression(node.arguments[1])
        self.asm.emit_mov_rbx_rax()
        self.asm.emit_pop_rax()
        
        # Logical AND: both must be non-zero
        # Test RAX
        self.asm.emit_bytes(0x48, 0x85, 0xC0)  # TEST RAX, RAX
        # Set AL to 1 if not zero
        self.asm.emit_bytes(0x0F, 0x95, 0xC0)  # SETNE AL
        # Test RBX
        self.asm.emit_bytes(0x48, 0x85, 0xDB)  # TEST RBX, RBX
        # Set BL to 1 if not zero
        self.asm.emit_bytes(0x0F, 0x95, 0xC3)  # SETNE BL
        # AND the results
        self.asm.emit_bytes(0x20, 0xD8)  # AND AL, BL
        # Zero-extend to RAX
        self.asm.emit_bytes(0x48, 0x0F, 0xB6, 0xC0)  # MOVZX RAX, AL
        print("DEBUG: And operation completed")
        return True
    
    
    def compile_or(self, node):
        """Compile logical Or(a, b) - returns 1 if either is non-zero"""
        if len(node.arguments) != 2:
            raise ValueError("Or requires two arguments")
        
        print("DEBUG: Compiling Or operation")
        
        # Compile first argument
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_push_rax()
        
        # Compile second argument
        self.compiler.compile_expression(node.arguments[1])
        self.asm.emit_mov_rbx_rax()
        self.asm.emit_pop_rax()
        
        # Logical OR: either must be non-zero
        # Test RAX
        self.asm.emit_bytes(0x48, 0x85, 0xC0)  # TEST RAX, RAX
        # Set AL to 1 if not zero
        self.asm.emit_bytes(0x0F, 0x95, 0xC0)  # SETNE AL
        # Test RBX
        self.asm.emit_bytes(0x48, 0x85, 0xDB)  # TEST RBX, RBX
        # Set BL to 1 if not zero
        self.asm.emit_bytes(0x0F, 0x95, 0xC3)  # SETNE BL
        # OR the results
        self.asm.emit_bytes(0x08, 0xD8)  # OR AL, BL
        # Zero-extend to RAX
        self.asm.emit_bytes(0x48, 0x0F, 0xB6, 0xC0)  # MOVZX RAX, AL
        print("DEBUG: Or operation completed")
        return True

    def compile_not(self, node):
        """Compile logical Not(a) - returns 1 if zero, 0 if non-zero"""
        if len(node.arguments) != 1:
            raise ValueError("Not requires one argument")
        
        print("DEBUG: Compiling Not operation")
        
        # Compile the argument
        self.compiler.compile_expression(node.arguments[0])
        
        # Test if RAX is zero
        self.asm.emit_bytes(0x48, 0x85, 0xC0)  # TEST RAX, RAX
        # Set AL to 1 if zero (SETZ)
        self.asm.emit_bytes(0x0F, 0x94, 0xC0)  # SETZ AL
        # Zero-extend to RAX
        self.asm.emit_bytes(0x48, 0x0F, 0xB6, 0xC0)  # MOVZX RAX, AL
        print("DEBUG: Not operation completed")
        return True
    
    def compile_power(self, node):
        """Compile Power(base, exponent) - integer exponentiation"""
        if len(node.arguments) != 2:
            raise ValueError("Power requires two arguments")
        
        print("DEBUG: Compiling Power operation")
        
        # Compile base
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_push_rax()
        
        # Compile exponent
        self.compiler.compile_expression(node.arguments[1])
        self.asm.emit_mov_rcx_rax()  # Exponent in RCX
        
        # Pop base into RBX
        self.asm.emit_pop_rbx()
        
        # Integer power algorithm:
        # Result = 1
        # While exponent > 0:
        #   if exponent is odd: result *= base
        #   base *= base
        #   exponent >>= 1
        
        # Initialize result to 1 in RAX
        self.asm.emit_mov_rax_imm64(1)
        
        # Create loop label
        loop_label = self.asm.create_label()
        done_label = self.asm.create_label()
        
        # Loop start
        self.asm.mark_label(loop_label)
        
        # Check if exponent (RCX) is 0
        self.asm.emit_bytes(0x48, 0x85, 0xC9)  # TEST RCX, RCX
        self.asm.emit_jump_to_label(done_label, "JZ")
        
        # Check if exponent is odd (test bit 0)
        self.asm.emit_bytes(0xF6, 0xC1, 0x01)  # TEST CL, 1
        
        # Skip multiplication if even
        skip_mult = self.asm.create_label()
        self.asm.emit_jump_to_label(skip_mult, "JZ")
        
        # Multiply result by base (RAX *= RBX)
        self.asm.emit_bytes(0x48, 0x0F, 0xAF, 0xC3)  # IMUL RAX, RBX
        
        self.asm.mark_label(skip_mult)
        
        # Square the base (RBX *= RBX)
        self.asm.emit_bytes(0x48, 0x0F, 0xAF, 0xDB)  # IMUL RBX, RBX
        
        # Shift exponent right by 1
        self.asm.emit_bytes(0x48, 0xD1, 0xE9)  # SHR RCX, 1
        
        # Jump back to loop start
        self.asm.emit_jump_to_label(loop_label, "JMP")
        
        # Done
        self.asm.mark_label(done_label)
        
        print("DEBUG: Power operation completed")
        return True
    
    def compile_equal(self, node):
        """Compile Equal(a, b) - returns 1 if equal, 0 if not"""
        if len(node.arguments) != 2:
            raise ValueError("Equal requires exactly 2 arguments")
        
        # Compile first argument
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_push_rax()
        
        # Compile second argument
        self.compiler.compile_expression(node.arguments[1])
        self.asm.emit_mov_rbx_rax()
        
        # Compare
        self.asm.emit_pop_rax()
        self.asm.emit_cmp_rax_rbx()
        
        # Set result based on equality
        self.asm.emit_mov_rax_imm64(0)  # Default false
        equal_label = self.asm.create_label()
        end_label = self.asm.create_label()
        
        self.asm.emit_jump_to_label(equal_label, "JE")
        self.asm.emit_jump_to_label(end_label, "JMP")
        
        self.asm.mark_label(equal_label)
        self.asm.emit_mov_rax_imm64(1)  # True if equal
        
        self.asm.mark_label(end_label)
        return True
        
    
    
    def compile_bitwise_not(self, node):
        """Compile BitwiseNot(a) - bitwise complement"""
        if len(node.arguments) != 1:
            raise ValueError("BitwiseNot requires one argument")
        
        print("DEBUG: Compiling BitwiseNot operation")
        
        # Compile the argument
        self.compiler.compile_expression(node.arguments[0])
        
        # NOT RAX (bitwise complement)
        self.asm.emit_bytes(0x48, 0xF7, 0xD0)  # NOT RAX
        
        print("DEBUG: BitwiseNot operation completed")
        return True
            
    
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
        """Compile RightShift(a, b) - use arithmetic shift for sign preservation"""
        if len(node.arguments) != 2:
            raise ValueError("RightShift requires two arguments")
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_push_rax()
        self.compiler.compile_expression(node.arguments[1])
        self.asm.emit_mov_rcx_rax()  # Shift amount in CL
        self.asm.emit_pop_rax()
        # Use SAR (arithmetic shift) instead of SHR (logical shift)
        self.asm.emit_bytes(0x48, 0xD3, 0xF8)  # SAR RAX, CL
        print("DEBUG: Emitted RightShift operation (arithmetic)")
        return True
        
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
