# Copyright (c) 2025 Sean Collins, 2 Paws Machine and Engineering. All rights reserved.

# ailang_compiler/compiler/modules/arithmetic_ops.py
"""
Arithmetic Operations Module - DEPTH-BASED REGISTER ALLOCATION
Handles deeply nested expressions by cycling through R12->R13->R14->R15 based on depth
This solves the register collision problem for arbitrary nesting levels
"""

from ailang_parser.ailang_ast import *

class ArithmeticOps:
    """Handles basic arithmetic operations with depth-aware register allocation"""
    
    def __init__(self, compiler_context):
        self.compiler = compiler_context
        self.asm = compiler_context.asm
        
        # Initialize depth tracker
        if not hasattr(self.compiler, '_binary_op_depth'):
            self.compiler._binary_op_depth = 0
    
    def _get_depth_register(self):
        """
        Get register info for current nesting depth.
        Uses R12/R13 for depth 0-1, then STACK for deeper nesting.
        R14/R15 are RESERVED for FixedPool/LinkagePool base registers.
        Returns: (name, mov_to_reg_bytes, mov_to_rbx_bytes, push_fn, pop_fn)
        """
        depth = self.compiler._binary_op_depth
        
        # ONLY use R12 and R13 - R14/R15 are reserved for pool access
        if depth == 0:
            return ('R12', [0x49, 0x89, 0xC4], [0x4C, 0x89, 0xE3], 
                    self.asm.emit_push_r12, self.asm.emit_pop_r12)
        elif depth == 1:
            return ('R13', [0x49, 0x89, 0xC5], [0x4C, 0x89, 0xEB],
                    self.asm.emit_push_r13, self.asm.emit_pop_r13)
        else:
            # Depth 2+: Use stack instead of registers to avoid pool register collision
            return ('STACK', None, None, 
                    lambda: None,  # Stack push happens in _compile_binary_op
                    lambda: None)
    
    def _compile_binary_op(self, node, op_name, op_fn):
        """
        Generic binary operation compiler with depth tracking.
        Uses R12/R13 for depth 0-1, stack for depth 2+.
        op_fn: function to emit the actual operation (like emit_add_rax_rbx)
        """
        if not (hasattr(node, "arguments") and len(node.arguments) == 2):
            raise ValueError(f"{op_name} expects exactly 2 arguments")
        
        reg_name, mov_to_reg, mov_to_rbx, push_fn, pop_fn = self._get_depth_register()
        print(f"DEBUG: {op_name} using {reg_name} at depth {self.compiler._binary_op_depth}")
        
        # Increment depth for nested expression compilation
        self.compiler._binary_op_depth += 1
        
        try:
            if reg_name == 'STACK':
                # Depth 2+: Use stack to avoid R14/R15 collision with pool registers
                self.compiler.compile_expression(node.arguments[1])  # RIGHT operand
                self.asm.emit_push_rax()  # Save on stack
                self.compiler.compile_expression(node.arguments[0])  # LEFT operand
                self.asm.emit_pop_rbx()  # Pop RIGHT into RBX
            else:
                # Depth 0-1: Use R12 or R13
                push_fn()
                self.compiler.compile_expression(node.arguments[1])  # RIGHT operand
                self.asm.emit_bytes(*mov_to_reg)  # MOV Rxx, RAX
                self.compiler.compile_expression(node.arguments[0])  # LEFT operand  
                self.asm.emit_bytes(*mov_to_rbx)  # MOV RBX, Rxx
                pop_fn()
            
            op_fn()  # Emit the actual operation
        finally:
            # Always decrement depth, even if exception occurs
            self.compiler._binary_op_depth -= 1
        
        return True
    
    def compile_operation(self, node):
        """Main dispatcher for arithmetic operations"""
        
        operations = {
            'Add': self.compile_add,
            'Subtract': self.compile_subtract,
            'Multiply': self.compile_multiply,
            'Divide': self.compile_divide,
            'Modulo': self.compile_modulo,
            'Power': self.compile_power,
            
            'BitwiseAnd': self.compile_bitwise_and,
            'BitwiseOr': self.compile_bitwise_or,
            'BitwiseXor': self.compile_bitwise_xor,
            'BitwiseNot': self.compile_bitwise_not,
            'LeftShift': self.compile_left_shift,
            'RightShift': self.compile_right_shift,
            
            'LessThan': self.compile_less_than,
            'GreaterThan': self.compile_greater_than,
            'LessEqual': self.compile_less_equal,
            'GreaterEqual': self.compile_greater_equal,
            'EqualTo': self.compile_equal_to,
            'NotEqual': self.compile_not_equal,
            
            'And': self.compile_logical_and,
            'Or': self.compile_logical_or,
            'Not': self.compile_logical_not,
        }
        
        func_name = node.function
        
        if func_name in operations:
            return operations[func_name](node)
        else:
            return False
    
    # === BASIC ARITHMETIC ===
    
    def compile_add(self, node):
        return self._compile_binary_op(node, "Add", self.asm.emit_add_rax_rbx)
    
    def compile_subtract(self, node):
        return self._compile_binary_op(node, "Subtract", self.asm.emit_sub_rax_rbx)
    
    def compile_multiply(self, node):
        return self._compile_binary_op(node, "Multiply", self.asm.emit_imul_rax_rbx)
    
    def compile_divide(self, node):
        """Division with sign extension"""
        if not (hasattr(node, "arguments") and len(node.arguments) == 2):
            raise ValueError("Divide expects exactly 2 arguments")
        
        reg_name, mov_to_reg, mov_to_rbx, push_fn, pop_fn = self._get_depth_register()
        print(f"DEBUG: Divide using {reg_name} at depth {self.compiler._binary_op_depth}")
        
        self.compiler._binary_op_depth += 1
        
        try:
            if reg_name == 'STACK':
                self.compiler.compile_expression(node.arguments[1])  # divisor
                self.asm.emit_push_rax()
                self.compiler.compile_expression(node.arguments[0])  # dividend
                self.asm.emit_pop_rbx()
            else:
                push_fn()
                self.compiler.compile_expression(node.arguments[1])  # divisor
                self.asm.emit_bytes(*mov_to_reg)
                self.compiler.compile_expression(node.arguments[0])  # dividend
                self.asm.emit_bytes(*mov_to_rbx)
                pop_fn()
            
            # Sign extend and divide
            self.asm.emit_cqo()
            self.asm.emit_bytes(0x48, 0xF7, 0xFB)  # IDIV RBX
        finally:
            self.compiler._binary_op_depth -= 1
        
        return True
    
    def compile_modulo(self, node):
        """Modulo (remainder after division)"""
        if not (hasattr(node, "arguments") and len(node.arguments) == 2):
            raise ValueError("Modulo expects exactly 2 arguments")
        
        reg_name, mov_to_reg, mov_to_rbx, push_fn, pop_fn = self._get_depth_register()
        print(f"DEBUG: Modulo using {reg_name} at depth {self.compiler._binary_op_depth}")
        
        self.compiler._binary_op_depth += 1
        
        try:
            if reg_name == 'STACK':
                self.compiler.compile_expression(node.arguments[1])
                self.asm.emit_push_rax()
                self.compiler.compile_expression(node.arguments[0])
                self.asm.emit_pop_rbx()
            else:
                push_fn()
                self.compiler.compile_expression(node.arguments[1])
                self.asm.emit_bytes(*mov_to_reg)
                self.compiler.compile_expression(node.arguments[0])
                self.asm.emit_bytes(*mov_to_rbx)
                pop_fn()
            
            self.asm.emit_cqo()
            self.asm.emit_bytes(0x48, 0xF7, 0xFB)  # IDIV RBX
            self.asm.emit_bytes(0x48, 0x89, 0xD0)  # MOV RAX, RDX (remainder)
        finally:
            self.compiler._binary_op_depth -= 1
        
        return True
    
    def compile_power(self, node):
        """Power operation (requires loop, uses R12 for base regardless of depth)"""
        if not (hasattr(node, "arguments") and len(node.arguments) == 2):
            raise ValueError("Power expects exactly 2 arguments")
        
        print("DEBUG: Power operation")
        
        # Power is special - always uses R12 for base, R13 for exponent
        self.asm.emit_push_r12()
        self.asm.emit_push_r13()
        
        self.compiler._binary_op_depth += 1
        
        try:
            # Exponent in R13
            self.compiler.compile_expression(node.arguments[1])
            self.asm.emit_bytes(0x49, 0x89, 0xC5)  # MOV R13, RAX
            
            # Base in R12
            self.compiler.compile_expression(node.arguments[0])
            self.asm.emit_bytes(0x49, 0x89, 0xC4)  # MOV R12, RAX
            
            # Result = 1
            self.asm.emit_mov_rax_imm64(1)
            
            loop_start = self.asm.create_label()
            loop_end = self.asm.create_label()
            
            self.asm.mark_label(loop_start)
            self.asm.emit_bytes(0x4D, 0x85, 0xED)  # TEST R13, R13
            self.asm.emit_jump_to_label(loop_end, "JZ")
            
            self.asm.emit_bytes(0x49, 0x0F, 0xAF, 0xC4)  # IMUL RAX, R12
            self.asm.emit_bytes(0x49, 0xFF, 0xCD)  # DEC R13
            self.asm.emit_jump_to_label(loop_start, "JMP")
            
            self.asm.mark_label(loop_end)
        finally:
            self.asm.emit_pop_r13()
            self.asm.emit_pop_r12()
            self.compiler._binary_op_depth -= 1
        
        return True
    
    # === BITWISE OPERATIONS ===
    
    def compile_bitwise_and(self, node):
        return self._compile_binary_op(node, "BitwiseAnd", self.asm.emit_and_rax_rbx)
    
    def compile_bitwise_or(self, node):
        return self._compile_binary_op(node, "BitwiseOr", self.asm.emit_or_rax_rbx)
    
    def compile_bitwise_xor(self, node):
        return self._compile_binary_op(node, "BitwiseXor", self.asm.emit_xor_rax_rbx)
    
    def compile_bitwise_not(self, node):
        """Unary NOT operation"""
        if not (hasattr(node, "arguments") and len(node.arguments) == 1):
            raise ValueError("BitwiseNot expects exactly 1 argument")
        
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_not_rax()
        return True
    
    def compile_left_shift(self, node):
        """Left shift (amount goes in RCX)"""
        if not (hasattr(node, "arguments") and len(node.arguments) == 2):
            raise ValueError("LeftShift expects exactly 2 arguments")
        
        reg_name, mov_to_reg, _, push_fn, pop_fn = self._get_depth_register()
        print(f"DEBUG: LeftShift using {reg_name}")
        
        self.compiler._binary_op_depth += 1
        
        try:
            push_fn()
            self.compiler.compile_expression(node.arguments[1])  # shift amount
            self.asm.emit_bytes(*mov_to_reg)
            self.compiler.compile_expression(node.arguments[0])  # value
            # Move shift amount from Rxx to RCX
            if reg_name == 'R12':
                self.asm.emit_bytes(0x4C, 0x89, 0xE1)  # MOV RCX, R12
            elif reg_name == 'R13':
                self.asm.emit_bytes(0x4C, 0x89, 0xE9)  # MOV RCX, R13
            elif reg_name == 'R14':
                self.asm.emit_bytes(0x4C, 0x89, 0xF1)  # MOV RCX, R14
            else:  # R15
                self.asm.emit_bytes(0x4C, 0x89, 0xF9)  # MOV RCX, R15
            pop_fn()
            
            self.asm.emit_shl_rax_cl()
        finally:
            self.compiler._binary_op_depth -= 1
        
        return True
    
    def compile_right_shift(self, node):
        """Right shift (arithmetic)"""
        if not (hasattr(node, "arguments") and len(node.arguments) == 2):
            raise ValueError("RightShift expects exactly 2 arguments")
        
        reg_name, mov_to_reg, _, push_fn, pop_fn = self._get_depth_register()
        print(f"DEBUG: RightShift using {reg_name}")
        
        self.compiler._binary_op_depth += 1
        
        try:
            push_fn()
            self.compiler.compile_expression(node.arguments[1])
            self.asm.emit_bytes(*mov_to_reg)
            self.compiler.compile_expression(node.arguments[0])
            # Move to RCX
            if reg_name == 'R12':
                self.asm.emit_bytes(0x4C, 0x89, 0xE1)
            elif reg_name == 'R13':
                self.asm.emit_bytes(0x4C, 0x89, 0xE9)
            elif reg_name == 'R14':
                self.asm.emit_bytes(0x4C, 0x89, 0xF1)
            else:
                self.asm.emit_bytes(0x4C, 0x89, 0xF9)
            pop_fn()
            
            self.asm.emit_sar_rax_cl()
        finally:
            self.compiler._binary_op_depth -= 1
        
        return True
    
    # === COMPARISON OPERATIONS ===
    
    def compile_less_than(self, node):
        def op():
            self.asm.emit_cmp_rax_rbx()
            self.asm.emit_bytes(0x0F, 0x9C, 0xC0)  # SETL AL
            self.asm.emit_bytes(0x48, 0x0F, 0xB6, 0xC0)  # MOVZX RAX, AL
        return self._compile_binary_op(node, "LessThan", op)
    
    def compile_greater_than(self, node):
        def op():
            self.asm.emit_cmp_rax_rbx()
            self.asm.emit_bytes(0x0F, 0x9F, 0xC0)  # SETG AL
            self.asm.emit_bytes(0x48, 0x0F, 0xB6, 0xC0)
        return self._compile_binary_op(node, "GreaterThan", op)
    
    def compile_less_equal(self, node):
        def op():
            self.asm.emit_cmp_rax_rbx()
            self.asm.emit_bytes(0x0F, 0x9E, 0xC0)  # SETLE AL
            self.asm.emit_bytes(0x48, 0x0F, 0xB6, 0xC0)
        return self._compile_binary_op(node, "LessEqual", op)
    
    def compile_greater_equal(self, node):
        def op():
            self.asm.emit_cmp_rax_rbx()
            self.asm.emit_bytes(0x0F, 0x9D, 0xC0)  # SETGE AL
            self.asm.emit_bytes(0x48, 0x0F, 0xB6, 0xC0)
        return self._compile_binary_op(node, "GreaterEqual", op)
    
    def compile_equal_to(self, node):
        def op():
            self.asm.emit_cmp_rax_rbx()
            self.asm.emit_bytes(0x0F, 0x94, 0xC0)  # SETE AL
            self.asm.emit_bytes(0x48, 0x0F, 0xB6, 0xC0)
        return self._compile_binary_op(node, "EqualTo", op)
    
    def compile_not_equal(self, node):
        def op():
            self.asm.emit_cmp_rax_rbx()
            self.asm.emit_bytes(0x0F, 0x95, 0xC0)  # SETNE AL
            self.asm.emit_bytes(0x48, 0x0F, 0xB6, 0xC0)
        return self._compile_binary_op(node, "NotEqual", op)
    
    # === LOGICAL OPERATIONS ===
    
    def compile_logical_and(self, node):
        """Short-circuit AND"""
        if not (hasattr(node, "arguments") and len(node.arguments) == 2):
            raise ValueError("And expects exactly 2 arguments")
        
        false_label = self.asm.create_label()
        end_label = self.asm.create_label()
        
        self.compiler._binary_op_depth += 1
        try:
            self.compiler.compile_expression(node.arguments[0])
            self.asm.emit_bytes(0x48, 0x85, 0xC0)  # TEST RAX, RAX
            self.asm.emit_jump_to_label(false_label, "JZ")
            
            self.compiler.compile_expression(node.arguments[1])
            self.asm.emit_bytes(0x48, 0x85, 0xC0)
            self.asm.emit_jump_to_label(false_label, "JZ")
            
            self.asm.emit_mov_rax_imm64(1)
            self.asm.emit_jump_to_label(end_label, "JMP")
            
            self.asm.mark_label(false_label)
            self.asm.emit_mov_rax_imm64(0)
            
            self.asm.mark_label(end_label)
        finally:
            self.compiler._binary_op_depth -= 1
        
        return True
    
    def compile_logical_or(self, node):
        """Short-circuit OR"""
        if not (hasattr(node, "arguments") and len(node.arguments) == 2):
            raise ValueError("Or expects exactly 2 arguments")
        
        true_label = self.asm.create_label()
        end_label = self.asm.create_label()
        
        self.compiler._binary_op_depth += 1
        try:
            self.compiler.compile_expression(node.arguments[0])
            self.asm.emit_bytes(0x48, 0x85, 0xC0)
            self.asm.emit_jump_to_label(true_label, "JNZ")
            
            self.compiler.compile_expression(node.arguments[1])
            self.asm.emit_bytes(0x48, 0x85, 0xC0)
            self.asm.emit_jump_to_label(true_label, "JNZ")
            
            self.asm.emit_mov_rax_imm64(0)
            self.asm.emit_jump_to_label(end_label, "JMP")
            
            self.asm.mark_label(true_label)
            self.asm.emit_mov_rax_imm64(1)
            
            self.asm.mark_label(end_label)
        finally:
            self.compiler._binary_op_depth -= 1
        
        return True
    
    def compile_logical_not(self, node):
        """Logical NOT"""
        if not (hasattr(node, "arguments") and len(node.arguments) == 1):
            raise ValueError("Not expects exactly 1 argument")
        
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_bytes(0x48, 0x85, 0xC0)  # TEST RAX, RAX
        self.asm.emit_bytes(0x0F, 0x94, 0xC0)  # SETZ AL
        self.asm.emit_bytes(0x48, 0x0F, 0xB6, 0xC0)  # MOVZX RAX, AL
        return True