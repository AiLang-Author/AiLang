# Copyright (c) 2025 Sean Collins, 2 Paws Machine and Engineering. All rights reserved.
#
# Licensed under the Sean Collins Software License (SCSL). See the LICENSE file in the root directory of this project
# for the full terms and conditions, including restrictions on forking, corporate use, and permissions for private/teaching purposes.

# ailang_compiler/compiler/modules/math_ops.py
"""
Advanced Math Operations Module for AILANG Compiler - NESTED CALL FIX VERSION
Implements the 50 math primitives from the grammar
FIX: Uses R13 for nested function calls, R12 for simple expressions
"""

from ailang_parser.ailang_ast import *

class MathOperations:
    """Handles advanced math operations beyond basic arithmetic"""
    
    def __init__(self, compiler_context):
        self.compiler = compiler_context
        self.asm = compiler_context.asm
    
    def _is_complex_expression(self, node):
        """Determine if an expression might contain function calls"""
        if isinstance(node, FunctionCall):
            return True
        if isinstance(node, Number) or isinstance(node, Identifier):
            return False
        return True
        
    def compile_operation(self, node):
        """Main dispatcher for math operations"""
        
        # Map function names to implementation methods
        operations = {
            
             # === CORE PRIMITIVES (NEW) ===
            'ISqrt': self.compile_isqrt,
            'Abs': self.compile_abs,
            'Min': self.compile_min,
            'Max': self.compile_max,
            'Pow': self.compile_pow,           
            
            
            # === ROUNDING OPERATIONS ===
            'Floor': self.compile_floor,
            'Ceil': self.compile_ceil,
            'Round': self.compile_round,
            'RoundEven': self.compile_round_even,
            'Trunc': self.compile_trunc,
            'Frac': self.compile_frac,
            
            # === MIN/MAX OPERATIONS ===
            'Min': self.compile_min,
            'Max': self.compile_max,
            'Clamp': self.compile_clamp,
            'Saturate': self.compile_saturate,
            'Sign': self.compile_sign,
            
            # === DIVISION VARIANTS ===
            'FloorDivide': self.compile_floor_divide,
            'Remainder': self.compile_remainder,
            'DivMod': self.compile_divmod,
            
            # === ADVANCED ARITHMETIC ===
            'FusedMultiplyAdd': self.compile_fma,
            'Hypotenuse': self.compile_hypot,
            'Lerp': self.compile_lerp,
            
            # === ANGLE CONVERSION ===
            'DegToRad': self.compile_deg_to_rad,
            'RadToDeg': self.compile_rad_to_deg,
            
            # === TRIGONOMETRY ===
            'Sin': self.compile_sin,
            'Cos': self.compile_cos,
            'Tan': self.compile_tan,
            'Asin': self.compile_asin,
            'Acos': self.compile_acos,
            'Atan': self.compile_atan,
            'Atan2': self.compile_atan2,
            'Tanh': self.compile_tanh,
            
            # === EXPONENTIAL ===
            'Exp': self.compile_exp,
            'Expm1': self.compile_expm1,
            'Exp2': self.compile_exp2,
            
            # === LOGARITHMS ===
            'Log': self.compile_log,
            'Log1p': self.compile_log1p,
            'Log2': self.compile_log2,
            'Log10': self.compile_log10,
            
            # === FLOATING POINT ===
            'NextAfter': self.compile_next_after,
            'Frexp': self.compile_frexp,
            'Ldexp': self.compile_ldexp,
            'NearlyEqual': self.compile_nearly_equal,
            
            # === BIT OPERATIONS ===
            'PopCount': self.compile_popcount,
            'CountLeadingZeros': self.compile_clz,
            'CountTrailingZeros': self.compile_ctz,
            'RotateLeft': self.compile_rotate_left,
            'RotateRight': self.compile_rotate_right,
            'ByteSwap': self.compile_byte_swap,
            'BitReverse': self.compile_bit_reverse,
            
            # === ALIGNMENT ===
            'AlignUp': self.compile_align_up,
            'AlignDown': self.compile_align_down,
            'IsPowerOfTwo': self.compile_is_power_of_two,
            'NextPowerOfTwo': self.compile_next_power_of_two,
            'FloorLog2': self.compile_floor_log2,
        }
        
        handler = operations.get(node.function)
        if handler:
            return handler(node)
        return False
    
     # ========== CORE PRIMITIVES (NEW) ==========
    
    def compile_isqrt(self, node):
        """ISqrt(n) - Integer square root using Newton's method"""
        if len(node.arguments) != 1:
            raise ValueError("ISqrt requires one argument")
        
        print("DEBUG: Compiling ISqrt - Integer square root")
        
        # Get input value in RAX
        self.compiler.compile_expression(node.arguments[0])
        
        # Check for zero/negative
        self.asm.emit_bytes(0x48, 0x85, 0xC0)  # TEST RAX, RAX
        self.asm.emit_bytes(0x7E, 0x04)  # JLE +4 (skip to return 0)
        
        # Newton's method: x_new = (x + n/x) / 2
        # We'll use a simplified iterative approach
        
        # Save n in RBX
        self.asm.emit_bytes(0x48, 0x89, 0xC3)  # MOV RBX, RAX
        
        # Initial guess: x = n
        # R12 will hold our current estimate
        self.asm.emit_push_r12()
        self.asm.emit_bytes(0x49, 0x89, 0xC4)  # MOV R12, RAX
        
        # Loop: iterate until convergence
        # R13 will hold the new estimate
        self.asm.emit_push_r13()
        
        # loop_start:
        loop_start = len(self.asm.code)
        
        # Calculate new estimate: y = (x + n/x) / 2
        self.asm.emit_bytes(0x48, 0x89, 0xD8)  # MOV RAX, RBX (n)
        self.asm.emit_bytes(0x48, 0x99)  # CQO (sign extend)
        self.asm.emit_bytes(0x49, 0xF7, 0xFC)  # IDIV R12 (n/x)
        self.asm.emit_bytes(0x49, 0x01, 0xC4)  # ADD R12, RAX (x + n/x)
        self.asm.emit_bytes(0x49, 0xD1, 0xFC)  # SAR R12, 1 (divide by 2)
        
        # Compare with previous estimate
        # If new >= old, we're done
        self.asm.emit_bytes(0x4C, 0x89, 0xE0)  # MOV RAX, R12
        self.asm.emit_bytes(0x4C, 0x39, 0xE8)  # CMP RAX, R13
        
        # Update R13 with new estimate
        self.asm.emit_bytes(0x4D, 0x89, 0xE5)  # MOV R13, R12
        
        # If RAX < R13, continue loop
        loop_offset = loop_start - (len(self.asm.code) + 2)
        self.asm.emit_bytes(0x7C, loop_offset & 0xFF)  # JL loop_start
        
        # Result in R12
        self.asm.emit_bytes(0x4C, 0x89, 0xE0)  # MOV RAX, R12
        
        self.asm.emit_pop_r13()
        self.asm.emit_pop_r12()
        
        print("DEBUG: ISqrt completed")
        return True
    
    def compile_abs(self, node):
        """Abs(x) - Absolute value"""
        if len(node.arguments) != 1:
            raise ValueError("Abs requires one argument")
        
        print("DEBUG: Compiling Abs - Absolute value")
        
        # Get value in RAX
        self.compiler.compile_expression(node.arguments[0])
        
        # Use conditional move for branchless abs
        # MOV RBX, RAX
        self.asm.emit_bytes(0x48, 0x89, 0xC3)
        
        # NEG RAX (compute -x)
        self.asm.emit_bytes(0x48, 0xF7, 0xD8)
        
        # CMOVL RAX, RBX (if x was negative, keep -x, else restore x)
        self.asm.emit_bytes(0x48, 0x0F, 0x4C, 0xC3)
        
        print("DEBUG: Abs completed")
        return True
    
    def compile_pow(self, node):
        """Pow(base, exp) - Integer power using exponentiation by squaring"""
        if len(node.arguments) != 2:
            raise ValueError("Pow requires two arguments (base, exponent)")
        
        print("DEBUG: Compiling Pow - Integer power")
        
        # Evaluate exponent first (save in R13)
        arg1_complex = self._is_complex_expression(node.arguments[1])
        
        if arg1_complex:
            self.asm.emit_push_r13()
            self.compiler.compile_expression(node.arguments[1])
            self.asm.emit_bytes(0x49, 0x89, 0xC5)  # MOV R13, RAX
            self.compiler.compile_expression(node.arguments[0])
            self.asm.emit_bytes(0x4C, 0x89, 0xEB)  # MOV RBX, R13 (exp in RBX)
            self.asm.emit_pop_r13()
        else:
            self.asm.emit_push_r12()
            self.compiler.compile_expression(node.arguments[1])
            self.asm.emit_bytes(0x49, 0x89, 0xC4)  # MOV R12, RAX
            self.compiler.compile_expression(node.arguments[0])
            self.asm.emit_bytes(0x4C, 0x89, 0xE3)  # MOV RBX, R12 (exp in RBX)
            self.asm.emit_pop_r12()
        
        # Now RAX = base, RBX = exponent
        # Handle special cases
        self.asm.emit_bytes(0x48, 0x85, 0xDB)  # TEST RBX, RBX
        
        # If exp == 0, return 1
        skip_one = len(self.asm.code) + 7
        self.asm.emit_bytes(0x75, 0x05)  # JNZ +5
        self.asm.emit_mov_rax_imm64(1)
        # JMP to end would go here
        
        # Save base in R12, result in R13
        self.asm.emit_push_r12()
        self.asm.emit_push_r13()
        self.asm.emit_bytes(0x49, 0x89, 0xC4)  # MOV R12, RAX (base)
        self.asm.emit_mov_rax_imm64(1)
        self.asm.emit_bytes(0x49, 0x89, 0xC5)  # MOV R13, RAX (result = 1)
        
        # Exponentiation by squaring loop
        # while exp > 0:
        #   if exp & 1: result *= base
        #   base *= base
        #   exp >>= 1
        
        loop_start = len(self.asm.code)
        
        # Check if exp > 0
        self.asm.emit_bytes(0x48, 0x85, 0xDB)  # TEST RBX, RBX
        loop_end_offset = 30  # Will be patched
        self.asm.emit_bytes(0x7E, loop_end_offset)  # JLE loop_end
        
        # Check if exp & 1
        self.asm.emit_bytes(0x48, 0xF7, 0xC3, 0x01, 0x00, 0x00, 0x00)  # TEST RBX, 1
        skip_mult = 12
        self.asm.emit_bytes(0x74, skip_mult)  # JZ skip_mult
        
        # result *= base (R13 *= R12)
        self.asm.emit_bytes(0x4C, 0x89, 0xE8)  # MOV RAX, R13
        self.asm.emit_bytes(0x49, 0xF7, 0xEC)  # IMUL R12
        self.asm.emit_bytes(0x49, 0x89, 0xC5)  # MOV R13, RAX
        
        # base *= base (R12 *= R12)
        self.asm.emit_bytes(0x4C, 0x89, 0xE0)  # MOV RAX, R12
        self.asm.emit_bytes(0x49, 0xF7, 0xEC)  # IMUL R12
        self.asm.emit_bytes(0x49, 0x89, 0xC4)  # MOV R12, RAX
        
        # exp >>= 1
        self.asm.emit_bytes(0x48, 0xD1, 0xFB)  # SAR RBX, 1
        
        # Jump back to loop start
        loop_offset = loop_start - (len(self.asm.code) + 2)
        self.asm.emit_bytes(0xEB, loop_offset & 0xFF)  # JMP loop_start
        
        # loop_end:
        # Move result to RAX
        self.asm.emit_bytes(0x4C, 0x89, 0xE8)  # MOV RAX, R13
        
        self.asm.emit_pop_r13()
        self.asm.emit_pop_r12()
        
        print("DEBUG: Pow completed")
        return True
    
    
    # ========== ROUNDING OPERATIONS ==========
    
    def compile_floor(self, node):
        """Floor(x) - Round down to nearest integer"""
        if len(node.arguments) != 1:
            raise ValueError("Floor requires one argument")
        
        print("DEBUG: Compiling Floor")
        self.compiler.compile_expression(node.arguments[0])
        return True
    
    def compile_ceil(self, node):
        """Ceil(x) - Round up to nearest integer"""
        if len(node.arguments) != 1:
            raise ValueError("Ceil requires one argument")
        
        print("DEBUG: Compiling Ceil")
        self.compiler.compile_expression(node.arguments[0])
        return True
    
    def compile_round(self, node):
        """Round(x) - Round to nearest integer"""
        if len(node.arguments) != 1:
            raise ValueError("Round requires one argument")
        
        print("DEBUG: Compiling Round")
        self.compiler.compile_expression(node.arguments[0])
        return True
    
    def compile_round_even(self, node):
        """RoundEven(x) - Round to nearest even integer (banker's rounding)"""
        if len(node.arguments) != 1:
            raise ValueError("RoundEven requires one argument")
        
        print("DEBUG: Compiling RoundEven")
        self.compiler.compile_expression(node.arguments[0])
        return True
    
    def compile_trunc(self, node):
        """Trunc(x) - Truncate towards zero"""
        if len(node.arguments) != 1:
            raise ValueError("Trunc requires one argument")
        
        print("DEBUG: Compiling Trunc")
        self.compiler.compile_expression(node.arguments[0])
        return True
    
    def compile_frac(self, node):
        """Frac(x) - Fractional part"""
        if len(node.arguments) != 1:
            raise ValueError("Frac requires one argument")
        
        print("DEBUG: Compiling Frac")
        # For integers, always 0
        self.asm.emit_mov_rax_imm64(0)
        return True
    
    # ========== MIN/MAX OPERATIONS ==========
    
    def compile_min(self, node):
        """Min(a, b) - Return smaller value"""
        if len(node.arguments) != 2:
            raise ValueError("Min requires two arguments")
        
        print("DEBUG: Compiling Min with nested call detection")
        
        arg1_complex = self._is_complex_expression(node.arguments[1])
        
        if arg1_complex:
            print("DEBUG: Using R13 (nested call detected)")
            self.asm.emit_push_r13()
            self.compiler.compile_expression(node.arguments[1])
            self.asm.emit_bytes(0x49, 0x89, 0xC5)  # MOV R13, RAX
            self.compiler.compile_expression(node.arguments[0])
            self.asm.emit_bytes(0x4C, 0x89, 0xEB)  # MOV RBX, R13
            self.asm.emit_pop_r13()
        else:
            print("DEBUG: Using R12 (simple expression)")
            self.asm.emit_push_r12()
            self.compiler.compile_expression(node.arguments[1])
            self.asm.emit_bytes(0x49, 0x89, 0xC4)  # MOV R12, RAX
            self.compiler.compile_expression(node.arguments[0])
            self.asm.emit_bytes(0x4C, 0x89, 0xE3)  # MOV RBX, R12
            self.asm.emit_pop_r12()
        
        # Compare and select minimum
        self.asm.emit_bytes(0x48, 0x39, 0xD8)  # CMP RAX, RBX
        self.asm.emit_bytes(0x48, 0x0F, 0x4F, 0xC3)  # CMOVG RAX, RBX
        
        print("DEBUG: Min operation completed")
        return True
    
    def compile_max(self, node):
        """Max(a, b) - Return larger value"""
        if len(node.arguments) != 2:
            raise ValueError("Max requires two arguments")
        
        print("DEBUG: Compiling Max with nested call detection")
        
        arg1_complex = self._is_complex_expression(node.arguments[1])
        
        if arg1_complex:
            print("DEBUG: Using R13 (nested call detected)")
            self.asm.emit_push_r13()
            self.compiler.compile_expression(node.arguments[1])
            self.asm.emit_bytes(0x49, 0x89, 0xC5)  # MOV R13, RAX
            self.compiler.compile_expression(node.arguments[0])
            self.asm.emit_bytes(0x4C, 0x89, 0xEB)  # MOV RBX, R13
            self.asm.emit_pop_r13()
        else:
            print("DEBUG: Using R12 (simple expression)")
            self.asm.emit_push_r12()
            self.compiler.compile_expression(node.arguments[1])
            self.asm.emit_bytes(0x49, 0x89, 0xC4)  # MOV R12, RAX
            self.compiler.compile_expression(node.arguments[0])
            self.asm.emit_bytes(0x4C, 0x89, 0xE3)  # MOV RBX, R12
            self.asm.emit_pop_r12()
        
        # Compare and select maximum
        self.asm.emit_bytes(0x48, 0x39, 0xD8)  # CMP RAX, RBX
        self.asm.emit_bytes(0x48, 0x0F, 0x4C, 0xC3)  # CMOVL RAX, RBX
        
        print("DEBUG: Max operation completed")
        return True
    
    def compile_clamp(self, node):
        """Clamp(value, min, max) - Clamp value between min and max"""
        if len(node.arguments) != 3:
            raise ValueError("Clamp requires three arguments")
        
        print("DEBUG: Compiling Clamp (uses R12 and R13)")
        
        # Clamp always uses both registers
        self.asm.emit_push_r12()
        self.asm.emit_push_r13()
        
        # Get max value -> R13
        self.compiler.compile_expression(node.arguments[2])
        self.asm.emit_bytes(0x49, 0x89, 0xC5)  # MOV R13, RAX
        
        # Get min value -> R12
        self.compiler.compile_expression(node.arguments[1])
        self.asm.emit_bytes(0x49, 0x89, 0xC4)  # MOV R12, RAX
        
        # Get value -> RAX
        self.compiler.compile_expression(node.arguments[0])
        
        # Clamp to minimum: RAX = max(RAX, R12)
        self.asm.emit_bytes(0x4C, 0x39, 0xE0)  # CMP RAX, R12
        self.asm.emit_bytes(0x4C, 0x0F, 0x4C, 0xE0)  # CMOVL R12, RAX
        self.asm.emit_bytes(0x4C, 0x89, 0xE0)  # MOV RAX, R12
        
        # Clamp to maximum: RAX = min(RAX, R13)
        self.asm.emit_bytes(0x4C, 0x39, 0xE8)  # CMP RAX, R13
        self.asm.emit_bytes(0x4C, 0x0F, 0x4F, 0xE8)  # CMOVG R13, RAX
        self.asm.emit_bytes(0x4C, 0x89, 0xE8)  # MOV RAX, R13
        
        self.asm.emit_pop_r13()
        self.asm.emit_pop_r12()
        
        print("DEBUG: Clamp operation completed")
        return True
    
    def compile_saturate(self, node):
        """Saturate(x) - Clamp to 0..1 range"""
        if len(node.arguments) != 1:
            raise ValueError("Saturate requires one argument")
        
        print("DEBUG: Compiling Saturate")
        self.compiler.compile_expression(node.arguments[0])
        # Would need floating point implementation
        return True
    
    def compile_sign(self, node):
        """Sign(x) - Return -1, 0, or 1"""
        if len(node.arguments) != 1:
            raise ValueError("Sign requires one argument")
        
        print("DEBUG: Compiling Sign")
        self.compiler.compile_expression(node.arguments[0])
        
        # Check if zero
        zero_label = self.asm.create_label()
        negative_label = self.asm.create_label()
        done_label = self.asm.create_label()
        
        self.asm.emit_bytes(0x48, 0x85, 0xC0)  # TEST RAX, RAX
        self.asm.emit_jump_to_label(zero_label, "JZ")
        self.asm.emit_jump_to_label(negative_label, "JS")
        
        # Positive
        self.asm.emit_mov_rax_imm64(1)
        self.asm.emit_jump_to_label(done_label, "JMP")
        
        # Zero
        self.asm.mark_label(zero_label)
        self.asm.emit_mov_rax_imm64(0)
        self.asm.emit_jump_to_label(done_label, "JMP")
        
        # Negative
        self.asm.mark_label(negative_label)
        self.asm.emit_mov_rax_imm64(-1)
        
        self.asm.mark_label(done_label)
        return True
    
    # ========== BIT OPERATIONS ==========
    
    def compile_popcount(self, node):
        """PopCount(x) - Count set bits"""
        if len(node.arguments) != 1:
            raise ValueError("PopCount requires one argument")
        
        print("DEBUG: Compiling PopCount")
        self.compiler.compile_expression(node.arguments[0])
        
        # POPCNT RAX, RAX (requires SSE4.2)
        self.asm.emit_bytes(0xF3, 0x48, 0x0F, 0xB8, 0xC0)
        
        print("DEBUG: PopCount operation completed")
        return True
    
    def compile_clz(self, node):
        """CountLeadingZeros(x)"""
        if len(node.arguments) != 1:
            raise ValueError("CountLeadingZeros requires one argument")
        
        print("DEBUG: Compiling CountLeadingZeros")
        self.compiler.compile_expression(node.arguments[0])
        
        # LZCNT RAX, RAX (BMI1)
        self.asm.emit_bytes(0xF3, 0x48, 0x0F, 0xBD, 0xC0)
        
        print("DEBUG: CountLeadingZeros operation completed")
        return True
    
    def compile_ctz(self, node):
        """CountTrailingZeros(x)"""
        if len(node.arguments) != 1:
            raise ValueError("CountTrailingZeros requires one argument")
        
        print("DEBUG: Compiling CountTrailingZeros")
        self.compiler.compile_expression(node.arguments[0])
        
        # TZCNT RAX, RAX (BMI1)
        self.asm.emit_bytes(0xF3, 0x48, 0x0F, 0xBC, 0xC0)
        
        print("DEBUG: CountTrailingZeros operation completed")
        return True
    
    def compile_rotate_left(self, node):
        """RotateLeft(value, count)"""
        if len(node.arguments) != 2:
            raise ValueError("RotateLeft requires two arguments")
        
        print("DEBUG: Compiling RotateLeft with nested call detection")
        
        arg1_complex = self._is_complex_expression(node.arguments[1])
        
        if arg1_complex:
            print("DEBUG: Using R13 (nested call detected)")
            self.asm.emit_push_r13()
            self.compiler.compile_expression(node.arguments[1])
            self.asm.emit_bytes(0x49, 0x89, 0xC5)  # MOV R13, RAX
            self.compiler.compile_expression(node.arguments[0])
            self.asm.emit_bytes(0x4C, 0x89, 0xE9)  # MOV RCX, R13
            self.asm.emit_pop_r13()
        else:
            print("DEBUG: Using R12 (simple expression)")
            self.asm.emit_push_r12()
            self.compiler.compile_expression(node.arguments[1])
            self.asm.emit_bytes(0x49, 0x89, 0xC4)  # MOV R12, RAX
            self.compiler.compile_expression(node.arguments[0])
            self.asm.emit_bytes(0x4C, 0x89, 0xE1)  # MOV RCX, R12
            self.asm.emit_pop_r12()
        
        # ROL RAX, CL
        self.asm.emit_bytes(0x48, 0xD3, 0xC0)
        
        print("DEBUG: RotateLeft operation completed")
        return True
    
    def compile_rotate_right(self, node):
        """RotateRight(value, count)"""
        if len(node.arguments) != 2:
            raise ValueError("RotateRight requires two arguments")
        
        print("DEBUG: Compiling RotateRight with nested call detection")
        
        arg1_complex = self._is_complex_expression(node.arguments[1])
        
        if arg1_complex:
            print("DEBUG: Using R13 (nested call detected)")
            self.asm.emit_push_r13()
            self.compiler.compile_expression(node.arguments[1])
            self.asm.emit_bytes(0x49, 0x89, 0xC5)  # MOV R13, RAX
            self.compiler.compile_expression(node.arguments[0])
            self.asm.emit_bytes(0x4C, 0x89, 0xE9)  # MOV RCX, R13
            self.asm.emit_pop_r13()
        else:
            print("DEBUG: Using R12 (simple expression)")
            self.asm.emit_push_r12()
            self.compiler.compile_expression(node.arguments[1])
            self.asm.emit_bytes(0x49, 0x89, 0xC4)  # MOV R12, RAX
            self.compiler.compile_expression(node.arguments[0])
            self.asm.emit_bytes(0x4C, 0x89, 0xE1)  # MOV RCX, R12
            self.asm.emit_pop_r12()
        
        # ROR RAX, CL
        self.asm.emit_bytes(0x48, 0xD3, 0xC8)
        
        print("DEBUG: RotateRight operation completed")
        return True
    
    def compile_byte_swap(self, node):
        """ByteSwap(x) - Reverse byte order"""
        if len(node.arguments) != 1:
            raise ValueError("ByteSwap requires one argument")
        
        print("DEBUG: Compiling ByteSwap")
        self.compiler.compile_expression(node.arguments[0])
        
        # BSWAP RAX
        self.asm.emit_bytes(0x48, 0x0F, 0xC8)
        
        print("DEBUG: ByteSwap operation completed")
        return True
    
    def compile_bit_reverse(self, node):
        """BitReverse(x) - Reverse bit order"""
        if len(node.arguments) != 1:
            raise ValueError("BitReverse requires one argument")
        
        print("DEBUG: Compiling BitReverse (placeholder)")
        self.compiler.compile_expression(node.arguments[0])
        # Would need complex bit manipulation
        return True
    
    # ========== ALIGNMENT OPERATIONS ==========
    
    def compile_align_up(self, node):
        """AlignUp(value, alignment)"""
        if len(node.arguments) != 2:
            raise ValueError("AlignUp requires two arguments")
        
        print("DEBUG: Compiling AlignUp with nested call detection")
        
        arg1_complex = self._is_complex_expression(node.arguments[1])
        
        if arg1_complex:
            print("DEBUG: Using R13 (nested call detected)")
            self.asm.emit_push_r13()
            self.compiler.compile_expression(node.arguments[1])
            self.asm.emit_bytes(0x49, 0x89, 0xC5)  # MOV R13, RAX
            self.compiler.compile_expression(node.arguments[0])
            
            # value + alignment - 1
            self.asm.emit_bytes(0x4C, 0x01, 0xE8)  # ADD RAX, R13
            self.asm.emit_bytes(0x48, 0xFF, 0xC8)  # DEC RAX
            
            # ~(alignment - 1)
            self.asm.emit_bytes(0x49, 0xFF, 0xCD)  # DEC R13
            self.asm.emit_bytes(0x49, 0xF7, 0xD5)  # NOT R13
            
            # result & mask
            self.asm.emit_bytes(0x4C, 0x21, 0xE8)  # AND RAX, R13
            
            self.asm.emit_pop_r13()
        else:
            print("DEBUG: Using R12 (simple expression)")
            self.asm.emit_push_r12()
            self.compiler.compile_expression(node.arguments[1])
            self.asm.emit_bytes(0x49, 0x89, 0xC4)  # MOV R12, RAX
            self.compiler.compile_expression(node.arguments[0])
            
            # value + alignment - 1
            self.asm.emit_bytes(0x4C, 0x01, 0xE0)  # ADD RAX, R12
            self.asm.emit_bytes(0x48, 0xFF, 0xC8)  # DEC RAX
            
            # ~(alignment - 1)
            self.asm.emit_bytes(0x49, 0xFF, 0xCC)  # DEC R12
            self.asm.emit_bytes(0x49, 0xF7, 0xD4)  # NOT R12
            
            # result & mask
            self.asm.emit_bytes(0x4C, 0x21, 0xE0)  # AND RAX, R12
            
            self.asm.emit_pop_r12()
        
        print("DEBUG: AlignUp operation completed")
        return True
    
    def compile_align_down(self, node):
        """AlignDown(value, alignment)"""
        if len(node.arguments) != 2:
            raise ValueError("AlignDown requires two arguments")
        
        print("DEBUG: Compiling AlignDown with nested call detection")
        
        arg1_complex = self._is_complex_expression(node.arguments[1])
        
        if arg1_complex:
            print("DEBUG: Using R13 (nested call detected)")
            self.asm.emit_push_r13()
            self.compiler.compile_expression(node.arguments[1])
            self.asm.emit_bytes(0x49, 0x89, 0xC5)  # MOV R13, RAX
            self.compiler.compile_expression(node.arguments[0])
            
            # ~(alignment - 1)
            self.asm.emit_bytes(0x49, 0xFF, 0xCD)  # DEC R13
            self.asm.emit_bytes(0x49, 0xF7, 0xD5)  # NOT R13
            
            # result & mask
            self.asm.emit_bytes(0x4C, 0x21, 0xE8)  # AND RAX, R13
            
            self.asm.emit_pop_r13()
        else:
            print("DEBUG: Using R12 (simple expression)")
            self.asm.emit_push_r12()
            self.compiler.compile_expression(node.arguments[1])
            self.asm.emit_bytes(0x49, 0x89, 0xC4)  # MOV R12, RAX
            self.compiler.compile_expression(node.arguments[0])
            
            # ~(alignment - 1)
            self.asm.emit_bytes(0x49, 0xFF, 0xCC)  # DEC R12
            self.asm.emit_bytes(0x49, 0xF7, 0xD4)  # NOT R12
            
            # result & mask
            self.asm.emit_bytes(0x4C, 0x21, 0xE0)  # AND RAX, R12
            
            self.asm.emit_pop_r12()
        
        print("DEBUG: AlignDown operation completed")
        return True
    
    def compile_is_power_of_two(self, node):
        """IsPowerOfTwo(x)"""
        if len(node.arguments) != 1:
            raise ValueError("IsPowerOfTwo requires one argument")
        
        print("DEBUG: Compiling IsPowerOfTwo")
        self.compiler.compile_expression(node.arguments[0])
        
        # (x != 0) && ((x & (x - 1)) == 0)
        zero_label = self.asm.create_label()
        done_label = self.asm.create_label()
        
        self.asm.emit_bytes(0x48, 0x85, 0xC0)  # TEST RAX, RAX
        self.asm.emit_jump_to_label(zero_label, "JZ")
        
        # x & (x - 1)
        self.asm.emit_mov_rbx_rax()
        self.asm.emit_bytes(0x48, 0xFF, 0xCB)  # DEC RBX
        self.asm.emit_bytes(0x48, 0x21, 0xD8)  # AND RAX, RBX
        
        # Check if zero
        self.asm.emit_bytes(0x48, 0x85, 0xC0)  # TEST RAX, RAX
        self.asm.emit_bytes(0x0F, 0x94, 0xC0)  # SETZ AL
        self.asm.emit_bytes(0x48, 0x0F, 0xB6, 0xC0)  # MOVZX RAX, AL
        
        self.asm.emit_jump_to_label(done_label, "JMP")
        
        self.asm.mark_label(zero_label)
        self.asm.emit_mov_rax_imm64(0)
        
        self.asm.mark_label(done_label)
        return True
    
    def compile_next_power_of_two(self, node):
        """NextPowerOfTwo(x)"""
        if len(node.arguments) != 1:
            raise ValueError("NextPowerOfTwo requires one argument")
        
        print("DEBUG: Compiling NextPowerOfTwo")
        self.compiler.compile_expression(node.arguments[0])
        
        # Fill all bits right of highest set bit
        self.asm.emit_bytes(0x48, 0xFF, 0xC8)  # DEC RAX
        
        for shift in [1, 2, 4, 8, 16, 32]:
            self.asm.emit_mov_rbx_rax()
            if shift == 1:
                self.asm.emit_bytes(0x48, 0xD1, 0xEB)  # SHR RBX, 1
            else:
                self.asm.emit_bytes(0x48, 0xC1, 0xEB, shift)  # SHR RBX, shift
            self.asm.emit_bytes(0x48, 0x09, 0xD8)  # OR RAX, RBX
        
        self.asm.emit_bytes(0x48, 0xFF, 0xC0)  # INC RAX
        return True
    
    def compile_floor_log2(self, node):
        """FloorLog2(x)"""
        if len(node.arguments) != 1:
            raise ValueError("FloorLog2 requires one argument")
        
        print("DEBUG: Compiling FloorLog2")
        self.compiler.compile_expression(node.arguments[0])
        
        zero_label = self.asm.create_label()
        done_label = self.asm.create_label()
        
        self.asm.emit_bytes(0x48, 0x85, 0xC0)  # TEST RAX, RAX
        self.asm.emit_jump_to_label(zero_label, "JZ")
        
        # BSR RAX, RAX
        self.asm.emit_bytes(0x48, 0x0F, 0xBD, 0xC0)
        
        self.asm.emit_jump_to_label(done_label, "JMP")
        
        self.asm.mark_label(zero_label)
        self.asm.emit_mov_rax_imm64(0)
        
        self.asm.mark_label(done_label)
        return True
    
    # ========== PLACEHOLDER IMPLEMENTATIONS ==========
    # These require floating point or complex algorithms
    
    def compile_floor_divide(self, node):
        return False  # Let arithmetic_ops handle it
    
    def compile_remainder(self, node):
        return False
    
    def compile_divmod(self, node):
        return False
    
    def compile_fma(self, node):
        return False
    
    def compile_hypot(self, node):
        return False
    
    def compile_lerp(self, node):
        return False
    
    # Trig functions - need floating point
    def compile_sin(self, node): return False
    def compile_cos(self, node): return False
    def compile_tan(self, node): return False
    def compile_asin(self, node): return False
    def compile_acos(self, node): return False
    def compile_atan(self, node): return False
    def compile_atan2(self, node): return False
    def compile_tanh(self, node): return False
    
    # Angle conversion
    def compile_deg_to_rad(self, node): return False
    def compile_rad_to_deg(self, node): return False
    
    # Exponential/Log - need floating point
    def compile_exp(self, node): return False
    def compile_expm1(self, node): return False
    def compile_exp2(self, node): return False
    def compile_log(self, node): return False
    def compile_log1p(self, node): return False
    def compile_log2(self, node): return False
    def compile_log10(self, node): return False
    
    # Floating point specific
    def compile_next_after(self, node): return False
    def compile_frexp(self, node): return False
    def compile_ldexp(self, node): return False
    def compile_nearly_equal(self, node): return False