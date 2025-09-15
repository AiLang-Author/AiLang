# ailang_compiler/compiler/modules/math_ops.py
"""
Advanced Math Operations Module for AILANG Compiler
Implements the 50 math primitives from the grammar
"""

class MathOperations:
    """Handles advanced math operations beyond basic arithmetic"""
    
    def __init__(self, compiler_context):
        self.compiler = compiler_context
        self.asm = compiler_context.asm
        
    def compile_operation(self, node):
        """Main dispatcher for math operations"""
        
        # Map function names to implementation methods
        operations = {
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
    
    # ========== ROUNDING OPERATIONS ==========
    
    def compile_floor(self, node):
        """Floor(x) - Round down to nearest integer"""
        if len(node.arguments) != 1:
            raise ValueError("Floor requires one argument")
        
        print("DEBUG: Compiling Floor")
        # For now, integer floor is identity
        self.compiler.compile_expression(node.arguments[0])
        # In future, handle floating point
        return True
    
    def compile_ceil(self, node):
        """Ceil(x) - Round up to nearest integer"""
        if len(node.arguments) != 1:
            raise ValueError("Ceil requires one argument")
        
        print("DEBUG: Compiling Ceil")
        # For integers, this is identity
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
        
        print("DEBUG: Compiling Min")
        
        # Compile both arguments
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_push_rax()
        self.compiler.compile_expression(node.arguments[1])
        self.asm.emit_mov_rbx_rax()
        self.asm.emit_pop_rax()
        
        # Compare and select minimum
        self.asm.emit_bytes(0x48, 0x39, 0xD8)  # CMP RAX, RBX
        
        # CMOVG - Move if greater (select RBX if RAX > RBX)
        self.asm.emit_bytes(0x48, 0x0F, 0x4F, 0xC3)  # CMOVG RAX, RBX
        
        print("DEBUG: Min operation completed")
        return True
    
    def compile_max(self, node):
        """Max(a, b) - Return larger value"""
        if len(node.arguments) != 2:
            raise ValueError("Max requires two arguments")
        
        print("DEBUG: Compiling Max")
        
        # Compile both arguments
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_push_rax()
        self.compiler.compile_expression(node.arguments[1])
        self.asm.emit_mov_rbx_rax()
        self.asm.emit_pop_rax()
        
        # Compare and select maximum
        self.asm.emit_bytes(0x48, 0x39, 0xD8)  # CMP RAX, RBX
        
        # CMOVL - Move if less (select RBX if RAX < RBX)
        self.asm.emit_bytes(0x48, 0x0F, 0x4C, 0xC3)  # CMOVL RAX, RBX
        
        print("DEBUG: Max operation completed")
        return True
    
    def compile_clamp(self, node):
        """Clamp(value, min, max) - Constrain value to range"""
        if len(node.arguments) != 3:
            raise ValueError("Clamp requires three arguments")
        
        print("DEBUG: Compiling Clamp")
        
        # value in RAX
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_push_rax()
        
        # min in RBX
        self.compiler.compile_expression(node.arguments[1])
        self.asm.emit_push_rax()
        
        # max in RCX
        self.compiler.compile_expression(node.arguments[2])
        self.asm.emit_mov_rcx_rax()
        
        # Get min in RBX
        self.asm.emit_pop_rbx()
        
        # Get value in RAX
        self.asm.emit_pop_rax()
        
        # Clamp to minimum
        self.asm.emit_bytes(0x48, 0x39, 0xD8)  # CMP RAX, RBX
        self.asm.emit_bytes(0x48, 0x0F, 0x4C, 0xC3)  # CMOVL RAX, RBX
        
        # Clamp to maximum
        self.asm.emit_bytes(0x48, 0x39, 0xC8)  # CMP RAX, RCX
        self.asm.emit_bytes(0x48, 0x0F, 0x4F, 0xC1)  # CMOVG RAX, RCX
        
        print("DEBUG: Clamp operation completed")
        return True
    
    def compile_saturate(self, node):
        """Saturate(x) - Clamp to [0, 1] range"""
        if len(node.arguments) != 1:
            raise ValueError("Saturate requires one argument")
        
        print("DEBUG: Compiling Saturate")
        
        self.compiler.compile_expression(node.arguments[0])
        
        # For integers: 0 if negative, 1 if positive, value if 0 or 1
        # Test if negative
        self.asm.emit_bytes(0x48, 0x85, 0xC0)  # TEST RAX, RAX
        
        # Set to 0 if negative
        self.asm.emit_mov_rbx_imm64(0)
        self.asm.emit_bytes(0x48, 0x0F, 0x48, 0xC3)  # CMOVS RAX, RBX (move if sign)
        
        # Compare with 1
        self.asm.emit_mov_rbx_imm64(1)
        self.asm.emit_bytes(0x48, 0x39, 0xD8)  # CMP RAX, RBX
        
        # Set to 1 if greater
        self.asm.emit_bytes(0x48, 0x0F, 0x4F, 0xC3)  # CMOVG RAX, RBX
        
        print("DEBUG: Saturate operation completed")
        return True
    
    def compile_sign(self, node):
        """Sign(x) - Return -1, 0, or 1"""
        if len(node.arguments) != 1:
            raise ValueError("Sign requires one argument")
        
        print("DEBUG: Compiling Sign")
        
        self.compiler.compile_expression(node.arguments[0])
        
        # Save original value
        self.asm.emit_push_rax()
        
        # Test if zero
        self.asm.emit_bytes(0x48, 0x85, 0xC0)  # TEST RAX, RAX
        
        # Set result based on flags
        self.asm.emit_mov_rax_imm64(0)  # Default to 0
        self.asm.emit_mov_rbx_imm64(1)   # Positive result
        self.asm.emit_mov_rcx_imm64(-1)  # Negative result (will be sign-extended)
        
        # Use conditional moves based on sign and zero flags
        # If positive (not zero, not sign)
        self.asm.emit_bytes(0x48, 0x0F, 0x4F, 0xC3)  # CMOVG RAX, RBX (if greater than 0)
        
        # If negative (sign flag set)
        self.asm.emit_bytes(0x48, 0x0F, 0x48, 0xC1)  # CMOVS RAX, RCX (if sign)
        
        # Clean up stack
        self.asm.emit_bytes(0x48, 0x83, 0xC4, 0x08)  # ADD RSP, 8
        
        print("DEBUG: Sign operation completed")
        return True
    
    # ========== BIT OPERATIONS ==========
    
    def compile_popcount(self, node):
        """PopCount(x) - Count set bits"""
        if len(node.arguments) != 1:
            raise ValueError("PopCount requires one argument")
        
        print("DEBUG: Compiling PopCount")
        
        self.compiler.compile_expression(node.arguments[0])
        
        # POPCNT instruction (requires POPCNT CPU feature)
        # For compatibility, we'll use a software fallback
        
        # Brian Kernighan's algorithm
        self.asm.emit_mov_rcx_imm64(0)  # Counter in RCX
        
        loop_start = self.asm.create_label()
        loop_end = self.asm.create_label()
        
        self.asm.mark_label(loop_start)
        
        # Test if RAX is zero
        self.asm.emit_bytes(0x48, 0x85, 0xC0)  # TEST RAX, RAX
        self.asm.emit_jump_to_label(loop_end, "JZ")
        
        # n = n & (n - 1) - clears lowest set bit
        self.asm.emit_mov_rbx_rax()  # MOV RBX, RAX
        self.asm.emit_bytes(0x48, 0xFF, 0xCB)  # DEC RBX
        self.asm.emit_bytes(0x48, 0x21, 0xD8)  # AND RAX, RBX
        
        # Increment counter
        self.asm.emit_bytes(0x48, 0xFF, 0xC1)  # INC RCX
        
        # Loop
        self.asm.emit_jump_to_label(loop_start, "JMP")
        
        self.asm.mark_label(loop_end)
        
        # Move result to RAX
        self.asm.emit_mov_rax_rcx()
        
        print("DEBUG: PopCount operation completed")
        return True
    
    def compile_clz(self, node):
        """CountLeadingZeros(x) - Count leading zero bits"""
        if len(node.arguments) != 1:
            raise ValueError("CountLeadingZeros requires one argument")
        
        print("DEBUG: Compiling CountLeadingZeros")
        
        self.compiler.compile_expression(node.arguments[0])
        
        # BSR - Bit Scan Reverse (finds position of highest set bit)
        # Result is 63 - position for CLZ
        
        # Handle zero case
        zero_label = self.asm.create_label()
        done_label = self.asm.create_label()
        
        self.asm.emit_bytes(0x48, 0x85, 0xC0)  # TEST RAX, RAX
        self.asm.emit_jump_to_label(zero_label, "JZ")
        
        # BSR RBX, RAX
        self.asm.emit_bytes(0x48, 0x0F, 0xBD, 0xD8)  # BSR RBX, RAX
        
        # CLZ = 63 - position
        self.asm.emit_mov_rax_imm64(63)
        self.asm.emit_bytes(0x48, 0x29, 0xD8)  # SUB RAX, RBX
        
        self.asm.emit_jump_to_label(done_label, "JMP")
        
        self.asm.mark_label(zero_label)
        self.asm.emit_mov_rax_imm64(64)  # All bits are zero
        
        self.asm.mark_label(done_label)
        
        print("DEBUG: CountLeadingZeros operation completed")
        return True
    
    def compile_ctz(self, node):
        """CountTrailingZeros(x) - Count trailing zero bits"""
        if len(node.arguments) != 1:
            raise ValueError("CountTrailingZeros requires one argument")
        
        print("DEBUG: Compiling CountTrailingZeros")
        
        self.compiler.compile_expression(node.arguments[0])
        
        # BSF - Bit Scan Forward (finds position of lowest set bit)
        
        # Handle zero case
        zero_label = self.asm.create_label()
        done_label = self.asm.create_label()
        
        self.asm.emit_bytes(0x48, 0x85, 0xC0)  # TEST RAX, RAX
        self.asm.emit_jump_to_label(zero_label, "JZ")
        
        # BSF RAX, RAX (result is position of first set bit = CTZ)
        self.asm.emit_bytes(0x48, 0x0F, 0xBC, 0xC0)  # BSF RAX, RAX
        
        self.asm.emit_jump_to_label(done_label, "JMP")
        
        self.asm.mark_label(zero_label)
        self.asm.emit_mov_rax_imm64(64)  # All bits are zero
        
        self.asm.mark_label(done_label)
        
        print("DEBUG: CountTrailingZeros operation completed")
        return True
    
    def compile_rotate_left(self, node):
        """RotateLeft(value, count) - Rotate bits left"""
        if len(node.arguments) != 2:
            raise ValueError("RotateLeft requires two arguments")
        
        print("DEBUG: Compiling RotateLeft")
        
        # Value in RAX
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_push_rax()
        
        # Count in CL
        self.compiler.compile_expression(node.arguments[1])
        self.asm.emit_mov_rcx_rax()
        
        # Get value back
        self.asm.emit_pop_rax()
        
        # ROL RAX, CL
        self.asm.emit_bytes(0x48, 0xD3, 0xC0)  # ROL RAX, CL
        
        print("DEBUG: RotateLeft operation completed")
        return True
    
    def compile_rotate_right(self, node):
        """RotateRight(value, count) - Rotate bits right"""
        if len(node.arguments) != 2:
            raise ValueError("RotateRight requires two arguments")
        
        print("DEBUG: Compiling RotateRight")
        
        # Value in RAX
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_push_rax()
        
        # Count in CL
        self.compiler.compile_expression(node.arguments[1])
        self.asm.emit_mov_rcx_rax()
        
        # Get value back
        self.asm.emit_pop_rax()
        
        # ROR RAX, CL
        self.asm.emit_bytes(0x48, 0xD3, 0xC8)  # ROR RAX, CL
        
        print("DEBUG: RotateRight operation completed")
        return True
    
    def compile_byte_swap(self, node):
        """ByteSwap(x) - Reverse byte order"""
        if len(node.arguments) != 1:
            raise ValueError("ByteSwap requires one argument")
        
        print("DEBUG: Compiling ByteSwap")
        
        self.compiler.compile_expression(node.arguments[0])
        
        # BSWAP RAX - reverses byte order
        self.asm.emit_bytes(0x48, 0x0F, 0xC8)  # BSWAP RAX
        
        print("DEBUG: ByteSwap operation completed")
        return True
    
    def compile_bit_reverse(self, node):
        """BitReverse(x) - Reverse bit order"""
        if len(node.arguments) != 1:
            raise ValueError("BitReverse requires one argument")
        
        print("DEBUG: Compiling BitReverse")
        
        # This is complex without dedicated instruction
        # For now, placeholder
        self.compiler.compile_expression(node.arguments[0])
        
        print("DEBUG: BitReverse operation completed (placeholder)")
        return True
    
    # ========== ALIGNMENT OPERATIONS ==========
    
    def compile_align_up(self, node):
        """AlignUp(value, alignment) - Round up to alignment boundary"""
        if len(node.arguments) != 2:
            raise ValueError("AlignUp requires two arguments")
        
        print("DEBUG: Compiling AlignUp")
        
        # Formula: (value + alignment - 1) & ~(alignment - 1)
        
        # Get value in RAX
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_push_rax()
        
        # Get alignment in RBX
        self.compiler.compile_expression(node.arguments[1])
        self.asm.emit_mov_rbx_rax()
        
        # Get value back
        self.asm.emit_pop_rax()
        
        # Add (alignment - 1) to value
        self.asm.emit_push_rbx()  # Save alignment
        self.asm.emit_bytes(0x48, 0xFF, 0xCB)  # DEC RBX
        self.asm.emit_bytes(0x48, 0x01, 0xD8)  # ADD RAX, RBX
        
        # Create mask: ~(alignment - 1)
        self.asm.emit_bytes(0x48, 0xF7, 0xD3)  # NOT RBX
        
        # Apply mask
        self.asm.emit_bytes(0x48, 0x21, 0xD8)  # AND RAX, RBX
        
        # Clean up stack
        self.asm.emit_bytes(0x48, 0x83, 0xC4, 0x08)  # ADD RSP, 8
        
        print("DEBUG: AlignUp operation completed")
        return True
    
    def compile_align_down(self, node):
        """AlignDown(value, alignment) - Round down to alignment boundary"""
        if len(node.arguments) != 2:
            raise ValueError("AlignDown requires two arguments")
        
        print("DEBUG: Compiling AlignDown")
        
        # Formula: value & ~(alignment - 1)
        
        # Get value in RAX
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_push_rax()
        
        # Get alignment in RBX
        self.compiler.compile_expression(node.arguments[1])
        self.asm.emit_mov_rbx_rax()
        
        # Get value back
        self.asm.emit_pop_rax()
        
        # Create mask: ~(alignment - 1)
        self.asm.emit_bytes(0x48, 0xFF, 0xCB)  # DEC RBX
        self.asm.emit_bytes(0x48, 0xF7, 0xD3)  # NOT RBX
        
        # Apply mask
        self.asm.emit_bytes(0x48, 0x21, 0xD8)  # AND RAX, RBX
        
        print("DEBUG: AlignDown operation completed")
        return True
    
    def compile_is_power_of_two(self, node):
        """IsPowerOfTwo(x) - Check if x is a power of 2"""
        if len(node.arguments) != 1:
            raise ValueError("IsPowerOfTwo requires one argument")
        
        print("DEBUG: Compiling IsPowerOfTwo")
        
        self.compiler.compile_expression(node.arguments[0])
        
        # Power of 2 check: (x != 0) && ((x & (x - 1)) == 0)
        
        # Check if zero
        self.asm.emit_bytes(0x48, 0x85, 0xC0)  # TEST RAX, RAX
        
        zero_label = self.asm.create_label()
        done_label = self.asm.create_label()
        
        self.asm.emit_jump_to_label(zero_label, "JZ")
        
        # x & (x - 1)
        self.asm.emit_mov_rbx_rax()  # MOV RBX, RAX
        self.asm.emit_bytes(0x48, 0xFF, 0xCB)  # DEC RBX
        self.asm.emit_bytes(0x48, 0x21, 0xD8)  # AND RAX, RBX
        
        # Check if result is zero
        self.asm.emit_bytes(0x48, 0x85, 0xC0)  # TEST RAX, RAX
        
        # Set result
        self.asm.emit_bytes(0x0F, 0x94, 0xC0)  # SETZ AL
        self.asm.emit_bytes(0x48, 0x0F, 0xB6, 0xC0)  # MOVZX RAX, AL
        
        self.asm.emit_jump_to_label(done_label, "JMP")
        
        self.asm.mark_label(zero_label)
        self.asm.emit_mov_rax_imm64(0)  # Not a power of 2
        
        self.asm.mark_label(done_label)
        
        print("DEBUG: IsPowerOfTwo operation completed")
        return True
    
    def compile_next_power_of_two(self, node):
        """NextPowerOfTwo(x) - Round up to next power of 2"""
        if len(node.arguments) != 1:
            raise ValueError("NextPowerOfTwo requires one argument")
        
        print("DEBUG: Compiling NextPowerOfTwo")
        
        self.compiler.compile_expression(node.arguments[0])
        
        # Bit manipulation trick: fill all bits to the right, then add 1
        # x--; x |= x >> 1; x |= x >> 2; x |= x >> 4; x |= x >> 8; x |= x >> 16; x |= x >> 32; x++;
        
        # Decrement
        self.asm.emit_bytes(0x48, 0xFF, 0xC8)  # DEC RAX
        
        # x |= x >> 1
        self.asm.emit_mov_rbx_rax()
        self.asm.emit_bytes(0x48, 0xD1, 0xEB)  # SHR RBX, 1
        self.asm.emit_bytes(0x48, 0x09, 0xD8)  # OR RAX, RBX
        
        # x |= x >> 2
        self.asm.emit_mov_rbx_rax()
        self.asm.emit_bytes(0x48, 0xC1, 0xEB, 0x02)  # SHR RBX, 2
        self.asm.emit_bytes(0x48, 0x09, 0xD8)  # OR RAX, RBX
        
        # x |= x >> 4
        self.asm.emit_mov_rbx_rax()
        self.asm.emit_bytes(0x48, 0xC1, 0xEB, 0x04)  # SHR RBX, 4
        self.asm.emit_bytes(0x48, 0x09, 0xD8)  # OR RAX, RBX
        
        # x |= x >> 8
        self.asm.emit_mov_rbx_rax()
        self.asm.emit_bytes(0x48, 0xC1, 0xEB, 0x08)  # SHR RBX, 8
        self.asm.emit_bytes(0x48, 0x09, 0xD8)  # OR RAX, RBX
        
        # x |= x >> 16
        self.asm.emit_mov_rbx_rax()
        self.asm.emit_bytes(0x48, 0xC1, 0xEB, 0x10)  # SHR RBX, 16
        self.asm.emit_bytes(0x48, 0x09, 0xD8)  # OR RAX, RBX
        
        # x |= x >> 32
        self.asm.emit_mov_rbx_rax()
        self.asm.emit_bytes(0x48, 0xC1, 0xEB, 0x20)  # SHR RBX, 32
        self.asm.emit_bytes(0x48, 0x09, 0xD8)  # OR RAX, RBX
        
        # Increment
        self.asm.emit_bytes(0x48, 0xFF, 0xC0)  # INC RAX
        
        print("DEBUG: NextPowerOfTwo operation completed")
        return True
    
    def compile_floor_log2(self, node):
        """FloorLog2(x) - Floor of log base 2"""
        if len(node.arguments) != 1:
            raise ValueError("FloorLog2 requires one argument")
        
        print("DEBUG: Compiling FloorLog2")
        
        self.compiler.compile_expression(node.arguments[0])
        
        # BSR gives position of highest set bit, which is floor(log2(x))
        
        # Handle zero case
        zero_label = self.asm.create_label()
        done_label = self.asm.create_label()
        
        self.asm.emit_bytes(0x48, 0x85, 0xC0)  # TEST RAX, RAX
        self.asm.emit_jump_to_label(zero_label, "JZ")
        
        # BSR RAX, RAX
        self.asm.emit_bytes(0x48, 0x0F, 0xBD, 0xC0)  # BSR RAX, RAX
        
        self.asm.emit_jump_to_label(done_label, "JMP")
        
        self.asm.mark_label(zero_label)
        self.asm.emit_mov_rax_imm64(0)  # Undefined, return 0
        
        self.asm.mark_label(done_label)
        
        print("DEBUG: FloorLog2 operation completed")
        return True
    
    # ========== PLACEHOLDER IMPLEMENTATIONS ==========
    # These require floating point or complex algorithms
    
    def compile_floor_divide(self, node):
        """FloorDivide - division rounded towards negative infinity"""
        # For now, same as regular divide for integers
        return False  # Let arithmetic_ops handle it
    
    def compile_remainder(self, node):
        """Remainder - modulo operation"""
        # Would be same as Modulo when implemented
        return False
    
    def compile_divmod(self, node):
        """DivMod - return both quotient and remainder"""
        # Needs tuple return support
        return False
    
    def compile_fma(self, node):
        """FusedMultiplyAdd(a, b, c) - a*b + c with no intermediate rounding"""
        # Needs floating point
        return False
    
    def compile_hypot(self, node):
        """Hypotenuse - sqrt(x^2 + y^2)"""
        # Needs floating point
        return False
    
    def compile_lerp(self, node):
        """Linear interpolation"""
        # Needs floating point
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