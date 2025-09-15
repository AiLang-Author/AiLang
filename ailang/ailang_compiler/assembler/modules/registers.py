# ailang_compiler/assembler/modules/registers.py
"""Register-to-register move operations and immediate loads"""

import struct

class RegisterOperations:
    """Register movement and immediate loading operations"""
    
    # === IMMEDIATE LOADS TO REGISTERS ===
    
    def emit_mov_rax_imm64(self, value: int):
        """MOV RAX, imm64 - ENHANCED FOR LARGE NUMBERS"""
        self.emit_bytes(0x48, 0xB8)
        
        # ENHANCED ARITHMETIC: Handle large numbers
        if value > 18446744073709551615:  # 2^64 - 1
            truncated_value = value % (2**64)
            print(f"DEBUG: LARGE NUMBER TRUNCATED: {value} -> {truncated_value}")
            self.emit_bytes(*struct.pack('<Q', truncated_value))
        elif value < 0 and value < -9223372036854775808:  # 2^63
            truncated_value = value % (2**64)
            print(f"DEBUG: LARGE NEGATIVE TRUNCATED: {value} -> {truncated_value}")
            self.emit_bytes(*struct.pack('<Q', truncated_value))
        else:
            try:
                self.emit_bytes(*struct.pack('<Q', value))
            except struct.error as e:
                print(f"DEBUG: Struct pack failed for {value}, using modulo fallback")
                safe_value = value % (2**64)
                self.emit_bytes(*struct.pack('<Q', safe_value))
        
        print(f"DEBUG: MOV RAX, {value}")
    
    def emit_mov_rbx_imm64(self, value: int):
        """MOV RBX, imm64"""
        self.emit_bytes(0x48, 0xBB)
        if value < 0:
            value = value & 0xFFFFFFFFFFFFFFFF
        self.emit_bytes(*struct.pack('<Q', value))
        print(f"DEBUG: MOV RBX, {value}")
    
    def emit_mov_rcx_imm64(self, value: int):
        """MOV RCX, imm64"""
        self.emit_bytes(0x48, 0xB9)
        if value < 0:
            value = value & 0xFFFFFFFFFFFFFFFF
        self.emit_bytes(*struct.pack('<Q', value))
        print(f"DEBUG: MOV RCX, {value}")
    
    def emit_mov_rdx_imm64(self, value: int):
        """MOV RDX, imm64"""
        self.emit_bytes(0x48, 0xBA)
        if value < 0:
            value = value & 0xFFFFFFFFFFFFFFFF
        self.emit_bytes(*struct.pack('<Q', value))
        print(f"DEBUG: MOV RDX, {value}")
    
    def emit_mov_rsi_imm64(self, value: int):
        """MOV RSI, imm64"""
        self.emit_bytes(0x48, 0xBE)
        if value < 0:
            value = value & 0xFFFFFFFFFFFFFFFF
        self.emit_bytes(*struct.pack('<Q', value))
        print(f"DEBUG: MOV RSI, {value}")
    
    def emit_mov_rdi_imm64(self, value: int):
        """MOV RDI, imm64"""
        self.emit_bytes(0x48, 0xBF)
        if value < 0:
            value = value & 0xFFFFFFFFFFFFFFFF
        self.emit_bytes(*struct.pack('<Q', value))
        print(f"DEBUG: MOV RDI, {value}")
    
    def emit_mov_r8_imm64(self, value: int):
        """MOV R8, imm64"""
        self.emit_bytes(0x49, 0xB8)
        if value < 0:
            value = value & 0xFFFFFFFFFFFFFFFF
        self.emit_bytes(*struct.pack('<Q', value))
        print(f"DEBUG: MOV R8, {value}")
    
    def emit_mov_r9_imm64(self, value: int):
        """MOV R9, imm64"""
        self.emit_bytes(0x49, 0xB9)
        if value < 0:
            value = value & 0xFFFFFFFFFFFFFFFF
        self.emit_bytes(*struct.pack('<Q', value))
        print(f"DEBUG: MOV R9, {value}")
    
    def emit_mov_r10_imm64(self, value: int):
        """MOV R10, imm64"""
        self.emit_bytes(0x49, 0xBA)
        if value < 0:
            value = value & 0xFFFFFFFFFFFFFFFF
        self.emit_bytes(*struct.pack('<Q', value))
        print(f"DEBUG: MOV R10, {value}")
    
    # === REGISTER-TO-REGISTER MOVES ===
    
    def emit_mov_rax_rbx(self):
        """MOV RAX, RBX"""
        self.emit_bytes(0x48, 0x89, 0xD8)
    
    def emit_mov_rbx_rax(self):
        """MOV RBX, RAX"""
        self.emit_bytes(0x48, 0x89, 0xC3)
        print("DEBUG: MOV RBX, RAX")
    
    def emit_mov_rcx_rax(self):
        """MOV RCX, RAX"""
        self.emit_bytes(0x48, 0x89, 0xC1)
        print("DEBUG: MOV RCX, RAX")
    
    def emit_mov_rdx_rax(self):
        """MOV RDX, RAX"""
        self.emit_bytes(0x48, 0x89, 0xC2)
    
    def emit_mov_rdi_rax(self):
        """MOV RDI, RAX"""
        self.emit_bytes(0x48, 0x89, 0xC7)
        print("DEBUG: MOV RDI, RAX")
    
    def emit_mov_rsi_rax(self):
        """MOV RSI, RAX"""
        self.emit_bytes(0x48, 0x89, 0xC6)
        print("DEBUG: MOV RSI, RAX")
    
    def emit_mov_rax_rdi(self):
        """MOV RAX, RDI"""
        self.emit_bytes(0x48, 0x89, 0xF8)
        print("DEBUG: MOV RAX, RDI")
    
    def emit_mov_rax_rsi(self):
        """MOV RAX, RSI"""
        self.emit_bytes(0x48, 0x89, 0xF0)
        print("DEBUG: Emitted MOV RAX, RSI")
    
    def emit_mov_rax_rcx(self):
        """MOV RAX, RCX"""
        self.emit_bytes(0x48, 0x89, 0xC8)
        print("DEBUG: MOV RAX, RCX")
    
    def emit_mov_rax_rsp(self):
        """MOV RAX, RSP"""
        self.emit_bytes(0x48, 0x89, 0xE0)
        print("DEBUG: MOV RAX, RSP")
    
    def emit_mov_rdi_rbx(self):
        """MOV RDI, RBX"""
        self.emit_bytes(0x48, 0x89, 0xDF)
        print("DEBUG: MOV RDI, RBX")
    
    def emit_mov_rdi_from_rbx(self):
        """MOV RDI, RBX (alias)"""
        self.emit_mov_rdi_rbx()
    
    def emit_mov_rdi_rsp(self):
        """MOV RDI, RSP"""
        self.emit_bytes(0x48, 0x89, 0xE7)
        print("DEBUG: MOV RDI, RSP")
    
    def emit_mov_rdi_rsi(self):
        """MOV RDI, RSI"""
        self.emit_bytes(0x48, 0x89, 0xF7)
    
    def emit_mov_rsi_rbx(self):
        """MOV RSI, RBX"""
        self.emit_bytes(0x48, 0x89, 0xDE)
        print("DEBUG: MOV RSI, RBX")
    
    def emit_mov_rsi_from_rbx(self):
        """MOV RSI, RBX (alias)"""
        self.emit_mov_rsi_rbx()
    
    def emit_mov_rsi_rcx(self):
        """MOV RSI, RCX"""
        self.emit_bytes(0x48, 0x89, 0xCE)
        print("DEBUG: MOV RSI, RCX")
    
    def emit_mov_rsi_rdi(self):
        """MOV RSI, RDI"""
        self.emit_bytes(0x48, 0x89, 0xFE)
        print("DEBUG: MOV RSI, RDI")
    
    def emit_mov_rsi_rsp(self):
        """MOV RSI, RSP"""
        self.emit_bytes(0x48, 0x89, 0xE6)
        print("DEBUG: MOV RSI, RSP")
    
    def emit_mov_rdx_rbx(self):
        """MOV RDX, RBX"""
        self.emit_bytes(0x48, 0x89, 0xDA)
        print("DEBUG: Emitted MOV RDX, RBX")
    
    def emit_mov_rdx_r10(self):
        """MOV RDX, R10"""
        self.emit_bytes(0x4C, 0x89, 0xD2)
        print("DEBUG: MOV RDX, R10")
    
    def emit_mov_rbx_rsi(self):
        """MOV RBX, RSI"""
        self.emit_bytes(0x48, 0x89, 0xF3)
        print("DEBUG: Emitted MOV RBX, RSI")
    
    def emit_mov_rbp_rsp(self):
        """MOV RBP, RSP - Set up stack frame"""
        self.emit_bytes(0x48, 0x89, 0xE5)
        print("DEBUG: MOV RBP, RSP")
    
    def emit_mov_rsp_rbp(self):
        """MOV RSP, RBP - Restore stack pointer"""
        self.emit_bytes(0x48, 0x89, 0xEC)
        print("DEBUG: MOV RSP, RBP")
    
    def emit_mov_rsp_rax(self):
        """MOV RSP, RAX"""
        self.emit_bytes(0x48, 0x89, 0xC4)
        print("DEBUG: MOV RSP, RAX")
    
    # === Extended register moves (R8-R15) ===
    
    def emit_mov_r8_rdi(self):
        """MOV R8, RDI"""
        self.emit_bytes(0x49, 0x89, 0xF8)
        print("DEBUG: MOV R8, RDI")
    
    def emit_mov_r8_rax(self):
        """MOV R8, RAX"""
        self.emit_bytes(0x49, 0x89, 0xC0)
        print("DEBUG: MOV R8, RAX")
    
    def emit_mov_r9_rsi(self):
        """MOV R9, RSI"""
        self.emit_bytes(0x49, 0x89, 0xF1)
        print("DEBUG: MOV R9, RSI")
    
    def emit_mov_r9_rax(self):
        """MOV R9, RAX"""
        self.emit_bytes(0x49, 0x89, 0xC1)
        print("DEBUG: MOV R9, RAX")
    
    def emit_mov_r10_rax(self):
        """MOV R10, RAX"""
        self.emit_bytes(0x49, 0x89, 0xC2)
        print("DEBUG: MOV R10, RAX")
    
    def emit_mov_rax_r8(self):
        """MOV RAX, R8"""
        self.emit_bytes(0x4C, 0x89, 0xC0)
        print("DEBUG: MOV RAX, R8")
    
    def emit_mov_rax_r9(self):
        """MOV RAX, R9"""
        self.emit_bytes(0x4C, 0x89, 0xC8)
        print("DEBUG: MOV RAX, R9")
    
    def emit_mov_rax_r10(self):
        """MOV RAX, R10"""
        self.emit_bytes(0x4C, 0x89, 0xD0)
        print("DEBUG: MOV RAX, R10")
    
    def emit_mov_rdi_r8(self):
        """MOV RDI, R8"""
        self.emit_bytes(0x4C, 0x89, 0xC7)
        print("DEBUG: MOV RDI, R8")
    
    def emit_mov_rsi_r9(self):
        """MOV RSI, R9"""
        self.emit_bytes(0x4C, 0x89, 0xCE)
        print("DEBUG: MOV RSI, R9")
    
    # === TEST OPERATIONS ===
    
    def emit_test_rax_rax(self):
        """TEST RAX, RAX - Set flags based on RAX"""
        self.emit_bytes(0x48, 0x85, 0xC0)
        print("DEBUG: TEST RAX, RAX")
    
    def emit_test_rcx_rcx(self):
        """TEST RCX, RCX"""
        self.emit_bytes(0x48, 0x85, 0xC9)
    
    def emit_test_rdi_rdi(self):
        """TEST RDI, RDI - Set flags based on RDI"""
        self.emit_bytes(0x48, 0x85, 0xFF)
        print("DEBUG: TEST RDI, RDI")
    
    def emit_test_rsi_rsi(self):
        """TEST RSI, RSI - Set flags based on RSI"""
        self.emit_bytes(0x48, 0x85, 0xF6)
        print("DEBUG: TEST RSI, RSI")
    
    def emit_test_r10_r10(self):
        """TEST R10, R10 - Set flags based on R10"""
        self.emit_bytes(0x4D, 0x85, 0xD2)
        print("DEBUG: TEST R10, R10")
    
    def emit_test_rax_imm8(self, value: int):
        """Test the least significant byte of RAX with an 8-bit immediate"""
        if not 0 <= value <= 0xFF:
            raise ValueError(f"Immediate value {value} out of 8-bit range (0-255)")
        self.emit_bytes(0xA8)  # TEST AL, imm8
        self.emit_bytes(*struct.pack('<B', value))
        print(f"DEBUG: Emitted TEST AL, {value:#x}")
        
        
    def emit_mov_rcx_rbx(self):
        """MOV RCX, RBX - Move RBX to RCX"""
        self.emit_bytes(0x48, 0x89, 0xD9)
        print("DEBUG: MOV RCX, RBX")

    # Also add the reverse operation for completeness:
    def emit_mov_rbx_rcx(self):
        """MOV RBX, RCX - Move RCX to RBX"""
        self.emit_bytes(0x48, 0x89, 0xCB)
        print("DEBUG: MOV RBX, RCX")    
    
    # === COMPARE OPERATIONS ===
    
    def emit_cmp_rax_imm8(self, value):
        """CMP RAX, imm8"""
        self.emit_bytes(0x48, 0x83, 0xF8, value & 0xFF)
        print(f"DEBUG: CMP RAX, {value}")
    
    def emit_cmp_rax_imm32(self, value: int):
        """CMP RAX, imm32 - Compare RAX with 32-bit immediate"""
        self.emit_bytes(0x48, 0x3D)  # CMP RAX, imm32
        self.emit_bytes(*struct.pack('<i', value))
        print(f"DEBUG: CMP RAX, {value}")
    
    def emit_cmp_rax_imm64(self, value: int):
        """Compare RAX with a 64-bit immediate using a register intermediate"""
        value = value & 0xFFFFFFFFFFFFFFFF
        self.emit_mov_rbx_imm64(value)
        self.emit_bytes(0x48, 0x39, 0xD8)  # CMP RAX, RBX
        print(f"DEBUG: Emitted CMP RAX, {value:#x}")
    
    # === INCREMENT/DECREMENT ===
    
    def emit_inc_rdi(self):
        """INC RDI - Increment RDI by 1"""
        self.emit_bytes(0x48, 0xFF, 0xC7)
        print("DEBUG: INC RDI")
    
    def emit_dec_rdi(self):
        """DEC RDI - Decrement RDI"""
        self.emit_bytes(0x48, 0xFF, 0xCF)