# ailang_compiler/assembler/modules/stack.py
"""Stack operations - push, pop, stack pointer manipulation"""

import struct

class StackOperations:
    """Stack manipulation operations"""
    
    # === PUSH OPERATIONS ===
    
    def emit_push_rax(self):
        """PUSH RAX"""
        self.emit_bytes(0x50)
    
    def emit_push_rbx(self):
        """PUSH RBX"""
        self.emit_bytes(0x53)
    
    def emit_push_rcx(self):
        """PUSH RCX"""
        self.emit_bytes(0x51)
    
    def emit_push_rdx(self):
        """PUSH RDX"""
        self.emit_bytes(0x52)
    
    def emit_push_rsi(self):
        """PUSH RSI"""
        self.emit_bytes(0x56)
    
    def emit_push_rdi(self):
        """PUSH RDI"""
        self.emit_bytes(0x57)
    
    def emit_push_rbp(self):
        """PUSH RBP - Standard frame pointer save"""
        self.emit_bytes(0x55)
        print("DEBUG: PUSH RBP")
    
    def emit_push_r8(self):
        """PUSH R8"""
        self.emit_bytes(0x41, 0x50)
    
    def emit_push_r9(self):
        """PUSH R9"""
        self.emit_bytes(0x41, 0x51)
    
    def emit_push_r10(self):
        """PUSH R10"""
        self.emit_bytes(0x41, 0x52)
        print("DEBUG: PUSH R10")
    
    def emit_push_r11(self):
        """PUSH R11"""
        self.emit_bytes(0x41, 0x53)
    
    def emit_push_r12(self):
        """PUSH R12 - Callee-saved register"""
        self.emit_bytes(0x41, 0x54)
    
    def emit_push_r13(self):
        """PUSH R13 - Callee-saved register"""
        self.emit_bytes(0x41, 0x55)
    
    def emit_push_r14(self):
        """PUSH R14 - Callee-saved register"""
        self.emit_bytes(0x41, 0x56)
    
    def emit_push_r15(self):
        """PUSH R15 - Callee-saved register"""
        self.emit_bytes(0x41, 0x57)
    
    # === POP OPERATIONS ===
    
    def emit_pop_rax(self):
        """POP RAX"""
        self.emit_bytes(0x58)
    
    def emit_pop_rbx(self):
        """POP RBX"""
        self.emit_bytes(0x5B)
    
    def emit_pop_rcx(self):
        """POP RCX"""
        self.emit_bytes(0x59)
    
    def emit_pop_rdx(self):
        """POP RDX"""
        self.emit_bytes(0x5A)
    
    def emit_pop_rsi(self):
        """POP RSI"""
        self.emit_bytes(0x5E)
    
    def emit_pop_rdi(self):
        """POP RDI"""
        self.emit_bytes(0x5F)
    
    def emit_pop_rbp(self):
        """POP RBP - Standard frame pointer restore"""
        self.emit_bytes(0x5D)
        print("DEBUG: POP RBP")
    
    def emit_pop_r8(self):
        """POP R8"""
        self.emit_bytes(0x41, 0x58)
    
    def emit_pop_r9(self):
        """POP R9"""
        self.emit_bytes(0x41, 0x59)
    
    def emit_pop_r10(self):
        """POP R10"""
        self.emit_bytes(0x41, 0x5A)
        print("DEBUG: POP R10")
    
    def emit_pop_r11(self):
        """POP R11"""
        self.emit_bytes(0x41, 0x5B)
    
    def emit_pop_r12(self):
        """POP R12 - Callee-saved register"""
        self.emit_bytes(0x41, 0x5C)
    
    def emit_pop_r13(self):
        """POP R13 - Callee-saved register"""
        self.emit_bytes(0x41, 0x5D)
    
    def emit_pop_r14(self):
        """POP R14 - Callee-saved register"""
        self.emit_bytes(0x41, 0x5E)
    
    def emit_pop_r15(self):
        """POP R15 - Callee-saved register"""
        self.emit_bytes(0x41, 0x5F)
    
    # === STACK POINTER MANIPULATION ===
    
    def emit_add_rsp_imm8(self, value):
        """ADD RSP, imm8 - Adjust stack pointer"""
        self.emit_bytes(0x48, 0x83, 0xC4, value & 0xFF)
        print(f"DEBUG: ADD RSP, {value}")
    
    def emit_add_rsp_imm32(self, value):
        """ADD RSP, imm32 - Add 32-bit immediate to RSP"""
        if value <= 127:
            # Use 8-bit form if possible
            self.emit_add_rsp_imm8(value)
        else:
            # ADD RSP, imm32: REX.W + 81 /0 id
            self.emit_bytes(0x48, 0x81, 0xC4)
            self.emit_bytes(*struct.pack('<I', value))
            print(f"DEBUG: ADD RSP, {value}")
    
    def emit_sub_rsp_imm32(self, value):
        """SUB RSP, imm32 - Allocate stack space"""
        self.emit_bytes(0x48, 0x81, 0xEC)
        self.emit_bytes(*struct.pack('<I', value))
        print(f"DEBUG: SUB RSP, {value}")