# ailang_compiler/assembler/modules/syscalls.py
"""System call and interrupt operations"""

class SystemCallOperations:
    """System call instruction generation"""
    
    def emit_syscall(self):
        """SYSCALL instruction"""
        self.emit_bytes(0x0F, 0x05)
    
    def emit_sys_exit(self, status: int = 0):
        """exit(status) -> RAX=60, RDI=status, SYSCALL"""
        self.emit_mov_rax_imm64(60)      # __NR_exit
        self.emit_mov_rdi_imm64(status)  # exit status
        self.emit_syscall()
    
    # === INTERRUPT OPERATIONS ===
    
    def emit_cli(self):
        """CLI - Clear interrupt flag (disable interrupts)"""
        self.emit_bytes(0xFA)
        print("DEBUG: CLI")
    
    def emit_sti(self):
        """STI - Set interrupt flag (enable interrupts)"""
        self.emit_bytes(0xFB)
        print("DEBUG: STI")
    
    def emit_hlt(self):
        """HLT - Halt processor until interrupt"""
        self.emit_bytes(0xF4)
        print("DEBUG: HLT")
    
    def emit_int(self, interrupt_number: int):
        """INT n - Software interrupt"""
        self.emit_bytes(0xCD, interrupt_number & 0xFF)
        print(f"DEBUG: INT {hex(interrupt_number)}")
    
    def emit_iret(self):
        """IRETQ - Return from interrupt (64-bit)"""
        self.emit_bytes(0x48, 0xCF)
        print("DEBUG: IRETQ")
        
        
   
    def emit_syscall_read(self):
        """Emit sys_read syscall (rax=0)"""
        self.emit_mov_rax_imm64(0)
        self.emit_syscall()

    def emit_test_rax_rax(self):
        """TEST RAX, RAX"""
        self.emit_bytes(0x48, 0x85, 0xC0)

    def emit_jle(self, label):
        """Jump if less or equal (JLE)"""
        self.emit_jump_to_label(label, "JLE")

    def emit_mov_al_byte_ptr_rsi(self):
        """MOV AL, BYTE PTR [RSI]"""
        self.emit_bytes(0x8A, 0x06)

    def emit_cmp_al_imm(self, imm8):
        """CMP AL, imm8"""
        self.emit_bytes(0x3C, imm8 & 0xFF)

    def emit_mov_byte_ptr_rsi_imm(self, imm8):
        """MOV BYTE PTR [RSI], imm8"""
        self.emit_bytes(0xC6, 0x06, imm8 & 0xFF)

    def emit_inc_rsi(self):
        """INC RSI"""
        self.emit_bytes(0x48, 0xFF, 0xC6)

    def emit_dec_rcx(self):
        """DEC RCX"""
        self.emit_bytes(0x48, 0xFF, 0xC9)