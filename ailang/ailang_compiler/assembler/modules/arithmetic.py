# ailang_compiler/assembler/modules/arithmetic.py
"""Arithmetic operations - ADD, SUB, MUL, DIV, etc."""

class ArithmeticOperations:
    """Arithmetic instruction generation"""
    
    # === ADDITION ===
    
    def emit_add_rax_rbx(self):
        """ADD RAX, RBX"""
        self.emit_bytes(0x48, 0x01, 0xD8)
    
    def emit_add_rdi_rax(self):
        """ADD RDI, RAX - Add RAX to RDI"""
        self.emit_bytes(0x48, 0x01, 0xC7)
    
    # === SUBTRACTION ===
    
    def emit_sub_rax_rbx(self):
        """SUB RAX, RBX"""
        self.emit_bytes(0x48, 0x29, 0xD8)
        print("DEBUG: SUB RAX, RBX")
    
    # === MULTIPLICATION ===
    
    def emit_imul_rax_rbx(self):
        """IMUL RAX, RBX"""
        self.emit_bytes(0x48, 0x0F, 0xAF, 0xC3)
        
    # === DIVISION ===
    
    def emit_cqo(self):
        """
        CQO - Convert Quadword to Octaword
        Sign-extends RAX into RDX:RAX (needed for signed division)
        This instruction sign-extends the 64-bit value in RAX to 128-bit RDX:RAX
        """
        self.emit_bytes(0x48, 0x99)
        print("DEBUG: CQO - sign-extended RAX to RDX:RAX")

    def emit_cdq(self):
        """
        CDQ - Convert Doubleword to Quadword  
        Sign-extends EAX into EDX:EAX (32-bit version)
        """
        self.emit_bytes(0x99)
        print("DEBUG: CDQ - sign-extended EAX to EDX:EAX")

    def emit_idiv_rbx(self):
        """
        IDIV RBX - Signed integer division
        Divides RDX:RAX by RBX
        Result: quotient in RAX, remainder in RDX
        """
        self.emit_bytes(0x48, 0xF7, 0xFB)
        print("DEBUG: IDIV RBX - signed division")

    def emit_div_rbx(self):
        """
        IDIV RBX - Rewired to signed integer division
        """
        self.emit_bytes(0x48, 0xF7, 0xFB)  # <- changed F3 to FB
        print("DEBUG: IDIV RBX - (rewired, signed division)")


    def emit_xor_rdx_rdx(self):
        """
        XOR RDX, RDX - Clear RDX register
        Used before unsigned division to zero-extend
        """
        self.emit_bytes(0x48, 0x31, 0xD2)
        print("DEBUG: XOR RDX, RDX - cleared RDX")
        
    def emit_cmovl_rax_rbx(self):
        """CMOVL RAX, RBX - Move if less"""
        self.emit_bytes(0x48, 0x0F, 0x4C, 0xC3)

    def emit_cmovg_rax_rbx(self):
        """CMOVG RAX, RBX - Move if greater"""
        self.emit_bytes(0x48, 0x0F, 0x4F, 0xC3)

    def emit_cmovs_rax_rbx(self):
        """CMOVS RAX, RBX - Move if sign flag set"""
        self.emit_bytes(0x48, 0x0F, 0x48, 0xC3)
        
    def emit_cmovs_rax_rcx(self):
        """CMOVS RAX, RCX - Conditional move if sign flag"""
        self.emit_bytes(0x48, 0x0F, 0x48, 0xC1)
        print("DEBUG: CMOVS RAX, RCX")

    def emit_mov_rax_rcx(self):
        """MOV RAX, RCX"""
        self.emit_bytes(0x48, 0x89, 0xC8)

    def emit_mov_rcx_rax(self):
        """MOV RCX, RAX"""
        self.emit_bytes(0x48, 0x89, 0xC1)
        
    # Add to x64_assembler.py or arithmetic.py

# Quick fix - add alias for the missing method
def emit_xor_rdi_rdi(self):
    """XOR RDI, RDI - Zero RDI register (alias for edi version)"""
    # XOR EDI, EDI zeros the entire RDI due to zero extension
    self.emit_xor_edi_edi()

# Or if you want the actual 64-bit version:
def emit_xor_rdi_rdi_proper(self):
    """XOR RDI, RDI - Zero RDI register (64-bit)"""
    # REX.W prefix for 64-bit operation
    self.emit_bytes(0x48, 0x31, 0xFF)  # XOR RDI, RDI
    print("DEBUG: XOR RDI, RDI")

# The existing method (32-bit but zeros entire 64-bit register)
def emit_xor_edi_edi(self):
    """XOR EDI, EDI - Zero EDI/RDI register"""
    self.emit_bytes(0x31, 0xFF)  # XOR EDI, EDI
    print("DEBUG: XOR EDI, EDI (zeros RDI)")

# Add both RDI methods for completeness:
def emit_xor_rdi_rdi(self):
    self.emit_xor_edi_edi()  # Same effect, shorter encoding

def emit_xor_rsi_rsi(self):
    """XOR RSI, RSI - Zero RSI register"""
    self.emit_bytes(0x48, 0x31, 0xF6)  # XOR RSI, RSI
    
def emit_xor_rdx_rdx(self):
    """XOR RDX, RDX - Zero RDX register"""
    self.emit_bytes(0x48, 0x31, 0xD2)  # XOR RDX, RDX
    
def emit_xor_rcx_rcx(self):
    """XOR RCX, RCX - Zero RCX register"""
    self.emit_bytes(0x48, 0x31, 0xC9)  # XOR RCX, RCX