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