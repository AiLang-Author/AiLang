# Copyright (c) 2025 Sean Collins, 2 Paws Machine and Engineering. All rights reserved.
#
# Licensed under the Sean Collins Software License (SCSL). See the LICENSE file in the root directory of this project
# for the full terms and conditions, including restrictions on forking, corporate use, and permissions for private/teaching purposes.

# ailang_compiler/assembler/modules/bitwise.py
"""Bitwise operations - XOR, AND, OR, shifts"""

class BitwiseOperations:
    """Bitwise instruction generation"""
    
    # === XOR OPERATIONS ===
    
    def emit_xor_eax_eax(self):
        """XOR EAX, EAX - zeros RAX (upper 32 bits cleared automatically)"""
        self.emit_bytes(0x31, 0xC0)
    
    def emit_xor_edi_edi(self):
        """XOR EDI, EDI - zeros RDI"""
        self.emit_bytes(0x31, 0xFF)
    
    def emit_xor_esi_esi(self):
        """XOR ESI, ESI - zeros RSI (upper 32 bits cleared automatically)"""
        self.emit_bytes(0x31, 0xF6)
    
    def emit_xor_edx_edx(self):
        """XOR EDX, EDX - zeros RDX (upper 32 bits cleared automatically)"""
        self.emit_bytes(0x31, 0xD2)
    
    def emit_xor_rdx_rdx(self):
        """XOR RDX, RDX (zero rdx) - 64-bit version"""
        self.emit_bytes(0x48, 0x31, 0xD2)
        print("DEBUG: XOR RDX, RDX")
    
    def emit_xor_rbx_rbx(self):
        """Clear RBX by XORing it with itself"""
        self.emit_bytes(0x48, 0x31, 0xDB)
        print("DEBUG: Emitted XOR RBX, RBX")
    
    def emit_xor_r9_r9(self):
        """XOR R9, R9"""
        self.emit_bytes(0x4D, 0x31, 0xC9)