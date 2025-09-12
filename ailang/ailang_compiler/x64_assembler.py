#!/usr/bin/env python3

"""
x86-64 Machine Code Generator - ENHANCED FOR SYSTEMS PROGRAMMING
Generates raw machine code for AILANG compiler with low-level memory operations
Adds pointer operations, hardware access, and atomic operations
"""

import struct
from typing import Dict, Union
from ailang_compiler.jump_manager import JumpManager
from ailang_compiler.modules.relocation_manager import RelocationManager

class X64Assembler:
    """Raw x86-64 machine code generation with ENHANCED low-level operations"""
    
    def set_base_addresses(self, code_addr, data_addr):
        """Set base addresses - called by ELF generator after layout calculation"""
        self.code_base_address = code_addr
        self.data_base_address = data_addr
        print(f"Dynamic addresses set - Code: 0x{code_addr:08x}, Data: 0x{data_addr:08x}")
        
    def add_data_relocation(self, code_offset, data_offset):
        """Mark a location that needs data address relocation"""
        self.relocations.append({
            'type': 'data',
            'code_offset': code_offset,
            'data_offset': data_offset
        })
        
    def apply_relocations(self):
        """Apply all address relocations after layout is known"""
        if self.data_base_address is None:
            raise ValueError("Cannot apply relocations - addresses not set!")
            
        import struct
        for reloc in self.relocations:
            if reloc['type'] == 'data':
                # Calculate actual address
                actual_addr = self.data_base_address + reloc['data_offset']
                
                # Patch it in the code (assuming MOV instruction with 64-bit immediate)
                # The immediate starts 2 bytes after the MOV opcode
                offset = reloc['code_offset']
                addr_bytes = struct.pack('<Q', actual_addr)
                
                # Patch the code
                for i in range(8):
                    self.code[offset + i] = addr_bytes[i]
                    
        print(f"Applied {len(self.relocations)} relocations")
        self.relocations = []  # Clear after applying

    class X64Assembler:
        """Raw x86-64 machine code generation with ENHANCED low-level operations"""
    
    def __init__(self, elf_generator=None):
        self.code = bytearray()  # Changed from list to bytearray
        self.data = []
        self.data_offset = 0
        self.strings = {}
        self.elf = elf_generator
        self.jump_manager = JumpManager()
        self.relocation_manager = RelocationManager()
        self.code_base_address = None
        self.data_base_address = None  # NEW: Centralized jump management
        # Dynamic addressing support
        self.relocations = []  # Track address fixups
        self.data_base_address = None  # Will be set by ELF generator
        self.code_base_address = None  # Will be set by ELF generator
    def emit_bytes(self, *bytes_to_emit):
        """Emit bytes to the code buffer"""
        for byte in bytes_to_emit:
            if isinstance(byte, (list, bytearray)):
                self.code.extend(byte)
            else:
                self.code.append(byte)
        
        # Debug output
        if bytes_to_emit:
            hex_str = [f'0x{b:x}' if isinstance(b, int) else str(b) 
                      for b in bytes_to_emit]
            print(f"DEBUG: Emitted bytes: {hex_str}")

    def emit_syscall(self):
        """SYSCALL instruction"""
        self.emit_bytes(0x0F, 0x05)

    
    
    def emit_sys_exit(self, status: int = 0):
        """exit(status) -> RAX=60, RDI=status, SYSCALL"""
        self.emit_mov_rax_imm64(60)      # __NR_exit
        self.emit_mov_rdi_imm64(status)  # exit status
        self.emit_syscall()


    def emit_ret(self):
        """RET instruction"""
        self.emit_bytes(0xC3)
    
    def emit_nop(self):
        """NOP instruction"""
        self.emit_bytes(0x90)
    
    
    def emit_call_to_label(self, label):
        """Emit CALL to a label"""
        current_pos = len(self.code)
        
        # Emit CALL opcode
        self.emit_bytes(0xE8)
        
        # Calculate and emit offset if label is already known
        if label in self.labels:
            target_pos = self.labels[label]
            offset = target_pos - (current_pos + 5)  # CALL uses next instruction address
            self.emit_bytes(*struct.pack('<i', offset))
        else:
            # Emit placeholder - will be fixed in resolve phase
            self.emit_bytes(0x00, 0x00, 0x00, 0x00)
            # Add to pending jumps for resolution
            if not hasattr(self, 'pending_calls'):
                self.pending_calls = []
            self.pending_calls.append((current_pos, label))
        
        print(f"DEBUG: Emitted CALL to label {label}")

    def get_position(self):
        """Get current position in code buffer"""
        return len(self.code)       
    
    
    
    # === REGISTER MOVEMENT INSTRUCTIONS (preserved) ===
    
    def emit_mov_rax_imm64(self, value: int):
        """MOV RAX, imm64 - ENHANCED FOR LARGE NUMBERS"""
        self.emit_bytes(0x48, 0xB8)
        
        # ENHANCED ARITHMETIC: Handle large numbers
        if value > 18446744073709551615:  # 2^64 - 1
            # For huge numbers, use modulo to fit in 64-bit and mark as truncated
            truncated_value = value % (2**64)
            print(f"DEBUG: LARGE NUMBER TRUNCATED: {value} -> {truncated_value}")
            self.emit_bytes(*struct.pack('<Q', truncated_value))
        elif value < 0 and value < -9223372036854775808:  # 2^63
            # Handle very negative numbers
            truncated_value = value % (2**64)
            print(f"DEBUG: LARGE NEGATIVE TRUNCATED: {value} -> {truncated_value}")
            self.emit_bytes(*struct.pack('<Q', truncated_value))
        else:
            # Normal case - fits in 64-bit
            try:
                self.emit_bytes(*struct.pack('<Q', value))
            except struct.error as e:
                # Fallback for edge cases
                print(f"DEBUG: Struct pack failed for {value}, using modulo fallback")
                safe_value = value % (2**64)
                self.emit_bytes(*struct.pack('<Q', safe_value))
                
        print(f"DEBUG: MOV RAX, {value}")
    
    def emit_lea_rax_rbx(self, offset):
        """Emit LEA RAX, [RBX + offset]"""
        self.emit_bytes(0x48, 0x8D, 0x43, offset)
        print(f"DEBUG: Emitted LEA RAX, [RBX + {offset}]")
     
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
    
    def emit_inc_rdi(self):
        """INC RDI - Increment RDI by 1"""
        self.emit_bytes(0x48, 0xFF, 0xC7)
        print("DEBUG: INC RDI")
        
    def emit_mov_rsi_mem_offset(self, base_reg: str, offset: int):
        """MOV RSI, [base_reg + offset] - Load with offset"""
        if base_reg != 'RSP':
            raise ValueError(f"emit_mov_rsi_mem_offset only supports RSP for now")
        
        # MOV RSI, [RSP + offset]: 48 8B 74 24 offset
        self.emit_bytes(0x48, 0x8B, 0x74, 0x24, offset)
        print(f"DEBUG: MOV RSI, [RSP+{offset}]")         
        
    def emit_mov_rdi_mem_offset(self, base_reg: str, offset: int):
        """MOV RDI, [base_reg + offset] - Load with offset"""
        if base_reg != 'RSP':
            raise ValueError(f"emit_mov_rdi_mem_offset only supports RSP for now")
        
        # MOV RDI, [RSP + offset]: 48 8B 7C 24 offset
        self.emit_bytes(0x48, 0x8B, 0x7C, 0x24, offset)
        print(f"DEBUG: MOV RDI, [RSP+{offset}]")        
        
    def emit_mov_byte_mem_offset_imm(self, base_reg: str, index_reg, value: int):
        """MOV BYTE [base_reg + index_reg], imm8 - Store byte immediate to memory"""
        if base_reg != 'RAX':
            raise ValueError(f"emit_mov_byte_mem_offset_imm only supports RAX for now")
        
        # MOV BYTE [RAX + RBX], 0: C6 04 18 00
        self.emit_bytes(0xC6, 0x04, 0x18, value)
        print(f"DEBUG: MOV BYTE [RAX+RBX], {value}")        
        
    def emit_mov_rdi_rbx(self):
        """MOV RDI, RBX"""
        self.emit_bytes(0x48, 0x89, 0xDF)
        print("DEBUG: MOV RDI, RBX")
        
    def emit_xor_esi_esi(self):
        """XOR ESI, ESI - zeros RSI (upper 32 bits cleared automatically)"""
        self.emit_bytes(0x31, 0xF6)

    def emit_xor_edx_edx(self):
        """XOR EDX, EDX - zeros RDX (upper 32 bits cleared automatically)"""
        self.emit_bytes(0x31, 0xD2)

    def emit_xor_eax_eax(self):
        """XOR EAX, EAX - zeros RAX (upper 32 bits cleared automatically)"""
        self.emit_bytes(0x31, 0xC0)        
    
    def emit_test_rcx_rcx(self):
        """TEST RCX, RCX"""
        self.emit_bytes(0x48, 0x85, 0xC9)
        
    def emit_mov_rax_rsi(self):
        """Emit MOV RAX, RSI"""
        self.emit_bytes(0x48, 0x89, 0xF0)
        print("DEBUG: Emitted MOV RAX, RSI")
        
    def emit_mov_rdx_rbx(self):
        """Move RBX to RDX"""
        self.emit_bytes(0x48, 0x89, 0xDA)  # MOV RDX, RBX
        print("DEBUG: Emitted MOV RDX, RBX")
        
    def emit_mov_rbx_rsi(self):
        """Move RSI to RBX"""
        self.emit_bytes(0x48, 0x89, 0xF3)  # MOV RBX, RSI
        print("DEBUG: Emitted MOV RBX, RSI")
    
    def emit_test_rax_imm8(self, value: int):
        """Test the least significant byte of RAX with an 8-bit immediate"""
        if not 0 <= value <= 0xFF:
            raise ValueError(f"Immediate value {value} out of 8-bit range (0-255)")
        self.emit_bytes(0xA8)  # TEST AL, imm8
        self.emit_bytes(*struct.pack('<B', value))  # 8-bit immediate
        print(f"DEBUG: Emitted TEST AL, {value:#x}")
    
    def emit_cmp_rax_imm64(self, value: int):
        """Compare RAX with a 64-bit immediate using a register intermediate"""
        # Convert negative value to unsigned 64-bit
        value = value & 0xFFFFFFFFFFFFFFFF
        self.emit_mov_rbx_imm64(value)  # Move immediate to RBX
        self.emit_bytes(0x48, 0x39, 0xD8)  # CMP RAX, RBX
        print(f"DEBUG: Emitted CMP RAX, {value:#x}")    
    
    def emit_mov_rbx_imm64(self, value: int):
        """MOV RBX, imm64"""
        self.emit_bytes(0x48, 0xBB)
        if value < 0:
            value = value & 0xFFFFFFFFFFFFFFFF
        self.emit_bytes(*struct.pack('<Q', value))
        print(f"DEBUG: MOV RBX, {value}")

    def emit_mov_rdi_imm64(self, value: int):
        """MOV RDI, imm64"""
        self.emit_bytes(0x48, 0xBF)
        if value < 0:
            value = value & 0xFFFFFFFFFFFFFFFF
        self.emit_bytes(*struct.pack('<Q', value))
        print(f"DEBUG: MOV RDI, {value}")

    def emit_mov_rsi_imm64(self, value: int):
        """MOV RSI, imm64"""
        self.emit_bytes(0x48, 0xBE)
        if value < 0:
            value = value & 0xFFFFFFFFFFFFFFFF
        self.emit_bytes(*struct.pack('<Q', value))
        print(f"DEBUG: MOV RSI, {value}")

    def emit_mov_rdx_imm64(self, value: int):
        """MOV RDX, imm64"""
        self.emit_bytes(0x48, 0xBA)
        if value < 0:
            value = value & 0xFFFFFFFFFFFFFFFF
        self.emit_bytes(*struct.pack('<Q', value))
        print(f"DEBUG: MOV RDX, {value}")

    def emit_mov_rcx_imm64(self, value: int):
        """MOV RCX, imm64"""
        self.emit_bytes(0x48, 0xB9)
        if value < 0:
            value = value & 0xFFFFFFFFFFFFFFFF
        self.emit_bytes(*struct.pack('<Q', value))
        print(f"DEBUG: MOV RCX, {value}")

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
        
        
    def emit_xor_edi_edi(self):
        """XOR EDI, EDI - zeros RDI"""
        self.emit_bytes(0x31, 0xFF)
        
    def emit_mov_rcx_rax(self):
        """MOV RCX, RAX"""
        self.emit_bytes(0x48, 0x89, 0xC1)
        print("DEBUG: MOV RCX, RAX")

    def emit_mov_r10_imm64(self, value):
        """MOV R10, imm64"""
        self.emit_bytes(0x49, 0xBA)
        self.emit_bytes(*struct.pack('<Q', value))

    def emit_mov_r8_imm64(self, value):
        """MOV R8, imm64"""
        self.emit_bytes(0x49, 0xB8)
        # Handle negative values by converting to unsigned representation
        if value < 0:
            value = value & 0xFFFFFFFFFFFFFFFF
        self.emit_bytes(*struct.pack('<Q', value))

    def emit_xor_r9_r9(self):
        """XOR R9, R9"""
        self.emit_bytes(0x4D, 0x31, 0xC9)
    
        
    # === REGISTER-TO-REGISTER MOVES (preserved) ===
    
    def emit_mov_rax_rbx(self):
        """MOV RAX, RBX"""
        self.emit_bytes(0x48, 0x89, 0xD8)
    
    def emit_mov_rbx_rax(self):
        """MOV RBX, RAX"""
        self.emit_bytes(0x48, 0x89, 0xC3)
        print("DEBUG: MOV RBX, RAX")
    
    def emit_mov_rdx_rax(self):
        """MOV RDX, RAX"""
        self.emit_bytes(0x48, 0x89, 0xC2)
    
    def emit_mov_rdi_rax(self):
        """MOV RDI, RAX"""
        self.emit_bytes(0x48, 0x89, 0xC7)
        print("DEBUG: MOV RDI, RAX")
    
    def emit_mov_rax_rdi(self):
        """MOV RAX, RDI"""
        self.emit_bytes(0x48, 0x89, 0xF8)
        print("DEBUG: MOV RAX, RDI")
        
    def emit_mov_rdi_rsp(self):
        """MOV RDI, RSP"""
        self.emit_bytes(0x48, 0x89, 0xE7)
        print("DEBUG: MOV RDI, RSP")
    
    def emit_mov_rsi_rsp(self):
        """MOV RSI, RSP"""
        self.emit_bytes(0x48, 0x89, 0xE6)
        print("DEBUG: MOV RSI, RSP")
    
    def emit_mov_rax_rsp(self):
        """MOV RAX, RSP"""
        self.emit_bytes(0x48, 0x89, 0xE0)
        print("DEBUG: MOV RAX, RSP")
    
    def emit_mov_rdi_from_rbx(self):
        """MOV RDI, RBX"""
        self.emit_bytes(0x48, 0x89, 0xDF)
        print("DEBUG: MOV RDI, RBX")
        
    def emit_mov_rcx_rax(self):
        """MOV RCX, RAX"""
        self.emit_bytes(0x48, 0x89, 0xC1)
        print("DEBUG: MOV RCX, RAX")

    def emit_mov_rsi_rax(self):
        """MOV RSI, RAX"""
        self.emit_bytes(0x48, 0x89, 0xC6)
        print("DEBUG: MOV RSI, RAX")

    def emit_mov_rsi_rcx(self):
        """MOV RSI, RCX"""
        self.emit_bytes(0x48, 0x89, 0xCE)
        print("DEBUG: MOV RSI, RCX")

    def emit_mov_rsi_from_rbx(self):
        """MOV RSI, RBX"""
        self.emit_bytes(0x48, 0x89, 0xDE)
        print("DEBUG: MOV RSI, RBX")   
        
    def emit_mov_rax_rcx(self):
        """MOV RAX, RCX"""
        self.emit_bytes(0x48, 0x89, 0xC8)
        print("DEBUG: MOV RAX, RCX")
    
    def emit_test_rdi_rdi(self):
        """TEST RDI, RDI - Set flags based on RDI"""
        self.emit_bytes(0x48, 0x85, 0xFF)
        print("DEBUG: TEST RDI, RDI")
        
    def emit_test_rsi_rsi(self):
        """TEST RSI, RSI - Set flags based on RSI"""
        self.emit_bytes(0x48, 0x85, 0xF6)
        print("DEBUG: TEST RSI, RSI")
        
    def emit_mov_rsi_rdi(self):
        """MOV RSI, RDI"""
        self.emit_bytes(0x48, 0x89, 0xFE)
        print("DEBUG: MOV RSI, RDI")
        
    def emit_test_rax_rax(self):
        """TEST RAX, RAX - Set flags based on RAX"""
        self.emit_bytes(0x48, 0x85, 0xC0)
        print("DEBUG: TEST RAX, RAX")    

    def emit_test_r10_r10(self):
        """TEST R10, R10 - Set flags based on R10 (length/size check)"""
        # Extended regs need REX.R and REX.B. REX = 0x4D (W=1,R=1,B=1)
        # ModRM: 11 | reg=R10(2) | r/m=R10(2) -> 0xD2
        self.emit_bytes(0x4D, 0x85, 0xD2)
        print("DEBUG: TEST R10, R10")

    def emit_mov_rdx_r10(self):
        """MOV RDX, R10"""
        # MOV r/m64, r64: 0x89 /r
        # dst=RDX (r/m=2), src=R10 (reg=2 with REX.R=1) -> REX=0x4C, ModRM=0xD2
        self.emit_bytes(0x4C, 0x89, 0xD2)
        print("DEBUG: MOV RDX, R10")

     
    def emit_write_guarded_rdi_rsi_size_in_rdx(self):
        """Guarded write(fd=RDI, buf=RSI, size=RDX). Skip if buf==NULL or size==0."""
        skip = self.create_label(); cont = self.create_label()
        self.emit_bytes(0x48, 0x85, 0xF6); self.emit_jump_to_label(skip, "JZ")  # TEST RSI,RSI
        self.emit_bytes(0x48, 0x85, 0xD2); self.emit_jump_to_label(skip, "JZ")  # TEST RDX,RDX
        self.emit_mov_rax_imm64(1); self.emit_syscall(); self.emit_jump_to_label(cont, "JMP")
        self.mark_label(skip); self.emit_mov_rax_imm64(0); self.mark_label(cont)
  
        
    
    # === NEW: ENHANCED MEMORY ACCESS OPERATIONS ===
    
    def emit_mov_rax_mem(self, address: int):
        """MOV RAX, [address] - Load from absolute memory address"""
        # Use RIP-relative addressing when possible for better performance
        if 0 <= address <= 0x7FFFFFFF:
            # MOV RAX, [RIP + offset] - more efficient for known addresses
            self.emit_bytes(0x48, 0x8B, 0x05)
            self.emit_bytes(*struct.pack('<I', address))
            print(f"DEBUG: MOV RAX, [RIP + {address}]")
        else:
            # MOV RAX, address; MOV RAX, [RAX] - for arbitrary addresses
            self.emit_mov_rax_imm64(address)
            self.emit_bytes(0x48, 0x8B, 0x00)  # MOV RAX, [RAX]
            print(f"DEBUG: MOV RAX, [{hex(address)}]")
    
    def emit_mov_mem_rax(self, address: int):
        """MOV [address], RAX - Store to absolute memory address"""
        if 0 <= address <= 0x7FFFFFFF:
            # MOV [RIP + offset], RAX
            self.emit_bytes(0x48, 0x89, 0x05)
            self.emit_bytes(*struct.pack('<I', address))
            print(f"DEBUG: MOV [RIP + {address}], RAX")
        else:
            # MOV RBX, address; MOV [RBX], RAX
            self.emit_push_rax()  # Save RAX
            self.emit_mov_rax_imm64(address)
            self.emit_mov_rbx_rax()  # Address to RBX
            self.emit_pop_rax()   # Restore RAX
            self.emit_bytes(0x48, 0x89, 0x03)  # MOV [RBX], RAX
            print(f"DEBUG: MOV [{hex(address)}], RAX")
    
    def emit_mov_rax_mem_offset(self, base_reg: str, offset: int):
        """MOV RAX, [base_reg + offset] - Load with offset"""
        reg_codes = {
            'RAX': 0x00, 'RBX': 0x03, 'RCX': 0x01, 'RDX': 0x02,
            'RSI': 0x06, 'RDI': 0x07, 'RSP': 0x04, 'RBP': 0x05,
            'R8': 0x00, 'R9': 0x01, 'R10': 0x02, 'R11': 0x03,
            'R12': 0x04, 'R13': 0x05, 'R14': 0x06, 'R15': 0x07
        }
        
        if base_reg not in reg_codes:
            raise ValueError(f"Invalid register: {base_reg}")
        
        reg_code = reg_codes[base_reg]
        rex_prefix = 0x48
        
        # Handle R8-R15 registers
        if base_reg.startswith('R') and len(base_reg) > 2:
            rex_prefix |= 0x01  # REX.B bit for extended base register
        
        if -128 <= offset <= 127:
            # 8-bit signed displacement
            self.emit_bytes(rex_prefix, 0x8B, 0x40 | reg_code, offset & 0xFF)
        else:
            # 32-bit signed displacement
            self.emit_bytes(rex_prefix, 0x8B, 0x80 | reg_code)
            self.emit_bytes(*struct.pack('<i', offset))
        
        print(f"DEBUG: MOV RAX, [{base_reg} + {offset}]")
    
    def emit_mov_mem_offset_rax(self, base_reg: str, offset: int):
        """MOV [base_reg + offset], RAX - Store with offset"""
        reg_codes = {
            'RAX': 0x00, 'RBX': 0x03, 'RCX': 0x01, 'RDX': 0x02,
            'RSI': 0x06, 'RDI': 0x07, 'RSP': 0x04, 'RBP': 0x05,
            'R8': 0x00, 'R9': 0x01, 'R10': 0x02, 'R11': 0x03,
            'R12': 0x04, 'R13': 0x05, 'R14': 0x06, 'R15': 0x07
        }
        
        if base_reg not in reg_codes:
            raise ValueError(f"Invalid register: {base_reg}")
        
        reg_code = reg_codes[base_reg]
        rex_prefix = 0x48
        
        # Handle R8-R15 registers
        if base_reg.startswith('R') and len(base_reg) > 2:
            rex_prefix |= 0x01  # REX.B bit for extended base register
        
        if -128 <= offset <= 127:
            # 8-bit signed displacement
            self.emit_bytes(rex_prefix, 0x89, 0x40 | reg_code, offset & 0xFF)
        else:
            # 32-bit signed displacement
            self.emit_bytes(rex_prefix, 0x89, 0x80 | reg_code)
            self.emit_bytes(*struct.pack('<i', offset))
        
        print(f"DEBUG: MOV [{base_reg} + {offset}], RAX")
    
    # === New : Pool Operations ===
      
    def emit_mov_qword_ptr_rax_rcx(self):
        """MOV [RAX], RCX - Store RCX at address in RAX"""
        self.emit_bytes(0x48, 0x89, 0x08)

    def emit_mov_rcx_qword_ptr_rax_minus_8(self):
        """MOV RCX, [RAX-8] - Load from 8 bytes before RAX"""
        self.emit_bytes(0x48, 0x8B, 0x48, 0xF8)

    def emit_lea_rax_rbx_plus_8(self):
        """LEA RAX, [RBX+8] - Load effective address"""
        self.emit_bytes(0x48, 0x8D, 0x43, 0x08)

    def emit_lea_rax_rdi_plus_8(self):
        """LEA RAX, [RDI+8] - Load effective address"""
        self.emit_bytes(0x48, 0x8D, 0x47, 0x08)
        
        
    # === Additional Pool Support Methods ===
    
    def emit_mov_rsi_from_stack_offset(self, offset):
        """MOV RSI, [RSP+offset] - Load from stack at offset"""
        if offset <= 127:
            self.emit_bytes(0x48, 0x8B, 0x74, 0x24, offset)
        else:
            self.emit_bytes(0x48, 0x8B, 0xB4, 0x24)
            self.emit_bytes(*struct.pack('<I', offset))
        print(f"DEBUG: MOV RSI, [RSP+{offset}]")

    def emit_add_rsp_imm8(self, value):
        """ADD RSP, imm8 - Adjust stack pointer"""
        self.emit_bytes(0x48, 0x83, 0xC4, value & 0xFF)
        print(f"DEBUG: ADD RSP, {value}")

        
    def emit_xor_rdx_rdx(self):
        """Clear RDX by XORing it with itself"""
        self.emit_bytes(0x48, 0x31, 0xD2)  # XOR RDX, RDX
        print("DEBUG: Emitted XOR RDX, RDX")

    def emit_xor_rbx_rbx(self):
        """Clear RBX by XORing it with itself"""
        self.emit_bytes(0x48, 0x31, 0xDB)  # XOR RBX, RBX
        print("DEBUG: Emitted XOR RBX, RBX")
    
    
    
    
    # === NEW: POINTER OPERATIONS ===
    
    def emit_lea_rax(self, base_reg: str, offset: int = 0):
        """LEA RAX, [base_reg + offset] - Load Effective Address"""
        reg_codes = {
            'RAX': 0x00, 'RBX': 0x03, 'RCX': 0x01, 'RDX': 0x02,
            'RSI': 0x06, 'RDI': 0x07, 'RSP': 0x04, 'RBP': 0x05,
            'R8': 0x00, 'R9': 0x01, 'R10': 0x02, 'R11': 0x03,
            'R12': 0x04, 'R13': 0x05, 'R14': 0x06, 'R15': 0x07
        }
        
        if base_reg not in reg_codes:
            raise ValueError(f"Invalid register: {base_reg}")
        
        reg_code = reg_codes[base_reg]
        rex_prefix = 0x48  # REX.W for 64-bit
        
        # Handle R8-R15 registers
        if base_reg.startswith('R') and len(base_reg) > 1:
            rex_prefix |= 0x01  # REX.B bit for extended base register
        
        # RBP always needs displacement
        if base_reg == 'RBP':
            if -128 <= offset <= 127:
                # 8-bit displacement
                self.emit_bytes(rex_prefix, 0x8D, 0x45)
                if offset < 0:
                    self.emit_bytes((offset + 256) & 0xFF)  # Two's complement
                else:
                    self.emit_bytes(offset & 0xFF)
            else:
                # 32-bit displacement
                self.emit_bytes(rex_prefix, 0x8D, 0x85)
                self.emit_bytes(*struct.pack('<i', offset))
        elif base_reg == 'R13':  # R13 also needs special handling like RBP
            if -128 <= offset <= 127:
                self.emit_bytes(rex_prefix, 0x8D, 0x45)
                if offset < 0:
                    self.emit_bytes((offset + 256) & 0xFF)
                else:
                    self.emit_bytes(offset & 0xFF)
            else:
                self.emit_bytes(rex_prefix, 0x8D, 0x85)
                self.emit_bytes(*struct.pack('<i', offset))
        elif offset == 0 and base_reg not in ['RSP', 'R12']:
            # No displacement (except RSP/R12 need SIB byte)
            self.emit_bytes(rex_prefix, 0x8D, 0x00 | reg_code)
        elif -128 <= offset <= 127:
            # 8-bit displacement
            self.emit_bytes(rex_prefix, 0x8D, 0x40 | reg_code)
            if offset < 0:
                self.emit_bytes((offset + 256) & 0xFF)
            else:
                self.emit_bytes(offset & 0xFF)
        else:
            # 32-bit displacement
            self.emit_bytes(rex_prefix, 0x8D, 0x80 | reg_code)
            self.emit_bytes(*struct.pack('<i', offset))
        
        print(f"DEBUG: LEA RAX, [{base_reg} + {offset}]")
    
    def emit_mov_rbp_rsp(self):
        """MOV RBP, RSP - Set up stack frame"""
        self.emit_bytes(0x48, 0x89, 0xE5)
        print("DEBUG: MOV RBP, RSP")

    def emit_mov_rsp_rbp(self):
        """MOV RSP, RBP - Restore stack pointer"""
        self.emit_bytes(0x48, 0x89, 0xEC)
        print("DEBUG: MOV RSP, RBP") 
        
    def emit_mov_rdi_rsi(self):
        """MOV RDI, RSI - Copy RSI to RDI"""
        self.emit_bytes(0x48, 0x89, 0xF7)
        
    def emit_add_rdi_rax(self):
        """ADD RDI, RAX - Add RAX to RDI"""
        self.emit_bytes(0x48, 0x01, 0xC7)
    
    def emit_mov_byte_ptr_rdi_zero(self):
        """MOV BYTE PTR [RDI], 0 - Store zero byte at [RDI]"""
        self.emit_bytes(0xC6, 0x07, 0x00)
        
    def emit_dec_rdi(self):
        """DEC RDI - Decrement RDI"""
        self.emit_bytes(0x48, 0xFF, 0xCF)
        
    def emit_mov_rsi_rbx(self):
        """MOV RSI, RBX"""
        self.emit_bytes(0x48, 0x89, 0xDE)
        print("DEBUG: MOV RSI, RBX")
    
    
    def emit_dereference_byte(self):
        """Dereference RAX as byte pointer - MOVZX RAX, BYTE PTR [RAX]"""
        self.emit_bytes(0x48, 0x0F, 0xB6, 0x00)
        print("DEBUG: MOVZX RAX, BYTE PTR [RAX]")
    
    def emit_dereference_word(self):
        """Dereference RAX as word pointer - MOVZX RAX, WORD PTR [RAX]"""
        self.emit_bytes(0x48, 0x0F, 0xB7, 0x00)
        print("DEBUG: MOVZX RAX, WORD PTR [RAX]")
    
    def emit_dereference_dword(self):
        """Dereference RAX as dword pointer - MOV EAX, DWORD PTR [RAX]"""
        self.emit_bytes(0x8B, 0x00)
        print("DEBUG: MOV EAX, DWORD PTR [RAX]")
    
    def emit_dereference_qword(self):
        """Dereference RAX as qword pointer - MOV RAX, QWORD PTR [RAX]"""
        self.emit_bytes(0x48, 0x8B, 0x00)
        print("DEBUG: MOV RAX, QWORD PTR [RAX]")
    
    def emit_store_to_pointer_byte(self, value_reg: str = "RBX"):
        """Store byte from value_reg to address in RAX"""
        reg_codes = {'RAX': 0x00, 'RBX': 0x03, 'RCX': 0x01, 'RDX': 0x02}
        if value_reg not in reg_codes:
            raise ValueError(f"Invalid register: {value_reg}")
        
        reg_code = reg_codes[value_reg]
        self.emit_bytes(0x88, 0x00 | (reg_code << 3))  # MOV [RAX], value_reg_byte
        print(f"DEBUG: MOV [RAX], {value_reg.lower()}")
    
    def emit_store_to_pointer_qword(self, value_reg: str = "RBX"):
        """Store qword from value_reg to address in RAX"""
        reg_codes = {'RAX': 0x00, 'RBX': 0x03, 'RCX': 0x01, 'RDX': 0x02}
        if value_reg not in reg_codes:
            raise ValueError(f"Invalid register: {value_reg}")
        
        reg_code = reg_codes[value_reg]
        self.emit_bytes(0x48, 0x89, 0x00 | (reg_code << 3))  # MOV [RAX], value_reg
        print(f"DEBUG: MOV [RAX], {value_reg}")
    
    # === NEW: ATOMIC OPERATIONS ===
    
    def emit_atomic_compare_exchange(self, memory_address: int):
        """LOCK CMPXCHG [address], RBX - Atomic compare and exchange"""
        # Load address into RDX
        self.emit_push_rax()
        self.emit_mov_rax_imm64(memory_address)
        self.emit_mov_rdx_rax()
        self.emit_pop_rax()
        
        # LOCK CMPXCHG [RDX], RBX
        self.emit_bytes(0xF0, 0x48, 0x0F, 0xB1, 0x1A)
        print(f"DEBUG: LOCK CMPXCHG [{hex(memory_address)}], RBX")
    
    def emit_atomic_add(self, memory_address: int, value: int):
        """LOCK ADD [address], value - Atomic addition"""
        # Load address into RDX
        self.emit_push_rax()
        self.emit_mov_rax_imm64(memory_address)
        self.emit_mov_rdx_rax()
        self.emit_pop_rax()
        
        if -128 <= value <= 127:
            # LOCK ADD QWORD PTR [RDX], imm8
            self.emit_bytes(0xF0, 0x48, 0x83, 0x02, value & 0xFF)
        else:
            # LOCK ADD QWORD PTR [RDX], imm32
            self.emit_bytes(0xF0, 0x48, 0x81, 0x02)
            self.emit_bytes(*struct.pack('<i', value))
        
        print(f"DEBUG: LOCK ADD [{hex(memory_address)}], {value}")
    
    def emit_memory_fence(self):
        """MFENCE - Memory fence for ordering"""
        self.emit_bytes(0x0F, 0xAE, 0xF0)
        print("DEBUG: MFENCE")
    
    def emit_store_fence(self):
        """SFENCE - Store fence"""
        self.emit_bytes(0x0F, 0xAE, 0xF8)
        print("DEBUG: SFENCE")
    
    def emit_load_fence(self):
        """LFENCE - Load fence"""
        self.emit_bytes(0x0F, 0xAE, 0xE8)
        print("DEBUG: LFENCE")
    
    # === NEW: HARDWARE REGISTER ACCESS ===
    
    def emit_read_cr(self, cr_number: int):
        """MOV RAX, CRn - Read control register"""
        if cr_number == 0:
            self.emit_bytes(0x0F, 0x20, 0xC0)  # MOV RAX, CR0
        elif cr_number == 2:
            self.emit_bytes(0x0F, 0x20, 0xD0)  # MOV RAX, CR2
        elif cr_number == 3:
            self.emit_bytes(0x0F, 0x20, 0xD8)  # MOV RAX, CR3
        elif cr_number == 4:
            self.emit_bytes(0x0F, 0x20, 0xE0)  # MOV RAX, CR4
        elif cr_number == 8:
            self.emit_bytes(0x0F, 0x20, 0xC0)  # MOV RAX, CR8 (actually TPR)
        else:
            raise ValueError(f"Invalid control register: CR{cr_number}")
        
        print(f"DEBUG: MOV RAX, CR{cr_number}")
    
    def emit_write_cr(self, cr_number: int):
        """MOV CRn, RAX - Write control register"""
        if cr_number == 0:
            self.emit_bytes(0x0F, 0x22, 0xC0)  # MOV CR0, RAX
        elif cr_number == 2:
            self.emit_bytes(0x0F, 0x22, 0xD0)  # MOV CR2, RAX
        elif cr_number == 3:
            self.emit_bytes(0x0F, 0x22, 0xD8)  # MOV CR3, RAX
        elif cr_number == 4:
            self.emit_bytes(0x0F, 0x22, 0xE0)  # MOV CR4, RAX
        elif cr_number == 8:
            self.emit_bytes(0x0F, 0x22, 0xC0)  # MOV CR8, RAX
        else:
            raise ValueError(f"Invalid control register: CR{cr_number}")
        
        print(f"DEBUG: MOV CR{cr_number}, RAX")
    
    def emit_read_msr(self):
        """RDMSR - Read model-specific register (ECX contains MSR number)"""
        self.emit_bytes(0x0F, 0x32)
        print("DEBUG: RDMSR")
    
    def emit_write_msr(self):
        """WRMSR - Write model-specific register"""
        self.emit_bytes(0x0F, 0x30)
        print("DEBUG: WRMSR")
    
    # === NEW: PORT I/O OPERATIONS ===
    
    def emit_in_al_dx(self):
        """IN AL, DX - Read byte from port in DX"""
        self.emit_bytes(0xEC)
        print("DEBUG: IN AL, DX")
    
    def emit_in_ax_dx(self):
        """IN AX, DX - Read word from port in DX"""
        self.emit_bytes(0x66, 0xED)
        print("DEBUG: IN AX, DX")
    
    def emit_in_eax_dx(self):
        """IN EAX, DX - Read dword from port in DX"""
        self.emit_bytes(0xED)
        print("DEBUG: IN EAX, DX")
    
    def emit_out_dx_al(self):
        """OUT DX, AL - Write byte to port in DX"""
        self.emit_bytes(0xEE)
        print("DEBUG: OUT DX, AL")
    
    def emit_out_dx_ax(self):
        """OUT DX, AX - Write word to port in DX"""
        self.emit_bytes(0x66, 0xEF)
        print("DEBUG: OUT DX, AX")
    
    def emit_out_dx_eax(self):
        """OUT DX, EAX - Write dword to port in DX"""
        self.emit_bytes(0xEF)
        print("DEBUG: OUT DX, EAX")
    
    def emit_port_read(self, port: int, size: str):
        """High-level port read operation"""
        # Load port number into DX
        self.emit_bytes(0x66, 0xBA)  # MOV DX, imm16
        self.emit_bytes(*struct.pack('<H', port))
        
        if size == "byte":
            self.emit_in_al_dx()
            # Zero-extend AL to RAX
            self.emit_bytes(0x48, 0x0F, 0xB6, 0xC0)  # MOVZX RAX, AL
        elif size == "word":
            self.emit_in_ax_dx()
            # Zero-extend AX to RAX
            self.emit_bytes(0x48, 0x0F, 0xB7, 0xC0)  # MOVZX RAX, AX
        elif size == "dword":
            self.emit_in_eax_dx()
            # Zero-extend EAX to RAX (automatic in 64-bit mode)
        else:
            raise ValueError(f"Invalid port I/O size: {size}")
        
        print(f"DEBUG: Port read from {hex(port)} ({size})")
    
    def emit_port_write(self, port: int, size: str):
        """High-level port write operation (value in RAX)"""
        # Load port number into DX
        self.emit_bytes(0x66, 0xBA)  # MOV DX, imm16
        self.emit_bytes(*struct.pack('<H', port))
        
        if size == "byte":
            self.emit_out_dx_al()
        elif size == "word":
            self.emit_out_dx_ax()
        elif size == "dword":
            self.emit_out_dx_eax()
        else:
            raise ValueError(f"Invalid port I/O size: {size}")
        
        print(f"DEBUG: Port write to {hex(port)} ({size})")
    
    # === NEW: INTERRUPT OPERATIONS ===
    
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
    
    # === NEW: CACHE OPERATIONS ===
    
    def emit_invlpg(self, address: int):
        """INVLPG [address] - Invalidate page in TLB"""
        # Load address into RAX, then invalidate
        self.emit_mov_rax_imm64(address)
        self.emit_bytes(0x0F, 0x01, 0x38)  # INVLPG [RAX]
        print(f"DEBUG: INVLPG [{hex(address)}]")
    
    def emit_wbinvd(self):
        """WBINVD - Write back and invalidate cache"""
        self.emit_bytes(0x0F, 0x09)
        print("DEBUG: WBINVD")
    
    def emit_invd(self):
        """INVD - Invalidate cache without writeback"""
        self.emit_bytes(0x0F, 0x08)
        print("DEBUG: INVD")
    
    # === NEW: INLINE ASSEMBLY SUPPORT ===
    
    def emit_inline_assembly(self, assembly_code: str):
        """Emit inline assembly - WARNING: Direct byte emission"""
        print(f"DEBUG: INLINE ASSEMBLY: {assembly_code}")
        
        # Simple assembly parser for common instructions
        lines = assembly_code.strip().split('\n')
        for line in lines:
            line = line.strip()
            if not line or line.startswith(';'):
                continue
            
            # Parse simple instructions
            if line.upper() == "NOP":
                self.emit_nop()
            elif line.upper() == "CLI":
                self.emit_cli()
            elif line.upper() == "STI":
                self.emit_sti()
            elif line.upper() == "HLT":
                self.emit_hlt()
            elif line.upper() == "MFENCE":
                self.emit_memory_fence()
            elif line.upper() == "SFENCE":
                self.emit_store_fence()
            elif line.upper() == "LFENCE":
                self.emit_load_fence()
            elif line.upper() == "RDMSR":
                self.emit_read_msr()
            elif line.upper() == "WRMSR":
                self.emit_write_msr()
            elif line.upper().startswith("MOV RAX, CR"):
                cr_num = int(line.split("CR")[1])
                self.emit_read_cr(cr_num)
            elif line.upper().startswith("MOV CR") and ", RAX" in line.upper():
                cr_num = int(line.split("CR")[1].split(",")[0])
                self.emit_write_cr(cr_num)
            else:
                print(f"WARNING: Unrecognized assembly instruction: {line}")
                # For unknown instructions, emit NOP as placeholder
                self.emit_nop()
    # === SCHEDULING PRIMITIVES FOR LOOP SUB ROUTINE ======
    
    def emit_sub_rsp_imm32(self, value):
        """SUB RSP, imm32 - Allocate stack space"""
        self.emit_bytes(0x48, 0x81, 0xEC)
        self.emit_bytes(*struct.pack('<I', value))
        print(f"DEBUG: SUB RSP, {value}")

    def emit_mov_rbx_from_stack(self, offset):
        """MOV RBX, [RSP + offset]"""
        if offset == 0:
            self.emit_bytes(0x48, 0x8B, 0x1C, 0x24)
        else:
            self.emit_bytes(0x48, 0x8B, 0x9C, 0x24)
            self.emit_bytes(*struct.pack('<I', offset))
        print(f"DEBUG: MOV RBX, [RSP + {offset}]")

    def emit_cmp_rax_imm8(self, value):
        """CMP RAX, imm8"""
        self.emit_bytes(0x48, 0x83, 0xF8, value & 0xFF)
        print(f"DEBUG: CMP RAX, {value}")
    
    def emit_cmp_rax_imm32(self, value: int):
        """CMP RAX, imm32 - Compare RAX with 32-bit immediate"""
        self.emit_bytes(0x48, 0x3D)  # CMP RAX, imm32
        self.emit_bytes(*struct.pack('<i', value))  # 32-bit signed immediate
        print(f"DEBUG: CMP RAX, {value}")

    def emit_mov_r10_rax(self):
        """MOV R10, RAX"""
        self.emit_bytes(0x49, 0x89, 0xC2)
        print("DEBUG: MOV R10, RAX")

    def emit_mov_rax_r10(self):
        """MOV RAX, R10"""
        self.emit_bytes(0x4C, 0x89, 0xD0)
        print("DEBUG: MOV RAX, R10")

    def emit_push_r10(self):
        """PUSH R10 - Already exists but verify"""
        self.emit_bytes(0x41, 0x52)
        print("DEBUG: PUSH R10")

    def emit_mov_rsp_rax(self):
        """MOV RSP, RAX"""
        self.emit_bytes(0x48, 0x89, 0xC4)
        print("DEBUG: MOV RSP, RAX")
    
    
    
    # === BITWISE OPERATIONS (preserved) ===
    
    def emit_xor_rdx_rdx(self):
        """XOR RDX, RDX (zero rdx)"""
        self.emit_bytes(0x48, 0x31, 0xD2)
        print("DEBUG: XOR RDX, RDX")
    
    # === ARITHMETIC OPERATIONS (preserved) ===
    
    def emit_add_rax_rbx(self):
        """ADD RAX, RBX"""
        self.emit_bytes(0x48, 0x01, 0xD8)
    
    def emit_sub_rax_rbx(self):
        """SUB RAX, RBX"""
        self.emit_bytes(0x48, 0x29, 0xD8)
        print("DEBUG: SUB RAX, RBX")
    
    def emit_imul_rax_rbx(self):
        """IMUL RAX, RBX"""
        self.emit_bytes(0x48, 0x0F, 0xAF, 0xC3)
    
    # === STACK OPERATIONS ) ===
    
    def emit_mov_r8_rdi(self):
        """MOV R8, RDI"""
        self.emit_bytes(0x49, 0x89, 0xF8)
        print("DEBUG: MOV R8, RDI")

    def emit_mov_r9_rsi(self):
        """MOV R9, RSI"""
        self.emit_bytes(0x49, 0x89, 0xF1)
        print("DEBUG: MOV R9, RSI")

    def emit_mov_r8_rax(self):
        """MOV R8, RAX"""
        self.emit_bytes(0x49, 0x89, 0xC0)
        print("DEBUG: MOV R8, RAX")

    def emit_mov_r9_rax(self):
        """MOV R9, RAX"""
        self.emit_bytes(0x49, 0x89, 0xC1)
        print("DEBUG: MOV R9, RAX")

    def emit_mov_rax_r8(self):
        """MOV RAX, R8"""
        self.emit_bytes(0x4C, 0x89, 0xC0)
        print("DEBUG: MOV RAX, R8")

    def emit_mov_rax_r9(self):
        """MOV RAX, R9"""
        self.emit_bytes(0x4C, 0x89, 0xC8)
        print("DEBUG: MOV RAX, R9")

    def emit_mov_rdi_r8(self):
        """MOV RDI, R8"""
        self.emit_bytes(0x4C, 0x89, 0xC7)
        print("DEBUG: MOV RDI, R8")

    def emit_mov_rsi_r9(self):
        """MOV RSI, R9"""
        self.emit_bytes(0x4C, 0x89, 0xCE)
        print("DEBUG: MOV RSI, R9")    
    
    def emit_mov_r10_rax(self):
        """MOV R10, RAX"""
        self.emit_bytes(0x49, 0x89, 0xC2)
        print("DEBUG: MOV R10, RAX")

    def emit_mov_rax_r10(self):
        """MOV RAX, R10"""
        self.emit_bytes(0x4C, 0x89, 0xD0)
        print("DEBUG: MOV RAX, R10")

    def emit_push_r10(self):
        """PUSH R10"""
        self.emit_bytes(0x41, 0x52)
        print("DEBUG: PUSH R10")

    def emit_pop_r10(self):
        """POP R10"""
        self.emit_bytes(0x41, 0x5A)
        print("DEBUG: POP R10")
    
    def emit_push_rax(self):
        """PUSH RAX"""
        self.emit_bytes(0x50)
    
    def emit_pop_rax(self):
        """POP RAX"""
        self.emit_bytes(0x58)
    
    def emit_push_rbx(self):
        """PUSH RBX"""
        self.emit_bytes(0x53)
    
    def emit_pop_rbx(self):
        """POP RBX"""
        self.emit_bytes(0x5B)
    
    def emit_push_rcx(self):
        """PUSH RCX"""
        self.emit_bytes(0x51)
    
    def emit_pop_rcx(self):
        """POP RCX"""
        self.emit_bytes(0x59)
    
    def emit_push_rdx(self):
        """PUSH RDX"""
        self.emit_bytes(0x52)
    
    def emit_pop_rdx(self):
        """POP RDX"""
        self.emit_bytes(0x5A)
    
    def emit_push_rsi(self):
        """PUSH RSI"""
        self.emit_bytes(0x56)
    
    def emit_pop_rsi(self):
        """POP RSI"""
        self.emit_bytes(0x5E)
    
    def emit_push_rdi(self):
        """PUSH RDI"""
        self.emit_bytes(0x57)
    
    def emit_pop_rdi(self):
        """POP RDI"""
        self.emit_bytes(0x5F)
    
    def emit_push_rbp(self):
        """PUSH RBP - Standard frame pointer save"""
        self.emit_bytes(0x55)
        print("DEBUG: PUSH RBP")

    def emit_pop_rbp(self):
        """POP RBP - Standard frame pointer restore"""
        self.emit_bytes(0x5D)
        print("DEBUG: POP RBP")

    def emit_push_r12(self):
        """PUSH R12 - Callee-saved register"""
        self.emit_bytes(0x41, 0x54)

    def emit_pop_r12(self):
        """POP R12 - Callee-saved register"""
        self.emit_bytes(0x41, 0x5C)

    def emit_push_r13(self):
        """PUSH R13 - Callee-saved register"""
        self.emit_bytes(0x41, 0x55)

    def emit_pop_r13(self):
        """POP R13 - Callee-saved register"""
        self.emit_bytes(0x41, 0x5D)

    def emit_push_r14(self):
        """PUSH R14 - Callee-saved register"""
        self.emit_bytes(0x41, 0x56)

    def emit_pop_r14(self):
        """POP R14 - Callee-saved register"""
        self.emit_bytes(0x41, 0x5E)

    def emit_push_r15(self):
        """PUSH R15 - Callee-saved register"""
        self.emit_bytes(0x41, 0x57)

    def emit_pop_r15(self):
        """POP R15 - Callee-saved register"""
        self.emit_bytes(0x41, 0x5F)

    def emit_push_r8(self):
        """PUSH R8"""
        self.emit_bytes(0x41, 0x50)

    def emit_pop_r8(self):
        """POP R8"""
        self.emit_bytes(0x41, 0x58)

    def emit_push_r9(self):
        """PUSH R9"""
        self.emit_bytes(0x41, 0x51)

    def emit_pop_r9(self):
        """POP R9"""
        self.emit_bytes(0x41, 0x59)
        
    def emit_push_r10(self):
        """PUSH R10"""
        self.emit_bytes(0x41, 0x52)

    def emit_pop_r10(self):
        """POP R10"""
        self.emit_bytes(0x41, 0x5A)

    def emit_push_r11(self):
        """PUSH R11"""
        self.emit_bytes(0x41, 0x53)
        
    def emit_pop_r11(self):
        """POP R11"""
        self.emit_bytes(0x41, 0x5B)
    
    # === STRING OPERATIONS (preserved from original) ===
    
    def add_string(self, s: str) -> int:
        """Add string to data section, return offset"""
        try:
            if s in self.strings:
                print(f"DEBUG: String '{s}' already in data section at offset {self.strings[s]}")
                return self.strings[s]
            
            offset = len(self.data)
            self.strings[s] = offset
            self.data.extend(s.encode('utf-8'))
            self.data.append(0)  # Null terminator
            print(f"DEBUG: Added string '{s}' to data section at offset {offset}")
            return offset
        except Exception as e:
            print(f"ERROR: Failed to add string '{s}': {str(e)}")
            raise
    
    

    def emit_load_data_address(self, register, data_offset):
        """Emit instruction to load data address into register with relocation"""
        # Map register names to encoding
        reg_map = {
            'rax': 0xB8, 'rcx': 0xB9, 'rdx': 0xBA, 'rbx': 0xBB,
            'rsp': 0xBC, 'rbp': 0xBD, 'rsi': 0xBE, 'rdi': 0xBF
        }
        
        if register not in reg_map:
            raise ValueError(f"Unsupported register: {register}")
        
        # Emit MOV register, imm64 with placeholder
        self.emit_bytes(0x48, reg_map[register])
        
        # Add placeholder for address (will be patched later)
        current_offset = len(self.code)
        self.emit_bytes(0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00)
        
        # Register relocation for this address
        self.add_data_relocation(current_offset, data_offset)
        
        print(f"DEBUG: Emitted load data address to {register} for offset {data_offset}")


    def emit_print_string(self, s: str):
        """Emit code to print a string exactly as provided"""
        try:
            # Store the string in data section
            offset = self.add_string(s)
            byte_length = len(s.encode('utf-8'))
            
            print(f"DEBUG: Printing string '{s[:30]}...', offset {offset}, length {byte_length}")
            
            # Print the string
            self.emit_mov_rax_imm64(1)  # sys_write
            self.emit_mov_rdi_imm64(1)  # stdout
            self.emit_load_data_address('rsi', offset)
            self.emit_mov_rdx_imm64(byte_length)
            self.emit_syscall()
            
            # Add newline if the string doesn't end with one
            if not s.endswith('\n'):
                newline_offset = self.add_string("\n")
                self.emit_mov_rax_imm64(1)  # sys_write
                self.emit_mov_rdi_imm64(1)  # stdout
                self.emit_load_data_address('rsi', newline_offset)
                self.emit_mov_rdx_imm64(1)
                self.emit_syscall()
                print(f"Added newline after string")
            
        except Exception as e:
            print(f"ERROR in emit_print_string: {e}")
            raise
        
    def emit_print_system_string(self, s: str):
        """Emit code to print a system/debug string - bypasses any pool mechanism"""
        try:
            # Direct string storage and printing for system messages
            offset = self.add_string(s)
            byte_length = len(s.encode('utf-8'))
            
            # Save all registers to avoid interference
            self.emit_push_rax()
            self.emit_push_rdi()
            self.emit_push_rsi()
            self.emit_push_rdx()
            
            # Print the string
            self.emit_mov_rax_imm64(1)  # sys_write
            self.emit_mov_rdi_imm64(1)  # stdout
            self.emit_load_data_address('rsi', offset)
            self.emit_mov_rdx_imm64(byte_length)
            self.emit_syscall()
            
            # Restore registers
            self.emit_pop_rdx()
            self.emit_pop_rsi()
            self.emit_pop_rdi()
            self.emit_pop_rax()
            
        except Exception as e:
            print(f"ERROR in emit_print_system_string: {e}")
            raise    
        
        
    def emit_print_string_raw(self, s: str):
        """Emit code to print a string without adding to data section"""
        try:
            offset = self.add_string(s)
            byte_length = len(s.encode('utf-8'))
        
            self.emit_mov_rax_imm64(1)
            self.emit_mov_rdi_imm64(1)
            data_addr = self.data_base_address + offset
            self.emit_mov_rsi_imm64(data_addr)
            self.emit_mov_rdx_imm64(byte_length)
            self.emit_syscall()
            print(f"DEBUG: Printed raw string '{s}', byte length {byte_length}")
        except Exception as e:
            print(f"ERROR: Failed to print raw string '{s}': {str(e)}")
            raise
    
    def emit_print_number(self):
        """Print number in RAX with proper sign handling"""
        print("DEBUG: Number printing with sign checking")
        
        # Save all registers we'll use
        self.emit_push_rax()
        self.emit_push_rbx()
        self.emit_push_rdx()
        self.emit_push_rsi()
        self.emit_push_rdi()
        
        # Check if negative
        self.emit_bytes(0x48, 0x85, 0xC0)  # TEST RAX, RAX
        positive_label = self.create_label()
        self.emit_jump_to_label(positive_label, "JNS")  # Jump if not signed (positive)
        
        # Negative number - print minus sign first
        self.emit_push_rax()  # Save the negative value
        
        # Print '-' character
        self.emit_mov_rax_imm64(1)  # sys_write
        self.emit_mov_rdi_imm64(1)  # stdout  
        minus_offset = self.add_string("-")
        self.emit_load_data_address('rsi', minus_offset)
        self.emit_mov_rdx_imm64(1)  # length = 1
        self.emit_syscall()
        
        # Negate the value to make it positive
        self.emit_pop_rax()  # Get value back
        self.emit_bytes(0x48, 0xF7, 0xD8)  # NEG RAX (negate)
        
        self.mark_label(positive_label)
        
        # Now RAX has positive value - continue with digit conversion
        # Allocate buffer on stack
        self.emit_bytes(0x48, 0x83, 0xEC, 0x20)  # SUB RSP, 32
        
        # Point RSI to end of buffer
        self.emit_bytes(0x48, 0x8D, 0x74, 0x24, 0x1F)  # LEA RSI, [RSP+31]
        self.emit_bytes(0xC6, 0x06, 0x00)  # MOV BYTE [RSI], 0 (null terminator)
        
        # Restore RAX from stack (saved at beginning)
        self.emit_bytes(0x48, 0x8B, 0x44, 0x24, 0x40)  # MOV RAX, [RSP+64]
        
        # Check if already positive (from above) or needs another check
        self.emit_bytes(0x48, 0x85, 0xC0)  # TEST RAX, RAX
        already_positive = self.create_label()
        self.emit_jump_to_label(already_positive, "JNS")
        self.emit_bytes(0x48, 0xF7, 0xD8)  # NEG RAX if still negative
        self.mark_label(already_positive)
        
        # Set divisor to 10
        self.emit_bytes(0x48, 0xBB, 0x0A, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00)  # MOV RBX, 10
        
        # Check if zero
        self.emit_bytes(0x48, 0x85, 0xC0)  # TEST RAX, RAX
        self.emit_bytes(0x75, 0x08)  # JNE +8 (skip zero case)
        
        # Zero case
        self.emit_bytes(0x48, 0xFF, 0xCE)  # DEC RSI
        self.emit_bytes(0xC6, 0x06, 0x30)  # MOV BYTE [RSI], '0'
        self.emit_bytes(0xEB, 0x14)  # JMP to print section
        
        # Conversion loop (for non-zero)
        self.emit_bytes(0x48, 0x31, 0xD2)  # XOR RDX, RDX
        self.emit_bytes(0x48, 0xF7, 0xF3)  # DIV RBX (RAX/10, remainder in RDX)
        self.emit_bytes(0x48, 0x83, 0xC2, 0x30)  # ADD RDX, '0'
        self.emit_bytes(0x48, 0xFF, 0xCE)  # DEC RSI
        self.emit_bytes(0x88, 0x16)  # MOV [RSI], DL
        self.emit_bytes(0x48, 0x85, 0xC0)  # TEST RAX, RAX
        self.emit_bytes(0x75, 0xEC)  # JNE -20 (loop back)
        
        # Print section - calculate length
        self.emit_bytes(0x48, 0x8D, 0x54, 0x24, 0x1F)  # LEA RDX, [RSP+31]
        self.emit_bytes(0x48, 0x29, 0xF2)  # SUB RDX, RSI (length)
        
        # Write syscall
        self.emit_bytes(0x48, 0xB8, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00)  # MOV RAX, 1
        self.emit_bytes(0x48, 0xBF, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00)  # MOV RDI, 1
        self.emit_bytes(0x0F, 0x05)  # SYSCALL
        
        # Clean up stack
        self.emit_bytes(0x48, 0x83, 0xC4, 0x20)  # ADD RSP, 32
        
        # Restore registers
        self.emit_pop_rdi()
        self.emit_pop_rsi()
        self.emit_pop_rdx()
        self.emit_pop_rbx()
        self.emit_pop_rax()

    def create_label(self):
        """Generate unique label name"""
        if not hasattr(self, '_label_counter'):
            self._label_counter = 0
        self._label_counter += 1
        return f"L{self._label_counter}"
    
    def mark_label(self, label_name, is_local=False):
        """Mark a label at current position"""
        position = len(self.code)
        self.jump_manager.add_label(label_name, position, is_local)
        
        # Initialize labels dict if needed (don't reset it!)
        if not hasattr(self, 'labels'):
            self.labels = {}
        if not hasattr(self, 'pending_jumps'):
            self.pending_jumps = []
            
        self.labels[label_name] = position
        print(f"DEBUG: Marked label {label_name} at position {position}")
    
    def emit_jump_to_label(self, label_name, jump_type, is_local=False):
        position = len(self.code)
        if jump_type == "JE" or jump_type == "JZ":
            self.emit_bytes(0x0F, 0x84, 0x00, 0x00, 0x00, 0x00)
        elif jump_type == "JNE" or jump_type == "JNZ":
            self.emit_bytes(0x0F, 0x85, 0x00, 0x00, 0x00, 0x00)
        elif jump_type == "JMP":
            self.emit_bytes(0xE9, 0x00, 0x00, 0x00, 0x00)
        elif jump_type == "JL":
            self.emit_bytes(0x0F, 0x8C, 0x00, 0x00, 0x00, 0x00)
        elif jump_type == "JGE":
            self.emit_bytes(0x0F, 0x8D, 0x00, 0x00, 0x00, 0x00)
        elif jump_type == "JG":
            self.emit_bytes(0x0F, 0x8F, 0x00, 0x00, 0x00, 0x00)
        elif jump_type == "JLE":
            self.emit_bytes(0x0F, 0x8E, 0x00, 0x00, 0x00, 0x00)
        elif jump_type == "JNS":
            self.emit_bytes(0x0F, 0x89, 0x00, 0x00, 0x00, 0x00)
        elif jump_type == "JB":
            self.emit_bytes(0x0F, 0x82, 0x00, 0x00, 0x00, 0x00)
        elif jump_type == "JA":
            self.emit_bytes(0x0F, 0x87, 0x00, 0x00, 0x00, 0x00)
        elif jump_type == "JAE":  # New: Jump if above or equal (unsigned >=)
            self.emit_bytes(0x0F, 0x83, 0x00, 0x00, 0x00, 0x00)
        elif jump_type == "JBE":  # Optional new: Jump if below or equal (unsigned <=)
            self.emit_bytes(0x0F, 0x86, 0x00, 0x00, 0x00, 0x00)
        else:
            raise ValueError(f"Unknown jump type: {jump_type}")
        
        # Register with jump manager (unchanged)
        self.jump_manager.add_jump(position, label_name, jump_type, is_local)
        print(f"DEBUG: Emitted 32-bit {jump_type} to {label_name} at position {position}")
    
    def resolve_jumps(self):
        """Resolve all global jumps"""
        jump_count = len(self.jump_manager.global_jumps)
        if jump_count > 0:
            self.jump_manager.resolve_global_jumps(self.code)
            print(f"DEBUG: Successfully resolved {jump_count} global jumps")
    