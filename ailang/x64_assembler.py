#!/usr/bin/env python3
"""
x86-64 Machine Code Generator - ENHANCED FOR SYSTEMS PROGRAMMING
Generates raw machine code for AILANG compiler with low-level memory operations
Adds pointer operations, hardware access, and atomic operations
"""

import struct
from typing import Dict, Union

class X64Assembler:
    """Raw x86-64 machine code generation with ENHANCED low-level operations"""
    
    def __init__(self):
        self.code = bytearray()
        self.data = bytearray()
        self.strings: Dict[str, int] = {}  # string -> offset
        self.labels: Dict[str, int] = {}    # label -> code offset
        self.variables: Dict[str, int] = {} # variable -> stack offset
        self.stack_offset = 0
        
        # === NEW: Low-Level State Tracking ===
        self.interrupt_handlers: Dict[int, str] = {}  # vector -> handler_name
        self.device_registers: Dict[str, int] = {}    # device -> base_address
        self.memory_maps: Dict[str, int] = {}         # name -> physical_address
        
    def emit_bytes(self, *bytes_vals: Union[int, bytes]):
        """Emit raw bytes"""
        try:
            debug_bytes = []
            for b in bytes_vals:
                if isinstance(b, int):
                    self.code.append(b)
                    debug_bytes.append(hex(b))
                elif isinstance(b, bytes):
                    self.code.extend(b)
                    debug_bytes.extend([hex(x) for x in b])
                else:
                    raise ValueError(f"Invalid byte type: {type(b)}")
            print(f"DEBUG: Emitted bytes: {debug_bytes}")
        except Exception as e:
            print(f"ERROR: Failed to emit bytes: {str(e)}")
            raise
    
    # === BASIC INSTRUCTIONS (preserved from original) ===
    
    def emit_syscall(self):
        """SYSCALL instruction"""
        self.emit_bytes(0x0F, 0x05)
    
    def emit_ret(self):
        """RET instruction"""
        self.emit_bytes(0xC3)
    
    def emit_nop(self):
        """NOP instruction"""
        self.emit_bytes(0x90)
    
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
    
    def emit_mov_rdi_imm64(self, value: int):
        """MOV RDI, imm64"""
        self.emit_bytes(0x48, 0xBF)
        self.emit_bytes(*struct.pack('<Q', value))
        print(f"DEBUG: MOV RDI, {value}")
    
    def emit_mov_rsi_imm64(self, value: int):
        """MOV RSI, imm64"""
        self.emit_bytes(0x48, 0xBE)
        self.emit_bytes(*struct.pack('<Q', value))
        print(f"DEBUG: MOV RSI, {value}")
    
    def emit_mov_rdx_imm64(self, value: int):
        """MOV RDX, imm64"""
        self.emit_bytes(0x48, 0xBA)
        self.emit_bytes(*struct.pack('<Q', value))
        print(f"DEBUG: MOV RDX, {value}")
    
    def emit_mov_rcx_imm64(self, value: int):
        """MOV RCX, imm64"""
        self.emit_bytes(0x48, 0xB9)
        self.emit_bytes(*struct.pack('<Q', value))
        print(f"DEBUG: MOV RCX, {value}")
    
    def emit_mov_r8_imm64(self, value: int):
        """MOV R8, imm64"""
        self.emit_bytes(0x49, 0xB8)
        self.emit_bytes(*struct.pack('<Q', value))
        print(f"DEBUG: MOV R8, {value}")

    def emit_mov_r9_imm64(self, value: int):
        """MOV R9, imm64"""
        self.emit_bytes(0x49, 0xB9)
        self.emit_bytes(*struct.pack('<Q', value))
        print(f"DEBUG: MOV R9, {value}")
    
    def emit_mov_r10_imm64(self, value: int):
        """MOV R10, imm64"""
        self.emit_bytes(0x49, 0xBA)
        self.emit_bytes(*struct.pack('<Q', value))
        print(f"DEBUG: MOV R10, {value}")
    
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
        rex_prefix = 0x48
        
        if base_reg.startswith('R') and len(base_reg) > 2:
            rex_prefix |= 0x01
        
        if offset == 0:
            self.emit_bytes(rex_prefix, 0x8D, 0x00 | reg_code)
        elif -128 <= offset <= 127:
            self.emit_bytes(rex_prefix, 0x8D, 0x40 | reg_code, offset & 0xFF)
        else:
            self.emit_bytes(rex_prefix, 0x8D, 0x80 | reg_code)
            self.emit_bytes(*struct.pack('<i', offset))
        
        print(f"DEBUG: LEA RAX, [{base_reg} + {offset}]")
    
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
    
    # === STACK OPERATIONS (preserved) ===
    
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
    
    def emit_print_string(self, s: str):
        """Emit code to print a string via syscall"""
        try:
            offset = self.add_string(s)
            byte_length = len(s.encode('utf-8'))
        
            self.emit_mov_rax_imm64(1)  # sys_write
            self.emit_mov_rdi_imm64(1)  # stdout
            data_addr = 0x402000 + offset
            self.emit_mov_rsi_imm64(data_addr)
            self.emit_mov_rdx_imm64(byte_length)
            self.emit_syscall()
            self.emit_print_string_raw("\n")
            print(f"DEBUG: Printed string '{s}', byte length {byte_length}")
        except Exception as e:
            print(f"ERROR: Failed to print string '{s}': {str(e)}")
            raise
    
    def emit_print_string_raw(self, s: str):
        """Emit code to print a string without adding to data section"""
        try:
            offset = self.add_string(s)
            byte_length = len(s.encode('utf-8'))
        
            self.emit_mov_rax_imm64(1)
            self.emit_mov_rdi_imm64(1)
            data_addr = 0x402000 + offset
            self.emit_mov_rsi_imm64(data_addr)
            self.emit_mov_rdx_imm64(byte_length)
            self.emit_syscall()
            print(f"DEBUG: Printed raw string '{s}', byte length {byte_length}")
        except Exception as e:
            print(f"ERROR: Failed to print raw string '{s}': {str(e)}")
            raise
    
    def emit_print_number(self):
        """Professional 64-bit number printing with proper stack management"""
        try:
            print("DEBUG: Professional number printing - 64-bit stack management")
            
            # === FUNCTION PROLOGUE ===
            # Save the number we're printing (in RAX)
            self.emit_push_rax()
            
            # Save all registers we'll modify (following x86-64 ABI)
            self.emit_push_rbx()
            self.emit_push_rcx()
            self.emit_push_rdx()
            self.emit_push_rsi()
            self.emit_push_rdi()
            self.emit_push_r8()
            self.emit_push_r9()
            
            # Allocate buffer space (64 bytes, 16-byte aligned)
            buffer_size = 64
            self.emit_bytes(0x48, 0x83, 0xEC, buffer_size)  # SUB RSP, 64
            
            # Get the number to print (now at [RSP + 64 + 56] = [RSP + 120])
            self.emit_bytes(0x48, 0x8B, 0x84, 0x24)  # MOV RAX, [RSP + offset]
            self.emit_bytes(*struct.pack('<I', buffer_size + 56))  # +56 for 7 saved registers
            
            # === CONVERSION LOGIC ===
            
            # Set up buffer pointer (end of buffer)
            self.emit_bytes(0x48, 0x89, 0xE6)          # MOV RSI, RSP (buffer start)
            self.emit_bytes(0x48, 0x83, 0xC6, 0x3F)    # ADD RSI, 63 (point to end)
            self.emit_bytes(0xC6, 0x06, 0x00)          # MOV BYTE PTR [RSI], 0 (null term)
            
            # Handle zero case
            self.emit_bytes(0x48, 0x83, 0xF8, 0x00)    # CMP RAX, 0
            self.emit_bytes(0x75, 0x08)                # JNZ skip_zero
            self.emit_bytes(0x48, 0xFF, 0xCE)          # DEC RSI
            self.emit_bytes(0xC6, 0x06, 0x30)          # MOV BYTE PTR [RSI], '0'
            self.emit_bytes(0xEB, 0x1E)                # JMP to_print
            
            # Conversion loop
            loop_start = len(self.code)
            
            # Divide by 10
            self.emit_bytes(0x48, 0x31, 0xD2)          # XOR RDX, RDX
            self.emit_bytes(0x48, 0xB9, 0x0A, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00)  # MOV RCX, 10
            self.emit_bytes(0x48, 0xF7, 0xF1)          # DIV RCX (quotient in RAX, remainder in RDX)
            
            # Convert remainder to ASCII and store
            self.emit_bytes(0x48, 0x83, 0xC2, 0x30)    # ADD RDX, '0'
            self.emit_bytes(0x48, 0xFF, 0xCE)          # DEC RSI
            self.emit_bytes(0x88, 0x16)                # MOV [RSI], DL
            
            # Continue if quotient is not zero
            self.emit_bytes(0x48, 0x83, 0xF8, 0x00)    # CMP RAX, 0
            
            # Calculate jump offset
            current_pos = len(self.code) + 2
            jump_offset = loop_start - current_pos
            self.emit_bytes(0x75, jump_offset & 0xFF)   # JNZ loop_start
            
            # === OUTPUT THE STRING ===
            
            # Calculate string length
            self.emit_bytes(0x48, 0x89, 0xE0)          # MOV RAX, RSP (buffer start)
            self.emit_bytes(0x48, 0x83, 0xC0, 0x3F)    # ADD RAX, 63 (buffer end)
            self.emit_bytes(0x48, 0x29, 0xF0)          # SUB RAX, RSI (length in RAX)
            
            # Print the number
            self.emit_bytes(0x48, 0x89, 0xC2)          # MOV RDX, RAX (length)
            self.emit_mov_rax_imm64(1)                 # sys_write
            self.emit_mov_rdi_imm64(1)                 # stdout
            # RSI already points to the string
            self.emit_syscall()
            
            # Print newline
            newline_offset = self.add_string("\n")
            self.emit_mov_rax_imm64(1)                 # sys_write
            self.emit_mov_rdi_imm64(1)                 # stdout
            self.emit_mov_rsi_imm64(0x402000 + newline_offset)  # newline string
            self.emit_mov_rdx_imm64(1)                 # length
            self.emit_syscall()
            
            # === FUNCTION EPILOGUE ===
            
            # Restore stack
            self.emit_bytes(0x48, 0x83, 0xC4, buffer_size)  # ADD RSP, 64
            
            # Restore registers (reverse order)
            self.emit_pop_r9()
            self.emit_pop_r8()
            self.emit_pop_rdi()
            self.emit_pop_rsi()
            self.emit_pop_rdx()
            self.emit_pop_rcx()
            self.emit_pop_rbx()
            self.emit_pop_rax()  # Restore original value
            
            print("DEBUG: Professional number printing completed")
            
        except Exception as e:
            print(f"ERROR: Professional number printing failed: {str(e)}")
            # Emergency cleanup
            try:
                self.emit_bytes(0x48, 0x83, 0xC4, 64)  # ADD RSP, 64
                # Pop all registers
                for _ in range(8):  # 8 registers saved
                    self.emit_bytes(0x58)  # POP RAX (generic pop)
            except:
                pass
            raise
    
    # === COMPLETE JUMP SYSTEM METHODS (preserved) ===
    
    def create_label(self):
        """Generate unique label name"""
        if not hasattr(self, '_label_counter'):
            self._label_counter = 0
        self._label_counter += 1
        return f"L{self._label_counter}"
    
    def mark_label(self, label_name):
        """Mark current position as label"""
        if not hasattr(self, 'labels'):
            self.labels = {}
        if not hasattr(self, 'pending_jumps'):
            self.pending_jumps = []
        self.labels[label_name] = len(self.code)
        print(f"DEBUG: Marked label {label_name} at position {len(self.code)}")
    
    def emit_jump_to_label(self, label_name, jump_type="JE"):
        """Emit jump with ENHANCED 32-bit range (±2GB)"""
        if not hasattr(self, 'pending_jumps'):
            self.pending_jumps = []
            
        # Use 32-bit jumps for virtually unlimited range
        if jump_type == "JE":
            # JE with 32-bit offset: 0x0F 0x84 + 4-byte offset
            self.emit_bytes(0x0F, 0x84, 0x00, 0x00, 0x00, 0x00)
            jump_pos = len(self.code) - 4
        elif jump_type == "JMP":
            # JMP with 32-bit offset: 0xE9 + 4-byte offset  
            self.emit_bytes(0xE9, 0x00, 0x00, 0x00, 0x00)
            jump_pos = len(self.code) - 4
        elif jump_type == "JL":
            # JL with 32-bit offset: 0x0F 0x8C + 4-byte offset
            self.emit_bytes(0x0F, 0x8C, 0x00, 0x00, 0x00, 0x00)
            jump_pos = len(self.code) - 4
        elif jump_type == "JG":
            # JG with 32-bit offset: 0x0F 0x8F + 4-byte offset
            self.emit_bytes(0x0F, 0x8F, 0x00, 0x00, 0x00, 0x00)
            jump_pos = len(self.code) - 4
        elif jump_type == "JNE":
            # JNE with 32-bit offset: 0x0F 0x85 + 4-byte offset
            self.emit_bytes(0x0F, 0x85, 0x00, 0x00, 0x00, 0x00)
            jump_pos = len(self.code) - 4
        else:
            raise ValueError(f"Unsupported jump type: {jump_type}")
        
        # Record for later fixup
        self.pending_jumps.append((jump_pos, label_name, jump_type))
        print(f"DEBUG: Emitted 32-bit {jump_type} to {label_name} at position {jump_pos}")

    def resolve_jumps(self):
        """Fix all jump offsets - ENHANCED for 32-bit jumps (±2GB range)"""
        if not hasattr(self, 'pending_jumps'):
            return
        if not hasattr(self, 'labels'):
            self.labels = {}
            
        print(f"DEBUG: Resolving {len(self.pending_jumps)} pending 32-bit jumps...")
        
        for jump_pos, label_name, jump_type in self.pending_jumps:
            if label_name in self.labels:
                target_addr = self.labels[label_name]
                current_addr = jump_pos + 4  # Address after the 32-bit jump instruction
                offset = target_addr - current_addr
                
                # 32-bit signed offset range: ±2GB (virtually unlimited)
                if -2147483648 <= offset <= 2147483647:
                    # Update the 4-byte offset
                    import struct
                    offset_bytes = struct.pack('<i', offset)
                    self.code[jump_pos:jump_pos+4] = offset_bytes
                    print(f"DEBUG: Fixed 32-bit {jump_type} to {label_name}: offset {offset}")
                else:
                    print(f"ERROR: Jump offset {offset} exceeds ±2GB limit for {label_name}")
                    # This should never happen in practice
                    import struct
                    self.code[jump_pos:jump_pos+4] = struct.pack('<i', 0)
            else:
                print(f"ERROR: Undefined label: {label_name}")
                import struct
                self.code[jump_pos:jump_pos+4] = struct.pack('<i', 0)
        
        # Clear pending jumps
        resolved_count = len(self.pending_jumps)
        self.pending_jumps = []
        print(f"DEBUG: Successfully resolved {resolved_count} 32-bit jumps")