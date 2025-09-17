# ailang_compiler/assembler/modules/memory.py
"""Memory access and addressing operations"""

import struct

class MemoryOperations:
    """Memory load/store operations and addressing modes"""
    
    # === BASIC MEMORY ACCESS ===
    
    def emit_mov_rax_mem(self, address: int):
        """MOV RAX, [address] - Load from absolute memory address"""
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
    
    # === OFFSET-BASED MEMORY ACCESS ===
    
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
    
    def emit_mov_rsi_from_stack_offset(self, offset):
        """MOV RSI, [RSP+offset] - Load from stack at offset"""
        if offset <= 127:
            self.emit_bytes(0x48, 0x8B, 0x74, 0x24, offset)
        else:
            self.emit_bytes(0x48, 0x8B, 0xB4, 0x24)
            self.emit_bytes(*struct.pack('<I', offset))
        print(f"DEBUG: MOV RSI, [RSP+{offset}]")
    
    def emit_mov_rbx_from_stack(self, offset):
        """MOV RBX, [RSP + offset]"""
        if offset == 0:
            self.emit_bytes(0x48, 0x8B, 0x1C, 0x24)
        else:
            self.emit_bytes(0x48, 0x8B, 0x9C, 0x24)
            self.emit_bytes(*struct.pack('<I', offset))
        print(f"DEBUG: MOV RBX, [RSP + {offset}]")
    
    def emit_mov_byte_mem_offset_imm(self, base_reg: str, index_reg, value: int):
        """MOV BYTE [base_reg + index_reg], imm8 - Store byte immediate to memory"""
        if base_reg != 'RAX':
            raise ValueError(f"emit_mov_byte_mem_offset_imm only supports RAX for now")
        
        # MOV BYTE [RAX + RBX], value: C6 04 18 value
        self.emit_bytes(0xC6, 0x04, 0x18, value)
        print(f"DEBUG: MOV BYTE [RAX+RBX], {value}")
    
    # === POOL OPERATIONS ===
    
    def emit_mov_qword_ptr_rax_rcx(self):
        """MOV [RAX], RCX - Store RCX at address in RAX"""
        self.emit_bytes(0x48, 0x89, 0x08)
    
    def emit_mov_rcx_qword_ptr_rax_minus_8(self):
        """MOV RCX, [RAX-8] - Load from 8 bytes before RAX"""
        self.emit_bytes(0x48, 0x8B, 0x48, 0xF8)
    
    # === LEA (Load Effective Address) ===
    
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
                self.emit_bytes(rex_prefix, 0x8D, 0x45)
                if offset < 0:
                    self.emit_bytes((offset + 256) & 0xFF)
                else:
                    self.emit_bytes(offset & 0xFF)
            else:
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
    
    def emit_lea_rax_rbx(self, offset):
        """Emit LEA RAX, [RBX + offset]"""
        self.emit_bytes(0x48, 0x8D, 0x43, offset)
        print(f"DEBUG: Emitted LEA RAX, [RBX + {offset}]")
    
    def emit_lea_rax_rbx_plus_8(self):
        """LEA RAX, [RBX+8] - Load effective address"""
        self.emit_bytes(0x48, 0x8D, 0x43, 0x08)
    
    def emit_lea_rax_rdi_plus_8(self):
        """LEA RAX, [RDI+8] - Load effective address"""
        self.emit_bytes(0x48, 0x8D, 0x47, 0x08)
    
    # === DATA ADDRESS LOADING ===
    
    def emit_load_data_address(self, register, data_offset):
        """Emit instruction to load data address into register with relocation"""
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
    
    # === SPECIAL MEMORY OPS ===
    
    def emit_mov_byte_ptr_rdi_zero(self):
        """MOV BYTE PTR [RDI], 0 - Store zero byte at [RDI]"""
        self.emit_bytes(0xC6, 0x07, 0x00)
        
    # === FUNCTION ADDRESS LOADING ===
    
    def emit_load_label_address(self, reg, label):
        """Load address of label into register using RIP-relative addressing"""
        print(f"DEBUG: Loading address of label {label} into {reg}")
        
        current_pos = len(self.code)
        
        if reg.lower() == 'rax':
            self.emit_bytes(0x48, 0x8D, 0x05)  # LEA RAX, [RIP + disp32]
        elif reg.lower() == 'rbx':
            self.emit_bytes(0x48, 0x8D, 0x1D)  # LEA RBX, [RIP + disp32]
        elif reg.lower() == 'rcx':
            self.emit_bytes(0x48, 0x8D, 0x0D)  # LEA RCX, [RIP + disp32]
        else:
            raise ValueError(f"emit_load_label_address not implemented for register {reg}")
        
        # Emit placeholder
        self.emit_bytes(0xFF, 0xFF, 0xFF, 0xFF)  # Distinctive pattern for debugging
        
        # Register with JumpManager instead of separate system
        self.jump_manager.add_lea_fixup(current_pos + 3, label)  # +3 for opcode bytes
    
    def resolve_label_relocations(self):
        """Resolve label relocations for function pointers"""
        if not hasattr(self, 'label_relocations'):
            return
            
        print(f"DEBUG: Resolving {len(self.label_relocations)} label relocations")
        
        for reloc in self.label_relocations:
            label = reloc['label']
            position = reloc['position']
            
            if label in self.labels:
                label_addr = self.labels[label]
                
                # Calculate RIP-relative offset
                # RIP points to the instruction after the displacement
                instruction_end = position + 4
                offset = label_addr - instruction_end
                
                print(f"DEBUG: Resolving {label} at pos {position}: offset = {offset}")
                
                # Patch the displacement
                if reloc['type'] == 'RIP_RELATIVE' and reloc['size'] == 4:
                    offset_bytes = struct.pack('<i', offset)
                    for i, byte in enumerate(offset_bytes):
                        self.code[position + i] = byte
                else:
                    print(f"WARNING: Unknown relocation type: {reloc['type']}")
            else:
                print(f"ERROR: Unresolved label: {label}")
                # Leave as zero - will likely crash but compilation continues