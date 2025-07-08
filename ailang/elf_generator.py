#!/usr/bin/env python3
"""
ELF Executable Generator
Generates 64-bit ELF executables for AILANG compiler
"""

import struct

class ELFGenerator:
    """Generate 64-bit ELF executables"""
    
    def __init__(self):
        self.load_addr = 0x400000
        self.data_addr = 0x402000
    
    def generate(self, code: bytes, data: bytes) -> bytes:
        """Generate complete ELF executable"""
        
        # ELF Header
        elf_header = bytearray()
        
        # Magic number
        elf_header.extend(b'\x7fELF')
        
        # Class (64-bit)
        elf_header.append(2)
        
        # Data encoding (little-endian)
        elf_header.append(1)
        
        # Version
        elf_header.append(1)
        
        # OS ABI (System V)
        elf_header.append(0)
        
        # ABI version
        elf_header.append(0)
        
        # Padding
        elf_header.extend(b'\x00' * 7)
        
        # Type (executable)
        elf_header.extend(struct.pack('<H', 2))
        
        # Machine (x86-64)
        elf_header.extend(struct.pack('<H', 0x3E))
        
        # Version
        elf_header.extend(struct.pack('<I', 1))
        
        # Entry point
        entry = self.load_addr + 0x1000
        elf_header.extend(struct.pack('<Q', entry))
        
        # Program header offset
        elf_header.extend(struct.pack('<Q', 64))
        
        # Section header offset (none)
        elf_header.extend(struct.pack('<Q', 0))
        
        # Flags
        elf_header.extend(struct.pack('<I', 0))
        
        # ELF header size
        elf_header.extend(struct.pack('<H', 64))
        
        # Program header size
        elf_header.extend(struct.pack('<H', 56))
        
        # Number of program headers
        elf_header.extend(struct.pack('<H', 2))
        
        # Section header size
        elf_header.extend(struct.pack('<H', 0))
        
        # Number of section headers
        elf_header.extend(struct.pack('<H', 0))
        
        # Section header string table index
        elf_header.extend(struct.pack('<H', 0))
        
        # Program Header 1 (Code)
        code_header = bytearray()
        
        # Type (LOAD)
        code_header.extend(struct.pack('<I', 1))
        
        # Flags (Execute + Read)
        code_header.extend(struct.pack('<I', 5))
        
        # Offset in file
        code_header.extend(struct.pack('<Q', 0x1000))
        
        # Virtual address
        code_header.extend(struct.pack('<Q', self.load_addr + 0x1000))
        
        # Physical address
        code_header.extend(struct.pack('<Q', self.load_addr + 0x1000))
        
        # Size in file
        code_header.extend(struct.pack('<Q', len(code)))
        
        # Size in memory
        code_header.extend(struct.pack('<Q', len(code)))
        
        # Alignment
        code_header.extend(struct.pack('<Q', 0x1000))
        
        # Program Header 2 (Data)
        data_header = bytearray()
        
        # Type (LOAD)
        data_header.extend(struct.pack('<I', 1))
        
        # Flags (Read + Write)
        data_header.extend(struct.pack('<I', 6))
        
        # Offset in file
        data_offset = 0x2000
        data_header.extend(struct.pack('<Q', data_offset))
        
        # Virtual address
        data_header.extend(struct.pack('<Q', self.data_addr))
        
        # Physical address
        data_header.extend(struct.pack('<Q', self.data_addr))
        
        # Size in file
        data_header.extend(struct.pack('<Q', len(data)))
        
        # Size in memory
        data_header.extend(struct.pack('<Q', len(data)))
        
        # Alignment
        data_header.extend(struct.pack('<Q', 0x1000))
        
        # Build complete file
        result = bytearray()
        
        # ELF header
        result.extend(elf_header)
        
        # Program headers
        result.extend(code_header)
        result.extend(data_header)
        
        # Pad to code section
        result.extend(b'\x00' * (0x1000 - len(result)))
        
        # Code section
        result.extend(code)
        
        # Pad to data section
        result.extend(b'\x00' * (0x2000 - len(result)))
        
        # Data section
        result.extend(data)
        
        return bytes(result)