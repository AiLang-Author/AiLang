#!/usr/bin/env python3
"""
Hash Table Operations Module for AILANG Compiler
Implements hash functions and hash table operations
"""

import sys
import os
import struct
from ailang_parser.ailang_ast import *

class HashOps:
    """Handles hash table operations"""
    
    def __init__(self, compiler_context):
        self.compiler = compiler_context
        self.asm = compiler_context.asm
        
    def compile_operation(self, node):
        """Route operations to hash handlers"""
        if hasattr(node, "function"):
            return self.compile_hash_operation(node)
        return False
    
    def compile_hash_operation(self, node):
        """Route hash operations to specific handlers"""
        try:
            if isinstance(node, FunctionCall):
                function_name = node.function
                
                if function_name == 'HashCreate':
                    return self.compile_hash_create(node)
                elif function_name == 'HashSet':
                    return self.compile_hash_set(node)
                elif function_name == 'HashGet':
                    return self.compile_hash_get(node)
                elif function_name == 'HashFunction':
                    return self.compile_hash_function(node)
                else:
                    return False  # Not a hash operation
            
            return False
            
        except Exception as e:
            raise
    
    def compile_hash_create(self, node):
        """Create a hash table with given number of buckets"""
        
        # Default to 16 buckets
        bucket_count = 16
        if node.arguments:
            # Get bucket count from first argument
            if isinstance(node.arguments[0], Number):
                bucket_count = int(node.arguments[0].value)
            else:
                # For dynamic values, use default
                bucket_count = 16
        
        print(f"DEBUG: Creating hash table with {bucket_count} buckets")
        
        # Allocate memory for hash table
        # Structure: [bucket_count:8][size:8][buckets:bucket_count*24]
        # Each bucket: [key_ptr:8][value:8][next:8] (for future chaining)
        
        table_size = 16 + (bucket_count * 24)  # Header + buckets
        
        # Allocate using mmap
        self.asm.emit_mov_rax_imm64(9)      # mmap syscall
        self.asm.emit_mov_rdi_imm64(0)      # addr = NULL
        self.asm.emit_mov_rsi_imm64(table_size)
        self.asm.emit_mov_rdx_imm64(3)      # PROT_READ | PROT_WRITE
        self.asm.emit_mov_r10_imm64(0x22)   # MAP_PRIVATE | MAP_ANONYMOUS
        self.asm.emit_bytes(0x49, 0xC7, 0xC0, 0xFF, 0xFF, 0xFF, 0xFF)  # MOV R8, -1
        self.asm.emit_mov_r9_imm64(0)
        self.asm.emit_syscall()
        
        # Initialize header
        self.asm.emit_push_rax()  # Save table pointer
        self.asm.emit_mov_rbx_rax()
        
        # Store bucket_count at offset 0
        self.asm.emit_mov_rax_imm64(bucket_count)
        self.asm.emit_bytes(0x48, 0x89, 0x03)  # MOV [RBX], RAX
        
        # Store size (0) at offset 8
        self.asm.emit_mov_rax_imm64(0)
        self.asm.emit_bytes(0x48, 0x89, 0x43, 0x08)  # MOV [RBX+8], RAX
        
        # Return table pointer
        self.asm.emit_pop_rax()
        
        return True
    
    def compile_hash_function(self, node):
        """Compute hash of a string - DJB2 algorithm"""
        
        if not node.arguments:
            self.asm.emit_mov_rax_imm64(0)
            return True
        
        # Get string pointer
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_mov_rbx_rax()  # String in RBX
        
        # Initialize hash = 5381
        self.asm.emit_mov_rax_imm64(5381)
        
        # Hash loop: hash = hash * 33 + char
        hash_loop = self.asm.create_label()
        hash_done = self.asm.create_label()
        
        self.asm.mark_label(hash_loop)
        
        # Load byte
        self.asm.emit_bytes(0x0F, 0xB6, 0x13)  # MOVZX EDX, BYTE [RBX]
        
        # Check for null terminator
        self.asm.emit_bytes(0x84, 0xD2)  # TEST DL, DL
        self.asm.emit_jump_to_label(hash_done, "JZ")
        
        # hash = hash * 33
        self.asm.emit_bytes(0x48, 0x6B, 0xC0, 0x21)  # IMUL RAX, RAX, 33
        
        # hash = hash + char
        self.asm.emit_bytes(0x48, 0x01, 0xD0)  # ADD RAX, RDX
        
        # Next character
        self.asm.emit_bytes(0x48, 0xFF, 0xC3)  # INC RBX
        self.asm.emit_jump_to_label(hash_loop, "JMP")
        
        self.asm.mark_label(hash_done)
        
        # RAX contains hash value
        return True
    
    def compile_hash_set(self, node):
        """Set a key-value pair in hash table"""
        
        # Arguments: table, key, value
        if len(node.arguments) < 3:
            print("ERROR: HashSet requires 3 arguments")
            self.asm.emit_mov_rax_imm64(0)
            return True
        
        print(f"DEBUG: Compiling HashSet operation")
        
        # Save all registers we'll use
        self.asm.emit_push_rbx()
        self.asm.emit_push_rcx()
        self.asm.emit_push_rdx()
        self.asm.emit_push_rsi()
        self.asm.emit_push_rdi()
        
        # Get value first (compile it before we modify other registers)
        self.compiler.compile_expression(node.arguments[2])
        self.asm.emit_push_rax()  # Save value on stack
        
        # Get key pointer
        self.compiler.compile_expression(node.arguments[1])
        self.asm.emit_push_rax()  # Save key pointer
        
        # Get table pointer
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_mov_rdi_rax()  # Table in RDI
        
        # Get key back for hashing
        self.asm.emit_pop_rbx()  # Key in RBX
        self.asm.emit_push_rbx()  # Keep key for later
        
        # Hash the key (inline DJB2)
        self.asm.emit_mov_rax_imm64(5381)  # Initialize hash
        
        hash_loop = self.asm.create_label()
        hash_done = self.asm.create_label()
        
        self.asm.mark_label(hash_loop)
        self.asm.emit_bytes(0x0F, 0xB6, 0x13)  # MOVZX EDX, BYTE [RBX]
        self.asm.emit_bytes(0x84, 0xD2)  # TEST DL, DL
        self.asm.emit_jump_to_label(hash_done, "JZ")
        self.asm.emit_bytes(0x48, 0x6B, 0xC0, 0x21)  # IMUL RAX, RAX, 33
        self.asm.emit_bytes(0x48, 0x01, 0xD0)  # ADD RAX, RDX
        self.asm.emit_bytes(0x48, 0xFF, 0xC3)  # INC RBX
        self.asm.emit_jump_to_label(hash_loop, "JMP")
        self.asm.mark_label(hash_done)
        
        # Calculate bucket index
        # Get bucket count from table header
        self.asm.emit_bytes(0x48, 0x8B, 0x0F)  # MOV RCX, [RDI] (bucket count)
        
        # bucket = hash % bucket_count
        self.asm.emit_bytes(0x48, 0x31, 0xD2)  # XOR RDX, RDX
        self.asm.emit_bytes(0x48, 0xF7, 0xF1)  # DIV RCX
        # RDX now contains bucket index
        
        # Calculate bucket address: table + 16 + (bucket * 24)
        self.asm.emit_bytes(0x48, 0x6B, 0xD2, 0x18)  # IMUL RDX, RDX, 24
        self.asm.emit_bytes(0x48, 0x8D, 0x5C, 0x17, 0x10)  # LEA RBX, [RDI+RDX+16]
        
        # Get key and value from stack
        self.asm.emit_pop_rsi()  # Key pointer in RSI
        self.asm.emit_pop_rax()  # Value in RAX
        
        # Store in bucket (simple overwrite for now)
        self.asm.emit_bytes(0x48, 0x89, 0x33)  # MOV [RBX], RSI (key ptr)
        self.asm.emit_bytes(0x48, 0x89, 0x43, 0x08)  # MOV [RBX+8], RAX (value)
        # Leave next pointer as 0
        
        print(f"DEBUG: HashSet stored value in bucket")
        
        # Return success
        self.asm.emit_mov_rax_imm64(1)
        
        # Restore registers
        self.asm.emit_pop_rdi()
        self.asm.emit_pop_rsi()
        self.asm.emit_pop_rdx()
        self.asm.emit_pop_rcx()
        self.asm.emit_pop_rbx()
        
        return True
    
    def compile_hash_get(self, node):
        """Get value by key from hash table"""
        
        # Arguments: table, key
        if len(node.arguments) < 2:
            print("ERROR: HashGet requires 2 arguments")
            self.asm.emit_mov_rax_imm64(0)
            return True
        
        print(f"DEBUG: Compiling HashGet operation")
        
        # Save registers
        self.asm.emit_push_rbx()
        self.asm.emit_push_rcx()
        self.asm.emit_push_rdx()
        self.asm.emit_push_rsi()
        self.asm.emit_push_rdi()
        
        # Get key pointer
        self.compiler.compile_expression(node.arguments[1])
        self.asm.emit_push_rax()  # Save key pointer
        
        # Get table pointer
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_mov_rdi_rax()  # Table in RDI
        
        # Get key for hashing
        self.asm.emit_pop_rbx()  # Key in RBX
        
        # Hash the key (inline DJB2)
        self.asm.emit_mov_rax_imm64(5381)  # Initialize hash
        
        hash_loop = self.asm.create_label()
        hash_done = self.asm.create_label()
        
        self.asm.mark_label(hash_loop)
        self.asm.emit_bytes(0x0F, 0xB6, 0x13)  # MOVZX EDX, BYTE [RBX]
        self.asm.emit_bytes(0x84, 0xD2)  # TEST DL, DL
        self.asm.emit_jump_to_label(hash_done, "JZ")
        self.asm.emit_bytes(0x48, 0x6B, 0xC0, 0x21)  # IMUL RAX, RAX, 33
        self.asm.emit_bytes(0x48, 0x01, 0xD0)  # ADD RAX, RDX
        self.asm.emit_bytes(0x48, 0xFF, 0xC3)  # INC RBX
        self.asm.emit_jump_to_label(hash_loop, "JMP")
        self.asm.mark_label(hash_done)
        
        # Calculate bucket index
        # Get bucket count from table header
        self.asm.emit_bytes(0x48, 0x8B, 0x0F)  # MOV RCX, [RDI]
        
        # bucket = hash % bucket_count
        self.asm.emit_bytes(0x48, 0x31, 0xD2)  # XOR RDX, RDX
        self.asm.emit_bytes(0x48, 0xF7, 0xF1)  # DIV RCX
        # RDX now contains bucket index
        
        # Calculate bucket address: table + 16 + (bucket * 24)
        self.asm.emit_bytes(0x48, 0x6B, 0xD2, 0x18)  # IMUL RDX, RDX, 24
        self.asm.emit_bytes(0x48, 0x8D, 0x5C, 0x17, 0x10)  # LEA RBX, [RDI+RDX+16]
        
        # Check if bucket has a key (simplified - no string comparison)
        self.asm.emit_bytes(0x48, 0x8B, 0x03)  # MOV RAX, [RBX] (key ptr)
        self.asm.emit_bytes(0x48, 0x85, 0xC0)  # TEST RAX, RAX
        
        not_found = self.asm.create_label()
        found = self.asm.create_label()
        
        self.asm.emit_jump_to_label(not_found, "JZ")
        
        # Found something - return value (no key comparison for now)
        self.asm.emit_bytes(0x48, 0x8B, 0x43, 0x08)  # MOV RAX, [RBX+8] (value)
        self.asm.emit_jump_to_label(found, "JMP")
        
        self.asm.mark_label(not_found)
        self.asm.emit_mov_rax_imm64(0)  # Return 0 if not found
        
        self.asm.mark_label(found)
        
        print(f"DEBUG: HashGet retrieved value from bucket")
        
        # Restore registers
        self.asm.emit_pop_rdi()
        self.asm.emit_pop_rsi()
        self.asm.emit_pop_rdx()
        self.asm.emit_pop_rcx()
        self.asm.emit_pop_rbx()
        
        return True
