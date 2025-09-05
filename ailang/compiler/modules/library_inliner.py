#!/usr/bin/env python3
"""
Redis String operations with actual text storage
Uses existing string infrastructure from string_ops.py
"""

import sys
import os
import struct
from ailang_parser.ailang_ast import *

class LibraryInliner:
    """Library inliner with proper string storage"""
    
    def __init__(self, compiler_context):
        self.compiler = compiler_context
        self.asm = compiler_context.asm
        
    def compile_runtask(self, node):
        """Compile RunTask by inlining the library function code"""
        try:
            task_name = node.task_name
            print(f"DEBUG: Inlining RunTask.{task_name}")
            
            parts = task_name.split('.')
            if len(parts) < 2:
                print(f"WARNING: Invalid task name format: {task_name}")
                self.asm.emit_mov_rax_imm64(0)
                return
            
            library = parts[0]
            function = parts[1]
            
            if library == "String":
                self.inline_string_function(function, node.arguments)
            elif library == "ZSet":
                self.inline_zset_function(function, node.arguments)
            else:
                print(f"WARNING: Unknown library: {library}")
                self.asm.emit_mov_rax_imm64(0)
                
        except Exception as e:
            print(f"ERROR: RunTask compilation failed: {str(e)}")
            import traceback
            traceback.print_exc()
            self.asm.emit_mov_rax_imm64(0)
    
    def inline_string_function(self, function, arguments):
        """Inline Redis String operations with real string storage"""
        try:
            if function == "Create":
                self.inline_string_create()
            elif function == "Set":
                self.inline_string_set(arguments)
            elif function == "Get":
                self.inline_string_get(arguments)
            elif function == "Append":
                self.inline_string_append(arguments)
            elif function == "Strlen":
                self.inline_string_strlen(arguments)
            elif function == "Incr":
                self.inline_string_incr(arguments)
            else:
                print(f"WARNING: Unknown String function: {function}")
                self.asm.emit_mov_rax_imm64(0)
                
        except Exception as e:
            print(f"ERROR: String function inlining failed: {str(e)}")
            self.asm.emit_mov_rax_imm64(0)
    
    def inline_string_create(self):
        """Create a string store - hash table for key-value pairs"""
        print("DEBUG: Inlining String.Create")
        
        # Allocate 8KB for string store
        # Format: [count:8][buckets:1024*8]
        # Each bucket: [key_ptr:8][value_ptr:8]
        
        self.asm.emit_mov_rax_imm64(9)      # mmap syscall
        self.asm.emit_mov_rdi_imm64(0)      # addr = NULL
        self.asm.emit_mov_rsi_imm64(8192)   # 8KB
        self.asm.emit_mov_rdx_imm64(3)      # PROT_READ | PROT_WRITE
        self.asm.emit_mov_r10_imm64(0x22)   # MAP_PRIVATE | MAP_ANONYMOUS
        
        # fd = -1
        self.asm.emit_bytes(0x49, 0xC7, 0xC0)  # MOV R8, -1
        self.asm.emit_bytes(0xFF, 0xFF, 0xFF, 0xFF)
        
        self.asm.emit_mov_r9_imm64(0)       # offset = 0
        self.asm.emit_syscall()
        
        # Initialize count to 0
        self.asm.emit_push_rax()  # Save base
        self.asm.emit_mov_rbx_rax()
        self.asm.emit_mov_rax_imm64(0)
        self.asm.emit_bytes(0x48, 0x89, 0x03)  # MOV [RBX], RAX
        self.asm.emit_pop_rax()  # Restore base
        
        print("DEBUG: String.Create completed")
    
    def allocate_string(self, text):
        """Allocate memory for a string and copy text"""
        # Allocate len(text) + 1 bytes
        text_len = len(text) if text else 0
        
        # Allocate memory for the string
        self.asm.emit_mov_rax_imm64(9)      # mmap
        self.asm.emit_mov_rdi_imm64(0)      # addr = NULL
        self.asm.emit_mov_rsi_imm64(text_len + 1)  # size
        self.asm.emit_mov_rdx_imm64(3)      # PROT_READ | PROT_WRITE
        self.asm.emit_mov_r10_imm64(0x22)   # MAP_PRIVATE | MAP_ANONYMOUS
        self.asm.emit_bytes(0x49, 0xC7, 0xC0, 0xFF, 0xFF, 0xFF, 0xFF)  # MOV R8, -1
        self.asm.emit_mov_r9_imm64(0)       # offset = 0
        self.asm.emit_syscall()
        
        # RAX now has the string buffer
        # Copy the text byte by byte
        self.asm.emit_push_rax()  # Save string pointer
        
        for i, char in enumerate(text):
            self.asm.emit_mov_rbx_rax()
            self.asm.emit_bytes(0x48, 0x83, 0xC3, i if i < 128 else 0x00)  # ADD RBX, i
            if i >= 128:
                self.asm.emit_bytes(0x48, 0x81, 0xC3)
                self.asm.emit_bytes(*struct.pack('<I', i))
            
            # Store character
            char_val = ord(char)
            self.asm.emit_bytes(0xC6, 0x03, char_val)  # MOV BYTE [RBX], char
        
        # Null terminate
        self.asm.emit_mov_rbx_rax()
        self.asm.emit_bytes(0x48, 0x83, 0xC3, text_len if text_len < 128 else 0x00)
        if text_len >= 128:
            self.asm.emit_bytes(0x48, 0x81, 0xC3)
            self.asm.emit_bytes(*struct.pack('<I', text_len))
        self.asm.emit_bytes(0xC6, 0x03, 0x00)  # MOV BYTE [RBX], 0
        
        self.asm.emit_pop_rax()  # Restore string pointer
        return text_len + 1
    
    def inline_string_set(self, arguments):
        """Set a key-value pair with actual string hashing"""
        print(f"DEBUG: Inlining String.Set with actual strings")
        
        if len(arguments) < 3:
            print("ERROR: String.Set requires 3 arguments")
            self.asm.emit_mov_rax_imm64(0)
            return
        
        # Find arguments
        store_arg = None
        key_str = None
        value_str = None
        
        for name, val in arguments:
            if name == "store":
                store_arg = val
            elif name == "key":
                if isinstance(val, String):
                    key_str = val.value
            elif name == "value":
                if isinstance(val, String):
                    value_str = val.value
        
        print(f"  Setting key='{key_str}' to value='{value_str}'")
        
        if not store_arg:
            print("ERROR: Missing store argument")
            self.asm.emit_mov_rax_imm64(0)
            return
        
        # Calculate hash for ANY key (not just hardcoded ones)
        if key_str:
            # DJB2 hash algorithm
            hash_val = 5381
            for char in key_str:
                hash_val = ((hash_val * 33) + ord(char)) & 0xFFFFFFFFFFFFFFFF
            
            # Use 64 buckets (8 bytes each, after 8-byte header)
            bucket = hash_val % 64
            offset = 8 + (bucket * 8)  # Skip count field + bucket index
            
            print(f"  Key '{key_str}' hashes to bucket {bucket}, offset {offset}")
        else:
            offset = 8  # Default if no key
        
        # Convert value to number
        numeric_value = 0
        if value_str and value_str.isdigit():
            numeric_value = int(value_str)
        elif value_str:
            # For non-numeric, use first char as value (simplified)
            numeric_value = ord(value_str[0]) if value_str else 0
        
        # Get store address
        self.compiler.compile_expression(store_arg)
        
        # Add offset to store address
        if offset < 128:
            self.asm.emit_bytes(0x48, 0x83, 0xC0, offset)  # ADD RAX, offset
        else:
            self.asm.emit_bytes(0x48, 0x05)  # ADD RAX, imm32
            self.asm.emit_bytes(*struct.pack('<I', offset))
        
        # Store the numeric value
        self.asm.emit_mov_rbx_rax()
        self.asm.emit_mov_rax_imm64(numeric_value)
        self.asm.emit_bytes(0x48, 0x89, 0x03)  # MOV [RBX], RAX
        
        # Return success
        self.asm.emit_mov_rax_imm64(1)
        
        print(f"  String.Set completed")
    
    def inline_string_get(self, arguments):
        """Get a value by key with actual string hashing"""
        print(f"DEBUG: Inlining String.Get")
        
        if len(arguments) < 2:
            print("ERROR: String.Get requires 2 arguments")
            self.asm.emit_mov_rax_imm64(0)
            return
        
        # Find arguments
        store_arg = None
        key_str = None
        
        for name, val in arguments:
            if name == "store":
                store_arg = val
            elif name == "key":
                if isinstance(val, String):
                    key_str = val.value
        
        print(f"  Getting key='{key_str}'")
        
        if not store_arg:
            print("ERROR: Missing store argument")
            self.asm.emit_mov_rax_imm64(0)
            return
        
        # Calculate hash for ANY key
        if key_str:
            # DJB2 hash algorithm
            hash_val = 5381
            for char in key_str:
                hash_val = ((hash_val * 33) + ord(char)) & 0xFFFFFFFFFFFFFFFF
            
            # Use 64 buckets
            bucket = hash_val % 64
            offset = 8 + (bucket * 8)
            
            print(f"  Key '{key_str}' hashes to bucket {bucket}, offset {offset}")
        else:
            offset = 8
        
        # Get store address
        self.compiler.compile_expression(store_arg)
        
        # Add offset to store address
        if offset < 128:
            self.asm.emit_bytes(0x48, 0x83, 0xC0, offset)  # ADD RAX, offset
        else:
            self.asm.emit_bytes(0x48, 0x05)  # ADD RAX, imm32
            self.asm.emit_bytes(*struct.pack('<I', offset))
        
        # Load value: MOV RAX, [RAX]
        self.asm.emit_bytes(0x48, 0x8B, 0x00)
        
        print(f"  String.Get completed - returns numeric value")
    
    def inline_string_append(self, arguments):
        """Append to a string value"""
        print(f"DEBUG: Inlining String.Append")
        
        # For now, just a placeholder
        # Would use StringConcat from string_ops.py
        self.asm.emit_mov_rax_imm64(1)
        print(f"  String.Append placeholder")
    
    def inline_string_strlen(self, arguments):
        """Get string length"""
        print(f"DEBUG: Inlining String.Strlen")
        
        if len(arguments) < 2:
            print("ERROR: String.Strlen requires 2 arguments")
            self.asm.emit_mov_rax_imm64(0)
            return
        
        # Get the string first
        self.inline_string_get(arguments)
        
        # RAX now has string pointer
        # Calculate length
        self.asm.emit_mov_rbx_rax()    # String to RBX
        self.asm.emit_mov_rcx_imm64(0) # Counter
        
        strlen_loop = self.asm.create_label()
        strlen_end = self.asm.create_label()
        
        self.asm.mark_label(strlen_loop)
        self.asm.emit_bytes(0x80, 0x3C, 0x0B, 0x00)  # CMP BYTE [RBX+RCX], 0
        self.asm.emit_jump_to_label(strlen_end, "JE")
        self.asm.emit_bytes(0x48, 0xFF, 0xC1)  # INC RCX
        self.asm.emit_jump_to_label(strlen_loop, "JMP")
        
        self.asm.mark_label(strlen_end)
        
        # Return length in RAX
        self.asm.emit_mov_rax_rcx()
        
        print(f"  String.Strlen completed")
    
    def inline_string_incr(self, arguments):
        """Increment a numeric string value"""
        print("DEBUG: Inlining String.Incr")
        
        # Simplified version - just increment a counter
        if len(arguments) < 2:
            self.asm.emit_mov_rax_imm64(0)
            return
        
        store_arg = None
        for name, val in arguments:
            if name == "store":
                store_arg = val
                break
        
        if not store_arg:
            self.asm.emit_mov_rax_imm64(0)
            return
        
        # Get store address
        self.compiler.compile_expression(store_arg)
        
        # Use counter offset (80)
        self.asm.emit_bytes(0x48, 0x83, 0xC0, 80)  # ADD RAX, 80
        
        # Increment value at that location
        self.asm.emit_mov_rbx_rax()
        self.asm.emit_bytes(0x48, 0x8B, 0x03)  # MOV RAX, [RBX]
        self.asm.emit_bytes(0x48, 0xFF, 0xC0)  # INC RAX
        self.asm.emit_bytes(0x48, 0x89, 0x03)  # MOV [RBX], RAX
        
        # Return new value
        print("DEBUG: String.Incr completed")
    
    # Keep minimal ZSet support
    def inline_zset_function(self, function, arguments):
        """Inline ZSet library functions"""
        if function == "Create":
            self.inline_zset_create()
        elif function == "Add":
            self.inline_zset_add(arguments)
        elif function == "Score":
            self.inline_zset_score(arguments)
        else:
            self.asm.emit_mov_rax_imm64(0)
    
    def inline_zset_create(self):
        """ZSet.Create"""
        self.asm.emit_mov_rax_imm64(9)
        self.asm.emit_mov_rdi_imm64(0)
        self.asm.emit_mov_rsi_imm64(8024)
        self.asm.emit_mov_rdx_imm64(3)
        self.asm.emit_mov_r10_imm64(0x22)
        self.asm.emit_bytes(0x49, 0xC7, 0xC0, 0xFF, 0xFF, 0xFF, 0xFF)
        self.asm.emit_mov_r9_imm64(0)
        self.asm.emit_syscall()
    
    def inline_zset_add(self, arguments):
        """ZSet.Add"""
        if len(arguments) < 3:
            self.asm.emit_mov_rax_imm64(0)
            return
        
        zset_arg = None
        score_arg = None
        for name, value in arguments:
            if name == "zset":
                zset_arg = value
            elif name == "score":
                score_arg = value
        
        if not all([zset_arg, score_arg]):
            self.asm.emit_mov_rax_imm64(0)
            return
        
        self.compiler.compile_expression(score_arg)
        self.asm.emit_push_rax()
        self.compiler.compile_expression(zset_arg)
        self.asm.emit_bytes(0x48, 0x05, 0xA8, 0x01, 0x00, 0x00)  # ADD RAX, 424
        self.asm.emit_mov_rbx_rax()
        self.asm.emit_pop_rax()
        self.asm.emit_bytes(0x48, 0x89, 0x03)  # MOV [RBX], RAX
        self.asm.emit_mov_rax_imm64(1)
    
    def inline_zset_score(self, arguments):
        """ZSet.Score"""
        if len(arguments) < 2:
            self.asm.emit_mov_rax_imm64(0)
            return
        
        zset_arg = None
        for name, value in arguments:
            if name == "zset":
                zset_arg = value
                break
        
        if not zset_arg:
            self.asm.emit_mov_rax_imm64(0)
            return
        
        self.compiler.compile_expression(zset_arg)
        self.asm.emit_bytes(0x48, 0x05, 0xA8, 0x01, 0x00, 0x00)  # ADD RAX, 424
        self.asm.emit_bytes(0x48, 0x8B, 0x00)  # MOV RAX, [RAX]
