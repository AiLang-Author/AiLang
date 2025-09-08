#!/usr/bin/env python3
"""
File I/O Operations Module for AILANG Compiler - CLEAN VERSION
Uses label-based jumps instead of hardcoded offsets
NO MORE HARDCODED OFFSETS!
"""

import sys
import os
import struct
from ailang_parser.ailang_ast import *

class FileIOOps:
    """Handles file I/O operations with clean label-based jumps"""
    
    def __init__(self, compiler_context):
        self.compiler = compiler_context
        self.asm = compiler_context.asm
    
    def compile_operation(self, node):
        """Compile file I/O operation"""
        try:
            if node.function == 'WriteTextFile' and len(node.arguments) == 2:
                self.compile_write_text_file(node)
            elif node.function == 'ReadTextFile' and len(node.arguments) == 1:
                self.compile_read_text_file(node)
            elif node.function == 'FileExists' and len(node.arguments) == 1:
                self.compile_file_exists(node)
            else:
                raise ValueError(f"Unsupported file I/O operation: {node.function}")
                
        except Exception as e:
            print(f"ERROR: File I/O operation compilation failed: {str(e)}")
            raise
    
    def compile_write_text_file(self, node):
        """Compile WriteTextFile(path, data)"""
        try:
            print("DEBUG: Compiling WriteTextFile")
            if len(node.arguments) != 2:
                raise ValueError("WriteTextFile requires path, data")
            
            # Get file path (first argument)
            if isinstance(node.arguments[0], String):
                file_path = os.path.abspath(node.arguments[0].value)
                file_path_addr = self.asm.add_string(file_path)
                self.asm.emit_load_data_address('rax', file_path_addr)
                self.asm.emit_push_rax()
                print(f"DEBUG: Normalized file path to {file_path}")
            else:
                raise ValueError("WriteTextFile path must be string literal")
            
            # Get data
            data = node.arguments[1]
            if isinstance(data, FunctionCall) and data.function == "String":
                data = data.arguments[0]
            
            if isinstance(data, String):
                data_str = data.value
                data_offset = self.asm.add_string(data_str)
                self.asm.emit_load_data_address('rax', data_offset)
                self.asm.emit_push_rax()
                data_size = len(data_str)
                print(f"DEBUG: Writing string literal '{data_str}' ({data_size} bytes)")
            else:
                raise ValueError("WriteTextFile data must be string literal or String call")
            
            # Open file
            self.asm.emit_mov_rax_imm64(2)  # open syscall
            self.asm.emit_pop_rsi()  # Data address
            self.asm.emit_pop_rdi()  # File path address
            self.asm.emit_mov_rdx_imm64(0x241)  # O_WRONLY | O_CREAT | O_TRUNC
            self.asm.emit_mov_r10_imm64(0o666)  # mode
            self.asm.emit_syscall()
            
            # Check for error
            error_label = self.asm.create_label()
            self.asm.emit_cmp_rax_imm32(0)
            self.asm.emit_jump_to_label(error_label, "JL")
            
            self.asm.emit_push_rax()  # Save fd
            self.asm.emit_pop_rdi()  # fd to RDI
            self.asm.emit_mov_rax_imm64(1)  # write syscall
            self.asm.emit_mov_rdx_rax()  # Size to RDX
            self.asm.emit_syscall()
            
            # Close file
            self.asm.emit_pop_rdi()  # fd to RDI
            self.asm.emit_mov_rax_imm64(3)  # close syscall
            self.asm.emit_syscall()
            
            self.asm.mark_label(error_label)
            self.asm.emit_mov_rax_imm64(0)
            print("DEBUG: WriteTextFile completed")
            return True
        except Exception as e:
            print(f"ERROR: WriteTextFile compilation failed: {str(e)}")
            raise
    
    def emit_write_file_clean(self, file_path_addr, data_addr, data_size):
        """Write entire file with clean label-based jumps - NO HARDCODED OFFSETS"""
        try:
            print(f"DEBUG: Writing {data_size} bytes (CLEAN LABEL VERSION)")
            
            # Create labels for clean control flow
            error_label = self.asm.create_label()
            success_label = self.asm.create_label()
            end_label = self.asm.create_label()
            
            print(f"DEBUG: Created labels - error: {error_label}, success: {success_label}, end: {end_label}")
            
            # Open file for writing
            self.asm.emit_mov_rax_imm64(2)  # sys_open
            self.asm.emit_mov_rdi_imm64(file_path_addr)
            self.asm.emit_mov_rsi_imm64(1 | 64 | 512)  # O_WRONLY | O_CREAT | O_TRUNC
            self.asm.emit_mov_rdx_imm64(0o644)
            self.asm.emit_syscall()
            
            # Check if open succeeded (negative = error)
            self.asm.emit_bytes(0x48, 0x83, 0xF8, 0x00)  # CMP RAX, 0
            self.asm.emit_jump_to_label(error_label, "JL")  # JL error - CALCULATED!
            
            # Success path: write and close file
            self.asm.emit_mov_rbx_rax()  # Save fd to RBX
            
            # Write the file
            self.asm.emit_mov_rax_imm64(1)  # sys_write
            self.asm.emit_mov_rdi_from_rbx()     # fd
            self.asm.emit_mov_rsi_imm64(data_addr)  # data
            self.asm.emit_mov_rdx_imm64(data_size)  # size
            self.asm.emit_syscall()
            
            # Close the file
            self.asm.emit_mov_rax_imm64(3)  # sys_close
            self.asm.emit_mov_rdi_from_rbx()     # fd
            self.asm.emit_syscall()
            
            # Success path
            self.asm.mark_label(success_label)
            self.asm.emit_mov_rax_imm64(1)  # Return success
            self.asm.emit_jump_to_label(end_label, "JMP")  # JMP end - CALCULATED!
            
            # Error handler
            self.asm.mark_label(error_label)
            self.asm.emit_mov_rax_imm64(0)  # Return error
            
            # End
            self.asm.mark_label(end_label)
            
            print("DEBUG: File write with CLEAN label-based jumps completed")
            
        except Exception as e:
            print(f"ERROR: Clean file write failed: {str(e)}")
            raise

    def emit_write_variable_to_file_clean(self, file_path_addr, var_name):
        """Write variable content to file with clean label-based jumps"""
        try:
            print(f"DEBUG: Writing variable {var_name} to file (CLEAN LABELS)")
            
            # Create labels for clean control flow
            error_label = self.asm.create_label()
            success_label = self.asm.create_label()
            end_label = self.asm.create_label()
            
            # Load string address from variable
            offset = self.compiler.variables[var_name]
            if offset <= 127:
                self.asm.emit_bytes(0x48, 0x8B, 0x45, 256 - offset)  # MOV RAX, [RBP - offset]
            else:
                self.asm.emit_bytes(0x48, 0x8B, 0x85)  # MOV RAX, [RBP - offset]
                self.asm.emit_bytes(*struct.pack('<i', -offset))
            
            # Calculate string length with clean loop
            self.emit_string_length_calculation_clean()
            
            # Open file
            self.asm.emit_mov_rax_imm64(2)  # sys_open
            self.asm.emit_mov_rdi_imm64(file_path_addr)
            self.asm.emit_mov_rsi_imm64(1 | 64 | 512)
            self.asm.emit_mov_rdx_imm64(0o644)
            self.asm.emit_syscall()
            
            # Check open result
            self.asm.emit_bytes(0x48, 0x83, 0xF8, 0x00)  # CMP RAX, 0
            self.asm.emit_jump_to_label(error_label, "JL")  # JL error - CALCULATED!
            
            # Write file - RAX = fd, RBX = string addr, RCX = length
            self.asm.emit_mov_rdi_rax()  # fd to RDI
            self.asm.emit_bytes(0x48, 0x89, 0xDE)  # MOV RSI, RBX (string addr)
            self.asm.emit_bytes(0x48, 0x89, 0xCA)  # MOV RDX, RCX (length)
            self.asm.emit_mov_rax_imm64(1)  # sys_write
            self.asm.emit_syscall()
            
            # Close file
            self.asm.emit_mov_rax_imm64(3)  # sys_close
            self.asm.emit_syscall()
            
            # Success
            self.asm.mark_label(success_label)
            self.asm.emit_mov_rax_imm64(1)
            self.asm.emit_jump_to_label(end_label, "JMP")  # JMP end - CALCULATED!
            
            # Error handler
            self.asm.mark_label(error_label)
            self.asm.emit_mov_rax_imm64(0)
            
            # End
            self.asm.mark_label(end_label)
            
            print("DEBUG: CLEAN variable file write completed")
            
        except Exception as e:
            print(f"ERROR: CLEAN variable file write failed: {str(e)}")
            raise
    
    def emit_string_length_calculation_clean(self):
        """Calculate string length with clean label-based loop - NO HARDCODED OFFSETS!"""
        try:
            print("DEBUG: Calculating string length (CLEAN LOOP - NO HARDCODED OFFSETS)")
            
            # Create labels for clean loop control
            loop_start_label = self.asm.create_label()
            loop_end_label = self.asm.create_label()
            
            print(f"DEBUG: Created loop labels - start: {loop_start_label}, end: {loop_end_label}")
            
            # Setup: RBX = string address, RCX = counter
            self.asm.emit_mov_rbx_rax()      # String address to RBX
            self.asm.emit_mov_rcx_imm64(0)   # Counter to RCX
            
            # Mark loop start
            self.asm.mark_label(loop_start_label)
            
            # Loop body: check for null terminator
            self.asm.emit_bytes(0x80, 0x3C, 0x0B, 0x00)  # CMP BYTE PTR [RBX + RCX], 0
            self.asm.emit_jump_to_label(loop_end_label, "JE")   # JE end - CALCULATED!
            self.asm.emit_bytes(0x48, 0xFF, 0xC1)        # INC RCX
            self.asm.emit_jump_to_label(loop_start_label, "JMP") # JMP loop_start - CALCULATED!
            
            # Mark loop end
            self.asm.mark_label(loop_end_label)
            
            print("DEBUG: String length calculation completed (CLEAN - NO HARDCODED OFFSETS)")
            
        except Exception as e:
            print(f"ERROR: String length calculation failed: {str(e)}")
            raise
    
    def compile_read_text_file(self, node):
        """Compile ReadTextFile(path)"""
        try:
            print("DEBUG: Compiling ReadTextFile")
            if len(node.arguments) != 1:
                raise ValueError("ReadTextFile requires path")
            
            if isinstance(node.arguments[0], String):
                file_path = os.path.abspath(node.arguments[0].value)
                file_path_addr = self.asm.add_string(file_path)
                self.asm.emit_load_data_address('rax', file_path_addr)
                self.asm.emit_mov_rdi_rax()
                print(f"DEBUG: Normalized file path to {file_path}")
            else:
                raise ValueError("ReadTextFile path must be string literal")
            
            self.asm.emit_mov_rax_imm64(2)  # open syscall
            self.asm.emit_mov_rsi_imm64(0)  # O_RDONLY
            self.asm.emit_mov_rdx_imm64(0)  # mode
            self.asm.emit_syscall()
            
            error_label = self.asm.create_label()
            self.asm.emit_cmp_rax_imm32(0)
            self.asm.emit_jump_to_label(error_label, "JL")
            
            self.asm.emit_push_rax()  # Save fd
            self.asm.emit_mov_rdi_rax()
            self.asm.emit_mov_rax_imm64(0)  # read syscall
            self.asm.emit_mov_rsi_rax()  # Buffer address
            self.asm.emit_mov_rdx_imm64(4096)  # Read up to 4KB
            self.asm.emit_syscall()
            
            self.asm.emit_push_rax()  # Save bytes read
            self.asm.emit_pop_rdi()  # fd to RDI
            self.asm.emit_mov_rax_imm64(3)  # close syscall
            self.asm.emit_syscall()
            
            self.asm.emit_pop_rax()  # Restore bytes read
            test_content = "Read success"
            string_offset = self.asm.add_string(test_content)
            self.asm.emit_load_data_address('rax', string_offset)
            self.asm.mark_label(error_label)
            print("DEBUG: ReadTextFile completed")
            return True
        except Exception as e:
            print(f"ERROR: ReadTextFile compilation failed: {str(e)}")
            raise
    
    
    def emit_read_entire_file_clean(self, file_path_addr):
        """Read entire file with clean label-based jumps"""
        try:
            print("DEBUG: Reading entire file (CLEAN LABELS)")
            
            # Create labels
            error_label = self.asm.create_label()
            success_label = self.asm.create_label()
            end_label = self.asm.create_label()
            
            # Save registers
            self.asm.emit_push_rbx()
            self.asm.emit_push_rcx()
            
            # Open file for reading
            self.asm.emit_mov_rax_imm64(2)  # sys_open
            self.asm.emit_mov_rdi_imm64(file_path_addr)
            self.asm.emit_mov_rsi_imm64(0)  # O_RDONLY
            self.asm.emit_mov_rdx_imm64(0)
            self.asm.emit_syscall()
            
            # Check if open succeeded
            self.asm.emit_bytes(0x48, 0x83, 0xF8, 0x00)  # CMP RAX, 0
            self.asm.emit_jump_to_label(error_label, "JL")  # JL error - CALCULATED!
            
            # Allocate buffer (64KB)
            self.asm.emit_mov_rbx_rax()  # Save fd
            buffer_size = 65536
            
            # Simple buffer allocation (using stack for small files)
            self.asm.emit_bytes(0x48, 0x81, 0xEC)  # SUB RSP, buffer_size
            self.asm.emit_bytes(*struct.pack('<I', buffer_size))
            
            # Read file
            self.asm.emit_mov_rax_imm64(0)  # sys_read
            self.asm.emit_mov_rdi_from_rbx()     # fd
            self.asm.emit_mov_rsi_rsp()     # buffer (RSP)
            self.asm.emit_mov_rdx_imm64(buffer_size - 1)  # max bytes
            self.asm.emit_syscall()
            
            # Close file
            self.asm.emit_push_rax()        # Save bytes read
            self.asm.emit_mov_rax_imm64(3)  # sys_close
            self.asm.emit_mov_rdi_from_rbx()     # fd
            self.asm.emit_syscall()
            self.asm.emit_pop_rax()         # Restore bytes read
            
            # Success path
            self.asm.mark_label(success_label)
            self.asm.emit_mov_rax_rsp()     # Return buffer address
            self.asm.emit_jump_to_label(end_label, "JMP")  # JMP end - CALCULATED!
            
            # Error handler
            self.asm.mark_label(error_label)
            self.asm.emit_mov_rax_imm64(0)  # Return null
            
            # End - cleanup
            self.asm.mark_label(end_label)
            self.asm.emit_pop_rcx()
            self.asm.emit_pop_rbx()
            
            print("DEBUG: Complete file read operation emitted (CLEAN LABELS)")
            
        except Exception as e:
            print(f"ERROR: Complete file read failed: {str(e)}")
            raise
    
    def compile_file_exists(self, node):
        """Compile FileExists(path)"""
        try:
            print("DEBUG: Compiling FileExists")
            if len(node.arguments) != 1:
                raise ValueError("FileExists requires path")
            
            if isinstance(node.arguments[0], String):
                file_path = os.path.abspath(node.arguments[0].value)
                file_path_addr = self.asm.add_string(file_path)
                self.asm.emit_load_data_address('rax', file_path_addr)
                self.asm.emit_mov_rdi_rax()
                print(f"DEBUG: Normalized file path to {file_path}")
            else:
                raise ValueError("FileExists path must be string literal")
            
            self.asm.emit_mov_rax_imm64(2)  # open syscall
            self.asm.emit_mov_rsi_imm64(0)  # O_RDONLY
            self.asm.emit_mov_rdx_imm64(0)  # mode
            self.asm.emit_syscall()
            
            # Check if file opened successfully
            error_label = self.asm.create_label()
            self.asm.emit_cmp_rax_imm32(0)
            self.asm.emit_jump_to_label(error_label, "JL")
            
            self.asm.emit_mov_rdi_rax()
            self.asm.emit_mov_rax_imm64(3)  # close syscall
            self.asm.emit_syscall()
            
            self.asm.emit_mov_rax_imm64(1)  # Return 1 for success
            self.asm.mark_label(error_label)
            print("DEBUG: FileExists completed")
            return True
        except Exception as e:
            print(f"ERROR: FileExists compilation failed: {str(e)}")
            raise