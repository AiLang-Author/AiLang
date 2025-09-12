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
        # Map function names to the methods that handle them
        op_map = {
            'WriteTextFile': self.compile_write_text_file,
            'ReadTextFile': self.compile_read_text_file,
            'FileExists': self.compile_file_exists,
        }

        # Look up the function in our map
        handler = op_map.get(node.function)

        if handler:
            # If we found a handler, call it and signal success to the main compiler
            handler(node)
            return True
        
        # If the function is not in our map, it's not our job.
        # Signal this by returning False.
        return False
    
    def compile_write_text_file(self, node):
        """SAFE v2 — WriteTextFile(path, data)
        Uses runtime RSI (data*) and R10 (size). No imm64 data_addr/data_size for variable cases.
        Guards NULL and zero-size writes.
        """
        try:
            print("DEBUG: Compiling WriteTextFile (SAFE v2)")
            if len(node.arguments) != 2:
                raise ValueError("WriteTextFile requires path, data")

            # ===== PATH ARG =====
            path_arg = node.arguments[0]
            if isinstance(path_arg, String):
                file_path = os.path.abspath(path_arg.value)
                file_path_addr = self.asm.add_string(file_path)
                self.asm.emit_load_data_address('rax', file_path_addr)  # RAX = path*
                print(f"DEBUG: Using literal path: {file_path}")
            elif isinstance(path_arg, Identifier):
                self.compiler.compile_expression(path_arg)              # RAX = path*
            elif isinstance(path_arg, FunctionCall):
                self.compiler.compile_expression(path_arg)              # RAX = path*
            else:
                raise ValueError("WriteTextFile path must be string, variable, or expression")
            self.asm.emit_push_rax()                                    # stack: [path*]

            # ===== DATA ARG =====
            data_arg = node.arguments[1]
            # Unwrap String(...) if present
            if isinstance(data_arg, FunctionCall) and data_arg.function == "String":
                data_arg = data_arg.arguments[0]

            if isinstance(data_arg, String):
                # Literal data → get pointer and length now
                data_str = data_arg.value
                data_off = self.asm.add_string(data_str)
                self.asm.emit_load_data_address('rax', data_off)        # RAX = data*
                self.asm.emit_push_rax()                                # stack: [path*][data*]
                self.asm.emit_mov_rax_imm64(len(data_str))              # RAX = size
                self.asm.emit_push_rax()                                # stack: [path*][data*][size]
                print(f"DEBUG: Literal data length {len(data_str)}")
            elif isinstance(data_arg, (Identifier, FunctionCall)):
                # Runtime string/bytes → pointer in RAX (maybe NULL)
                self.compiler.compile_expression(data_arg)              # RAX = data*
                self.asm.emit_push_rax()                                # stack: [path*][data*]
                # length = 0 if NULL else strlen(ptr)
                size_is_zero = self.asm.create_label()
                size_done    = self.asm.create_label()
                self.asm.emit_test_rax_rax()                            # NULL?
                self.asm.emit_jump_to_label(size_is_zero, "JZ")
                self.asm.emit_mov_rdi_rax()
                self._emit_strlen()                                     # RAX = strlen(data*)
                self.asm.emit_push_rax()                                # stack: [path*][data*][size]
                self.asm.emit_jump_to_label(size_done, "JMP")
                self.asm.mark_label(size_is_zero)
                self.asm.emit_mov_rax_imm64(0)                          # size = 0
                self.asm.emit_push_rax()
                self.asm.mark_label(size_done)
            else:
                raise ValueError("WriteTextFile data must be string, variable, or expression")

            # ===== OPEN & WRITE (NULL/zero-size safe) =====
            # Pop to R10 (size), RSI (data*), RDI (path*)
            self.asm.emit_pop_rax()     # size
            self.asm.emit_mov_r10_rax()
            self.asm.emit_pop_rsi()     # data*
            self.asm.emit_bytes(0x48, 0x89, 0xF3)  # MOV RBX, RSI (save data*)
            self.asm.emit_pop_rdi()     # path*

            # open(path, O_WRONLY|O_CREAT|O_TRUNC, 0666)
            self.asm.emit_mov_rax_imm64(2)         # sys_open
            self.asm.emit_mov_rsi_imm64(0x241)     # O_WRONLY|O_CREAT|O_TRUNC
            self.asm.emit_mov_rdx_imm64(0o666)     # mode
            self.asm.emit_syscall()

            error_label = self.asm.create_label()
            done_label  = self.asm.create_label()
            skip_write  = self.asm.create_label()

            # if (fd < 0) -> error
            self.asm.emit_cmp_rax_imm32(0)
            self.asm.emit_jump_to_label(error_label, "JL")

            # fd ok → maybe write
            self.asm.emit_mov_rdi_rax()            # RDI = fd
            self.asm.emit_bytes(0x48, 0x89, 0xDE)  # MOV RSI, RBX (restore data*)
            self.asm.emit_test_rsi_rsi()           # if (data* == NULL) skip
            self.asm.emit_jump_to_label(skip_write, "JZ")
            self.asm.emit_test_r10_r10()           # if (size == 0) skip
            self.asm.emit_jump_to_label(skip_write, "JZ")

            # write(fd, data*, size) — guarded
            self.asm.emit_push_rdi()               # save fd
            self.asm.emit_mov_rdx_r10()            # RDX = size
            self.asm.emit_write_guarded_rdi_rsi_size_in_rdx()
            self.asm.emit_pop_rdi()                # restore fd


            # close(fd)
            self.asm.emit_mov_rax_imm64(3)         # sys_close
            self.asm.emit_syscall()
            self.asm.emit_mov_rax_imm64(1)         # ret = 1
            self.asm.emit_jump_to_label(done_label, "JMP")

            # skip write but still close
            self.asm.mark_label(skip_write)
            self.asm.emit_mov_rax_imm64(3)         # sys_close
            self.asm.emit_syscall()
            self.asm.emit_mov_rax_imm64(1)         # ret = 1
            self.asm.emit_jump_to_label(done_label, "JMP")

            # error path
            self.asm.mark_label(error_label)
            self.asm.emit_mov_rax_imm64(0)

            self.asm.mark_label(done_label)
            print("DEBUG: WriteTextFile (SAFE v2) completed")
            return True

        except Exception as e:
            print(f"ERROR: WriteTextFile (SAFE v2) failed: {str(e)}")
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
            
            # Write the file — guarded
            self.asm.emit_mov_rdi_from_rbx()          # fd
            self.asm.emit_mov_rsi_imm64(data_addr)    # data
            self.asm.emit_mov_rdx_imm64(data_size)    # size
            self.asm.emit_write_guarded_rdi_rsi_size_in_rdx()
            
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
            self.asm.emit_write_guarded_rdi_rsi_size_in_rdx()

            
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
        """Compile ReadTextFile function"""
        if not node.arguments or len(node.arguments) != 1:
            raise ValueError("ReadTextFile requires exactly one argument")
        
        path_arg = node.arguments[0]

        # === Resolve path into RSI ===
        if hasattr(path_arg, 'type') and path_arg.type == 'String':
            string_offset = self.compiler.memory.allocate_string(path_arg.value)
            self.compiler.emit_lea_rsi_string(string_offset)
        elif hasattr(path_arg, 'type') and path_arg.type == 'Identifier':
            var_info = self.compiler.memory.get_variable(path_arg.name)
            if not var_info:
                raise ValueError(f"Undefined variable: {path_arg.name}")
            if var_info['type'] != 'text':
                raise ValueError(f"ReadTextFile requires text variable, got {var_info['type']}")
            self.compiler.asm.emit_mov_rsi_mem(var_info['offset'])
            self.compiler.asm.emit_test_rsi_rsi()
            skip_label = self.compiler.asm.create_label()
            self.compiler.asm.emit_jump_to_label(skip_label, "JNZ")
            empty_offset = self.compiler.memory.allocate_string("")
            self.compiler.emit_lea_rsi_string(empty_offset)
            self.compiler.asm.mark_label(skip_label)
        else:
            self.compiler.compile_expression(path_arg)
            self.compiler.asm.emit_pop_rsi()

        # Save RBX (callee-saved) — we'll use it for file size only
        self.compiler.asm.emit_push_rbx()

        # === open(path, O_RDONLY, 0) ===
        self.compiler.asm.emit_mov_rdi_rsi()   # RDI = path
        self.compiler.asm.emit_push_rax()
        self.compiler.asm.emit_xor_eax_eax()   # RAX=0
        self.compiler.asm.emit_mov_rsi_rax()   # RSI=0 (O_RDONLY)
        self.compiler.asm.emit_xor_edx_edx()   # RDX=0 (mode)
        self.compiler.asm.emit_pop_rax()
        self.compiler.asm.emit_mov_rax_imm64(2)  # sys_open

        error_open = self.compiler.asm.create_label()
        end_label  = self.compiler.asm.create_label()

        self.compiler.asm.emit_test_rax_rax()
        self.compiler.asm.emit_jump_to_label(error_open, "JL")  # fd < 0 → error

        # Save fd on stack
        self.compiler.asm.emit_push_rax()

        # === fstat(fd,&st) to get size ===
        self.compiler.asm.emit_mov_rdi_rax()
        self.compiler.asm.emit_sub_rsp_imm32(144)  # struct stat
        self.compiler.asm.emit_mov_rsi_rsp()
        self.compiler.asm.emit_mov_rax_imm64(5)    # sys_fstat
        self.compiler.asm.emit_syscall()

        # size = st.st_size (offset 48)
        self.compiler.asm.emit_mov_rax_mem_offset('RSP', 48)
        self.compiler.asm.emit_mov_rbx_rax()       # RBX = size
        self.compiler.asm.emit_add_rsp_imm32(144)

        # === mmap(size+1) ===
        self.compiler.asm.emit_mov_rdi_rbx()       # RDI = size
        self.compiler.asm.emit_inc_rdi()           # +1 for '\0'
        self.compiler.asm.emit_push_rbx()          # save size (to be discarded)
        self.compiler.asm.emit_push_rdi()          # save size+1 (to be discarded)
        self.compiler.asm.emit_mov_rsi_rdi()       # RSI = length
        self.compiler.asm.emit_xor_edi_edi()       # addr = NULL
        self.compiler.asm.emit_mov_rdx_imm64(3)    # PROT_READ|WRITE
        self.compiler.asm.emit_mov_r10_imm64(0x22) # MAP_PRIVATE|ANON
        self.compiler.asm.emit_mov_r8_imm64(0xFFFFFFFFFFFFFFFF)  # fd=-1
        self.compiler.asm.emit_xor_r9_r9()         # offset=0
        self.compiler.asm.emit_mov_rax_imm64(9)    # sys_mmap
        self.compiler.asm.emit_syscall()

        # Arrange stack for read(): want [dummy][buffer][fd]
        # so [RSP+8]=buffer and [RSP+16]=fd for your read sequence.
        self.compiler.asm.emit_mov_rcx_rax()  # RCX = buffer
        self.compiler.asm.emit_pop_rax()      # discard size+1
        self.compiler.asm.emit_pop_rax()      # discard original size
        # stack currently: [fd]
        self.compiler.asm.emit_push_rcx()     # push buffer   -> RSP=buffer, RSP+8=fd

        # push a ZERO as the dummy (do NOT use RBX here, avoid double-using RBX on stack)
        self.compiler.asm.emit_push_rax()     # save RAX
        self.compiler.asm.emit_xor_eax_eax()  # RAX = 0
        self.compiler.asm.emit_push_rax()     # push dummy=0  -> RSP=dummy, RSP+8=buffer, RSP+16=fd
        self.compiler.asm.emit_pop_rax()      # restore RAX (so we didn't clobber it for later)

        # === read(fd, buffer, size) ===
        self.compiler.asm.emit_mov_rsi_mem_offset('RSP', 8)   # buffer
        self.compiler.asm.emit_mov_rdx_rbx()                  # size (requested)
        self.compiler.asm.emit_mov_rdi_mem_offset('RSP', 16)  # fd
        self.compiler.asm.emit_xor_eax_eax()                  # sys_read (0)
        self.compiler.asm.emit_syscall()                      # RAX = bytes_read (or <0)

        read_ok = self.compiler.asm.create_label()
        self.compiler.asm.emit_test_rax_rax()
        self.compiler.asm.emit_jump_to_label(read_ok, "JGE")

        # read error: clean and return 0
        self.compiler.asm.emit_add_rsp_imm32(24)  # drop [dummy][buffer][fd]
        self.compiler.asm.emit_xor_eax_eax()      # RAX = 0
        self.compiler.asm.emit_pop_rbx()          # restore caller's RBX
        self.compiler.asm.emit_push_rax()
        self.compiler.asm.emit_jump_to_label(end_label, "JMP")

        self.compiler.asm.mark_label(read_ok)

        # === close(fd) ===
        self.compiler.asm.emit_mov_rdi_mem_offset('RSP', 16)
        self.compiler.asm.emit_mov_rax_imm64(3)   # sys_close
        self.compiler.asm.emit_syscall()

        # === null-terminate at bytes_read ===
        # stack: [dummy][buffer][fd]
        self.compiler.asm.emit_mov_rcx_rax()      # RCX = bytes_read
        self.compiler.asm.emit_add_rsp_imm32(8)   # drop dummy -> [buffer][fd]
        self.compiler.asm.emit_pop_rax()          # RAX = buffer; stack -> [fd]
        self.compiler.asm.emit_mov_byte_mem_offset_imm('RAX', 'RCX', 0)  # *(buffer + bytes_read) = 0

        # cleanup + return buffer in RAX
        self.compiler.asm.emit_add_rsp_imm32(8)   # drop [fd]
        self.compiler.asm.emit_pop_rbx()          # restore caller's RBX
        self.compiler.asm.emit_push_rax()
        self.compiler.asm.emit_jump_to_label(end_label, "JMP")

        # === open() error path ===
        self.compiler.asm.mark_label(error_open)
        self.compiler.asm.emit_xor_eax_eax()      # RAX = 0
        self.compiler.asm.emit_pop_rbx()          # restore caller's RBX
        self.compiler.asm.emit_push_rax()

        self.compiler.asm.mark_label(end_label)






    
    
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
        """Compile FileExists(path) - accepts both literals and variables"""
        try:
            print("DEBUG: Compiling FileExists")
            if len(node.arguments) != 1:
                raise ValueError("FileExists requires path")
            
            path_arg = node.arguments[0]
            
            if isinstance(path_arg, String):
                # String literal path
                file_path = os.path.abspath(path_arg.value)
                file_path_addr = self.asm.add_string(file_path)
                self.asm.emit_load_data_address('rax', file_path_addr)
                print(f"DEBUG: Checking literal path: {file_path}")
            elif isinstance(path_arg, Identifier):
                # Variable containing path
                print(f"DEBUG: Checking variable path: {path_arg.name}")
                self.compiler.compile_expression(path_arg)
            elif isinstance(path_arg, FunctionCall):
                # Result of function call
                print("DEBUG: Checking function result path")
                self.compiler.compile_expression(path_arg)
            else:
                raise ValueError(f"FileExists path must be string, variable, or expression")
            
            self.asm.emit_mov_rdi_rax()  # Path to RDI
            
            # Try to open file
            self.asm.emit_mov_rax_imm64(2)  # open syscall
            self.asm.emit_mov_rsi_imm64(0)  # O_RDONLY
            self.asm.emit_mov_rdx_imm64(0)  # mode
            self.asm.emit_syscall()
            
            # Check if file opened successfully
            error_label = self.asm.create_label()
            done_label = self.asm.create_label()
            
            self.asm.emit_cmp_rax_imm32(0)
            self.asm.emit_jump_to_label(error_label, "JL")
            
            # File opened, close it
            self.asm.emit_mov_rdi_rax()
            self.asm.emit_mov_rax_imm64(3)  # close syscall
            self.asm.emit_syscall()
            
            self.asm.emit_mov_rax_imm64(1)  # Return 1 (exists)
            self.asm.emit_jump_to_label(done_label, "JMP")
            
            self.asm.mark_label(error_label)
            self.asm.emit_mov_rax_imm64(0)  # Return 0 (doesn't exist)
            
            self.asm.mark_label(done_label)
            print("DEBUG: FileExists completed")
            return True
            
        except Exception as e:
            print(f"ERROR: FileExists compilation failed: {str(e)}")
            raise

    def _emit_strlen(self):
        """
        RDI = ptr
        Returns length in RAX. NULL-safe: if RDI==0, returns 0.
        """
        # rcx = 0
        self.asm.emit_mov_rcx_imm64(0)

        # if (rdi==0) goto len_zero
        len_zero = self.asm.create_label()
        len_loop = self.asm.create_label()
        len_done = self.asm.create_label()

        # TEST RDI,RDI
        self.asm.emit_bytes(0x48, 0x85, 0xFF)
        self.asm.emit_jump_to_label(len_zero, "JZ")

        # loop: while (*(rdi+rcx) != 0) rcx++
        self.asm.mark_label(len_loop)
        # CMP BYTE PTR [RDI+RCX], 0
        self.asm.emit_bytes(0x80, 0x3C, 0x0F, 0x00)
        self.asm.emit_jump_to_label(len_done, "JE")
        # INC RCX
        self.asm.emit_bytes(0x48, 0xFF, 0xC1)
        self.asm.emit_jump_to_label(len_loop, "JMP")

        # rcx = 0 for NULL
        self.asm.mark_label(len_zero)
        self.asm.emit_mov_rcx_imm64(0)

        # done: rax = rcx
        self.asm.mark_label(len_done)
        self.asm.emit_bytes(0x48, 0x89, 0xC8)  # MOV RAX, RCX
