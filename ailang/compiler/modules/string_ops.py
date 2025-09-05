"""
String Operations Module for AILANG Compiler
Handles string printing and manipulation operations
FIXED: Proper string address dereferencing for ReadTextFile
"""

import sys
import os
import struct
from ailang_parser.ailang_ast import *

class StringOps:
    """Handles string-related operations"""
    
    def __init__(self, compiler_context):
        self.compiler = compiler_context
        self.asm = compiler_context.asm
    
    def compile_print_message(self, node):
        """Compile PrintMessage with proper string address handling"""
        try:
            print(f"DEBUG: Compiling PrintMessage, message type: {type(node.message).__name__}")
            
            if isinstance(node.message, String):
                self.asm.emit_print_string(node.message.value)
                print(f"DEBUG: Printed string: {node.message.value}")
                
            elif isinstance(node.message, Number):
                self.asm.emit_mov_rax_imm64(int(node.message.value))
                print(f"DEBUG: Loading number {node.message.value} for printing")
                self.asm.emit_print_number()
                print("DEBUG: Number printing completed")
                
            elif isinstance(node.message, Identifier):
                # Resolve any acronyms before checking variables
                resolved_name = self.compiler.resolve_acronym_identifier(node.message.name)
                
                if resolved_name in self.compiler.variables:
                    offset = self.compiler.variables[resolved_name]
                    print(f"DEBUG: Printing variable {resolved_name} (from {node.message.name}) from [RBP - {offset}]")
                    
                    # Load the value into RAX
                    if offset <= 127:
                        self.asm.emit_bytes(0x48, 0x8B, 0x45, 256 - offset)
                    else:
                        self.asm.emit_bytes(0x48, 0x8B, 0x85)
                        self.asm.emit_bytes(*struct.pack('<i', -offset))
                    
                    print(f"DEBUG: Loaded variable {node.message.name} for printing")
                    
                    # Use smart printing with proper jump resolution
                    self.emit_smart_print_with_jumps()
                    print("DEBUG: Smart variable printing completed")
                else:
                    raise ValueError(f"Undefined variable: {node.message.name} (resolved to: {resolved_name})")
                    
            elif isinstance(node.message, FunctionCall):
                print("DEBUG: Printing result of function call")
                self.compiler.compile_function_call(node.message)
                
                # Function calls might return strings or numbers
                self.emit_smart_print_with_jumps()
                print("DEBUG: Function call printing completed")
                
            else:
                raise ValueError(f"Unsupported PrintMessage type: {type(node.message)}")
                
        except Exception as e:
            print(f"ERROR: PrintMessage compilation failed: {str(e)}")
            raise
    
    def emit_smart_print_with_jumps(self):
        """Smart print - check if value is likely a string address or number"""
        try:
            print("DEBUG: Starting smart print with jump system")
            
            # Create labels using the jump system
            string_label = self.asm.create_label()
            number_label = self.asm.create_label()
            end_label = self.asm.create_label()
            
            # Strategy: Check if the value is in reasonable address ranges
            # Numbers are typically small (< 1 million) or negative
            # Addresses are typically > 0x400000 (data section) or > 0x7f0000000000 (heap/mmap)
            
            # First check: Is it a small positive number (< 1000000)?
            # CMP RAX, 1000000
            self.asm.emit_bytes(0x48, 0x3D, 0x40, 0x42, 0x0F, 0x00)  # CMP RAX, 1000000
            self.asm.emit_jump_to_label(number_label, "JL")  # If < 1000000, likely a number
            
            # Second check: Is it in data section range (around 0x400000)?
            # CMP RAX, 0x400000
            self.asm.emit_bytes(0x48, 0x3D, 0x00, 0x00, 0x40, 0x00)  # CMP RAX, 0x400000
            self.asm.emit_jump_to_label(string_label, "JGE")  # If >= 0x400000, likely a string
            
            # Fall through to number (shouldn't happen, but safe default)
            
            # Number path: RAX contains regular number
            self.asm.mark_label(number_label)
            print("DEBUG: Detected number, printing as number")
            self.asm.emit_print_number()
            self.asm.emit_jump_to_label(end_label, "JMP")
            
            # String path: RAX contains string address
            self.asm.mark_label(string_label)
            print("DEBUG: Detected string address, printing as string")
            
            # Print string at address in RAX
            self.emit_print_string_at_address()
            
            # End label
            self.asm.mark_label(end_label)
            print("DEBUG: Smart print completed with jump system")
            
        except Exception as e:
            print(f"ERROR: Smart print failed: {str(e)}")
            # Fallback to number printing
            self.asm.emit_print_number()

    def emit_print_string_at_address(self):
        """Print null-terminated string at address in RAX"""
        try:
            print("DEBUG: Printing string at address")
            
            # Clear tag bit if present (ensure even address for dereferencing)
            # No tag clearing needed
            
            # Save the clean string address
            self.asm.emit_push_rax()
            
            # Calculate string length
            self.asm.emit_mov_rbx_rax()    # String address to RBX
            self.asm.emit_mov_rcx_imm64(0) # Counter to RCX
            
            # String length loop using jump system
            strlen_loop = self.asm.create_label()
            strlen_end = self.asm.create_label()
            
            self.asm.mark_label(strlen_loop)
            # Check for null terminator: CMP BYTE PTR [RBX + RCX], 0
            self.asm.emit_bytes(0x80, 0x3C, 0x0B, 0x00)
            self.asm.emit_jump_to_label(strlen_end, "JE")
            
            # Increment counter and continue
            self.asm.emit_bytes(0x48, 0xFF, 0xC1)  # INC RCX
            self.asm.emit_jump_to_label(strlen_loop, "JMP")
            
            self.asm.mark_label(strlen_end)
            
            # Now print: sys_write(stdout, string_addr, length)
            self.asm.emit_mov_rax_imm64(1)    # sys_write
            self.asm.emit_mov_rdi_imm64(1)    # stdout
            self.asm.emit_bytes(0x48, 0x89, 0xDE)  # MOV RSI, RBX (string address)
            self.asm.emit_bytes(0x48, 0x89, 0xCA)  # MOV RDX, RCX (length)
            self.asm.emit_syscall()
            
            # Print newline
            newline_offset = self.asm.add_string("\n")
            self.asm.emit_mov_rax_imm64(1)
            self.asm.emit_mov_rdi_imm64(1)
            self.asm.emit_mov_rsi_imm64(0x402000 + newline_offset)
            self.asm.emit_mov_rdx_imm64(1)
            self.asm.emit_syscall()
            
            # Restore original RAX
            self.asm.emit_pop_rax()
            
            print("DEBUG: String printing at address completed")
            
        except Exception as e:
            print(f"ERROR: String printing at address failed: {str(e)}")
            # Cleanup and fallback
            self.asm.emit_pop_rax()
            self.asm.emit_print_number()

    def compile_number_to_string(self, node):
        """Compile NumberToString(num) - properly convert number to string"""
        try:
            print(f"DEBUG: Compiling NumberToString")
            
            # Evaluate the number
            self.compiler.compile_expression(node.arguments[0])
            
            # Allocate 32 bytes for string (enough for 64-bit number)
            self.asm.emit_push_rax()  # Save number
            
            self.asm.emit_mov_rax_imm64(9)  # sys_mmap
            self.asm.emit_mov_rdi_imm64(0)  # addr = NULL
            self.asm.emit_mov_rsi_imm64(32)  # size = 32 bytes
            self.asm.emit_mov_rdx_imm64(3)  # PROT_READ | PROT_WRITE
            self.asm.emit_mov_r10_imm64(0x22)  # MAP_PRIVATE | MAP_ANONYMOUS
            self.asm.emit_mov_r8_imm64(0xFFFFFFFFFFFFFFFF)  # fd = -1
            self.asm.emit_mov_r9_imm64(0)  # offset = 0
            self.asm.emit_syscall()
            
            self.asm.emit_bytes(0x49, 0x89, 0xC7)  # MOV R15, RAX (save buffer pointer)
            self.asm.emit_pop_rax()  # Get number back
            
            # Convert number to string
            # RDI = buffer end pointer (we'll build backwards)
            self.asm.emit_bytes(0x4C, 0x89, 0xFF)  # MOV RDI, R15
            self.asm.emit_bytes(0x48, 0x83, 0xC7, 0x1F)  # ADD RDI, 31
            
            # Null terminate
            self.asm.emit_bytes(0xC6, 0x07, 0x00)  # MOV BYTE [RDI], 0
            self.asm.emit_bytes(0x48, 0xFF, 0xCF)  # DEC RDI
            
            # Check if zero
            self.asm.emit_bytes(0x48, 0x85, 0xC0)  # TEST RAX, RAX
            zero_case = self.asm.create_label()
            not_zero = self.asm.create_label()
            
            self.asm.emit_jump_to_label(not_zero, "JNZ")
            
            # Handle zero case
            self.asm.mark_label(zero_case)
            self.asm.emit_bytes(0xC6, 0x07, 0x30)  # MOV BYTE [RDI], '0'
            self.asm.emit_mov_rax_rdi()  # Return pointer
            done_label = self.asm.create_label()
            self.asm.emit_jump_to_label(done_label, "JMP")
            
            self.asm.mark_label(not_zero)
            
            # Convert digits
            # MOV RBX, 10 (divisor)
            self.asm.emit_bytes(0x48, 0xBB)  # MOV RBX, imm64
            self.asm.emit_bytes(0x0A, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00)  # 10 in little-endian
            
            convert_loop = self.asm.create_label()
            convert_done = self.asm.create_label()
            
            self.asm.mark_label(convert_loop)
            self.asm.emit_bytes(0x48, 0x85, 0xC0)  # TEST RAX, RAX
            self.asm.emit_jump_to_label(convert_done, "JZ")
            
            self.asm.emit_bytes(0x48, 0x31, 0xD2)  # XOR RDX, RDX
            self.asm.emit_bytes(0x48, 0xF7, 0xF3)  # DIV RBX
            
            # Add '0' to remainder to get ASCII
            self.asm.emit_bytes(0x48, 0x83, 0xC2, 0x30)  # ADD RDX, '0'
            self.asm.emit_bytes(0x88, 0x17)  # MOV [RDI], DL
            self.asm.emit_bytes(0x48, 0xFF, 0xCF)  # DEC RDI
            
            self.asm.emit_jump_to_label(convert_loop, "JMP")
            
            self.asm.mark_label(convert_done)
            
            # Return pointer to start of string
            self.asm.emit_bytes(0x48, 0xFF, 0xC7)  # INC RDI (point to first digit)
            self.asm.emit_mov_rax_rdi()
            
            self.asm.mark_label(done_label)
            
            print(f"DEBUG: NumberToString completed")
            return True
            
        except Exception as e:
            print(f"ERROR: NumberToString compilation failed: {str(e)}")
            raise
    def compile_string_concat(self, node):
        """Compile StringConcat with proper label-based jumps and type tagging"""
        try:
            print(f"DEBUG: Compiling StringConcat with labels and type tagging")
            
            if len(node.arguments) < 2:
                print("ERROR: StringConcat requires 2 arguments")
                self.asm.emit_mov_rax_imm64(0)
                return True
            
            # Evaluate str1 and push it
            self.compiler.compile_expression(node.arguments[0])
            # Clear tag bit from str1 if present
            # No tag clearing needed
            self.asm.emit_push_rax()  # Stack: [str1]
            
            # Evaluate str2 and push it  
            self.compiler.compile_expression(node.arguments[1])
            # Clear tag bit from str2 if present
            # No tag clearing needed
            self.asm.emit_push_rax()  # Stack: [str2, str1]
            
            # Allocate result buffer (256 bytes for simplicity)
            self.asm.emit_mov_rax_imm64(9)      # mmap syscall
            self.asm.emit_mov_rdi_imm64(0)      # addr = NULL
            self.asm.emit_mov_rsi_imm64(256)    # size = 256
            self.asm.emit_mov_rdx_imm64(3)      # PROT_READ|PROT_WRITE
            self.asm.emit_mov_r10_imm64(0x22)   # MAP_PRIVATE|MAP_ANONYMOUS
            self.asm.emit_bytes(0x49, 0xC7, 0xC0, 0xFF, 0xFF, 0xFF, 0xFF)  # MOV R8, -1
            self.asm.emit_mov_r9_imm64(0)
            self.asm.emit_syscall()
            
            # RAX now contains the allocated buffer
            self.asm.emit_push_rax()  # Stack: [result, str2, str1]
            self.asm.emit_mov_rdi_rax()  # RDI = destination
            
            # Copy str1 first
            # str1 is at [RSP+16] (skip result and str2)
            self.asm.emit_bytes(0x48, 0x8B, 0x74, 0x24, 0x10)  # MOV RSI, [RSP+16]
            
            # Create labels for str1 copy loop
            copy1_loop = self.asm.create_label()
            copy1_done = self.asm.create_label()
            
            self.asm.mark_label(copy1_loop)
            self.asm.emit_bytes(0x8A, 0x06)       # MOV AL, [RSI]
            self.asm.emit_bytes(0x84, 0xC0)       # TEST AL, AL
            self.asm.emit_jump_to_label(copy1_done, "JZ")
            self.asm.emit_bytes(0x88, 0x07)       # MOV [RDI], AL
            self.asm.emit_bytes(0x48, 0xFF, 0xC6) # INC RSI
            self.asm.emit_bytes(0x48, 0xFF, 0xC7) # INC RDI
            self.asm.emit_jump_to_label(copy1_loop, "JMP")
            
            self.asm.mark_label(copy1_done)
            # Don't increment RDI - we want to overwrite the null terminator
            
            # Copy str2
            # str2 is at [RSP+8]
            self.asm.emit_bytes(0x48, 0x8B, 0x74, 0x24, 0x08)  # MOV RSI, [RSP+8]
            
            # Create labels for str2 copy loop
            copy2_loop = self.asm.create_label()
            copy2_done = self.asm.create_label()
            
            self.asm.mark_label(copy2_loop)
            self.asm.emit_bytes(0x8A, 0x06)       # MOV AL, [RSI]
            self.asm.emit_bytes(0x88, 0x07)       # MOV [RDI], AL
            self.asm.emit_bytes(0x84, 0xC0)       # TEST AL, AL
            self.asm.emit_jump_to_label(copy2_done, "JZ")
            self.asm.emit_bytes(0x48, 0xFF, 0xC6) # INC RSI
            self.asm.emit_bytes(0x48, 0xFF, 0xC7) # INC RDI
            self.asm.emit_jump_to_label(copy2_loop, "JMP")
            
            self.asm.mark_label(copy2_done)
            
            # Get result and clean stack
            self.asm.emit_pop_rax()  # Get result buffer
            self.asm.emit_bytes(0x48, 0x83, 0xC4, 0x10)  # ADD RSP, 16 (remove str2, str1)
            
            # TAG THE RESULT AS A STRING (set bit 0)
            # No tagging needed
            print(f"DEBUG: StringConcat result ready")
            
            # RAX now contains the tagged concatenated string
            
            print(f"DEBUG: StringConcat completed with type tagging")
            return True
            
        except Exception as e:
            print(f"ERROR: StringConcat failed: {str(e)}")
            raise
