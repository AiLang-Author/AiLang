
"""
String Operations Module for AILANG Compiler
Handles string printing and manipulation operations
FIXED: Proper string address dereferencing for ReadTextFile
"""

import sys
import os
import struct
from ...parser.ailang_ast import *

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
        """Smart print using proper jump system - detects string addresses vs numbers"""
        try:
            print("DEBUG: Starting smart print with jump system")
            
            # Create labels using the jump system
            string_label = self.asm.create_label()
            number_label = self.asm.create_label()
            end_label = self.asm.create_label()
            
            # Check if RAX is in string address range (0x402000 to 0x420000)
            # Compare with lower bound: if RAX < 0x402000, it's a number
            self.asm.emit_bytes(0x48, 0x3D, 0x00, 0x20, 0x40, 0x00)  # CMP RAX, 0x402000
            self.asm.emit_jump_to_label(number_label, "JL")
            
            # Compare with upper bound: if RAX > 0x420000, it's a number  
            self.asm.emit_bytes(0x48, 0x3D, 0x00, 0x00, 0x42, 0x00)  # CMP RAX, 0x420000
            self.asm.emit_jump_to_label(number_label, "JG")
            
            # String path: RAX contains string address
            self.asm.mark_label(string_label)
            print("DEBUG: Detected string address, printing as string")
            
            # Print string at address in RAX
            self.emit_print_string_at_address()
            self.asm.emit_jump_to_label(end_label, "JMP")
            
            # Number path: RAX contains regular number
            self.asm.mark_label(number_label)
            print("DEBUG: Detected number, printing as number")
            self.asm.emit_print_number()
            
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
            
            # Save the string address
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
