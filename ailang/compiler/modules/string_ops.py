"""
String Operations Module for AILANG Compiler
Provides general-purpose string manipulation primitives.
"""

import sys
import os
import struct
from ailang_parser.ailang_ast import *

class StringOps:
    """General-purpose string operations"""

    def __init__(self, compiler_context):
        self.compiler = compiler_context
        self.asm = compiler_context.asm

    def compile_operation(self, node):
        """Route to specific string operation handlers"""
        function = node.function
        handlers = {
            'StringConcat': self.compile_string_concat,
            'StringCompare': self.compile_string_compare,
            'StringLength': self.compile_string_length,
            'StringCopy': self.compile_string_copy,
            'StringToNumber': self.compile_string_to_number,
            'NumberToString': self.compile_number_to_string,
            'StringEquals': self.compile_string_equals,
            'ReadInput': self.compile_read_input,
        }
        
        handler = handlers.get(function)
        if handler:
            return handler(node)
        return False

    def compile_string_to_number(self, node):
        """Convert ASCII string to integer - general purpose"""
        if len(node.arguments) < 1:
            raise ValueError("StringToNumber requires 1 argument")
        
        # Save registers
        self.asm.emit_push_rbx()
        self.asm.emit_push_rcx()
        self.asm.emit_push_rdx()
        self.asm.emit_push_rsi()
        
        # Get string pointer
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_mov_rsi_rax()  # String ptr in RSI
        
        # Initialize result = 0, sign = 1
        self.asm.emit_mov_rax_imm64(0)   # Result
        self.asm.emit_mov_rcx_imm64(1)   # Sign (1 = positive)
        self.asm.emit_mov_rbx_imm64(10)  # Base 10
        
        # Check for negative sign
        self.asm.emit_bytes(0x8A, 0x16)  # MOV DL, [RSI]
        self.asm.emit_bytes(0x80, 0xFA, 0x2D)  # CMP DL, '-'
        not_negative = self.asm.create_label()
        self.asm.emit_jump_to_label(not_negative, "JNE")
        
        # Handle negative
        # Use 2's complement for -1: 0xFFFFFFFFFFFFFFFF
        self.asm.emit_bytes(0x48, 0xC7, 0xC1, 0xFF, 0xFF, 0xFF, 0xFF)  # MOV RCX, -1
        self.asm.emit_bytes(0x48, 0xFF, 0xC6)  # INC RSI (skip '-')
        
        self.asm.mark_label(not_negative)
        
        # Parse digits
        parse_loop = self.asm.create_label()
        parse_done = self.asm.create_label()
        
        self.asm.mark_label(parse_loop)
        
        # Load byte
        self.asm.emit_bytes(0x0F, 0xB6, 0x16)  # MOVZX EDX, BYTE [RSI]
        
        # Check if null terminator
        self.asm.emit_bytes(0x84, 0xD2)  # TEST DL, DL
        self.asm.emit_jump_to_label(parse_done, "JE")
        
        # Check if digit ('0' to '9')
        self.asm.emit_bytes(0x80, 0xFA, 0x30)  # CMP DL, '0'
        self.asm.emit_jump_to_label(parse_done, "JL")
        self.asm.emit_bytes(0x80, 0xFA, 0x39)  # CMP DL, '9'
        self.asm.emit_jump_to_label(parse_done, "JG")
        
        # Convert digit: result = result * 10 + (char - '0')
        self.asm.emit_bytes(0x48, 0x0F, 0xAF, 0xC3)  # IMUL RAX, RBX (result * 10)
        self.asm.emit_bytes(0x48, 0x83, 0xEA, 0x30)  # SUB RDX, '0'
        self.asm.emit_bytes(0x48, 0x01, 0xD0)        # ADD RAX, RDX
        
        # Next character
        self.asm.emit_bytes(0x48, 0xFF, 0xC6)  # INC RSI
        self.asm.emit_jump_to_label(parse_loop, "JMP")
        
        self.asm.mark_label(parse_done)
        
        # Apply sign
        self.asm.emit_bytes(0x48, 0x0F, 0xAF, 0xC1)  # IMUL RAX, RCX
        
        # Restore registers
        self.asm.emit_pop_rsi()
        self.asm.emit_pop_rdx()
        self.asm.emit_pop_rcx()
        self.asm.emit_pop_rbx()
        
        return True

    def compile_print_message(self, node):
        """Compile PrintMessage with proper string address handling"""
        try:
            if isinstance(node.message, String):
                self.asm.emit_print_string(node.message.value)
            elif isinstance(node.message, Number):
                self.asm.emit_mov_rax_imm64(int(node.message.value))
                self.asm.emit_print_number()
            elif isinstance(node.message, Identifier):
                resolved_name = self.compiler.resolve_acronym_identifier(node.message.name)
                if resolved_name in self.compiler.variables:
                    offset = self.compiler.variables[resolved_name]
                    self.asm.emit_bytes(0x48, 0x8b, 0x85)
                    self.asm.emit_bytes(*struct.pack('<i', -offset))
                    self.emit_smart_print_with_jumps()
                else:
                    raise ValueError(f"Undefined variable: {node.message.name}")
            elif isinstance(node.message, FunctionCall):
                self.compiler.compile_function_call(node.message)
                self.emit_smart_print_with_jumps()
            else:
                raise ValueError(f"Unsupported PrintMessage type: {type(node.message)}")
        except Exception as e:
            raise ValueError(f"PrintMessage compilation failed: {str(e)}")


    def compile_print_number(self, node):
        """Compile PrintNumber function - prints numeric value"""
        try:
            if isinstance(node, FunctionCall) and node.function == 'PrintNumber':
                if len(node.arguments) != 1:
                    raise ValueError("PrintNumber requires exactly 1 argument")
                # Compile the argument to get value in RAX
                self.compiler.compile_expression(node.arguments[0])
                # Print the number in RAX
                self.asm.emit_print_number()
                # Print newline
                newline_offset = self.asm.add_string("\n")
                self.asm.emit_mov_rax_imm64(1)  # sys_write
                self.asm.emit_mov_rdi_imm64(1)  # stdout
                self.asm.emit_load_data_address('rsi', newline_offset)  # string address
                self.asm.emit_mov_rdx_imm64(1)  # length
                self.asm.emit_syscall()
                print("DEBUG: PrintNumber completed")
                return True
            return False
        except Exception as e:
            print(f"ERROR: PrintNumber compilation failed: {str(e)}")
            raise
    
    
    def emit_smart_print_with_jumps(self):
        """Smart print - check if value is likely a string address or number"""
        string_label = self.asm.create_label()
        number_label = self.asm.create_label()
        end_label = self.asm.create_label()

        # Check if small number (< 1000000)
        self.asm.emit_bytes(0x48, 0x3D, 0x40, 0x42, 0x0F, 0x00)  # CMP RAX, 1000000
        self.asm.emit_jump_to_label(number_label, "JL")

        # Check if in data section (>= 0x400000)
        self.asm.emit_bytes(0x48, 0x3D, 0x00, 0x00, 0x40, 0x00)  # CMP RAX, 0x400000
        self.asm.emit_jump_to_label(string_label, "JGE")

        # Number path
        self.asm.mark_label(number_label)
        self.asm.emit_print_number()
        self.asm.emit_jump_to_label(end_label, "JMP")

        # String path
        self.asm.mark_label(string_label)
        self.emit_print_string_at_address()

        self.asm.mark_label(end_label)

    def emit_print_string_at_address(self):
        """Print null-terminated string at address in RAX"""
        self.asm.emit_push_rax()
        self.asm.emit_mov_rbx_rax()
        self.asm.emit_mov_rcx_imm64(0)

        strlen_loop = self.asm.create_label()
        strlen_end = self.asm.create_label()

        self.asm.mark_label(strlen_loop)
        self.asm.emit_bytes(0x80, 0x3C, 0x0B, 0x00)
        self.asm.emit_jump_to_label(strlen_end, "JE")
        self.asm.emit_bytes(0x48, 0xFF, 0xC1)
        self.asm.emit_jump_to_label(strlen_loop, "JMP")

        self.asm.mark_label(strlen_end)

        self.asm.emit_mov_rax_imm64(1)  # sys_write
        self.asm.emit_mov_rdi_imm64(1)  # stdout
        self.asm.emit_bytes(0x48, 0x89, 0xDE)  # MOV RSI, RBX
        self.asm.emit_bytes(0x48, 0x89, 0xCA)  # MOV RDX, RCX
        self.asm.emit_syscall()

        # Newline
        newline_offset = self.asm.add_string("\n")
        self.asm.emit_mov_rax_imm64(1)
        self.asm.emit_mov_rdi_imm64(1)
        self.asm.emit_mov_rsi_imm64(0x402000 + newline_offset)
        self.asm.emit_mov_rdx_imm64(1)
        self.asm.emit_syscall()

        self.asm.emit_pop_rax()

    def compile_number_to_string(self, node):
        """Convert integer to ASCII string - general purpose"""
        if len(node.arguments) < 1:
            raise ValueError("NumberToString requires 1 argument")
        
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_push_rax()

        # Allocate buffer (32 bytes)
        self.asm.emit_mov_rax_imm64(9)
        self.asm.emit_mov_rdi_imm64(0)
        self.asm.emit_mov_rsi_imm64(32)
        self.asm.emit_mov_rdx_imm64(3)
        self.asm.emit_mov_r10_imm64(0x22)
        self.asm.emit_mov_r8_imm64(0xFFFFFFFFFFFFFFFF)  # R8 = -1 (fd)
        self.asm.emit_mov_r9_imm64(0)
        self.asm.emit_syscall()

        self.asm.emit_bytes(0x49, 0x89, 0xC7)  # MOV R15, RAX
        self.asm.emit_pop_rax()

        # Handle negative numbers
        self.asm.emit_push_rax()
        self.asm.emit_bytes(0x48, 0x85, 0xC0)  # TEST RAX, RAX
        not_negative = self.asm.create_label()
        self.asm.emit_jump_to_label(not_negative, "JNS")
        
        # Negate for processing
        self.asm.emit_bytes(0x48, 0xF7, 0xD8)  # NEG RAX
        
        self.asm.mark_label(not_negative)

        # Convert to string (backward from end of buffer)
        self.asm.emit_bytes(0x4C, 0x89, 0xFF)
        self.asm.emit_bytes(0x48, 0x83, 0xC7, 0x1F)
        self.asm.emit_bytes(0xC6, 0x07, 0x00)
        self.asm.emit_bytes(0x48, 0xFF, 0xCF)

        self.asm.emit_bytes(0x48, 0x85, 0xC0)
        zero_case = self.asm.create_label()
        not_zero = self.asm.create_label()
        self.asm.emit_jump_to_label(not_zero, "JNZ")

        self.asm.mark_label(zero_case)
        self.asm.emit_bytes(0xC6, 0x07, 0x30)
        self.asm.emit_mov_rax_rdi()
        done_label = self.asm.create_label()
        self.asm.emit_jump_to_label(done_label, "JMP")

        self.asm.mark_label(not_zero)
        self.asm.emit_bytes(0x48, 0xBB, 0x0A, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00)

        convert_loop = self.asm.create_label()
        convert_done = self.asm.create_label()

        self.asm.mark_label(convert_loop)
        self.asm.emit_bytes(0x48, 0x85, 0xC0)
        self.asm.emit_jump_to_label(convert_done, "JZ")
        self.asm.emit_bytes(0x48, 0x31, 0xD2)
        self.asm.emit_bytes(0x48, 0xF7, 0xF3)
        self.asm.emit_bytes(0x48, 0x83, 0xC2, 0x30)
        self.asm.emit_bytes(0x88, 0x17)
        self.asm.emit_bytes(0x48, 0xFF, 0xCF)
        self.asm.emit_jump_to_label(convert_loop, "JMP")

        self.asm.mark_label(convert_done)
        self.asm.emit_bytes(0x48, 0xFF, 0xC7)
        
        # Check if original was negative
        self.asm.emit_pop_rax()  # Original value
        self.asm.emit_bytes(0x48, 0x85, 0xC0)  # TEST RAX, RAX
        positive_done = self.asm.create_label()
        self.asm.emit_jump_to_label(positive_done, "JNS")
        
        # Add minus sign
        self.asm.emit_bytes(0x48, 0xFF, 0xCF)  # DEC RDI
        self.asm.emit_bytes(0xC6, 0x07, 0x2D)  # MOV BYTE [RDI], '-'
        
        self.asm.mark_label(positive_done)
        self.asm.emit_mov_rax_rdi()

        self.asm.mark_label(done_label)
        return True

    def compile_string_concat(self, node):
        """Concatenate two strings - general purpose"""
        if len(node.arguments) < 2:
            raise ValueError("StringConcat requires 2 arguments")

        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_push_rax()
        self.compiler.compile_expression(node.arguments[1])
        self.asm.emit_push_rax()

        # Allocate buffer (256 bytes)
        self.asm.emit_mov_rax_imm64(9)
        self.asm.emit_mov_rdi_imm64(0)
        self.asm.emit_mov_rsi_imm64(256)
        self.asm.emit_mov_rdx_imm64(3)
        self.asm.emit_mov_r10_imm64(0x22)
        self.asm.emit_bytes(0x49, 0xC7, 0xC0, 0xFF, 0xFF, 0xFF, 0xFF)
        self.asm.emit_mov_r9_imm64(0)
        self.asm.emit_syscall()

        self.asm.emit_push_rax()
        self.asm.emit_mov_rdi_rax()

        # Copy str1
        self.asm.emit_bytes(0x48, 0x8B, 0x74, 0x24, 0x10)
        copy1_loop = self.asm.create_label()
        copy1_done = self.asm.create_label()
        self.asm.mark_label(copy1_loop)
        self.asm.emit_bytes(0x8A, 0x06)
        self.asm.emit_bytes(0x84, 0xC0)
        self.asm.emit_jump_to_label(copy1_done, "JZ")
        self.asm.emit_bytes(0x88, 0x07)
        self.asm.emit_bytes(0x48, 0xFF, 0xC6)
        self.asm.emit_bytes(0x48, 0xFF, 0xC7)
        self.asm.emit_jump_to_label(copy1_loop, "JMP")
        self.asm.mark_label(copy1_done)

        # Copy str2 (overwrite null)
        self.asm.emit_bytes(0x48, 0x8B, 0x74, 0x24, 0x08)
        copy2_loop = self.asm.create_label()
        copy2_done = self.asm.create_label()
        self.asm.mark_label(copy2_loop)
        self.asm.emit_bytes(0x8A, 0x06)
        self.asm.emit_bytes(0x88, 0x07)
        self.asm.emit_bytes(0x84, 0xC0)
        self.asm.emit_jump_to_label(copy2_done, "JZ")
        self.asm.emit_bytes(0x48, 0xFF, 0xC6)
        self.asm.emit_bytes(0x48, 0xFF, 0xC7)
        self.asm.emit_jump_to_label(copy2_loop, "JMP")
        self.asm.mark_label(copy2_done)

        self.asm.emit_pop_rax()
        self.asm.emit_bytes(0x48, 0x83, 0xC4, 0x10)
        return True

    def compile_string_compare(self, node):
        """Compare two strings - returns 0 if equal, non-zero if different"""
        if len(node.arguments) < 2:
            raise ValueError("StringCompare requires 2 arguments")

        self.asm.emit_push_rbx()
        self.asm.emit_push_rcx()
        self.asm.emit_push_rdx()
        self.asm.emit_push_rsi()
        self.asm.emit_push_rdi()

        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_mov_rdi_rax()
        self.compiler.compile_expression(node.arguments[1])
        self.asm.emit_mov_rsi_rax()

        cmp_loop = self.asm.create_label()
        cmp_equal = self.asm.create_label()
        cmp_not_equal = self.asm.create_label()

        self.asm.mark_label(cmp_loop)
        self.asm.emit_bytes(0x8A, 0x07)  # MOV AL, [RDI]
        self.asm.emit_bytes(0x8A, 0x1E)  # MOV BL, [RSI]
        self.asm.emit_bytes(0x38, 0xD8)  # CMP AL, BL
        self.asm.emit_jump_to_label(cmp_not_equal, "JNE")
        self.asm.emit_bytes(0x84, 0xC0)  # TEST AL, AL
        self.asm.emit_jump_to_label(cmp_equal, "JZ")
        self.asm.emit_bytes(0x48, 0xFF, 0xC7)  # INC RDI
        self.asm.emit_bytes(0x48, 0xFF, 0xC6)  # INC RSI
        self.asm.emit_jump_to_label(cmp_loop, "JMP")

        self.asm.mark_label(cmp_equal)
        self.asm.emit_mov_rax_imm64(0)
        done_label = self.asm.create_label()
        self.asm.emit_jump_to_label(done_label, "JMP")

        self.asm.mark_label(cmp_not_equal)
        self.asm.emit_mov_rax_imm64(1)

        self.asm.mark_label(done_label)

        self.asm.emit_pop_rdi()
        self.asm.emit_pop_rsi()
        self.asm.emit_pop_rdx()
        self.asm.emit_pop_rcx()
        self.asm.emit_pop_rbx()
        return True

    def compile_string_length(self, node):
        """Get length of null-terminated string"""
        if len(node.arguments) < 1:
            raise ValueError("StringLength requires 1 argument")

        self.asm.emit_push_rbx()
        self.asm.emit_push_rcx()

        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_mov_rbx_rax()
        self.asm.emit_mov_rax_imm64(0)

        len_loop = self.asm.create_label()
        len_done = self.asm.create_label()

        self.asm.mark_label(len_loop)
        self.asm.emit_bytes(0x8A, 0x0B)  # MOV CL, [RBX]
        self.asm.emit_bytes(0x84, 0xC9)  # TEST CL, CL
        self.asm.emit_jump_to_label(len_done, "JZ")
        self.asm.emit_bytes(0x48, 0xFF, 0xC0)  # INC RAX
        self.asm.emit_bytes(0x48, 0xFF, 0xC3)  # INC RBX
        self.asm.emit_jump_to_label(len_loop, "JMP")

        self.asm.mark_label(len_done)

        self.asm.emit_pop_rcx()
        self.asm.emit_pop_rbx()
        return True

    def compile_string_copy(self, node):
        """Copy source string to destination"""
        if len(node.arguments) < 2:
            raise ValueError("StringCopy requires 2 arguments")

        self.asm.emit_push_rbx()
        self.asm.emit_push_rcx()
        self.asm.emit_push_rsi()
        self.asm.emit_push_rdi()

        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_mov_rdi_rax()
        self.compiler.compile_expression(node.arguments[1])
        self.asm.emit_mov_rsi_rax()

        copy_loop = self.asm.create_label()
        copy_done = self.asm.create_label()

        self.asm.mark_label(copy_loop)
        self.asm.emit_bytes(0x8A, 0x06)  # MOV AL, [RSI]
        self.asm.emit_bytes(0x88, 0x07)  # MOV [RDI], AL
        self.asm.emit_bytes(0x84, 0xC0)  # TEST AL, AL
        self.asm.emit_jump_to_label(copy_done, "JE")
        self.asm.emit_bytes(0x48, 0xFF, 0xC7)  # INC RDI
        self.asm.emit_bytes(0x48, 0xFF, 0xC6)  # INC RSI
        self.asm.emit_jump_to_label(copy_loop, "JMP")

        self.asm.mark_label(copy_done)
        self.asm.emit_mov_rax_imm64(1)

    def compile_string_equals(self, node):
        """Compare two strings - returns 1 if equal, 0 if different"""
        if len(node.arguments) < 2:
            raise ValueError("StringEquals requires 2 arguments")
        
        print("DEBUG: Compiling StringEquals")
        
        # Save registers
        self.asm.emit_push_rbx()
        self.asm.emit_push_rcx()
        self.asm.emit_push_rsi()
        self.asm.emit_push_rdi()
        
        # Get string pointers
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_mov_rdi_rax()  # First string in RDI
        
        self.compiler.compile_expression(node.arguments[1])
        self.asm.emit_mov_rsi_rax()  # Second string in RSI
        
        # Compare loop
        cmp_loop = self.asm.create_label()
        equal = self.asm.create_label()
        not_equal = self.asm.create_label()
        
        self.asm.mark_label(cmp_loop)
        
        # Load bytes
        self.asm.emit_bytes(0x8A, 0x07)  # MOV AL, [RDI]
        self.asm.emit_bytes(0x8A, 0x1E)  # MOV BL, [RSI]
        
        # Compare bytes
        self.asm.emit_bytes(0x38, 0xD8)  # CMP AL, BL
        self.asm.emit_jump_to_label(not_equal, "JNE")
        
        # Check for null terminator
        self.asm.emit_bytes(0x84, 0xC0)  # TEST AL, AL
        self.asm.emit_jump_to_label(equal, "JZ")
        
        # Next character
        self.asm.emit_bytes(0x48, 0xFF, 0xC7)  # INC RDI
        self.asm.emit_bytes(0x48, 0xFF, 0xC6)  # INC RSI
        self.asm.emit_jump_to_label(cmp_loop, "JMP")
        
        self.asm.mark_label(equal)
        self.asm.emit_mov_rax_imm64(1)  # Strings are equal
        done = self.asm.create_label()
        self.asm.emit_jump_to_label(done, "JMP")
        
        self.asm.mark_label(not_equal)
        self.asm.emit_mov_rax_imm64(0)  # Strings are different
        
        self.asm.mark_label(done)
        
        # Restore registers
        self.asm.emit_pop_rdi()
        self.asm.emit_pop_rsi()
        self.asm.emit_pop_rcx()
        self.asm.emit_pop_rbx()
        
        print("DEBUG: StringEquals completed")
        return True

    def compile_read_input(self, node):
        """Read input from stdin"""
        print("DEBUG: Compiling ReadInput")
        
        # Allocate buffer for input (256 bytes)
        buffer_size = 256
        
        # Use mmap to allocate buffer
        self.asm.emit_mov_rax_imm64(9)  # sys_mmap
        self.asm.emit_mov_rdi_imm64(0)  # addr = NULL
        self.asm.emit_mov_rsi_imm64(buffer_size)  # length
        self.asm.emit_mov_rdx_imm64(3)  # PROT_READ | PROT_WRITE
        self.asm.emit_mov_r10_imm64(0x22)  # MAP_PRIVATE | MAP_ANONYMOUS
        self.asm.emit_mov_r8_imm64(0xFFFFFFFFFFFFFFFF)  # fd = -1
        self.asm.emit_mov_r9_imm64(0)  # offset = 0
        self.asm.emit_syscall()
        
        # Save buffer address
        self.asm.emit_push_rax()
        
        # Read from stdin
        self.asm.emit_mov_rdi_imm64(0)  # fd = stdin
        self.asm.emit_mov_rsi_rax()  # buffer
        self.asm.emit_mov_rdx_imm64(buffer_size - 1)  # count (leave room for null)
        self.asm.emit_mov_rax_imm64(0)  # sys_read
        self.asm.emit_syscall()
        
        # Null terminate (replace newline if present)
        self.asm.emit_pop_rsi()  # Get buffer address
        self.asm.emit_mov_rdi_rsi()  # Copy to RDI
        self.asm.emit_add_rdi_rax()  # Point to end of input
        self.asm.emit_bytes(0x48, 0xFF, 0xCF)  # DEC RDI (back to last char)
        
        # Check if last char is newline
        self.asm.emit_bytes(0x80, 0x3F, 0x0A)  # CMP BYTE [RDI], 0x0A
        skip = self.asm.create_label()
        self.asm.emit_jump_to_label(skip, "JNE")
        self.asm.emit_bytes(0xC6, 0x07, 0x00)  # MOV BYTE [RDI], 0
        self.asm.mark_label(skip)
        
        # Return buffer address
        self.asm.emit_mov_rax_rsi()
        
        print("DEBUG: ReadInput completed")
        return True


        self.asm.emit_pop_rdi()
        self.asm.emit_pop_rsi()
        self.asm.emit_pop_rcx()
        self.asm.emit_pop_rbx()
        return True