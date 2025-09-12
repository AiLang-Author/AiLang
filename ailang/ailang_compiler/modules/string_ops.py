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
            'StringConcatPooled': self.compile_string_concat_pooled,
            'StringCompare': self.compile_string_compare,
            'StringLength': self.compile_string_length,
            'StringCopy': self.compile_string_copy,
            'StringToNumber': self.compile_string_to_number,
            'NumberToString': self.compile_number_to_string,
            'StringEquals': self.compile_string_equals,
            'ReadInput': self.compile_read_input,
            # === ADD THESE NEW HANDLERS ===
            'CharToString': self.compile_char_to_string,
            'StringToUpper': self.compile_string_to_upper,
            'StringToLower': self.compile_string_to_lower,
            'StringContains': self.compile_string_contains,
            'StringSubstring': self.compile_string_substring,
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

    # In string_ops.py
    def compile_print_message(self, node):
        """Compile PrintMessage with proper string address handling and newline"""
        try:
            if isinstance(node.message, String):
                # Print the string
                self.asm.emit_print_string(node.message.value)
                # Don't add extra newline here - emit_print_string handles it
                
            elif isinstance(node.message, Number):
                self.asm.emit_mov_rax_imm64(int(node.message.value))
                self.asm.emit_print_number()
                # Add newline after number
                newline_offset = self.asm.add_string("\n")
                self.asm.emit_mov_rax_imm64(1)  # sys_write
                self.asm.emit_mov_rdi_imm64(1)  # stdout
                self.asm.emit_load_data_address('rsi', newline_offset)
                self.asm.emit_mov_rdx_imm64(1)
                self.asm.emit_syscall()
                
            elif isinstance(node.message, Identifier):
                resolved_name = self.compiler.resolve_acronym_identifier(node.message.name)
                if resolved_name in self.compiler.variables:
                    offset = self.compiler.variables[resolved_name]
                    self.asm.emit_bytes(0x48, 0x8b, 0x85)
                    self.asm.emit_bytes(*struct.pack('<i', -offset))
                    self.emit_smart_print_with_jumps()
                    # Add newline after identifier value
                    newline_offset = self.asm.add_string("\n")
                    self.asm.emit_mov_rax_imm64(1)  # sys_write
                    self.asm.emit_mov_rdi_imm64(1)  # stdout
                    self.asm.emit_load_data_address('rsi', newline_offset)
                    self.asm.emit_mov_rdx_imm64(1)
                    self.asm.emit_syscall()
                else:
                    raise ValueError(f"Undefined variable: {node.message.name}")
                    
            elif isinstance(node.message, FunctionCall):
                self.compiler.compile_function_call(node.message)
                self.emit_smart_print_with_jumps()
                # Add newline after function result
                newline_offset = self.asm.add_string("\n")
                self.asm.emit_mov_rax_imm64(1)  # sys_write
                self.asm.emit_mov_rdi_imm64(1)  # stdout
                self.asm.emit_load_data_address('rsi', newline_offset)
                self.asm.emit_mov_rdx_imm64(1)
                self.asm.emit_syscall()
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
        """Print null-terminated string at address in RAX (NULL-safe)."""
        # Save the incoming pointer (could be NULL)
        self.asm.emit_push_rax()
        self.asm.emit_mov_rbx_rax()

        # ===== NULL GUARD =====
        null_done = self.asm.create_label()
        self.asm.emit_bytes(0x48, 0x85, 0xDB)      # TEST RBX, RBX
        self.asm.emit_jump_to_label(null_done, "JZ")  # if RBX==0 -> skip printing

        # Compute length (RCX) from RBX
        self.asm.emit_mov_rcx_imm64(0)

        strlen_loop = self.asm.create_label()
        strlen_end  = self.asm.create_label()

        self.asm.mark_label(strlen_loop)
        self.asm.emit_bytes(0x80, 0x3C, 0x0B, 0x00)   # CMP BYTE PTR [RBX+RCX], 0
        self.asm.emit_jump_to_label(strlen_end, "JE")
        self.asm.emit_bytes(0x48, 0xFF, 0xC1)         # INC RCX
        self.asm.emit_jump_to_label(strlen_loop, "JMP")

        self.asm.mark_label(strlen_end)

        # write(1, RBX, RCX)
        self.asm.emit_mov_rax_imm64(1)   # sys_write
        self.asm.emit_mov_rdi_imm64(1)   # stdout
        self.asm.emit_bytes(0x48, 0x89, 0xDE)  # MOV RSI, RBX
        self.asm.emit_bytes(0x48, 0x89, 0xCA)  # MOV RDX, RCX
        self.asm.emit_syscall()

        # newline
        newline_offset = self.asm.add_string("\n")
        self.asm.emit_mov_rax_imm64(1)          # sys_write
        self.asm.emit_mov_rdi_imm64(1)          # stdout
        self.asm.emit_load_data_address('rsi', newline_offset)
        self.asm.emit_mov_rdx_imm64(1)
        self.asm.emit_syscall()

        # ===== END NULL GUARD =====
        self.asm.mark_label(null_done)

        # Restore and return
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
        """Concatenate two strings with dynamic sizing - NULL-safe"""
        if len(node.arguments) < 2:
            raise ValueError("StringConcat requires 2 arguments")
        
        print("DEBUG: StringConcat with dynamic sizing")
        
        # Evaluate and push both arguments
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_push_rax()  # Stack: [str1]
        self.compiler.compile_expression(node.arguments[1])
        self.asm.emit_push_rax()  # Stack: [str1][str2]
        
        # Calculate len1 (NULL-safe)
        self.asm.emit_bytes(0x48, 0x8B, 0x7C, 0x24, 0x08)  # MOV RDI, [RSP+8] (str1)
        self.asm.emit_test_rdi_rdi()
        len1_zero = self.asm.create_label()
        len1_done = self.asm.create_label()
        self.asm.emit_jump_to_label(len1_zero, "JZ")
        self._emit_strlen()  # Returns length in RAX
        self.asm.emit_jump_to_label(len1_done, "JMP")
        self.asm.mark_label(len1_zero)
        self.asm.emit_mov_rax_imm64(0)
        self.asm.mark_label(len1_done)
        self.asm.emit_mov_rbx_rax()  # Save len1 in RBX
        
        # Calculate len2 (NULL-safe)
        self.asm.emit_bytes(0x48, 0x8B, 0x3C, 0x24)  # MOV RDI, [RSP] (str2)
        self.asm.emit_test_rdi_rdi()
        len2_zero = self.asm.create_label()
        len2_done = self.asm.create_label()
        self.asm.emit_jump_to_label(len2_zero, "JZ")
        self._emit_strlen()  # Returns length in RAX
        self.asm.emit_jump_to_label(len2_done, "JMP")
        self.asm.mark_label(len2_zero)
        self.asm.emit_mov_rax_imm64(0)
        self.asm.mark_label(len2_done)
        
        # Calculate total size = len1 + len2 + 1
        self.asm.emit_bytes(0x48, 0x01, 0xD8)  # ADD RAX, RBX (RAX = len1 + len2)
        self.asm.emit_bytes(0x48, 0xFF, 0xC0)  # INC RAX (for null terminator)
        self.asm.emit_mov_rsi_rax()  # RSI = total size
        
        # Allocate exact size with mmap
        self.asm.emit_mov_rax_imm64(9)  # sys_mmap
        self.asm.emit_mov_rdi_imm64(0)  # addr = NULL
        # RSI already has size
        self.asm.emit_mov_rdx_imm64(3)  # PROT_READ | PROT_WRITE
        self.asm.emit_mov_r10_imm64(0x22)  # MAP_PRIVATE | MAP_ANONYMOUS
        self.asm.emit_bytes(0x49, 0xC7, 0xC0, 0xFF, 0xFF, 0xFF, 0xFF)  # MOV R8, -1
        self.asm.emit_mov_r9_imm64(0)
        self.asm.emit_syscall()
        
        # Check for failure
        self.asm.emit_bytes(0x48, 0x83, 0xF8, 0xFF)  # CMP RAX, -1
        fail_label = self.asm.create_label()
        done_label = self.asm.create_label()
        self.asm.emit_jump_to_label(fail_label, "JE")
        
        # Success - save base immediately!
        self.asm.emit_push_rax()  # Save buffer base on stack
        self.asm.emit_mov_rdi_rax()  # RDI = destination
        
        # Copy str1 (NULL-safe) - FIXED OFFSET
        self.asm.emit_bytes(0x48, 0x8B, 0x74, 0x24, 0x10)  # MOV RSI, [RSP+16] (str1)
        self.asm.emit_test_rsi_rsi()
        skip_str1 = self.asm.create_label()
        self.asm.emit_jump_to_label(skip_str1, "JZ")
        
        copy1_loop = self.asm.create_label()
        copy1_done = self.asm.create_label()
        self.asm.mark_label(copy1_loop)
        self.asm.emit_bytes(0x8A, 0x06)  # MOV AL, [RSI]
        self.asm.emit_bytes(0x84, 0xC0)  # TEST AL, AL
        self.asm.emit_jump_to_label(copy1_done, "JZ")
        self.asm.emit_bytes(0x88, 0x07)  # MOV [RDI], AL
        self.asm.emit_bytes(0x48, 0xFF, 0xC6)  # INC RSI
        self.asm.emit_bytes(0x48, 0xFF, 0xC7)  # INC RDI
        self.asm.emit_jump_to_label(copy1_loop, "JMP")
        self.asm.mark_label(copy1_done)
        self.asm.mark_label(skip_str1)
        
        # Copy str2 (NULL-safe) - FIXED OFFSET
        self.asm.emit_bytes(0x48, 0x8B, 0x74, 0x24, 0x08)  # MOV RSI, [RSP+8] (str2)
        self.asm.emit_test_rsi_rsi()
        skip_str2 = self.asm.create_label()
        self.asm.emit_jump_to_label(skip_str2, "JZ")
        
        copy2_loop = self.asm.create_label()
        copy2_done = self.asm.create_label()
        self.asm.mark_label(copy2_loop)
        self.asm.emit_bytes(0x8A, 0x06)  # MOV AL, [RSI]
        self.asm.emit_bytes(0x88, 0x07)  # MOV [RDI], AL
        self.asm.emit_bytes(0x84, 0xC0)  # TEST AL, AL
        self.asm.emit_jump_to_label(copy2_done, "JZ")
        self.asm.emit_bytes(0x48, 0xFF, 0xC6)  # INC RSI
        self.asm.emit_bytes(0x48, 0xFF, 0xC7)  # INC RDI
        self.asm.emit_jump_to_label(copy2_loop, "JMP")
        self.asm.mark_label(copy2_done)
        self.asm.mark_label(skip_str2)
        
        # Null terminate
        self.asm.emit_bytes(0xC6, 0x07, 0x00)  # MOV BYTE [RDI], 0
        
        # Return base pointer (saved on stack)
        self.asm.emit_pop_rax()  # Get saved base
        self.asm.emit_bytes(0x48, 0x83, 0xC4, 0x10)  # ADD RSP, 16 (clean str1, str2)
        self.asm.emit_jump_to_label(done_label, "JMP")
        
        # Failure path - return str1
        self.asm.mark_label(fail_label)
        self.asm.emit_pop_rax()  # Pop str2
        self.asm.emit_pop_rax()  # Return str1
        
        self.asm.mark_label(done_label)
        print("DEBUG: Dynamic StringConcat complete")
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
        # NULL-safe: if first source RSI==0, skip first copy
        self.asm.emit_test_rsi_rsi()
        skip_first = self.asm.create_label()
        self.asm.emit_jump_to_label(skip_first, "JZ")
        # NULL-safe first source: treat NULL as empty
        self.asm.emit_test_rsi_rsi()
        copy_first_skip = self.asm.create_label()
        self.asm.emit_jump_to_label(copy_first_skip, "JZ")

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
        
        # Save registers we'll use
        self.asm.emit_push_rcx()
        self.asm.emit_push_rdx()
        self.asm.emit_push_rsi()
        self.asm.emit_push_rdi()
        
        # Get string pointers
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_mov_rdi_rax()  # First string in RDI
        
        self.compiler.compile_expression(node.arguments[1])
        self.asm.emit_mov_rsi_rax()  # Second string in RSI
        
        # Create labels for control flow
        cmp_loop = self.asm.create_label()
        strings_equal = self.asm.create_label()
        strings_not_equal = self.asm.create_label()
        done = self.asm.create_label()
        
        self.asm.mark_label(cmp_loop)
        
        # Load bytes to compare
        self.asm.emit_bytes(0x8A, 0x07)  # MOV AL, [RDI]
        self.asm.emit_bytes(0x8A, 0x16)  # MOV DL, [RSI]
        
        # Compare the bytes
        self.asm.emit_bytes(0x38, 0xD0)  # CMP AL, DL
        self.asm.emit_jump_to_label(strings_not_equal, "JNE")
        
        # Check if we hit null terminator (strings ended)
        self.asm.emit_bytes(0x84, 0xC0)  # TEST AL, AL
        self.asm.emit_jump_to_label(strings_equal, "JZ")
        
        # Move to next characters
        self.asm.emit_bytes(0x48, 0xFF, 0xC7)  # INC RDI
        self.asm.emit_bytes(0x48, 0xFF, 0xC6)  # INC RSI
        self.asm.emit_jump_to_label(cmp_loop, "JMP")
        
        # Strings are equal
        self.asm.mark_label(strings_equal)
        self.asm.emit_mov_rax_imm64(1)
        self.asm.emit_jump_to_label(done, "JMP")
        
        # Strings are not equal
        self.asm.mark_label(strings_not_equal)
        self.asm.emit_mov_rax_imm64(0)
        
        self.asm.mark_label(done)
        
        # Restore registers
        self.asm.emit_pop_rdi()
        self.asm.emit_pop_rsi()
        self.asm.emit_pop_rdx()
        self.asm.emit_pop_rcx()
        
        print("DEBUG: StringEquals completed")
        return True


    def compile_read_input(self, node):
        """Read input from stdin into dynamically allocated buffer"""
        print("DEBUG: Compiling ReadInput")
        
        # Handle optional argument (prompt string) - just ignore it for now
        # The argument would be a prompt to display, but we handle that separately
        
        # Allocate buffer for input (256 bytes) using mmap
        buffer_size = 256
        
        self.asm.emit_mov_rax_imm64(9)  # sys_mmap
        self.asm.emit_mov_rdi_imm64(0)  # addr = NULL
        self.asm.emit_mov_rsi_imm64(buffer_size)  # length
        self.asm.emit_mov_rdx_imm64(3)  # PROT_READ | PROT_WRITE
        self.asm.emit_mov_r10_imm64(0x22)  # MAP_PRIVATE | MAP_ANONYMOUS  
        self.asm.emit_bytes(0x49, 0xC7, 0xC0, 0xFF, 0xFF, 0xFF, 0xFF)  # MOV R8, -1 (fd)
        self.asm.emit_mov_r9_imm64(0)  # offset = 0
        self.asm.emit_syscall()
        
        # Save buffer address in RBX (preserved across syscalls)
        self.asm.emit_mov_rbx_rax()
        
        # Read from stdin
        self.asm.emit_mov_rdi_imm64(0)  # fd = stdin
        self.asm.emit_mov_rsi_from_rbx()  # buffer address from RBX (USE CORRECT METHOD NAME)
        self.asm.emit_mov_rdx_imm64(buffer_size - 1)  # leave room for null
        self.asm.emit_mov_rax_imm64(0)  # sys_read
        self.asm.emit_syscall()
        
        # RAX now contains number of bytes read
        # Check if we read anything
        self.asm.emit_bytes(0x48, 0x85, 0xC0)  # TEST RAX, RAX
        no_input = self.asm.create_label()
        has_input = self.asm.create_label()
        done = self.asm.create_label()
        self.asm.emit_jump_to_label(has_input, "JNZ")
        
        # No input case - just null terminate at start
        self.asm.mark_label(no_input)
        self.asm.emit_bytes(0xC6, 0x03, 0x00)  # MOV BYTE [RBX], 0
        self.asm.emit_jump_to_label(done, "JMP")
        
        # Has input - null terminate and strip newline if present
        self.asm.mark_label(has_input)
        
        # RBX still has buffer start, RAX has bytes read
        # Calculate position of last character: RBX + RAX - 1
        self.asm.emit_mov_rdi_from_rbx()  # Copy buffer address to RDI (USE CORRECT METHOD NAME)
        self.asm.emit_add_rdi_rax()  # ADD RDI, RAX (end of input)
        self.asm.emit_dec_rdi()  # DEC RDI (last char)
        
        # Check if it's a newline
        self.asm.emit_bytes(0x80, 0x3F, 0x0A)  # CMP BYTE [RDI], 0x0A
        not_newline = self.asm.create_label()
        self.asm.emit_jump_to_label(not_newline, "JNE")
        
        # Replace newline with null
        self.asm.emit_mov_byte_ptr_rdi_zero()  # MOV BYTE [RDI], 0
        self.asm.emit_jump_to_label(done, "JMP")
        
        # Not a newline - null terminate after the last char
        self.asm.mark_label(not_newline)
        self.asm.emit_bytes(0x48, 0xFF, 0xC7)  # INC RDI (move past last char)
        self.asm.emit_mov_byte_ptr_rdi_zero()  # MOV BYTE [RDI], 0
        
        self.asm.mark_label(done)
        
        # Return buffer address (still in RBX)
        self.asm.emit_mov_rax_rbx()
        
        print("DEBUG: ReadInput completed")
        return True
    
    
    # Add these methods to string_ops.py

    def compile_string_pool_init(self, node):
        """Initialize a pre-allocated string pool"""
        print("DEBUG: Initializing string pool")
        
        # Allocate a large buffer (64KB) once
        pool_size = 65536
        
        # Single mmap call for entire pool
        self.asm.emit_mov_rax_imm64(9)  # sys_mmap
        self.asm.emit_mov_rdi_imm64(0)  # addr = NULL
        self.asm.emit_mov_rsi_imm64(pool_size)  # 64KB
        self.asm.emit_mov_rdx_imm64(3)  # PROT_READ | PROT_WRITE
        self.asm.emit_mov_r10_imm64(0x22)  # MAP_PRIVATE | MAP_ANONYMOUS
        self.asm.emit_bytes(0x49, 0xC7, 0xC0, 0xFF, 0xFF, 0xFF, 0xFF)  # MOV R8, -1
        self.asm.emit_mov_r9_imm64(0)  # offset = 0
        self.asm.emit_syscall()
        
        # Store pool base address in a known location
        # We'll use a fixed memory location for simplicity
        pool_base_addr = self.asm.add_data_qword(0)  # Pool base
        pool_next_offset = self.asm.add_data_qword(0)  # Next free offset
        
        # Store pool base address
        self.asm.emit_push_rax()  # Save pool address
        self.asm.emit_load_data_address('rbx', pool_base_addr)
        self.asm.emit_bytes(0x48, 0x89, 0x03)  # MOV [RBX], RAX
        
        # Initialize next_offset to 0
        self.asm.emit_load_data_address('rbx', pool_next_offset)
        self.asm.emit_mov_rax_imm64(0)
        self.asm.emit_bytes(0x48, 0x89, 0x03)  # MOV [RBX], RAX
        
        # Return pool base address
        self.asm.emit_pop_rax()
        
        print("DEBUG: String pool initialized")
        return True

    def compile_string_pool_alloc(self, size):
        """Sub-allocate from the string pool"""
        print(f"DEBUG: Pool allocating {size} bytes")
        
        # Get current offset
        pool_next_offset = self.asm.get_data_offset('pool_next_offset')
        self.asm.emit_load_data_address('rbx', pool_next_offset)
        self.asm.emit_bytes(0x48, 0x8B, 0x03)  # MOV RAX, [RBX] - current offset
        
        # Save current offset (this is our allocation)
        self.asm.emit_push_rax()
        
        # Add size to offset
        self.asm.emit_bytes(0x48, 0x05)  # ADD RAX, imm32
        self.asm.emit_bytes(*struct.pack('<I', size))
        
        # Store new offset
        self.asm.emit_bytes(0x48, 0x89, 0x03)  # MOV [RBX], RAX
        
        # Get pool base
        pool_base_addr = self.asm.get_data_offset('pool_base_addr')
        self.asm.emit_load_data_address('rbx', pool_base_addr)
        self.asm.emit_bytes(0x48, 0x8B, 0x03)  # MOV RAX, [RBX] - pool base
        
        # Add offset to base
        self.asm.emit_pop_rbx()  # Get saved offset
        self.asm.emit_bytes(0x48, 0x01, 0xD8)  # ADD RAX, RBX
        
        # RAX now contains the allocated address
        return True

    def compile_string_pool_concat(self, node):
        """Optimized string concatenation using pool allocation"""
        if len(node.arguments) < 2:
            raise ValueError("StringPoolConcat requires 2 arguments")
        
        print("DEBUG: Pool-optimized string concatenation")
        
        # Get both strings
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_push_rax()  # Save str1
        self.compiler.compile_expression(node.arguments[1])
        self.asm.emit_push_rax()  # Save str2
        
        # Calculate total size needed
        # Get length of str1
        self.asm.emit_mov_rdi_from_stack(8)  # Get str1 from stack
        self.asm.emit_mov_rcx_imm64(0)
        
        len1_loop = self.asm.create_label()
        len1_done = self.asm.create_label()
        
        self.asm.mark_label(len1_loop)
        self.asm.emit_bytes(0x80, 0x3C, 0x0F, 0x00)  # CMP BYTE [RDI+RCX], 0
        self.asm.emit_jump_to_label(len1_done, "JE")
        self.asm.emit_bytes(0x48, 0xFF, 0xC1)  # INC RCX
        self.asm.emit_jump_to_label(len1_loop, "JMP")
        
        self.asm.mark_label(len1_done)
        self.asm.emit_push_rcx()  # Save len1
        
        # Get length of str2
        self.asm.emit_mov_rdi_from_stack(8)  # Get str2 from stack
        self.asm.emit_mov_rdx_imm64(0)
        
        len2_loop = self.asm.create_label()
        len2_done = self.asm.create_label()
        
        self.asm.mark_label(len2_loop)
        self.asm.emit_bytes(0x80, 0x3C, 0x17, 0x00)  # CMP BYTE [RDI+RDX], 0
        self.asm.emit_jump_to_label(len2_done, "JE")
        self.asm.emit_bytes(0x48, 0xFF, 0xC2)  # INC RDX
        self.asm.emit_jump_to_label(len2_loop, "JMP")
        
        self.asm.mark_label(len2_done)
        
        # Total size = len1 + len2 + 1
        self.asm.emit_pop_rcx()  # Get len1
        self.asm.emit_bytes(0x48, 0x01, 0xD1)  # ADD RCX, RDX
        self.asm.emit_bytes(0x48, 0xFF, 0xC1)  # INC RCX (for null terminator)
        
        # Allocate from pool
        self.compile_string_pool_alloc(rcx)  # Size in RCX
        self.asm.emit_mov_rdi_rax()  # Destination in RDI
        self.asm.emit_push_rdi()  # Save result address
        
        # Copy str1
        self.asm.emit_pop_rsi()  # str2 (discard for now)
        self.asm.emit_pop_rsi()  # str1
        
        copy1_loop = self.asm.create_label()
        copy1_done = self.asm.create_label()
        
        self.asm.mark_label(copy1_loop)
        self.asm.emit_bytes(0x8A, 0x06)  # MOV AL, [RSI]
        self.asm.emit_bytes(0x84, 0xC0)  # TEST AL, AL
        self.asm.emit_jump_to_label(copy1_done, "JZ")
        self.asm.emit_bytes(0x88, 0x07)  # MOV [RDI], AL
        self.asm.emit_bytes(0x48, 0xFF, 0xC6)  # INC RSI
        self.asm.emit_bytes(0x48, 0xFF, 0xC7)  # INC RDI
        self.asm.emit_jump_to_label(copy1_loop, "JMP")
        
        self.asm.mark_label(copy1_done)
        
        # Copy str2
        self.asm.emit_pop_rsi()  # str2
        
        copy2_loop = self.asm.create_label()
        copy2_done = self.asm.create_label()
        
        self.asm.mark_label(copy2_loop)
        self.asm.emit_bytes(0x8A, 0x06)  # MOV AL, [RSI]
        self.asm.emit_bytes(0x88, 0x07)  # MOV [RDI], AL
        self.asm.emit_bytes(0x84, 0xC0)  # TEST AL, AL
        self.asm.emit_jump_to_label(copy2_done, "JZ")
        self.asm.emit_bytes(0x48, 0xFF, 0xC6)  # INC RSI
        self.asm.emit_bytes(0x48, 0xFF, 0xC7)  # INC RDI
        self.asm.emit_jump_to_label(copy2_loop, "JMP")
        
        self.asm.mark_label(copy2_done)
        
        self.asm.emit_bytes(0xC6, 0x07, 0x00)  # MOV BYTE [RDI], 0
        
        # Return result address
        self.asm.emit_pop_rax()
        
        print("DEBUG: Pool concat completed")
        return True

    
    # In string_ops.py
    def compile_string_concat_pooled(self, node):
        """Concatenate strings using pool allocation with dynamic pool size"""
        if len(node.arguments) < 2:
            raise ValueError("StringConcatPooled requires 2 arguments")

        print("DEBUG: Compiling StringConcatPooled with pool allocation")

        # Save registers
        self.asm.emit_push_rbx()
        self.asm.emit_push_rcx()
        self.asm.emit_push_rdx()
        self.asm.emit_push_rsi()
        self.asm.emit_push_rdi()

        # Compile and save both string arguments
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_push_rax()  # Save first string pointer

        self.compiler.compile_expression(node.arguments[1])
        self.asm.emit_push_rax()  # Save second string pointer

        # Get pool variable offset
        pool_offset = self.compiler.variables.get('_pool_StringPool_base')
        if pool_offset is None:
            print("DEBUG: Pool not found, returning first string")
            self.asm.emit_bytes(0x48, 0x8B, 0x44, 0x24, 0x08)  # MOV RAX, [RSP+8]
            self.asm.emit_bytes(0x48, 0x83, 0xC4, 0x10)  # ADD RSP, 16
            self.asm.emit_pop_rdi()
            self.asm.emit_pop_rsi()
            self.asm.emit_pop_rdx()
            self.asm.emit_pop_rcx()
            self.asm.emit_pop_rbx()
            return True

        print(f"DEBUG: Pool found at offset {pool_offset}")

        # Load pool base address
        self.asm.emit_bytes(0x48, 0x8B, 0xBD)  # MOV RDI, [RBP + offset]
        self.asm.emit_bytes(*struct.pack('<i', pool_offset))

        # Load current pool offset
        self.asm.emit_bytes(0x48, 0x8B, 0x8D)  # MOV RCX, [RBP + offset+8]
        self.asm.emit_bytes(*struct.pack('<i', pool_offset + 8))

        # Load pool size
        self.asm.emit_bytes(0x48, 0x8B, 0x95)  # MOV RDX, [RBP + offset+16]
        self.asm.emit_bytes(*struct.pack('<i', pool_offset + 16))

        # Check if current offset is too large (10% margin)
        self.asm.emit_bytes(0x48, 0x89, 0xD0)  # MOV RAX, RDX
        self.asm.emit_bytes(0x48, 0xC1, 0xE8, 0x03)  # SHR RAX, 3
        self.asm.emit_bytes(0x48, 0x29, 0xC2)  # SUB RDX, RAX
        self.asm.emit_bytes(0x48, 0x39, 0xD1)  # CMP RCX, RDX
        overflow_label = self.asm.create_label()
        self.asm.emit_jump_to_label(overflow_label, "JAE")

        # Calculate allocation address
        self.asm.emit_bytes(0x48, 0x01, 0xCF)  # ADD RDI, RCX

        # Save start address
        self.asm.emit_push_rdi()

        # Calculate total length
        self.asm.emit_bytes(0x48, 0x8B, 0x74, 0x24, 0x10)  # MOV RSI, [RSP+16]
        self.asm.emit_bytes(0x48, 0x31, 0xDB)  # XOR RBX, RBX
        length_loop1 = self.asm.create_label()
        self.asm.mark_label(length_loop1)
        self.asm.emit_bytes(0x8A, 0x0E)  # MOV CL, [RSI]
        self.asm.emit_bytes(0x84, 0xC9)  # TEST CL, CL
        length_done1 = self.asm.create_label()
        self.asm.emit_jump_to_label(length_done1, "JZ")
        self.asm.emit_bytes(0x48, 0xFF, 0xC3)  # INC RBX
        self.asm.emit_bytes(0x48, 0xFF, 0xC6)  # INC RSI
        self.asm.emit_jump_to_label(length_loop1, "JMP")
        self.asm.mark_label(length_done1)

        self.asm.emit_bytes(0x48, 0x8B, 0x74, 0x24, 0x08)  # MOV RSI, [RSP+8]
        length_loop2 = self.asm.create_label()
        self.asm.mark_label(length_loop2)
        self.asm.emit_bytes(0x8A, 0x0E)  # MOV CL, [RSI]
        self.asm.emit_bytes(0x84, 0xC9)  # TEST CL, CL
        length_done2 = self.asm.create_label()
        self.asm.emit_jump_to_label(length_done2, "JZ")
        self.asm.emit_bytes(0x48, 0xFF, 0xC3)  # INC RBX
        self.asm.emit_bytes(0x48, 0xFF, 0xC6)  # INC RSI
        self.asm.emit_jump_to_label(length_loop2, "JMP")
        self.asm.mark_label(length_done2)

        self.asm.emit_bytes(0x48, 0xFF, 0xC3)  # INC RBX (null terminator)

        # Check length against remaining space
        self.asm.emit_bytes(0x48, 0x8B, 0x95)  # MOV RDX, [RBP + offset+16]
        self.asm.emit_bytes(*struct.pack('<i', pool_offset + 16))
        self.asm.emit_bytes(0x48, 0x29, 0xCA)  # SUB RDX, RCX
        self.asm.emit_bytes(0x48, 0x39, 0xD3)  # CMP RBX, RDX
        self.asm.emit_jump_to_label(overflow_label, "JAE")

        # Restore RDI to start of allocation
        self.asm.emit_bytes(0x48, 0x8B, 0x3C, 0x24)  # MOV RDI, [RSP]

        # Copy first string
        self.asm.emit_bytes(0x48, 0x8B, 0x74, 0x24, 0x10)  # MOV RSI, [RSP+16]
        copy1_loop = self.asm.create_label()
        self.asm.mark_label(copy1_loop)
        self.asm.emit_bytes(0x8A, 0x0E)  # MOV CL, [RSI]
        self.asm.emit_bytes(0x84, 0xC9)  # TEST CL, CL
        copy1_done = self.asm.create_label()
        self.asm.emit_jump_to_label(copy1_done, "JZ")
        self.asm.emit_bytes(0x88, 0x0F)  # MOV [RDI], CL
        self.asm.emit_bytes(0x48, 0xFF, 0xC6)  # INC RSI
        self.asm.emit_bytes(0x48, 0xFF, 0xC7)  # INC RDI
        self.asm.emit_jump_to_label(copy1_loop, "JMP")
        self.asm.mark_label(copy1_done)

        # Copy second string
        self.asm.emit_bytes(0x48, 0x8B, 0x74, 0x24, 0x08)  # MOV RSI, [RSP+8]
        copy2_loop = self.asm.create_label()
        self.asm.mark_label(copy2_loop)
        self.asm.emit_bytes(0x8A, 0x0E)  # MOV CL, [RSI]
        self.asm.emit_bytes(0x84, 0xC9)  # TEST CL, CL
        copy2_done = self.asm.create_label()
        self.asm.emit_jump_to_label(copy2_done, "JZ")
        self.asm.emit_bytes(0x88, 0x0F)  # MOV [RDI], CL
        self.asm.emit_bytes(0x48, 0xFF, 0xC6)  # INC RSI
        self.asm.emit_bytes(0x48, 0xFF, 0xC7)  # INC RDI
        self.asm.emit_jump_to_label(copy2_loop, "JMP")
        self.asm.mark_label(copy2_done)

        self.asm.emit_bytes(0xC6, 0x07, 0x00)  # MOV BYTE [RDI], 0
        
        # Update pool offset
        self.asm.emit_bytes(0x48, 0x8B, 0x04, 0x24)  # MOV RAX, [RSP] (start address)
        self.asm.emit_bytes(0x48, 0x89, 0xF9)  # MOV RCX, RDI (end address)
        self.asm.emit_bytes(0x48, 0xFF, 0xC1)  # INC RCX (past null)
        self.asm.emit_bytes(0x48, 0x29, 0xC1)  # SUB RCX, RAX (bytes used)
        self.asm.emit_bytes(0x48, 0x8B, 0x95)  # MOV RDX, [RBP + offset+8]
        self.asm.emit_bytes(*struct.pack('<i', pool_offset + 8))
        self.asm.emit_bytes(0x48, 0x01, 0xCA)  # ADD RDX, RCX
        self.asm.emit_bytes(0x48, 0x89, 0x95)  # MOV [RBP + offset+8], RDX
        self.asm.emit_bytes(*struct.pack('<i', pool_offset + 8))

        # Return result - pop saved allocation address
        self.asm.emit_pop_rax()  # Get the saved allocation address
        self.asm.emit_bytes(0x48, 0x83, 0xC4, 0x10)  # ADD RSP, 16 - clean string pointers
        
        # Jump to done
        done_label = self.asm.create_label()
        self.asm.emit_jump_to_label(done_label, "JMP")

        # Overflow case
        self.asm.mark_label(overflow_label)
        print("DEBUG: Pool overflow, returning first string")
        self.asm.emit_bytes(0x48, 0x8B, 0x44, 0x24, 0x08)  # MOV RAX, [RSP+8]
        self.asm.emit_bytes(0x48, 0x83, 0xC4, 0x18)  # ADD RSP, 24 (saved addr + 2 strings)

        # Done - restore registers
        self.asm.mark_label(done_label)
        self.asm.emit_pop_rdi()
        self.asm.emit_pop_rsi()
        self.asm.emit_pop_rdx()
        self.asm.emit_pop_rcx()
        self.asm.emit_pop_rbx()

        print("DEBUG: StringConcatPooled completed")
        return True
    
    def compile_char_to_string(self, node):
        """Convert single ASCII character code to string"""
        if len(node.arguments) < 1:
            raise ValueError("CharToString requires 1 argument (ASCII code)")
        
        print("DEBUG: Compiling CharToString")
        
        # Get ASCII code
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_push_rax()  # Save ASCII code
        
        # Allocate 2 bytes (char + null terminator)
        self.asm.emit_mov_rax_imm64(9)  # mmap syscall
        self.asm.emit_mov_rdi_imm64(0)  # addr = NULL
        self.asm.emit_mov_rsi_imm64(2)  # length = 2 bytes
        self.asm.emit_mov_rdx_imm64(0x3)  # PROT_READ | PROT_WRITE
        self.asm.emit_mov_r10_imm64(0x22)  # MAP_PRIVATE | MAP_ANONYMOUS
        self.asm.emit_bytes(0x49, 0xC7, 0xC0, 0xFF, 0xFF, 0xFF, 0xFF)  # MOV R8, -1
        self.asm.emit_mov_r9_imm64(0)  # offset = 0
        self.asm.emit_syscall()
        
        # RAX now contains allocated address
        self.asm.emit_mov_rdi_rax()  # Save address in RDI
        
        # Store ASCII character
        self.asm.emit_pop_rax()  # Get ASCII code
        self.asm.emit_bytes(0x88, 0x07)  # MOV [RDI], AL (low byte of RAX)
        
        # Store null terminator
        self.asm.emit_bytes(0xC6, 0x47, 0x01, 0x00)  # MOV BYTE [RDI+1], 0
        
        # Return string address
        self.asm.emit_mov_rax_rdi()
        
        print("DEBUG: CharToString completed")
        return True

    def compile_string_to_upper(self, node):
        """Convert string to uppercase"""
        if len(node.arguments) < 1:
            raise ValueError("StringToUpper requires 1 argument")
        
        print("DEBUG: Compiling StringToUpper")
        
        # Save registers
        self.asm.emit_push_rbx()
        self.asm.emit_push_rcx()
        self.asm.emit_push_rdx()
        self.asm.emit_push_rsi()
        self.asm.emit_push_rdi()
        
        # Get source string
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_mov_rsi_rax()  # Source in RSI
        
        # Calculate string length first
        self.asm.emit_push_rsi()  # Save source for later
        self.asm.emit_mov_rbx_rsi()
        self.asm.emit_mov_rcx_imm64(0)
        
        len_loop = self.asm.create_label()
        len_done = self.asm.create_label()
        
        self.asm.mark_label(len_loop)
        self.asm.emit_bytes(0x8A, 0x03)  # MOV AL, [RBX]
        self.asm.emit_bytes(0x84, 0xC0)  # TEST AL, AL
        self.asm.emit_jump_to_label(len_done, "JZ")
        self.asm.emit_bytes(0x48, 0xFF, 0xC3)  # INC RBX
        self.asm.emit_bytes(0x48, 0xFF, 0xC1)  # INC RCX
        self.asm.emit_jump_to_label(len_loop, "JMP")
        
        self.asm.mark_label(len_done)
        # RCX now contains length
        
        # Allocate new string (length + 1 for null terminator)
        self.asm.emit_bytes(0x48, 0xFF, 0xC1)  # INC RCX
        self.asm.emit_mov_rax_imm64(9)  # mmap
        self.asm.emit_mov_rdi_imm64(0)
        self.asm.emit_mov_rsi_rcx()  # size
        self.asm.emit_mov_rdx_imm64(0x3)
        self.asm.emit_mov_r10_imm64(0x22)
        self.asm.emit_bytes(0x49, 0xC7, 0xC0, 0xFF, 0xFF, 0xFF, 0xFF)  # MOV R8, -1
        self.asm.emit_mov_r9_imm64(0)
        self.asm.emit_syscall()
        
        self.asm.emit_mov_rdi_rax()  # Destination in RDI
        self.asm.emit_pop_rsi()  # Restore source
        self.asm.emit_push_rdi()  # Save result address
        
        # Copy and convert loop
        copy_loop = self.asm.create_label()
        copy_done = self.asm.create_label()
        
        self.asm.mark_label(copy_loop)
        self.asm.emit_bytes(0x8A, 0x06)  # MOV AL, [RSI]
        self.asm.emit_bytes(0x84, 0xC0)  # TEST AL, AL
        self.asm.emit_jump_to_label(copy_done, "JZ")
        
        # Check if lowercase letter (a-z: 97-122)
        self.asm.emit_bytes(0x3C, 0x61)  # CMP AL, 'a'
        not_lower = self.asm.create_label()
        self.asm.emit_jump_to_label(not_lower, "JB")
        self.asm.emit_bytes(0x3C, 0x7A)  # CMP AL, 'z'
        self.asm.emit_jump_to_label(not_lower, "JA")
        
        # Convert to uppercase (subtract 32)
        self.asm.emit_bytes(0x2C, 0x20)  # SUB AL, 32
        
        self.asm.mark_label(not_lower)
        self.asm.emit_bytes(0x88, 0x07)  # MOV [RDI], AL
        self.asm.emit_bytes(0x48, 0xFF, 0xC6)  # INC RSI
        self.asm.emit_bytes(0x48, 0xFF, 0xC7)  # INC RDI
        self.asm.emit_jump_to_label(copy_loop, "JMP")
        
        self.asm.mark_label(copy_done)
        self.asm.emit_bytes(0xC6, 0x07, 0x00)  # MOV BYTE [RDI], 0
        
        # Return result
        self.asm.emit_pop_rax()  # Get result address
        
        # Restore registers
        self.asm.emit_pop_rdi()
        self.asm.emit_pop_rsi()
        self.asm.emit_pop_rdx()
        self.asm.emit_pop_rcx()
        self.asm.emit_pop_rbx()
        
        print("DEBUG: StringToUpper completed")
        return True

    def compile_string_to_lower(self, node):
        """Convert string to lowercase"""
        if len(node.arguments) < 1:
            raise ValueError("StringToLower requires 1 argument")
        
        print("DEBUG: Compiling StringToLower")
        
        # Save registers
        self.asm.emit_push_rbx()
        self.asm.emit_push_rcx()
        self.asm.emit_push_rdx()
        self.asm.emit_push_rsi()
        self.asm.emit_push_rdi()
        
        # Get source string
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_mov_rsi_rax()  # Source in RSI
        
        # Calculate string length
        self.asm.emit_push_rsi()
        self.asm.emit_mov_rbx_rsi()
        self.asm.emit_mov_rcx_imm64(0)
        
        len_loop = self.asm.create_label()
        len_done = self.asm.create_label()
        
        self.asm.mark_label(len_loop)
        self.asm.emit_bytes(0x8A, 0x03)  # MOV AL, [RBX]
        self.asm.emit_bytes(0x84, 0xC0)  # TEST AL, AL
        self.asm.emit_jump_to_label(len_done, "JZ")
        self.asm.emit_bytes(0x48, 0xFF, 0xC3)  # INC RBX
        self.asm.emit_bytes(0x48, 0xFF, 0xC1)  # INC RCX
        self.asm.emit_jump_to_label(len_loop, "JMP")
        
        self.asm.mark_label(len_done)
        
        # Allocate new string
        self.asm.emit_bytes(0x48, 0xFF, 0xC1)  # INC RCX
        self.asm.emit_mov_rax_imm64(9)  # mmap
        self.asm.emit_mov_rdi_imm64(0)
        self.asm.emit_mov_rsi_rcx()
        self.asm.emit_mov_rdx_imm64(0x3)
        self.asm.emit_mov_r10_imm64(0x22)
        self.asm.emit_bytes(0x49, 0xC7, 0xC0, 0xFF, 0xFF, 0xFF, 0xFF)  # MOV R8, -1
        self.asm.emit_mov_r9_imm64(0)
        self.asm.emit_syscall()
        
        self.asm.emit_mov_rdi_rax()  # Destination in RDI
        self.asm.emit_pop_rsi()  # Restore source
        self.asm.emit_push_rdi()  # Save result
        
        # Copy and convert loop
        copy_loop = self.asm.create_label()
        copy_done = self.asm.create_label()
        
        self.asm.mark_label(copy_loop)
        self.asm.emit_bytes(0x8A, 0x06)  # MOV AL, [RSI]
        self.asm.emit_bytes(0x84, 0xC0)  # TEST AL, AL
        self.asm.emit_jump_to_label(copy_done, "JZ")
        
        # Check if uppercase letter (A-Z: 65-90)
        self.asm.emit_bytes(0x3C, 0x41)  # CMP AL, 'A'
        not_upper = self.asm.create_label()
        self.asm.emit_jump_to_label(not_upper, "JB")
        self.asm.emit_bytes(0x3C, 0x5A)  # CMP AL, 'Z'
        self.asm.emit_jump_to_label(not_upper, "JA")
        
        # Convert to lowercase (add 32)
        self.asm.emit_bytes(0x04, 0x20)  # ADD AL, 32
        
        self.asm.mark_label(not_upper)
        self.asm.emit_bytes(0x88, 0x07)  # MOV [RDI], AL
        self.asm.emit_bytes(0x48, 0xFF, 0xC6)  # INC RSI
        self.asm.emit_bytes(0x48, 0xFF, 0xC7)  # INC RDI
        self.asm.emit_jump_to_label(copy_loop, "JMP")
        
        self.asm.mark_label(copy_done)
        self.asm.emit_bytes(0xC6, 0x07, 0x00)  # MOV BYTE [RDI], 0
        
        # Return result
        self.asm.emit_pop_rax()
        
        # Restore registers
        self.asm.emit_pop_rdi()
        self.asm.emit_pop_rsi()
        self.asm.emit_pop_rdx()
        self.asm.emit_pop_rcx()
        self.asm.emit_pop_rbx()
        
        print("DEBUG: StringToLower completed")
        return True

    def compile_string_contains(self, node):
        """Check if string contains substring (naive search)"""
        if len(node.arguments) < 2:
            raise ValueError("StringContains requires 2 arguments")
        
        print("DEBUG: Compiling StringContains")
        
        # Save registers
        self.asm.emit_push_rbx()
        self.asm.emit_push_rcx()
        self.asm.emit_push_rdx()
        self.asm.emit_push_rsi()
        self.asm.emit_push_rdi()
        self.asm.emit_push_r8()
        self.asm.emit_push_r9()
        
        # Get haystack (string to search in)
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_mov_rdi_rax()  # Haystack in RDI
        
        # Get needle (string to search for)
        self.compiler.compile_expression(node.arguments[1])
        self.asm.emit_mov_rsi_rax()  # Needle in RSI
        
        # Main search loop
        outer_loop = self.asm.create_label()
        found = self.asm.create_label()
        not_found = self.asm.create_label()
        inner_loop = self.asm.create_label()
        continue_outer = self.asm.create_label()
        
        self.asm.mark_label(outer_loop)
        # Check if we've reached end of haystack
        self.asm.emit_bytes(0x8A, 0x07)  # MOV AL, [RDI]
        self.asm.emit_bytes(0x84, 0xC0)  # TEST AL, AL
        self.asm.emit_jump_to_label(not_found, "JZ")
        
        # Set up for inner comparison
        self.asm.emit_mov_r8_rdi()  # R8 = current haystack position
        self.asm.emit_mov_r9_rsi()  # R9 = needle start
        
        self.asm.mark_label(inner_loop)
        # Load characters to compare
        self.asm.emit_bytes(0x41, 0x8A, 0x01)  # MOV AL, [R9]
        self.asm.emit_bytes(0x41, 0x8A, 0x18)  # MOV BL, [R8]
        
        # Check if we've matched entire needle
        self.asm.emit_bytes(0x84, 0xC0)  # TEST AL, AL
        self.asm.emit_jump_to_label(found, "JZ")  # Needle ended = match found
        
        # Compare characters
        self.asm.emit_bytes(0x38, 0xD8)  # CMP AL, BL
        self.asm.emit_jump_to_label(continue_outer, "JNE")
        
        # Characters match, continue inner loop
        self.asm.emit_bytes(0x49, 0xFF, 0xC0)  # INC R8
        self.asm.emit_bytes(0x49, 0xFF, 0xC1)  # INC R9
        self.asm.emit_jump_to_label(inner_loop, "JMP")
        
        self.asm.mark_label(continue_outer)
        # No match at this position, try next
        self.asm.emit_bytes(0x48, 0xFF, 0xC7)  # INC RDI
        self.asm.emit_jump_to_label(outer_loop, "JMP")
        
        self.asm.mark_label(found)
        self.asm.emit_mov_rax_imm64(1)
        done_label = self.asm.create_label()
        self.asm.emit_jump_to_label(done_label, "JMP")
        
        self.asm.mark_label(not_found)
        self.asm.emit_mov_rax_imm64(0)
        
        self.asm.mark_label(done_label)
        
        # Restore registers
        self.asm.emit_pop_r9()
        self.asm.emit_pop_r8()
        self.asm.emit_pop_rdi()
        self.asm.emit_pop_rsi()
        self.asm.emit_pop_rdx()
        self.asm.emit_pop_rcx()
        self.asm.emit_pop_rbx()
        
        print("DEBUG: StringContains completed")
        return True
    
    
    def _emit_strlen(self):
        """
        Emit inline strlen(RDI).
        Expects: RDI = pointer to string (NULL-terminated)
        Returns: RAX = length (0 if NULL)
        Preserves: RDI
        """
        start_label = self.asm.create_label()
        end_label = self.asm.create_label()
        
        # Clear RAX (counter)
        self.asm.emit_bytes(0x48, 0x31, 0xC0)   # XOR RAX, RAX
        
        # NULL check
        self.asm.emit_test_rdi_rdi()
        self.asm.emit_jump_to_label(end_label, "JZ")  # If NULL, return 0
        
        # Save RDI
        self.asm.emit_push_rdi()
        
        self.asm.mark_label(start_label)
        self.asm.emit_bytes(0x80, 0x3F, 0x00)   # CMP BYTE [RDI], 0
        null_found = self.asm.create_label()
        self.asm.emit_jump_to_label(null_found, "JE")
        self.asm.emit_bytes(0x48, 0xFF, 0xC7)   # INC RDI
        self.asm.emit_bytes(0x48, 0xFF, 0xC0)   # INC RAX
        self.asm.emit_jump_to_label(start_label, "JMP")
        
        self.asm.mark_label(null_found)
        # Restore RDI (only if we pushed it)
        self.asm.emit_pop_rdi()
        
        self.asm.mark_label(end_label)
        # Result in RAX
        return True
    
    def compile_string_substring(self, node):
        """Extract substring(str, start_index, length)"""
        if len(node.arguments) != 3:
            raise ValueError("StringSubstring requires string, start, length")
        
        print("DEBUG: Compiling StringSubstring")
        
        # Get arguments
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_push_rax()  # String
        self.compiler.compile_expression(node.arguments[1])
        self.asm.emit_push_rax()  # Start
        self.compiler.compile_expression(node.arguments[2])
        self.asm.emit_mov_rcx_rax()  # Length in RCX
        
        # Pop start and string
        self.asm.emit_pop_rdx()  # RDX = start
        self.asm.emit_pop_rsi()  # RSI = string
        
        # NULL check
        self.asm.emit_test_rsi_rsi()
        null_case = self.asm.create_label()
        done_label = self.asm.create_label()
        self.asm.emit_jump_to_label(null_case, "JZ")
        
        # Allocate buffer (length + 1)
        self.asm.emit_mov_rax_rcx()
        self.asm.emit_bytes(0x48, 0xFF, 0xC0)  # INC RAX
        self.asm.emit_mov_rsi_rax()
        
        self.asm.emit_mov_rax_imm64(9)  # mmap
        self.asm.emit_mov_rdi_imm64(0)
        self.asm.emit_mov_rdx_imm64(3)
        self.asm.emit_mov_r10_imm64(0x22)
        self.asm.emit_bytes(0x49, 0xC7, 0xC0, 0xFF, 0xFF, 0xFF, 0xFF)
        self.asm.emit_mov_r9_imm64(0)
        self.asm.emit_syscall()
        
        self.asm.emit_mov_rdi_rax()  # Destination
        self.asm.emit_push_rdi()  # Save for return
        
        # Adjust source to start position
        self.asm.emit_pop_rsi()  # Original string
        self.asm.emit_pop_rdx()  # Start index  
        self.asm.emit_bytes(0x48, 0x01, 0xD6)  # ADD RSI, RDX
        
        # Copy loop
        copy_loop = self.asm.create_label()
        copy_done = self.asm.create_label()
        self.asm.mark_label(copy_loop)
        self.asm.emit_test_rcx_rcx()
        self.asm.emit_jump_to_label(copy_done, "JZ")
        self.asm.emit_bytes(0x8A, 0x06)  # MOV AL, [RSI]
        self.asm.emit_bytes(0x84, 0xC0)  # TEST AL, AL (check null)
        self.asm.emit_jump_to_label(copy_done, "JZ")
        self.asm.emit_bytes(0x88, 0x07)  # MOV [RDI], AL
        self.asm.emit_bytes(0x48, 0xFF, 0xC6)  # INC RSI
        self.asm.emit_bytes(0x48, 0xFF, 0xC7)  # INC RDI
        self.asm.emit_bytes(0x48, 0xFF, 0xC9)  # DEC RCX
        self.asm.emit_jump_to_label(copy_loop, "JMP")
        
        self.asm.mark_label(copy_done)
        self.asm.emit_bytes(0xC6, 0x07, 0x00)  # MOV BYTE [RDI], 0
        self.asm.emit_pop_rax()  # Return buffer
        self.asm.emit_jump_to_label(done_label, "JMP")
        
        self.asm.mark_label(null_case)
        empty_str = self.asm.add_string("")
        self.asm.emit_load_data_address('rax', empty_str)
        
        self.asm.mark_label(done_label)
        return True