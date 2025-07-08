#!/usr/bin/env python3
"""
Control Flow Module for AILANG Compiler
Handles if/else and while loops with proper condition type support
"""

import struct
from ailang.parser.ailang_ast import *

class ControlFlow:
    """Handles control flow constructs"""
    
    def __init__(self, compiler_context):
        self.compiler = compiler_context
        self.asm = compiler_context.asm
    
    def compile_condition(self, condition):
        """Compile different types of conditions"""
        try:
            if isinstance(condition, FunctionCall):
                # Function call condition: LessThan(x, y), GreaterThan(a, b), etc.
                print(f"DEBUG: Compiling FunctionCall condition: {condition.function}")
                self.compiler.compile_function_call(condition)
            elif isinstance(condition, Identifier):
                # Variable condition: flag, isRunning, etc.
                print(f"DEBUG: Compiling Identifier condition: {condition.name}")
                self.compiler.compile_expression(condition)
            elif isinstance(condition, Boolean):
                # Boolean literal condition: True, False
                print(f"DEBUG: Compiling Boolean condition: {condition.value}")
                if condition.value:  # True
                    self.asm.emit_mov_rax_imm64(1)
                else:  # False
                    self.asm.emit_mov_rax_imm64(0)
            elif isinstance(condition, Number):
                # Number condition: 0 = false, non-zero = true
                print(f"DEBUG: Compiling Number condition: {condition.value}")
                self.asm.emit_mov_rax_imm64(int(condition.value))
            else:
                # Fallback to expression compiler for other types
                print(f"DEBUG: Compiling generic condition: {type(condition).__name__}")
                self.compiler.compile_expression(condition)
        except Exception as e:
            print(f"ERROR: Condition compilation failed: {str(e)}")
            raise
    
    def compile_while_loop(self, node):
        """Compile While loop with flexible condition support"""
        try:
            print("DEBUG: Compiling While loop")
            
            loop_start = self.compiler.get_label()
            loop_end = self.compiler.get_label()
            
            loop_start_pos = len(self.asm.code)
            self.asm.labels[loop_start] = loop_start_pos
            print(f"DEBUG: While loop start at position {loop_start_pos}")
            
            # Compile condition (now supports multiple types)
            self.compile_condition(node.condition)
            
            # Test if condition is false (RAX == 0)
            self.asm.emit_bytes(0x48, 0x83, 0xF8, 0x00)  # CMP RAX, 0
            self.asm.emit_bytes(0x0F, 0x84, 0x00, 0x00, 0x00, 0x00)  # JE loop_end
            je_pos = len(self.asm.code) - 4
            print(f"DEBUG: Conditional jump at position {je_pos}")
            
            # Compile loop body
            for stmt in node.body:
                self.compiler.compile_node(stmt)
            
            # Jump back to loop start
            current_pos = len(self.asm.code)
            jmp_offset = loop_start_pos - (current_pos + 5)
            self.asm.emit_bytes(0xE9)  # JMP
            self.asm.emit_bytes(*struct.pack('<i', jmp_offset))
            print(f"DEBUG: Jump back to loop start, offset: {jmp_offset}")
            
            # Fix the conditional jump to point here (loop end)
            loop_end_pos = len(self.asm.code)
            self.asm.labels[loop_end] = loop_end_pos
            je_offset = loop_end_pos - (je_pos + 4)
            self.asm.code[je_pos:je_pos+4] = struct.pack('<i', je_offset)
            print(f"DEBUG: While loop end at position {loop_end_pos}, fixed JE offset: {je_offset}")
            
        except Exception as e:
            print(f"ERROR: While loop compilation failed: {str(e)}")
            raise
    
    def compile_if_condition(self, node):
        """Compile If condition with flexible condition support"""
        try:
            print("DEBUG: Compiling If condition")
            
            # Compile condition (now supports multiple types)
            self.compile_condition(node.condition)
            
            # Test if condition is false (RAX == 0)
            self.asm.emit_bytes(0x48, 0x83, 0xF8, 0x00)  # CMP RAX, 0
            else_label = self.compiler.get_label()
            end_label = self.compiler.get_label()
            
            self.asm.emit_bytes(0x0F, 0x84, 0x00, 0x00, 0x00, 0x00)  # JE else_block
            je_pos = len(self.asm.code) - 4
            print(f"DEBUG: If condition JE at position {je_pos}")
            
            # Compile then block
            for stmt in node.then_body:
                self.compiler.compile_node(stmt)
            
            # Jump to end (skip else block)
            self.asm.emit_bytes(0xE9, 0x00, 0x00, 0x00, 0x00)  # JMP end
            jmp_pos = len(self.asm.code) - 4
            print(f"DEBUG: Then block JMP at position {jmp_pos}")
            
            # Fix conditional jump to point to else block
            else_pos = len(self.asm.code)
            self.asm.labels[else_label] = else_pos
            je_offset = else_pos - (je_pos + 4)
            self.asm.code[je_pos:je_pos+4] = struct.pack('<i', je_offset)
            print(f"DEBUG: Else label at position {else_pos}, JE offset: {je_offset}")
            
            # Compile else block (if it exists)
            if node.else_body:
                for stmt in node.else_body:
                    self.compiler.compile_node(stmt)
            
            # Fix jump to end
            end_pos = len(self.asm.code)
            self.asm.labels[end_label] = end_pos
            jmp_offset = end_pos - (jmp_pos + 4)
            self.asm.code[jmp_pos:jmp_pos+4] = struct.pack('<i', jmp_offset)
            print(f"DEBUG: If end at position {end_pos}, JMP offset: {jmp_offset}")
            
        except Exception as e:
            print(f"ERROR: If condition compilation failed: {str(e)}")            raise
