#!/usr/bin/env python3
"""
Control Flow Module for AILANG Compiler
Handles if/else and while loops with proper condition type support
"""

import sys
import os
import struct
from ailang_parser.ailang_ast import *

class ControlFlow:
    """Handles control flow constructs"""
    
    def __init__(self, compiler_context):
        self.compiler = compiler_context
        self.asm = compiler_context.asm
        self.loop_labels_stack = []
    
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
        """Compile While loop using the modern label-based jump system"""
        try:
            print("DEBUG: Compiling While loop (using label system)")
            
            loop_start_label = self.asm.create_label()
            loop_end_label = self.asm.create_label()
            
            # Push the labels onto the stack so break/continue can find them
            self.loop_labels_stack.append((loop_start_label, loop_end_label))
            
            # Mark the start of the loop
            self.asm.mark_label(loop_start_label)
            
            # Compile the condition
            self.compile_condition(node.condition)
            
            # Test if the condition is false (RAX == 0) and jump to the end if so
            self.asm.emit_bytes(0x48, 0x83, 0xF8, 0x00)  # CMP RAX, 0
            self.asm.emit_jump_to_label(loop_end_label, "JE")
            
            # Compile the loop body
            for stmt in node.body:
                self.compiler.compile_node(stmt)
            
            # Jump back to the start to re-evaluate the condition
            self.asm.emit_jump_to_label(loop_start_label, "JMP")
            
            # Mark the end of the loop, where the exit jump will land
            self.asm.mark_label(loop_end_label)
            
            # Pop the labels off the stack after the loop is done
            self.loop_labels_stack.pop()
            
            
            print("DEBUG: While loop compilation completed (label system)")
            
        except Exception as e:
            print(f"ERROR: While loop compilation failed: {str(e)}")
            raise
    
    def compile_if_condition(self, node):
        """Compile If condition using the modern label-based jump system"""
        try:
            print("DEBUG: Compiling If condition (using label system)")
            
            else_label = self.asm.create_label()
            end_label = self.asm.create_label()
            
            # Compile the condition
            self.compile_condition(node.condition)
            
            # Test if condition is false (RAX == 0) and jump to the else block
            self.asm.emit_bytes(0x48, 0x83, 0xF8, 0x00)  # CMP RAX, 0
            self.asm.emit_jump_to_label(else_label, "JE")
            
            # Compile the 'then' block
            for stmt in node.then_body:
                self.compiler.compile_node(stmt)
            
            # If there's an 'else' block, jump over it to the end
            if node.else_body:
                self.asm.emit_jump_to_label(end_label, "JMP")
            
            # Mark the start of the 'else' block
            self.asm.mark_label(else_label)
            
            # Compile the 'else' block if it exists
            if node.else_body:
                for stmt in node.else_body:
                    self.compiler.compile_node(stmt)
            
            # Mark the end of the entire if-else structure
            self.asm.mark_label(end_label)

            print("DEBUG: If condition compilation completed (label system)")
            
        except Exception as e:
            print(f"ERROR: If condition compilation failed: {str(e)}")
            raise

        
        
        
    def compile_fork(self, node):
        """Compile Fork construct"""
        try:
            print("DEBUG: Compiling Fork construct")
            
            # Generate labels
            false_label = self.asm.create_label()
            end_label = self.asm.create_label()
            
            # Compile condition
            self.compile_condition(node.condition)
            
            # Test condition and jump if false
            self.asm.emit_bytes(0x48, 0x83, 0xF8, 0x00)  # CMP RAX, 0
            self.asm.emit_jump_to_label(false_label, "JE")
            
            # Compile true block
            for stmt in node.true_block:
                self.compiler.compile_node(stmt)
            self.asm.emit_jump_to_label(end_label, "JMP")
            
            # False block
            self.asm.mark_label(false_label)
            for stmt in node.false_block:
                self.compiler.compile_node(stmt)
            
            # End
            self.asm.mark_label(end_label)
            
            print("DEBUG: Fork compilation completed")
            
        except Exception as e:
            print(f"ERROR: Fork compilation failed: {str(e)}")
            raise

    def compile_branch(self, node):
        """Compile Branch construct"""
        try:
            print("DEBUG: Compiling Branch construct")
            
            # Compile expression once
            self.compiler.compile_expression(node.expression)
            self.asm.emit_push_rax()  # Save expression value
            
            end_label = self.asm.create_label()
            case_labels = []
            
            # Create labels for each case
            for _ in node.cases:
                case_labels.append(self.asm.create_label())
            
            # Generate comparison chain
            for i, (value, block) in enumerate(node.cases):
                self.asm.emit_bytes(0x48, 0x8B, 0x04, 0x24)  # MOV RAX, [RSP]
                
                # Compare with case value
                self.compiler.compile_expression(value)
                self.asm.emit_mov_rbx_rax()
                self.asm.emit_pop_rax()
                self.asm.emit_push_rax()
                self.asm.emit_bytes(0x48, 0x39, 0xD8)  # CMP RAX, RBX
                
                # Jump to case if equal
                self.asm.emit_jump_to_label(case_labels[i], "JE")
            
            # Default case or end
            if node.default:
                for stmt in node.default:
                    self.compiler.compile_node(stmt)
            self.asm.emit_jump_to_label(end_label, "JMP")
            
            # Compile case blocks
            for i, (_, block) in enumerate(node.cases):
                self.asm.mark_label(case_labels[i])
                for stmt in block:
                    self.compiler.compile_node(stmt)
                self.asm.emit_jump_to_label(end_label, "JMP")
            
            # End - clean up stack
            self.asm.mark_label(end_label)
            self.asm.emit_pop_rax()  # Remove saved expression value
            
            print("DEBUG: Branch compilation completed")
            
        except Exception as e:
            print(f"ERROR: Branch compilation failed: {str(e)}")
            raise
    
        
    def compile_break(self, node):
        """
        Handles BreakLoop nodes. Jumps to the end label of the current loop.
        """
        if not self.loop_labels_stack:
            raise ValueError("Syntax Error: 'BreakLoop' used outside of a loop.")

        end_label = self.loop_labels_stack[-1][1]
        self.asm.emit_jump_to_label(end_label, "JMP")

    def compile_continue(self, node):
        """
        Handles ContinueLoop nodes. Jumps to the start label (the condition check)
        of the current loop.
        """
        if not self.loop_labels_stack:
            raise ValueError("Syntax Error: 'ContinueLoop' used outside of a loop.")

        start_label = self.loop_labels_stack[-1][0]
        self.asm.emit_jump_to_label(start_label, "JMP")