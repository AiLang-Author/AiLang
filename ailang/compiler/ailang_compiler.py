#!/usr/bin/env python3
"""
AILANG to x86-64 Compiler - Fixed Main Orchestrator
Coordinates compilation process using modular components with proper integration
"""

import struct
import sys
import os
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', 'ailang_parser')))
from ailang_ast import *

from ailang.compiler.x64_assembler import X64Assembler
from ailang.compiler.elf_generator import ELFGenerator
from ailang_ast import *
from ailang.compiler.modules.arithmetic_ops import ArithmeticOps
from ailang.compiler.modules.fileio_ops import FileIOOps
from ailang.compiler.modules.control_flow import ControlFlow
from ailang.compiler.modules.memory_manager import MemoryManager
from ailang.compiler.modules.string_ops import StringOps
from ailang.compiler.modules.expression_compiler import ExpressionCompiler
from ailang.compiler.modules.code_generator import CodeGenerator
from ailang.compiler.modules.lowlevel_ops import LowLevelOps
from ailang.compiler.modules.virtual_memory import VirtualMemoryOps

class AILANGToX64Compiler:
    """Main compiler orchestrator for AILANG to x86-64 compilation"""
    
    def __init__(self, vm_mode="user"):
        self.asm = X64Assembler()
        self.elf = ELFGenerator()
        self.variables = {}  # Variable name -> stack offset
        self.stack_size = 0  # Total stack size
        self.label_counter = 0  # For unique labels
        self.max_depth = 0  # Track recursion depth
        self.acronym_table = {}
        self.file_handles = {}  # file_handle_name -> file_descriptor_variable
        self.file_buffer_size = 65536  # Default 64KB buffer
        self.open_files = {}  # Track open files
        
        # VM Mode Selection
        self.vm_mode = vm_mode.lower()
        print(f"DEBUG: VM Mode: {self.vm_mode.upper()}")
        
        # Initialize modules with dependency injection
        self.arithmetic = ArithmeticOps(self)
        self.fileio = FileIOOps(self)
        self.control_flow = ControlFlow(self)
        self.memory = MemoryManager(self)
        self.strings = StringOps(self)
        self.expressions = ExpressionCompiler(self)
        self.codegen = CodeGenerator(self)
        self.lowlevel = LowLevelOps(self)
        
        # VM OPERATIONS MODULE SELECTION
        if self.vm_mode == "kernel":
            from ailang.compiler.modules.virtual_memory import VirtualMemoryOps
            self.virtual_memory = VirtualMemoryOps(self)
            print("DEBUG: Using KERNEL MODE VM operations (privileged instructions)")
        else:  # Default to user mode
            from ailang.compiler.modules.usermode_vm_ops import VirtualMemoryOpsUserMode
            self.virtual_memory = VirtualMemoryOpsUserMode(self)
            print("DEBUG: Using USER MODE VM operations (safe for testing)")

    def get_label(self):
        """Generate a unique label"""
        label = f"L{self.label_counter}"
        self.label_counter += 1
        print(f"DEBUG: Generated label {label}")
        return label
    
    def compile_node(self, node):
        """Dispatch node compilation to appropriate module"""
        try:
            print(f"DEBUG: Compiling node: {type(node).__name__}")
            
            if isinstance(node, Program):
                self.memory.compile_program(node)
            
            elif isinstance(node, FunctionCall):
                self.compile_function_call(node)
            
            elif isinstance(node, PrintMessage):
                self.strings.compile_print_message(node)
            
            elif isinstance(node, Assignment):
                self.memory.compile_assignment(node)
            
            elif isinstance(node, While):
                self.control_flow.compile_while_loop(node)
            
            elif isinstance(node, If):
                self.control_flow.compile_if_condition(node)
            
            elif isinstance(node, AcronymDefinitions):
                self.memory.compile_acronym_definitions(node)
            
            elif isinstance(node, Pool):
                self.memory.compile_pool(node)
                
            else:
                print(f"DEBUG: Unhandled node type: {type(node).__name__}")
                
            
                
        except Exception as e:
            print(f"ERROR: Compilation failed for node {type(node).__name__}: {str(e)}")
            raise
    
    def compile_function_call(self, node):
        """Dispatch function call to appropriate module"""
        try:
            print(f"DEBUG: Compiling function call: {node.function}")
            
            if node.function in ['Add', 'Subtract', 'Multiply', 'Divide']:
                return self.arithmetic.compile_operation(node)
            
            elif node.function in ['LessThan', 'GreaterThan', 'EqualTo']:
                return self.arithmetic.compile_comparison(node)
            
            elif node.function in ['WriteTextFile', 'ReadTextFile', 'FileExists']:
                return self.fileio.compile_operation(node)
            
            elif self.lowlevel.compile_operation(node):
                return  # Low-level operation handled
            
            elif self.virtual_memory.compile_operation(node):
                return  # VM operation handled

            else:
                raise ValueError(f"Unsupported function: {node.function}")
                
        except Exception as e:
            print(f"ERROR: Function call compilation failed: {str(e)}")
            raise
    
    def compile_expression(self, expr):
        """Delegate expression compilation to expression compiler"""
        return self.expressions.compile_expression(expr)
    
    def resolve_acronym_identifier(self, identifier_name):
        """Resolve acronym.variable syntax to full namespace"""
        return self.memory.resolve_acronym_identifier(identifier_name)
    
    def compile(self, ast) -> bytes:
        """Compile AST to executable"""
        try:
            print("DEBUG: Starting AILANG compilation")
            print(f"DEBUG: AST root type: {type(ast).__name__}")
            
            # Reset state
            self.variables.clear()
            self.stack_size = 0
            self.label_counter = 0
            self.max_depth = 0
            
            # Compile the AST
            self.compile_node(ast)
            
            print(f"DEBUG: Compilation completed successfully")
            print(f"DEBUG: Generated {len(self.asm.code)} bytes of code")
            
            # Resolve all pending jumps
            self.asm.resolve_jumps()
            
            print(f"DEBUG: Generated {len(self.asm.data)} bytes of data")
            print(f"DEBUG: Maximum recursion depth: {self.max_depth}")
            
            # Generate ELF executable
            return self.elf.generate(self.asm.code, self.asm.data)
            
        except Exception as e:
            print(f"ERROR: AILANG compilation failed: {str(e)}")
            print(f"DEBUG: Partial code generated: {len(self.asm.code)} bytes")
            print(f"DEBUG: Variables allocated: {self.variables}")
            raise
def emit_dmasend(var_name, target_pool):
    return f"// DMA SEND: move {var_name} to {target_pool}\n"

def emit_dmareceive(var_name, source_pool):
    return f"// DMA RECEIVE: get {var_name} from {source_pool}\n"

def allocate_stack_slot(var_name, slot_index):
    return f"// STACK ALLOC: {var_name} @ slot {slot_index}\n"

def emit_variable_pool_comment(var_name, pool_name):
    return f"// VAR {var_name} is in pool '{pool_name}'\n"