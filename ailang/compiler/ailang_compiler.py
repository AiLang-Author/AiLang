#!/usr/bin/env python3
"""
AILANG to x86-64 Compiler - Fixed Main Orchestrator
Coordinates compilation process using modular components with proper integration
"""

import struct
import sys
import os
# --- FIX: Import the parser from the ailang_parser package ---
from ailang_parser.compiler import AILANGCompiler
# --- End FIX ---
from ailang_parser.ailang_ast import *

from ailang_compiler.x64_assembler import X64Assembler
from ailang_compiler.elf_generator import ELFGenerator
from ailang_compiler.modules.arithmetic_ops import ArithmeticOps
from ailang_compiler.modules.fileio_ops import FileIOOps
from ailang_compiler.modules.control_flow import ControlFlow
from ailang_compiler.modules.memory_manager import MemoryManager
from ailang_compiler.modules.string_ops import StringOps
from ailang_compiler.modules.expression_compiler import ExpressionCompiler
from ailang_compiler.modules.code_generator import CodeGenerator
from ailang_compiler.modules.lowlevel_ops import LowLevelOps
from ailang_compiler.modules.virtual_memory import VirtualMemoryOps
from ailang_compiler.modules.library_inliner import LibraryInliner
from ailang_compiler.modules.hash_ops import HashOps
from ailang_compiler.modules.network_ops import NetworkOps

class AILANGToX64Compiler:
    """Main compiler orchestrator for AILANG to x86-64 compilation"""

    def __init__(self, vm_mode="user"):
        # ... (keep the __init__ method as is) ...
        self.asm = X64Assembler()
        self.elf = ELFGenerator()
        self.variables = {}
        self.stack_size = 0
        self.label_counter = 0
        self.max_depth = 0
        self.acronym_table = {}
        self.file_handles = {}
        self.file_buffer_size = 65536
        self.open_files = {}
        self.vm_mode = vm_mode.lower()
        self.arithmetic = ArithmeticOps(self)
        self.fileio = FileIOOps(self)
        self.control_flow = ControlFlow(self)
        self.memory = MemoryManager(self)
        self.strings = StringOps(self)
        self.expressions = ExpressionCompiler(self)
        self.codegen = CodeGenerator(self)
        self.lowlevel = LowLevelOps(self)
        
        # VM mode handling - keep if/else together
        if self.vm_mode == "kernel":
            from ailang_compiler.modules.virtual_memory import VirtualMemoryOps
            self.virtual_memory = VirtualMemoryOps(self)
        else:
            from ailang_compiler.modules.usermode_vm_ops import VirtualMemoryOpsUserMode
            self.virtual_memory = VirtualMemoryOpsUserMode(self)
        
        # Initialize library inliner AFTER the if/else block
        self.library_inliner = LibraryInliner(self)
        self.hash_ops = HashOps(self)
        self.network_ops = NetworkOps(self)
        
        # --- NEW: Keep track of loaded libraries to prevent circular imports ---
        self.loaded_libraries = set()


    # --- NEW: Function to handle library compilation ---
    def compile_library(self, library_node):
        """Finds, parses, and compiles an AILANG library."""
        try:
            # The library name will be like "Core.DataStructures"
            library_path_parts = library_node.name.split('.')
            
            # Check if this library has already been processed to avoid infinite loops
            if library_node.name in self.loaded_libraries:
                return

            # Construct the file path: AILANG/Libraries/Core/Library.DataStructures.ailang
            file_name = f"Library.{library_path_parts[-1]}.ailang"
            
            # Start search in the 'Libraries' directory
            search_path_parts = ['Libraries'] + library_path_parts[:-1] + [file_name]
            library_file_path = os.path.join(*search_path_parts)


            if not os.path.exists(library_file_path):
                raise FileNotFoundError(f"Library file not found: {library_file_path}")

            with open(library_file_path, 'r') as f:
                library_source = f.read()

            # Recursively call the parser for the library's source code
            parser = AILANGCompiler() # This is the parser from ailang_parser
            library_ast = parser.compile(library_source)

            # Mark this library as loaded *before* compiling its content
            self.loaded_libraries.add(library_node.name)

            # Compile the declarations from the library into the current context
            for decl in library_ast.declarations:
                self.compile_node(decl)
            

        except FileNotFoundError as e:
            print(f"ERROR: Could not import library '{library_node.name}'. {e}")
            raise
        except Exception as e:
            print(f"ERROR: Failed during compilation of library '{library_node.name}': {e}")
            raise


    def get_label(self):
        # ... (keep this method as is) ...
        label = f"L{self.label_counter}"
        self.label_counter += 1
        return label

    def compile_node(self, node):
        """Dispatch node compilation to appropriate module"""
        try:
            
            if isinstance(node, Library):
                self.compile_library(node)
            elif isinstance(node, Program):
                self.memory.compile_program(node)
            elif isinstance(node, Loop):
                # Loop handler - compile body statements
                for stmt in node.body:
                    self.compile_node(stmt)
            elif isinstance(node, Fork):
                self.control_flow.compile_fork(node)
            elif isinstance(node, Branch):
                self.control_flow.compile_branch(node)
            elif isinstance(node, FunctionCall):
                # Check if this is a RunTask
                if hasattr(node, "function") and node.function == "RunTask":
                    # This is a standalone RunTask call (like String.Set)
                    if hasattr(node, "task_name") and hasattr(node, "arguments"):
                        if hasattr(self, "library_inliner"):
                            self.library_inliner.compile_runtask(node)
                        else:
                            print("WARNING: No library_inliner available")
                            self.asm.emit_mov_rax_imm64(0)
                else:
                    # Regular function call
                    self.compile_function_call(node)
            elif isinstance(node, PrintMessage):
                self.strings.compile_print_message(node)
            elif isinstance(node, Assignment):
                self.memory.compile_assignment(node)
            elif isinstance(node, RunTask):
                    # Handle standalone RunTask calls
                    if hasattr(self, "library_inliner"):
                        self.library_inliner.compile_runtask(node)
                    else:
                        print("ERROR: No library_inliner available")
                        self.asm.emit_mov_rax_imm64(0)
            elif isinstance(node, While):
                    self.control_flow.compile_while_loop(node)
            elif isinstance(node, If):
                    self.control_flow.compile_if_condition(node)
            elif isinstance(node, AcronymDefinitions):
                    self.memory.compile_acronym_definitions(node)
            elif isinstance(node, Pool):
                    self.memory.compile_pool(node)
            elif isinstance(node, RecordTypeDefinition):
                        # Record types are compile-time only - register the type
                if not hasattr(self, "record_types"):
                    self.record_types = {}
                    self.record_types[node.name] = node.record_type
                else:
                    self.record_types[node.name] = node.record_type
            else:
                raise ValueError(f"Unsupported node type: {type(node).__name__}")
        except Exception as e:
            print(f"ERROR: Compilation failed for node {type(node).__name__}: {str(e)}")
            raise

    def compile_function_call(self, node):
        try:
            # Check if this is actually a RunTask
            if hasattr(node, 'task_name'):
                if hasattr(self, 'library_inliner'):
                    self.library_inliner.compile_runtask(node)
                    return
                else:
                    print("WARNING: No library_inliner for RunTask")
                    self.asm.emit_mov_rax_imm64(0)
                    return
            
            # Original function call handling
            if node.function in ['Add', 'Subtract', 'Multiply', 'Divide']:
                return self.arithmetic.compile_operation(node)
            elif node.function in ['LessThan', 'GreaterThan', 'EqualTo']:
                return self.arithmetic.compile_comparison(node)
            elif node.function in ['WriteTextFile', 'ReadTextFile', 'FileExists']:
                return self.fileio.compile_operation(node)
            elif self.lowlevel.compile_operation(node):
                return
            elif self.hash_ops.compile_operation(node):
                return
            elif self.network_ops.compile_operation(node):
                return
            elif self.virtual_memory.compile_operation(node):
                return
            elif node.function in ['PoolResize', 'PoolMove', 'PoolCompact', 
                                'PoolAllocate', 'PoolFree', 'PoolGetSize']:
                if node.function == 'PoolResize':
                    return self.memory.compile_pool_resize(node)
                elif node.function == 'PoolMove':
                    return self.memory.compile_pool_move(node)
                elif node.function == 'PoolCompact':
                    return self.memory.compile_pool_compact(node)
                elif node.function == 'PoolAllocate':
                    return self.memory.compile_pool_allocate(node)
                elif node.function == 'PoolFree':
                    return self.memory.compile_pool_free(node)
                elif node.function == 'PoolGetSize':
                    return self.memory.compile_pool_get_size(node)
            elif node.function == 'StringConcat':
                return self.strings.compile_string_concat(node)
            elif node.function == 'NumberToString':
                return self.strings.compile_number_to_string(node)
            else:
                raise ValueError(f"Unsupported function: {node.function}")
        except Exception as e:
            print(f"ERROR: Function call compilation failed: {str(e)}")
            raise
    
    def compile_expression(self, expr):
        return self.expressions.compile_expression(expr)
    
    def resolve_acronym_identifier(self, identifier_name):
        return self.memory.resolve_acronym_identifier(identifier_name)
    
    def compile(self, ast) -> bytes:
        try:
            self.loaded_libraries.clear()
            self.variables.clear()
            self.stack_size = 0
            self.label_counter = 0
            self.max_depth = 0
            self.compile_node(ast)
            self.asm.resolve_jumps()
            return self.elf.generate(self.asm.code, self.asm.data)
        except Exception as e:
            print(f"ERROR: AILANG compilation failed: {str(e)}")
            raise

    def compile_record_type_definition(self, node):
        """Compile a Record type definition"""
        from ailang_ast import RecordTypeDefinition
        
        # Store the type definition in a symbol table for later use
        if not hasattr(self, 'record_types'):
            self.record_types = {}
        
        self.record_types[node.name] = node.record_type
        
        # Emit debug info
        
        # Record types are compile-time only, no runtime code needed
        # They're used for type checking and memory layout
