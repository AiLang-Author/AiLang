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
        """Initialize compiler with proper module initialization order"""
        self.elf = ELFGenerator()
        self.asm = X64Assembler(self.elf)
        self.variables = {}
        self.stack_size = 0
        self.label_counter = 0
        self.max_depth = 0
        self.acronym_table = {}
        self.file_handles = {}
        self.file_buffer_size = 65536
        self.open_files = {}
        self.vm_mode = vm_mode.lower()
        
        # Initialize core modules first
        self.arithmetic = ArithmeticOps(self)
        self.fileio = FileIOOps(self)
        self.control_flow = ControlFlow(self)
        self.memory = MemoryManager(self)
        self.strings = StringOps(self)
        self.expressions = ExpressionCompiler(self)
        self.codegen = CodeGenerator(self)
        self.lowlevel = LowLevelOps(self)
        
        # Loop model support
        self.subroutines = {}      # name -> label mapping
        self.loops = {}            # loop definitions
        self.actor_states = {}     # actor state storage
        self.task_fixups = []      # forward references to fix
        self.loop_starts = []      # LoopStart blocks to run first
        
        # VM mode handling
        if self.vm_mode == "kernel":
            from ailang_compiler.modules.virtual_memory import VirtualMemoryOps
            self.virtual_memory = VirtualMemoryOps(self)
        else:
            from ailang_compiler.modules.usermode_vm_ops import VirtualMemoryOpsUserMode
            self.virtual_memory = VirtualMemoryOpsUserMode(self)
        
        # IMPORTANT: Initialize HashOps BEFORE LibraryInliner
        # LibraryInliner depends on HashOps existing
        self.hash_ops = HashOps(self)
        self.network_ops = NetworkOps(self)
        
        # Initialize library inliner AFTER hash_ops
        self.library_inliner = LibraryInliner(self)
        
        # Keep track of loaded libraries to prevent circular imports
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
        
        # Initialize dispatch table if not exists
        if not hasattr(self, '_node_dispatch'):
            self._node_dispatch = {
                # Core structures
                'Program': lambda n: self.memory.compile_program(n),
                'Library': lambda n: self.compile_library(n),
                
                # Declarations
                'SubRoutine': lambda n: self.compile_subroutine(n),
                'AcronymDefinitions': lambda n: self.memory.compile_acronym_definitions(n),
                'Pool': lambda n: self.memory.compile_pool(n),
                'SubPool': lambda n: self.memory.compile_subpool(n),
                'RecordTypeDefinition': lambda n: self._compile_record_type(n),
                
                # Loop model structures
                'Loop': lambda n: self._compile_loop_body(n),
                'LoopMain': lambda n: self.compile_loop_main(n),
                'LoopActor': lambda n: self.compile_loop_actor(n),
                'LoopStart': lambda n: self.compile_loop_start(n),
                'LoopShadow': lambda n: self.compile_loop_shadow(n),
                'LoopContinue': lambda n: self.compile_loop_continue(n),
                
                # Loop communication
                'LoopSend': lambda n: self.compile_loop_send(n),
                'LoopReceive': lambda n: self.compile_loop_receive(n),
                'LoopReply': lambda n: self.compile_loop_reply(n),
                'LoopYield': lambda n: self.compile_loop_yield(n),
                
                # Control flow
                'Fork': lambda n: self.control_flow.compile_fork(n),
                'Branch': lambda n: self.control_flow.compile_branch(n),
                'While': lambda n: self.control_flow.compile_while_loop(n),
                'If': lambda n: self.control_flow.compile_if_condition(n),
                
                # Statements
                'Assignment': lambda n: self.memory.compile_assignment(n),
                'PrintMessage': lambda n: self.strings.compile_print_message(n),
                'RunTask': lambda n: self._compile_run_task_dispatch(n),
                'FunctionCall': lambda n: self._compile_function_call_dispatch(n),
            }
        
        try:
            node_type = type(node).__name__
            
            if node_type in self._node_dispatch:
                return self._node_dispatch[node_type](node)
            else:
                raise ValueError(f"Unsupported node type: {node_type}")
                
        except Exception as e:
            print(f"ERROR: Compilation failed for node {type(node).__name__}: {str(e)}")
            raise

    def _compile_loop_body(self, node):
        """Compile Loop body statements"""
        for stmt in node.body:
            self.compile_node(stmt)

    def _compile_record_type(self, node):
        """Register record type definition"""
        if not hasattr(self, "record_types"):
            self.record_types = {}
        self.record_types[node.name] = node.record_type

    def _compile_run_task_dispatch(self, node):
        """Handle RunTask with proper routing"""
        task_name = node.task_name if hasattr(node, 'task_name') else None
        
        if task_name and '.' not in task_name:
            # Simple name - local subroutine
            self.compile_run_task(node)
        elif hasattr(self, "library_inliner"):
            # Dotted name - library call
            self.library_inliner.compile_runtask(node)
        else:
            print(f"ERROR: Cannot handle task: {task_name}")
            self.asm.emit_mov_rax_imm64(0)

    def _compile_function_call_dispatch(self, node):
        """Handle FunctionCall with special cases"""
        if hasattr(node, "function") and node.function == "RunTask":
            # Legacy RunTask as function call
            if hasattr(node, "task_name") and hasattr(node, "arguments"):
                if hasattr(self, "library_inliner"):
                    self.library_inliner.compile_runtask(node)
                else:
                    print("WARNING: No library_inliner available")
                    self.asm.emit_mov_rax_imm64(0)
        else:
            # Regular function call
            self.compile_function_call(node)

    def compile_subroutine(self, node):
        """Compile SubRoutine as callable code"""
        print(f"DEBUG: Compiling SubRoutine.{node.name}")
        
        # Create label for the subroutine
        label = self.asm.create_label()
        self.subroutines[node.name] = label
        
        # Jump over subroutine definition
        skip_label = self.asm.create_label()
        self.asm.emit_jump_to_label(skip_label, "JMP")
        
        # Mark subroutine start
        self.asm.mark_label(label)
        
        # Save registers we might use
        self.asm.emit_push_rbx()
        
        # Compile body
        for stmt in node.body:
            self.compile_node(stmt)
        
        # Restore registers and return
        self.asm.emit_pop_rbx()
        self.asm.emit_ret()
        
        # Mark skip label (continue main execution)
        self.asm.mark_label(skip_label)
        return True

    def compile_run_task(self, node):
        """Compile RunTask - call to subroutine"""
        task_name = node.task_name
        print(f"DEBUG: Compiling RunTask.{task_name}")
        
        if task_name in self.subroutines:
            # Known subroutine - emit call with known offset
            label = self.subroutines[task_name]
            current_pos = len(self.asm.code)
            self.asm.emit_bytes(0xE8)  # CALL opcode
            
            if label in self.asm.labels:
                # Label position is known
                target_pos = self.asm.labels[label]
                offset = target_pos - (current_pos + 5)
                self.asm.emit_bytes(*struct.pack('<i', offset))
            else:
                # Label exists but position unknown yet
                self.asm.emit_bytes(0x00, 0x00, 0x00, 0x00)
                self.task_fixups.append((task_name, current_pos))
        else:
            # Forward reference - will be resolved later
            current_pos = len(self.asm.code)
            self.asm.emit_bytes(0xE8, 0x00, 0x00, 0x00, 0x00)
            self.task_fixups.append((task_name, current_pos))
        
        return True
            
    def compile_loop_main(self, node):
        """Compile LoopMain - main event loop"""
        print(f"DEBUG: Compiling LoopMain.{node.name}")
        
        # LoopMain runs inline in main execution
        for stmt in node.body:
            self.compile_node(stmt)
            
    def compile_loop_actor(self, node):
        """Compile LoopActor - isolated actor"""
        print(f"DEBUG: Compiling LoopActor.{node.name}")
        
        # Store actor definition for spawn
        self.loops[f"LoopActor.{node.name}"] = node
        
        # For now, compile inline (later: separate context)
        # Skip definition in main flow
        skip_label = self.asm.create_label()
        self.asm.emit_jump_to_label(skip_label, "JMP")
        
        # Mark actor code
        actor_label = self.asm.create_label()
        self.loops[f"LoopActor.{node.name}.label"] = actor_label
        self.asm.mark_label(actor_label)
        
        # Actor execution context
        for stmt in node.body:
            self.compile_node(stmt)
            
        self.asm.emit_ret()
        self.asm.mark_label(skip_label)
        
    def compile_loop_start(self, node):
        """Compile LoopStart - initialization"""
        print(f"DEBUG: Compiling LoopStart.{node.name}")
        
        # LoopStart runs before main
        # Store for later execution
        if not hasattr(self, 'loop_starts'):
            self.loop_starts = []
        self.loop_starts.append(node)
        
    def compile_loop_shadow(self, node):
        """Compile LoopShadow - background loop"""
        print(f"DEBUG: Compiling LoopShadow.{node.name}")
        
        # Similar to LoopActor but for background tasks
        self.loops[f"LoopShadow.{node.name}"] = node
        
        # Skip in main flow
        skip_label = self.asm.create_label()
        self.asm.emit_jump_to_label(skip_label, "JMP")
        
        shadow_label = self.asm.create_label()
        self.loops[f"LoopShadow.{node.name}.label"] = shadow_label
        self.asm.mark_label(shadow_label)
        
        for stmt in node.body:
            self.compile_node(stmt)
            
        self.asm.emit_ret()
        self.asm.mark_label(skip_label)
        
    def compile_loop_continue(self, node):
        """Compile LoopContinue - infinite loop with yield points"""
        print("DEBUG: Compiling LoopContinue")
        
        loop_start = self.asm.create_label()
        self.asm.mark_label(loop_start)
        
        for stmt in node.body:
            self.compile_node(stmt)
            
        # Jump back to start
        self.asm.emit_jump_to_label(loop_start, "JMP")
        
    def compile_loop_yield(self, node):
        """Compile LoopYield - cooperative yield"""
        print("DEBUG: Compiling LoopYield")
        
        if node.expression:
            # Yield with delay
            self.compile_expression(node.expression)
            # For now, just a NOP - later: actual scheduling
            self.asm.emit_nop()
        else:
            # Simple yield
            self.asm.emit_nop()
            
    def compile_loop_send(self, node):
        """Compile LoopSend - message passing"""
        print("DEBUG: Compiling LoopSend")
        
        # For MVP: store message in global queue
        # Later: proper actor mailboxes
        self.compile_expression(node.target)
        self.asm.emit_push_rax()
        self.compile_expression(node.message)
        self.asm.emit_pop_rbx()
        
        # Store in message queue (simplified)
        # Real implementation needs proper queue structure
        
    def compile_loop_receive(self, node):
        """Compile LoopReceive - message reception"""
        print("DEBUG: Compiling LoopReceive")
        
        # For MVP: simple pattern matching
        # Later: proper message queue handling
        for case in node.cases:
            # Compare message with pattern
            # Jump to action if match
            pass
            
    def compile_loop_reply(self, node):
        """Compile LoopReply - reply to sender"""
        print("DEBUG: Compiling LoopReply")
        
        # Store reply for sender
        self.compile_expression(node.message)
        # Real implementation needs sender tracking
        
    def fixup_forward_references(self):
        """Fix forward references to subroutines"""
        import struct
        
        print(f"DEBUG: Fixing {len(self.task_fixups)} forward references")
        print(f"DEBUG: Available subroutines: {self.subroutines}")
        print(f"DEBUG: Available labels in asm: {self.asm.labels}")
        
        for item in self.task_fixups:
            if len(item) == 2:
                task_name, call_pos = item
            else:
                task_name, call_pos, _ = item
                
            print(f"DEBUG: Fixing call to {task_name} at position {call_pos}")
            
            if task_name not in self.subroutines:
                raise ValueError(f"Undefined subroutine: {task_name}")
                
            label = self.subroutines[task_name]
            print(f"DEBUG: Subroutine {task_name} has label {label}")
            
            # Get the actual position of the subroutine
            if label in self.asm.labels:
                target_pos = self.asm.labels[label]
                
                # Calculate relative offset for CALL
                # CALL uses offset from instruction end (call_pos + 5)
                offset = target_pos - (call_pos + 5)
                
                # Patch the CALL instruction with the offset
                offset_bytes = struct.pack('<i', offset)  # Little-endian 32-bit
                for i in range(4):
                    self.asm.code[call_pos + 1 + i] = offset_bytes[i]
                
                print(f"DEBUG: Fixed call to {task_name}: offset={offset} ({hex(offset)})")
            else:
                print(f"DEBUG: ERROR - Label {label} not found in asm.labels!")
        


    def compile_function_call(self, node):
        """Compile function call with enhanced module support"""
        try:
            print(f"DEBUG: Compiling function call: {node.function}")

            # Handle RunTask
            if hasattr(node, 'task_name'):
                if hasattr(self, 'library_inliner'):
                    self.library_inliner.compile_runtask(node)
                else:
                    print("WARNING: No library_inliner for RunTask")
                    self.asm.emit_mov_rax_imm64(0)
                return

            # Arithmetic operations
            if node.function in ['Add', 'Subtract', 'Multiply', 'Divide']:
                return self.arithmetic.compile_operation(node)

            # Comparison operations
            if node.function in ['LessThan', 'GreaterThan', 'EqualTo', 'NotEqual']:
                if node.function == 'NotEqual':
                    return self.arithmetic.compile_not_equal(node)
                else:
                    return self.arithmetic.compile_comparison(node)

            # File I/O operations
            if node.function in ['WriteTextFile', 'ReadTextFile', 'FileExists']:
                return self.fileio.compile_operation(node)

            # String operations
            if node.function in ['StringConcat', 'StringCompare', 'StringLength',
                            'StringCopy', 'StringToNumber', 'NumberToString']:
                return self.strings.compile_operation(node)


            # Print operations
            if node.function == 'PrintNumber':
                return self.strings.compile_print_number(node)
            # Memory pool operations
            if node.function in ['PoolResize', 'PoolMove', 'PoolCompact',
                            'PoolAllocate', 'PoolFree', 'PoolGetSize']:
                memory_ops = {
                    'PoolResize': self.memory.compile_pool_resize,
                    'PoolMove': self.memory.compile_pool_move,
                    'PoolCompact': self.memory.compile_pool_compact,
                    'PoolAllocate': self.memory.compile_pool_allocate,
                    'PoolFree': self.memory.compile_pool_free,
                    'PoolGetSize': self.memory.compile_pool_get_size
                }
                return memory_ops[node.function](node)

            # Specialized operations
            if self.lowlevel.compile_operation(node):
                return
            if self.hash_ops.compile_operation(node):
                return
            if self.network_ops.compile_operation(node):
                return
            if self.virtual_memory.compile_operation(node):
                return

            raise ValueError(f"Unsupported function: {node.function}")

        except Exception as e:
            print(f"ERROR: Function call compilation failed: {str(e)}")
            raise
    
    def compile_expression(self, expr):
        return self.expressions.compile_expression(expr)
    
    def resolve_acronym_identifier(self, identifier_name):
        return self.memory.resolve_acronym_identifier(identifier_name)
    
    def compile(self, ast) -> bytes:
        """Compile AST to executable with full dynamic addressing"""
        # --- START OF SURGICAL REPLACEMENT ---
        print("\n=== COMPILATION STARTING ===")
        print("Phase 1: Generating machine code with placeholders...")
        self.compile_node(ast)
        self.fixup_forward_references()  # Fix SubRoutine calls
        print("Phase 2: Resolving internal jump offsets...")
        self.asm.resolve_jumps()

        print("Phase 3: Building executable and applying relocations...")
        code_bytes = bytes(self.asm.code)
        data_bytes = bytes(self.asm.data)
        
        # The ELF generator now orchestrates the final address calculation and patching.
        executable = self.elf.generate(code_bytes, data_bytes, self.asm)
        
        print(f"\n=== COMPILATION COMPLETE ({len(executable)} bytes) ===")
        return executable
        # --- END OF SURGICAL REPLACEMENT ---