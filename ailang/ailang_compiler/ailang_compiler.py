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

from ailang_compiler.assembler import X64Assembler
from ailang_compiler.elf_generator import ELFGenerator
from ailang_compiler.modules.arithmetic_ops import ArithmeticOps
from ailang_compiler.modules.math_ops import MathOperations
from ailang_compiler.modules.fileio_ops import FileIOOps
from ailang_compiler.modules.control_flow import ControlFlow
from ailang_compiler.modules.memory_manager import MemoryManager
from ailang_compiler.modules.debug_ops import DebugOps
from ailang_compiler.modules.code_generator import CodeGenerator
from ailang_compiler.modules.lowlevel_ops import LowLevelOps
from ailang_compiler.modules.virtual_memory import VirtualMemoryOps
from ailang_compiler.modules.library_inliner import LibraryInliner
from ailang_compiler.modules.hash_ops import HashOps
from ailang_compiler.modules.network_ops import NetworkOps
from ailang_compiler.modules.scheduling_primitives import SchedulingPrimitives
from ailang_compiler.modules.atomic_ops import AtomicOps
from ailang_compiler.modules.user_functions import UserFunctions
from ailang_compiler.modules.string_ops import StringOps
from ailang_compiler.modules.expression_compiler import ExpressionCompiler
from ailang_compiler.modules.library_inliner import LibraryInliner
from ailang_compiler.modules.user_functions import UserFunctions
from ailang_compiler.modules.memory_pool import MemoryPool



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
        self.math_ops = MathOperations(self)
        self.control_flow = ControlFlow(self)
        self.memory = MemoryManager(self)
        self.memory_pool = MemoryPool(self)
        self.strings = StringOps(self)
        self.debug_ops = DebugOps(self)
        self.expressions = ExpressionCompiler(self)
        self.codegen = CodeGenerator(self)
        self.lowlevel = LowLevelOps(self)
        self.user_functions = UserFunctions(self)
        
        # Loop model support
        self.subroutines = {}      # name -> label mapping
        self.loops = {}            # loop definitions
        self.actor_states = {}     # actor state storage
        self.task_fixups = []      # forward references to fix
        self.loop_starts = []      # LoopStart blocks to run first
        self.actor_blocks = {}  # Maps actor names to their entry labels
        
        # VM mode handling
        if self.vm_mode == "kernel":
            from ailang_compiler.modules.virtual_memory import VirtualMemoryOps
            self.virtual_memory = VirtualMemoryOps(self)
        else:
            from ailang_compiler.modules.usermode_vm_ops import VirtualMemoryOpsUserMode
            self.virtual_memory = VirtualMemoryOpsUserMode(self)
        
        # IMPORTANT: Initialize HashOps BEFORE LibraryInliner
        self.hash_ops = HashOps(self)
        self.scheduler = SchedulingPrimitives(self)
        self.atomics = AtomicOps(self) # <-- ADD THIS LINE
        
        self.network_ops = NetworkOps(self)
        
        # Initialize library inliner AFTER hash_ops
        self.library_inliner = LibraryInliner(self)
        
        # Keep track of loaded libraries to prevent circular imports
        self.loaded_libraries = set()
        
        self._node_dispatch = {
        'Program': lambda n: self.memory.compile_program(n),
        'Library': lambda n: self.compile_library(n),
        'SubRoutine': lambda n: self.compile_subroutine(n),
        'AcronymDefinitions': lambda n: self.memory.compile_acronym_definitions(n),
        'Pool': lambda n: self.memory.compile_pool(n),
        'SubPool': lambda n: self.memory.compile_subpool(n),
        'RecordTypeDefinition': lambda n: self._compile_record_type(n),
        'Loop': lambda n: self._compile_loop_body(n),
        'LoopMain': lambda n: self.compile_loop_main(n),
        'LoopActor': lambda n: (print(f"DISPATCH: LoopActor -> scheduler"), self.scheduler.compile_loop_actor(n))[1],
        'LoopStart': lambda n: self.compile_loop_start(n),
        'LoopShadow': lambda n: self.compile_loop_shadow(n),
        'LoopContinue': lambda n: self.compile_loop_continue(n),
        'LoopSpawn': lambda n: self.scheduler.compile_loop_spawn(n),
        'LoopSend': lambda n: self.compile_loop_send(n),
        'LoopReceive': lambda n: self.compile_loop_receive(n),
        'LoopReply': lambda n: self.compile_loop_reply(n),
        'LoopYield': lambda n: self.scheduler.compile_loop_yield(n),
        'Fork': lambda n: self.control_flow.compile_fork(n),
        'Branch': lambda n: self.control_flow.compile_branch(n),
        'While': lambda n: self.control_flow.compile_while_loop(n),
        'BreakLoop': lambda n: self.control_flow.compile_break(n),
        'ContinueLoop': lambda n: self.control_flow.compile_continue(n),
        'If': lambda n: self.control_flow.compile_if_condition(n),
                    'DebugBlock': lambda n: self.debug_ops.compile_operation(n) if hasattr(self, 'debug_ops') else None,
            'DebugAssert': lambda n: self.debug_ops.compile_operation(n) if hasattr(self, 'debug_ops') else None,
            'Assignment': lambda n: self.memory.compile_assignment(n),
        'PrintMessage': lambda n: self.strings.compile_print_message(n),
        'RunTask': lambda n: self._compile_run_task_dispatch(n),
        'FunctionCall': lambda n: self._compile_function_call_dispatch(n),
        
        # === User Functions Module ====
        
        'Function': lambda n: self.user_functions.compile_function_definition(n),
        'FunctionDefinition': lambda n: self.user_functions.compile_function_definition(n),
        'ReturnValue': lambda n: self.user_functions.compile_return(n.value if hasattr(n, 'value') else None),
        
        
        }


    # --- NEW: Function to handle library compilation ---
    def compile_library(self, library_node):
        """Finds, parses, and compiles an AILANG library with 2-pass support."""
        try:
            library_path_parts = library_node.name.split('.')
            
            if library_node.name in self.loaded_libraries:
                return

            # Try current directory first, then Libraries subdirectory
            file_name = f"Library.{library_path_parts[-1]}.ailang"
            library_file_path = file_name
            
            if not os.path.exists(library_file_path):
                search_path_parts = ['Libraries'] + library_path_parts[:-1] + [file_name]
                library_file_path = os.path.join(*search_path_parts)

            if not os.path.exists(library_file_path):
                raise FileNotFoundError(f"Library file not found: {library_file_path}")

            print(f"  Loading library: {library_file_path}")

            with open(library_file_path, 'r') as f:
                library_source = f.read()

            parser = AILANGCompiler()
            library_ast = parser.compile(library_source)

            self.loaded_libraries.add(library_node.name)

            # Extract library prefix (e.g., "RESP" from "Library.RESP")
            lib_prefix = library_path_parts[-1]
            
            # PASS 1: Register all library functions BEFORE compiling any
            print(f"  Pass 1: Registering {lib_prefix} library functions...")
            for decl in library_ast.declarations:
                if type(decl).__name__ in ('Function', 'FunctionDefinition'):
                    # Fix function name to include library prefix
                    if '.' in decl.name:
                        # Already has dots - parse it properly
                        parts = decl.name.split('.')
                        if parts[0] == lib_prefix:
                            # Already prefixed correctly
                            full_name = decl.name
                        else:
                            # Add library prefix
                            full_name = f"{lib_prefix}.{decl.name}"
                    else:
                        # Simple name - add library prefix
                        full_name = f"{lib_prefix}.{decl.name}"
                    
                    # Update the node's name
                    original_name = decl.name
                    decl.name = full_name
                    
                    # Register with user_functions module
                    self.user_functions.register_function(decl)
                    
                    print(f"    Registered: {full_name}")

            # PASS 2: Now compile all declarations
            print(f"  Pass 2: Compiling {lib_prefix} library functions...")
            for decl in library_ast.declarations:
                self.compile_node(decl)

        except FileNotFoundError as e:
            print(f"ERROR: Could not import library '{library_node.name}'. {e}")
            raise
        except Exception as e:
            print(f"ERROR: Failed during compilation of library '{library_node.name}': {e}")
            import traceback
            traceback.print_exc()
            raise

    def get_label(self):
        # ... (keep this method as is) ...
        label = f"L{self.label_counter}"
        self.label_counter += 1
        return label

    def compile_node(self, node):
        """Dispatch node compilation to appropriate module"""
        
        node_type = type(node).__name__
            
            # Handle dotted actor names like "LoopActor.TestActor"
        if node_type == 'FunctionCall' and hasattr(node, 'function'):  # ADD THIS BLOCK
            if node.function.startswith('LoopActor.'):
                print(f"DEBUG: Found LoopActor.{node.function[10:]}")
                # Create a proper actor node
                class ActorNode:
                    def __init__(self, name, body):
                        self.name = name
                        self.body = body
                actor = ActorNode(node.function[10:], node.arguments if hasattr(node, 'arguments') else [])
                return self.scheduler.compile_loop_actor(actor)
            
        if node_type in self._node_dispatch:
            return self._node_dispatch[node_type](node)

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
        """Handle FunctionCall with special cases including ReturnValue"""
        if hasattr(node, "function"):
            if node.function == "RunTask":
                # Legacy RunTask as function call
                if hasattr(node, "task_name") and hasattr(node, "arguments"):
                    if hasattr(self, "library_inliner"):
                        self.library_inliner.compile_runtask(node)
                    else:
                        print("WARNING: No library_inliner available")
                        self.asm.emit_mov_rax_imm64(0)
            elif node.function == "ReturnValue":
                # Handle ReturnValue as a function call
                value = node.arguments[0] if node.arguments else None
                self.user_functions.compile_return(value)
            elif node.function.startswith("DebugPerf_"):
                # Handle DebugPerf functions
                if hasattr(self, 'debug_ops'):
                    self.debug_ops.compile_operation(node)
                else:
                    print(f"WARNING: Debug ops not available for {node.function}")
                    self.asm.emit_mov_rax_imm64(0)
            else:
                # Regular function call
                self.compile_function_call(node)
        else:
            # Regular function call
            self.compile_function_call(node)
 
    def compile_comparison(self, node):
        """Delegate comparison operations to arithmetic module"""
        return self.arithmetic.compile_comparison(node)
            

    def compile_function_call(self, node):
        """Compile function call with user-defined functions and enhanced module support."""
        try:
            print(f"DEBUG: Compiling function call: {node.function}")
            
            # === NEW: Check if this is a registered library function first ===
            # This handles forward references from the 2-pass registration
            if hasattr(self.user_functions, 'is_function_registered'):
                if self.user_functions.is_function_registered(node.function):
                    print(f"DEBUG: Found registered function: {node.function}")
                    if self.user_functions.compile_function_call(node):
                        return
            
            # Extract base function name (handles both "Category.Name" and "Name")
            base_name = node.function
            if '.' in node.function:
                parts = node.function.split('.')
                
                # === NEW: Check for library function patterns (e.g., RESP.ParseInteger) ===
                if len(parts) == 2:
                    lib_name, func_name = parts
                    
                    # Try as registered library function
                    if hasattr(self.user_functions, 'is_function_registered'):
                        # Try full name first
                        if self.user_functions.is_function_registered(node.function):
                            print(f"DEBUG: Found library function: {node.function}")
                            if self.user_functions.compile_function_call(node):
                                return
                        
                        # Try with "Function." prefix removed if present
                        if lib_name == "Function" and self.user_functions.is_function_registered(func_name):
                            import copy
                            node_copy = copy.copy(node)
                            node_copy.function = func_name
                            if self.user_functions.compile_function_call(node_copy):
                                return
                
                # Check full name first for user functions (existing code)
                if node.function in self.user_functions.user_functions:
                    if self.user_functions.compile_function_call(node):
                        return
                
                # Try without "Function." prefix if present (existing code)
                if node.function.startswith("Function."):
                    clean_name = node.function[9:]
                    if clean_name in self.user_functions.user_functions:
                        import copy
                        node_copy = copy.copy(node)
                        node_copy.function = clean_name
                        if self.user_functions.compile_function_call(node_copy):
                            return
                
                # Check for pool operations (existing code)
                if len(parts) == 2 and parts[1] in ['Init', 'Alloc', 'Free', 'Reset', 'Status']:
                    if hasattr(self, 'memory_pool') and self.memory_pool.compile_operation(node):
                        return
                
                # Try base name for primitives
                base_name = node.function.split('.')[-1]
            
            # Check user functions with base name (existing code)
            if base_name in self.user_functions.user_functions:
                import copy
                node_copy = copy.copy(node)
                node_copy.function = base_name
                if self.user_functions.compile_function_call(node_copy):
                    return

            # Check for pooled string operations (existing code)
            if node.function == 'StringConcatPooled':
                if hasattr(self, 'strings') and self.strings.compile_operation(node):
                    return

            # Dispatch to modules (existing code)
            dispatch_modules = [
                self.math_ops,      # Try math operations first
                self.arithmetic,    # Then basic arithmetic
                self.fileio, self.strings,
                self.lowlevel, self.hash_ops, self.network_ops,
                self.virtual_memory, self.atomics
            ]

            print(f"DEBUG: Dispatching {node.function} to modules...")

            # Try with original name first
            for module in dispatch_modules:
                module_name = module.__class__.__name__ if hasattr(module, '__class__') else str(module)
                print(f"DEBUG: Trying module {module_name}")
                if hasattr(module, 'compile_operation'):
                    result = module.compile_operation(node)
                    print(f"DEBUG: {module_name}.compile_operation returned {result}")
                    if result:
                        return
                else:
                    print(f"DEBUG: {module_name} has no compile_operation method")

            # If dotted name failed, try with base name
            if '.' in node.function:
                import copy
                node_copy = copy.copy(node)
                node_copy.function = base_name
                print(f"DEBUG: Trying base name {base_name}")
                for module in dispatch_modules:
                    module_name = module.__class__.__name__ if hasattr(module, '__class__') else str(module)
                    print(f"DEBUG: Trying module {module_name} with base name")
                    if hasattr(module, 'compile_operation'):
                        result = module.compile_operation(node_copy)
                        print(f"DEBUG: {module_name}.compile_operation returned {result}")
                        if result:
                            return

            # Special cases (existing code)
            if base_name == 'PrintNumber':
                return self.strings.compile_print_number(node)
                
            # Memory pool operations (existing code)
            if base_name in ['PoolResize', 'PoolMove', 'PoolCompact',
                            'PoolAllocate', 'PoolFree', 'PoolGetSize',
                            'ArrayCreate', 'ArraySet', 'ArrayGet', 'ArrayLength','ArrayDestroy']:
                memory_ops = {
                    'PoolResize': self.memory.compile_pool_resize,
                    'PoolMove': self.memory.compile_pool_move,
                    'PoolCompact': self.memory.compile_pool_compact,
                    'PoolAllocate': self.memory.compile_pool_allocate,
                    'PoolFree': self.memory.compile_pool_free,
                    'PoolGetSize': self.memory.compile_pool_get_size,
                    'ArrayCreate': self.memory.compile_array_create,
                    'ArraySet': self.memory.compile_array_set,
                    'ArrayGet': self.memory.compile_array_get,
                    'ArrayLength': self.memory.compile_array_length,
                    'ArrayDestroy': self.memory.compile_array_destroy, 
                }
                if base_name in memory_ops:
                    return memory_ops[base_name](node)

            # Scheduling primitives (existing code)
            if self._is_scheduler_primitive(base_name):
                return self._compile_scheduler_primitive(node)

            # Exit (existing code)
            if base_name == 'Exit':
                self.asm.emit_mov_rax_imm64(60)
                self.asm.emit_xor_edi_edi()
                self.asm.emit_syscall()
                return

            # Unknown function
            raise ValueError(f"Unknown function: {node.function}")
            
        except Exception as e:
            print(f"ERROR: compile_function_call failed: {e}")
            import traceback
            traceback.print_exc()
            raise
    

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
        
        # Create return label for this subroutine
        return_label = self.asm.create_label()
        
        # Set context for ReturnValue
        self.compiling_subroutine = True
        self.subroutine_return_label = return_label
        
        # Compile body
        for stmt in node.body:
            self.compile_node(stmt)
        
        # Mark return point
        self.asm.mark_label(return_label)
        
        # Clear context
        self.compiling_subroutine = False
        self.subroutine_return_label = None
        
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
        """Write message to target actor's mailbox"""
        print("DEBUG: Compiling LoopSend")
        
        # Get target actor handle
        if len(node.arguments) >= 2:
            # First arg: target handle
            self.compile_expression(node.arguments[0])
            self.asm.emit_push_rax()  # Save target
            
            # Second arg: message value
            self.compile_expression(node.arguments[1])
            
            # Calculate mailbox address: ACB_base + (handle * 128) + 120
            # Using offset 120 in the ACB for mailbox (last 8 bytes)
            self.asm.emit_mov_rbx_rax()  # Message in RBX
            self.asm.emit_pop_rax()      # Target handle in RAX
            
            # Multiply handle by 128 (ACB size)
            self.asm.emit_bytes(0x48, 0xC1, 0xE0, 0x07)  # SHL RAX, 7
            
            # Add ACB base address
            if 'system_acb_table' in self.variables:
                offset = self.variables['system_acb_table']
                self.asm.emit_bytes(0x48, 0x03, 0x85)  # ADD RAX, [RBP-offset]
                self.asm.emit_bytes(*struct.pack('<i', -offset))
            
            # Add mailbox offset (120)
            self.asm.emit_bytes(0x48, 0x83, 0xC0, 0x78)  # ADD RAX, 120
            
            # Store message at mailbox address
            self.asm.emit_bytes(0x48, 0x89, 0x18)  # MOV [RAX], RBX
            
            print("DEBUG: Message sent to mailbox")
        
        return True
    def compile_loop_receive(self, node):
        """Read message from current actor's mailbox"""
        print("DEBUG: Compiling LoopReceive")
        
        # Get current actor handle
        if 'system_current_actor' in self.variables:
            offset = self.variables['system_current_actor']
            self.asm.emit_bytes(0x48, 0x8B, 0x85)  # MOV RAX, [RBP-offset]
            self.asm.emit_bytes(*struct.pack('<i', -offset))
            
            # Calculate mailbox address
            self.asm.emit_bytes(0x48, 0xC1, 0xE0, 0x07)  # SHL RAX, 7
            
            # Add ACB base
            if 'system_acb_table' in self.variables:
                acb_offset = self.variables['system_acb_table']
                self.asm.emit_bytes(0x48, 0x03, 0x85)  # ADD RAX, [RBP-offset]
                self.asm.emit_bytes(*struct.pack('<i', -acb_offset))
            
            # Add mailbox offset
            self.asm.emit_bytes(0x48, 0x83, 0xC0, 0x78)  # ADD RAX, 120
            
            # Load message from mailbox
            self.asm.emit_bytes(0x48, 0x8B, 0x00)  # MOV RAX, [RAX]
            
            print("DEBUG: Message received from mailbox")
        else:
            # No current actor, return 0
            self.asm.emit_mov_rax_imm64(0)
        
        return True
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
        if hasattr(self.asm, 'jump_manager') and hasattr(self.asm.jump_manager, 'labels'):
            print(f"DEBUG: Available labels in asm: {self.asm.jump_manager.labels}")
        else:
            print("DEBUG: No labels available in asm")
        
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
        
        # Fix user function forward references
        if hasattr(self, 'user_function_fixups'):
            print(f"DEBUG: Fixing {len(self.user_function_fixups)} user function references")
            for func_name, call_pos in self.user_function_fixups:
                print(f"DEBUG: Fixing call to user function {func_name} at position {call_pos}")
                
                if func_name in self.user_functions.user_functions:
                    func_info = self.user_functions.user_functions[func_name]
                    label = func_info['label']
                    
                    if label in self.asm.labels:
                        target_pos = self.asm.labels[label]
                        offset = target_pos - (call_pos + 5)
                        
                        # Patch the CALL instruction
                        offset_bytes = struct.pack('<i', offset)
                        for i in range(4):
                            self.asm.code[call_pos + 1 + i] = offset_bytes[i]
                        
                        print(f"DEBUG: Fixed call to {func_name}: offset={offset}")
                    else:
                        print(f"ERROR: Label {label} not found for function {func_name}")
                else:
                    print(f"ERROR: User function {func_name} not found")
    
    def _is_scheduler_primitive(self, func_name):
        """Check if a function is a scheduling primitive."""
        primitives = [
            'LoopYield', 'LoopSpawn', 'LoopJoin', 'LoopGetState',
            'LoopSetPriority', 'LoopGetCurrent', 'LoopSuspend', 'LoopResume'
        ]
        return func_name in primitives

    def _compile_scheduler_primitive(self, node):
        """Route to the correct scheduling primitive handler."""
        # Converts CamelCase (LoopYield) to snake_case (compile_loopyield)
        handler_name = "compile_" + ''.join(['_' + i.lower() if i.isupper() else i for i in node.function]).lstrip('_')
        handler = getattr(self.scheduler, handler_name, None)
        if handler:
            return handler(node)
        else:
            raise NotImplementedError(f"Scheduling primitive handler '{handler_name}' not found in scheduler module.")        

       
    def compile_expression(self, expr):
        return self.expressions.compile_expression(expr)
    
    def resolve_acronym_identifier(self, identifier_name):
        return self.memory.resolve_acronym_identifier(identifier_name)
    
    def compile(self, ast) -> bytes:
        """Compile AST to executable with 2-pass function registration"""
        print("\n=== COMPILATION STARTING ===")
        
        # PASS 1: Register ALL functions first (including in libraries)
        print("Phase 0: Registering all functions...")
        
        # Process top-level functions
        for decl in ast.declarations:
            if type(decl).__name__ in ('Function', 'FunctionDefinition'):
                self.user_functions.register_function(decl)
            elif type(decl).__name__ == 'Library':
                # Libraries handle their own registration in compile_library
                pass
        
        # Also let user_functions do its own scan if it has that method
        if hasattr(self.user_functions, 'process_all_functions'):
            self.user_functions.process_all_functions(ast)
        
        # PASS 2: Compile everything
        print("Phase 1: Generating machine code...")
        self.compile_node(ast)
        
        # Fix forward references
        self.fixup_forward_references()
        
        print("Phase 2: Resolving internal jump offsets...")
        self.asm.resolve_jumps()

        print("Phase 3: Building executable...")
        code_bytes = bytes(self.asm.code)
        data_bytes = bytes(self.asm.data)
        
        executable = self.elf.generate(code_bytes, data_bytes, self.asm)
        
        print(f"\n=== COMPILATION COMPLETE ({len(executable)} bytes) ===")
        return executable
            
        
    