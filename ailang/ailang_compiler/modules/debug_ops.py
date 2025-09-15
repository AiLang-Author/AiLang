"""
Debug Operations Module for AILANG Compiler
Handles compilation of debug primitives
"""

import struct
from typing import Optional, Dict, Any

class DebugOps:
    """Handles debug-related operations"""
    
    def __init__(self, compiler_context):
        self.compiler = compiler_context
        self.asm = compiler_context.asm
        self.debug_enabled = False
        self.debug_level = 0
        self.debug_flags = set()
        
    def set_debug_options(self, options: Dict[str, Any]):
        """Configure debug settings from compiler flags"""
        self.debug_enabled = options.get('debug', False)
        self.debug_level = options.get('debug_level', 1)
        self.debug_flags = options.get('debug_flags', set())
        
        print(f"DEBUG: Debug enabled={self.debug_enabled}, level={self.debug_level}")
        
    def compile_operation(self, node):
        """Main entry point for debug operations"""
        node_type = type(node).__name__
        
        # Handle FunctionCall nodes for DebugPerf
        if node_type == 'FunctionCall':
            if node.function.startswith('DebugPerf_'):
                operation = node.function.replace('DebugPerf_', '')
                label_node = node.arguments[0] if node.arguments else None
                label = label_node.value if label_node and hasattr(label_node, 'value') else None
                # Create a simple object with the needed attributes
                class PerfNode:
                    pass
                perf_node = PerfNode()
                perf_node.operation = operation
                perf_node.label = label
                return self.compile_debug_perf(perf_node)
        
        handlers = {
            'DebugBlock': self.compile_debug_block,
            'DebugAssert': self.compile_debug_assert,
            'DebugTrace': self.compile_debug_trace,
            'DebugBreak': self.compile_debug_break,
            'DebugMemory': self.compile_debug_memory,
            'DebugPerf': self.compile_debug_perf,
            'DebugInspect': self.compile_debug_inspect,
        }
        
        handler = handlers.get(node_type)
        if handler:
            return handler(node)
        else:
            raise ValueError(f"Unknown debug operation: {node_type}")
    
    def compile_debug_block(self, node):
        """Compile debug block - only if debug enabled and level matches"""
        if not self.debug_enabled:
            return  # Strip from binary
            
        if node.level > self.debug_level:
            return  # Skip higher-level debug blocks
            
        print(f"DEBUG: Compiling debug block '{node.label}' at level {node.level}")
        
        # Emit debug marker (for debugging tools)
        self.emit_debug_marker(node.label, "block_start")
        
        # Compile the debug body
        for stmt in node.body:
            self.compiler.compile_node(stmt)
            
        # Emit end marker
        self.emit_debug_marker(node.label, "block_end")
        
    def compile_debug_assert(self, node):
        """Compile debug assertion"""
        if not self.debug_enabled:
            return
            
        if 'assert' not in self.debug_flags and 'all' not in self.debug_flags:
            return
            
        print(f"DEBUG: Compiling assertion with message: {node.message}")
        
        # Evaluate condition
        self.compiler.compile_expression(node.condition)
        
        # Create labels
        pass_label = self.asm.create_label()
        
        # Test result in RAX
        self.asm.emit_bytes(0x48, 0x85, 0xC0)  # TEST RAX, RAX
        
        # Jump if non-zero (assertion passed)
        self.asm.emit_jump_to_label(pass_label, "JNZ")
        
        # Assertion failed - print message and halt
        self.emit_assertion_failure(node.message)
        
        # Mark pass label
        self.asm.mark_label(pass_label)
        
    def compile_debug_trace(self, node):
        """Compile trace point"""
        if not self.debug_enabled:
            return
            
        if 'trace' not in self.debug_flags and 'all' not in self.debug_flags:
            return
            
        print(f"DEBUG: Compiling trace point '{node.label}' type={node.trace_type}")
        
        # Emit trace marker
        self.emit_debug_marker(node.label, f"trace_{node.trace_type.lower()}")
        
        # For Entry/Exit traces, optionally log values
        if node.values:
            for value in node.values:
                self.compiler.compile_expression(value)
                # Could emit value to debug buffer here
                
    def compile_debug_break(self, node):
        """Compile breakpoint"""
        if not self.debug_enabled:
            return
            
        print(f"DEBUG: Compiling breakpoint '{node.label}'")
        
        if node.break_type == "Simple":
            # Emit INT3 instruction for breakpoint
            self.asm.emit_byte(0xCC)
        elif node.break_type == "Conditional" and node.condition:
            # Evaluate condition
            self.compiler.compile_expression(node.condition)
            
            # Skip breakpoint if condition false
            skip_label = self.asm.create_label()
            self.asm.emit_bytes(0x48, 0x85, 0xC0)  # TEST RAX, RAX
            self.asm.emit_jump_to_label(skip_label, "JZ")
            
            # Emit breakpoint
            self.asm.emit_byte(0xCC)
            
            self.asm.mark_label(skip_label)
            
    def compile_debug_memory(self, node):
        """Compile memory debugging operation"""
        if not self.debug_enabled:
            return
            
        if 'mem' not in self.debug_flags and 'all' not in self.debug_flags:
            return
            
        print(f"DEBUG: Memory debug operation: {node.operation}")
        
        # This would implement memory dump, leak detection, etc.
        # For now, just emit a marker
        self.emit_debug_marker(f"mem_{node.operation}", "memory")
        
    def compile_debug_perf(self, node):
        """Compile performance measurement operations"""
        if not self.debug_enabled:
            return
            
        if 'perf' not in self.debug_flags and 'all' not in self.debug_flags:
            return
            
        if node.operation == "Start":
            # Read timestamp counter at start
            self.asm.emit_bytes(0x0F, 0x31)  # RDTSC
            self.asm.emit_bytes(0x48, 0xC1, 0xE2, 0x20)  # SHL RDX, 32
            self.asm.emit_bytes(0x48, 0x09, 0xD0)  # OR RAX, RDX
            
            # Store start time in variable
            var_name = f"__perf_start_{node.label}"
            # The variable MUST have been pre-allocated by the memory manager's pre-scan.
            # The old logic to dynamically allocate it here was buggy.
            if var_name not in self.compiler.variables:
                raise Exception(f"Internal Compiler Error: Performance timer variable '{var_name}' was not pre-allocated by the memory manager.")
            
            offset = self.compiler.variables[var_name]
            if offset & 0x80000000: # Sanity check
                raise Exception(f"Internal Compiler Error: Performance timer variable '{var_name}' is incorrectly marked as a pool variable.")
            self.asm.emit_bytes(0x48, 0x89, 0x85)
            self.asm.emit_bytes(*struct.pack('<i', -offset))
            
            print(f"DEBUG: Started performance timer for '{node.label}' at offset {offset}")
            
        elif node.operation == "End":
            # Read timestamp counter at end
            self.asm.emit_bytes(0x0F, 0x31)  # RDTSC
            self.asm.emit_bytes(0x48, 0xC1, 0xE2, 0x20)  # SHL RDX, 32
            self.asm.emit_bytes(0x48, 0x09, 0xD0)  # OR RAX, RDX
            
            # Save end time in RBX
            self.asm.emit_mov_rbx_rax()
            
            # Load start time
            var_name = f"__perf_start_{node.label}"
            if var_name not in self.compiler.variables:
                # No start time - print error message
                print(f"WARNING: No start time for perf label '{node.label}'")
                dash_str = f"[PERF] {node.label}: - cycles\n"
                dash_offset = self.asm.add_string(dash_str)
                self.asm.emit_mov_rax_imm64(1)  # sys_write
                self.asm.emit_mov_rdi_imm64(1)  # stdout
                self.asm.emit_load_data_address('rsi', dash_offset)
                self.asm.emit_mov_rdx_imm64(len(dash_str))
                self.asm.emit_syscall()
                return
                
            offset = self.compiler.variables[var_name]
            
            # Load start timestamp into RAX
            self.asm.emit_bytes(0x48, 0x8B, 0x85)  # MOV RAX, [RBP-offset]
            self.asm.emit_bytes(*struct.pack('<i', -offset))
            
            # Calculate delta: RBX = end - start
            self.asm.emit_bytes(0x48, 0x29, 0xC3)  # SUB RBX, RAX
            
            # Print label
            label_str = f"[PERF] {node.label}: "
            label_offset = self.asm.add_string(label_str)
            self.asm.emit_mov_rax_imm64(1)  # sys_write
            self.asm.emit_mov_rdi_imm64(1)  # stdout
            self.asm.emit_load_data_address('rsi', label_offset)
            self.asm.emit_mov_rdx_imm64(len(label_str))
            self.asm.emit_syscall()
            
            # Print cycle count (in RBX)
            self.asm.emit_mov_rax_rbx()  # Move cycles to RAX
            self.asm.emit_print_number()
            
            # Print " cycles"
            cycles_str = " cycles"
            cycles_offset = self.asm.add_string(cycles_str)
            self.asm.emit_mov_rax_imm64(1)  # sys_write
            self.asm.emit_mov_rdi_imm64(1)  # stdout
            self.asm.emit_load_data_address('rsi', cycles_offset)
            self.asm.emit_mov_rdx_imm64(len(cycles_str))
            self.asm.emit_syscall()
            
            # Print newline separately
            newline_offset = self.asm.add_string("\n")
            self.asm.emit_mov_rax_imm64(1)  # sys_write
            self.asm.emit_mov_rdi_imm64(1)  # stdout
            self.asm.emit_load_data_address('rsi', newline_offset)
            self.asm.emit_mov_rdx_imm64(1)
            self.asm.emit_syscall()
            
            print(f"DEBUG: Ended performance timer for '{node.label}'")
        
        elif node.operation == "Mark":
            # Simple timestamp marker
            self.asm.emit_bytes(0x0F, 0x31)  # RDTSC
            print(f"DEBUG: Performance mark at '{node.label}'")

    def emit_rdtsc(self):
        """Helper method to emit RDTSC instruction"""
        # RDTSC - Read Time Stamp Counter
        self.emit_bytes(0x0F, 0x31)
        # Result is in EDX:EAX (high 32 bits in EDX, low 32 bits in EAX)
        print("DEBUG: Emitted RDTSC")

    def emit_rdpmc(self, counter):
        """Helper method to emit RDPMC instruction"""
        # RDPMC - Read Performance Monitoring Counter
        # ECX should contain the counter number
        self.emit_mov_rcx_imm64(counter)
        self.emit_bytes(0x0F, 0x33)
        # Result is in EDX:EAX
        print(f"DEBUG: Emitted RDPMC for counter {counter}")

    def emit_cpuid(self):
        """Helper method to emit CPUID instruction (for serialization)"""
        # CPUID - CPU Identification
        # Used as a serialization barrier before/after RDTSC for accuracy
        self.emit_bytes(0x0F, 0xA2)
        print("DEBUG: Emitted CPUID")
            
    def compile_debug_inspect(self, node):
        """Compile state inspection"""
        if not self.debug_enabled:
            return
            
        print(f"DEBUG: Inspect target: {node.target}")
        
        # This would dump variables, stack, etc.
        self.emit_debug_marker(f"inspect_{node.target}", "inspect")
        
    def emit_debug_marker(self, label: str, marker_type: str):
        """Emit a debug marker in the code"""
        # These markers can be picked up by debugging tools
        # For now, emit as NOP with specific pattern
        
        # Use multi-byte NOP as marker (won't affect execution)
        # 0F 1F 44 00 00 is a 5-byte NOP
        self.asm.emit_bytes(0x0F, 0x1F, 0x44, 0x00, 0x00)
        
        # Could also emit to debug section of ELF
        
    def emit_assertion_failure(self, message: str):
        """Emit code for assertion failure"""
        # Print assertion message
        msg = f"ASSERTION FAILED: {message}\n"
        msg_offset = self.asm.add_string(msg)
        
        # Set up write syscall
        self.asm.emit_mov_rax_imm64(1)  # sys_write
        self.asm.emit_mov_rdi_imm64(2)  # stderr
        self.asm.emit_load_data_address('rsi', msg_offset)
        self.asm.emit_mov_rdx_imm64(len(msg))
        self.asm.emit_syscall()
        
        # Exit with error code
        self.asm.emit_mov_rax_imm64(60)  # sys_exit
        self.asm.emit_mov_rdi_imm64(1)   # exit code 1
        self.asm.emit_syscall()