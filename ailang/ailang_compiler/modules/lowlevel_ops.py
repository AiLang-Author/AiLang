#!/usr/bin/env python3
"""
Low-Level Operations Module for AILANG Compiler
Handles systems programming operations: pointers, hardware access, atomic operations
FIXED: compile_operation now handles both AST nodes and FunctionCall nodes
"""

import sys
import os
import struct
from ailang_parser.ailang_ast import *

class LowLevelOps:
    """Handles low-level systems programming operations"""
    
    def __init__(self, compiler_context):
        self.compiler = compiler_context
        self.asm = compiler_context.asm
    
    def compile_operation(self, node):
        """Compile low-level operations - handles both AST nodes and FunctionCalls"""
        try:
            # Handle direct AST nodes (AddressOf, Dereference, SizeOf)
            if hasattr(node, '__class__'):
                node_type = node.__class__.__name__
                if node_type == 'AddressOf':
                    return self.compile_address_of_ast(node)
                elif node_type == 'Dereference':
                    return self.compile_dereference_ast(node)
                elif node_type == 'SizeOf':
                    return self.compile_sizeof_ast(node)
                elif node_type == 'InterruptControl':
                    return self.compile_interrupt_control(node)
                elif node_type == 'InlineAssembly':
                    return self.compile_inline_assembly(node)
                elif node_type == 'SystemCall':
                    return self.compile_system_call(node)
            
            # Handle FunctionCall nodes
            if hasattr(node, 'function'):
                if node.function == 'Dereference':
                    return self.compile_dereference(node)
                elif node.function == 'AddressOf':
                    return self.compile_address_of(node)
                elif node.function == 'SizeOf':
                    return self.compile_sizeof(node)
                elif node.function == 'Allocate':
                    return self.compile_allocate(node)
                elif node.function in ['Deallocate', 'Free']:  # Handle both names
                    return self.compile_deallocate(node)
                elif node.function == 'MemoryCopy':
                    return self.compile_memory_copy(node)
                elif node.function == 'MemorySet':
                    return self.compile_memory_set(node)
                elif node.function == 'MemoryCompare':
                    return self.compile_memory_compare(node)
                elif node.function == 'StoreValue':
                    return self.compile_storevalue(node)
                elif node.function == 'SetByte':  
                    return self.compile_setbyte(node)
                elif node.function == 'GetByte':  
                    return self.compile_getbyte(node)
                elif node.function in ['PortRead', 'PortWrite']:
                    return self.compile_port_operation(node)
                elif node.function in ['AtomicRead', 'AtomicWrite', 'AtomicAdd', 'AtomicCompareSwap']:
                    return self.compile_atomic_operation(node)
                elif node.function in ['MMIORead', 'MMIOWrite']:
                    return self.compile_mmio_operation(node)
                elif node.function == 'HardwareRegister':
                    return self.compile_hardware_register(node)
                # If none match, return False so dispatch continues
            
            return False  # Not a low-level operation
                
        except Exception as e:
            print(f"ERROR: Low-level operation compilation failed: {str(e)}")
            raise
        
    # Add this method to the LowLevelOps class in lowlevel_ops.py:

    def compile_setbyte(self, node):
        """SetByte(address, offset, value) - Write a byte to memory"""
        try:
            print("DEBUG: Compiling SetByte operation")
            
            if len(node.arguments) != 3:
                raise ValueError("SetByte requires 3 arguments (address, offset, value)")
            
            # Evaluate address -> push on stack
            self.compiler.compile_expression(node.arguments[0])
            self.asm.emit_push_rax()
            
            # Evaluate offset -> push on stack  
            self.compiler.compile_expression(node.arguments[1])
            self.asm.emit_push_rax()
            
            # Evaluate value -> stays in RAX
            self.compiler.compile_expression(node.arguments[2])
            
            # Pop offset into RCX
            self.asm.emit_pop_rcx()
            
            # Pop address into RDX
            self.asm.emit_pop_rdx()
            
            # Add offset to address: RDX = address + offset
            self.asm.emit_bytes(0x48, 0x01, 0xCA)  # ADD RDX, RCX
            
            # Store byte value (AL) at [RDX]
            self.asm.emit_bytes(0x88, 0x02)  # MOV [RDX], AL
            
            # Return the value that was written (still in RAX)
            print("DEBUG: SetByte completed")
            return True
            
        except Exception as e:
            print(f"ERROR: SetByte compilation failed: {str(e)}")
            raise  
        
        
    def compile_getbyte(self, node):
        """GetByte(address, offset) - Read a byte from memory"""
        try:
            print("DEBUG: Compiling GetByte operation")
            
            if len(node.arguments) != 2:
                raise ValueError("GetByte requires 2 arguments (address, offset)")
            
            # Evaluate the address into RAX
            self.compiler.compile_expression(node.arguments[0])
            
            # Save address to RDX
            self.asm.emit_push_rax()  # Save address
            
            # Evaluate the offset into RAX
            self.compiler.compile_expression(node.arguments[1])
            
            # RAX = offset, [RSP] = address
            # Move offset to RCX
            self.asm.emit_mov_rcx_rax()  # RCX = offset
            
            # Restore address to RDX
            self.asm.emit_pop_rdx()  # RDX = address
            
            # Add offset to address: RDX = RDX + RCX
            self.asm.emit_bytes(0x48, 0x01, 0xCA)  # ADD RDX, RCX
            
            # Load byte from [RDX] into RAX (zero-extended)
            self.asm.emit_bytes(0x48, 0x0F, 0xB6, 0x02)  # MOVZX RAX, BYTE [RDX]
            
            print("DEBUG: GetByte completed")
            return True
            
        except Exception as e:
            print(f"ERROR: GetByte compilation failed: {str(e)}")
            raise 
        
    
    def compile_dereference(self, node):
        """Compile pointer dereference operation (FunctionCall syntax)"""
        try:
            print(f"DEBUG: Compiling Dereference operation")
            
            if len(node.arguments) < 1:
                raise ValueError("Dereference requires at least 1 argument (pointer)")
            
            # Compile pointer expression to get address in RAX
            self.compiler.compile_expression(node.arguments[0])
            
            # Get size hint from second argument if present
            size_hint = "byte"  # default
            if len(node.arguments) > 1:
                if hasattr(node.arguments[1], 'value'):
                    size_hint = str(node.arguments[1].value).lower().strip('"').strip("'")
            
            # Perform dereference based on size
            if size_hint == "byte":
                # MOVZX RAX, BYTE [RAX] - proper zero-extend
                self.asm.emit_bytes(0x48, 0x8B, 0x00)  # MOV RAX, QWORD [RAX]
            elif size_hint == "word":
                self.asm.emit_dereference_word()
            elif size_hint == "dword":
                self.asm.emit_dereference_dword()
            else:
                self.asm.emit_dereference_qword()
            
            print(f"DEBUG: Dereferenced as {size_hint}")
            return True
            
        except Exception as e:
            print(f"ERROR: Dereference compilation failed: {str(e)}")
            raise
    
    def compile_address_of(self, node):
        """Compile address-of operation (&variable)"""
        try:
            print(f"DEBUG: Compiling AddressOf operation")
            
            if len(node.arguments) < 1:
                raise ValueError("AddressOf requires 1 argument (variable)")
            
            # Get variable name
            if isinstance(node.arguments[0], Identifier):
                var_name = node.arguments[0].name
                resolved_name = self.compiler.resolve_acronym_identifier(var_name)
                
                if resolved_name in self.compiler.variables:
                    # Get stack offset for variable
                    offset = self.compiler.variables[resolved_name]
                    
                    # Load effective address: LEA RAX, [RBP - offset]
                    self.asm.emit_lea_rax("RBP", -offset)
                    print(f"DEBUG: Got address of variable {resolved_name} at [RBP - {offset}]")
                else:
                    raise ValueError(f"Undefined variable: {var_name} (resolved: {resolved_name})")
            else:
                raise ValueError("AddressOf requires an identifier argument")
            
            print("DEBUG: AddressOf operation completed")
            return True
            
        except Exception as e:
            print(f"ERROR: AddressOf compilation failed: {str(e)}")
            raise
    
    def compile_sizeof(self, node):
        """Compile sizeof operation"""
        try:
            print(f"DEBUG: Compiling SizeOf operation")
            
            if len(node.arguments) < 1:
                raise ValueError("SizeOf requires 1 argument")
            
            # Simple type size mapping
            type_sizes = {
                'Integer': 8, 'Int64': 8, 'QWord': 8,
                'Int32': 4, 'DWord': 4,
                'Int16': 2, 'Word': 2,
                'Int8': 1, 'Byte': 1,
                'UInt64': 8, 'UInt32': 4, 'UInt16': 2, 'UInt8': 1,
                'FloatingPoint': 8,
                'Text': 8,  # Pointer to string
                'Boolean': 1,
                'Address': 8,  # 64-bit pointer
                'Pointer': 8   # 64-bit pointer
            }
            
            size = 8  # Default size
            
            if isinstance(node.arguments[0], Identifier):
                type_name = node.arguments[0].name
                size = type_sizes.get(type_name, 8)
                print(f"DEBUG: Size of type {type_name} is {size} bytes")
            elif hasattr(node.arguments[0], 'type_name'):
                type_name = node.arguments[0].type_name
                size = type_sizes.get(type_name, 8)
                print(f"DEBUG: Size of type {type_name} is {size} bytes")
            else:
                # For variables, assume 8 bytes (qword)
                size = 8
                print(f"DEBUG: Default size assumption: {size} bytes")
            
            # Load size into RAX
            self.asm.emit_mov_rax_imm64(size)
            
            print("DEBUG: SizeOf operation completed")
            return True
            
        except Exception as e:
            print(f"ERROR: SizeOf compilation failed: {str(e)}")
            raise
    
    def compile_allocate(self, node):
        """Compile memory allocation using mmap (heap allocation)
        
        This function properly preserves callee-saved registers that might
        be used by the compiler for special purposes (pools, frame pointers, etc.)
        """
        try:
            print(f"DEBUG: Compiling Allocate operation (heap-based)")

            if len(node.arguments) < 1:
                raise ValueError("Allocate requires 1 argument (size)")

            # Save callee-saved registers that we might clobber
            # We need to save:
            # - RBX (often used for local state)
            # - R12-R15 (often used for compiler-specific purposes like pools)
            # We don't save RBP/RSP as they're handled by the function frame
            self.asm.emit_push_rbx()
            self.asm.emit_bytes(0x41, 0x54)  # PUSH R12
            self.asm.emit_bytes(0x41, 0x55)  # PUSH R13  
            self.asm.emit_bytes(0x41, 0x56)  # PUSH R14
            self.asm.emit_bytes(0x41, 0x57)  # PUSH R15
            
            # Evaluate size argument -> RAX
            self.compiler.compile_expression(node.arguments[0])
            
            # Setup mmap parameters
            # mmap(NULL, size, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0)
            self.asm.emit_mov_rsi_rax()                       # RSI = size
            self.asm.emit_mov_rax_imm64(9)                    # RAX = sys_mmap
            self.asm.emit_mov_rdi_imm64(0)                    # RDI = NULL (let kernel choose address)
            self.asm.emit_mov_rdx_imm64(3)                    # RDX = PROT_READ|PROT_WRITE
            self.asm.emit_mov_r10_imm64(0x22)                 # R10 = MAP_PRIVATE|MAP_ANONYMOUS
            self.asm.emit_bytes(0x49, 0xC7, 0xC0, 0xFF, 0xFF, 0xFF, 0xFF)  # MOV R8, -1 (no fd)
            self.asm.emit_mov_r9_imm64(0)                     # R9 = 0 (offset)

            # Make the syscall
            self.asm.emit_syscall()
            
            # RAX now contains allocated address (or -1 on error)
            # For production code, you'd want to check for errors here:
            # cmp rax, -1
            # je allocation_failed
            
            # Save result before restoring registers
            self.asm.emit_mov_rcx_rax()  # Save result in RCX temporarily
            
            # Restore callee-saved registers in reverse order
            self.asm.emit_bytes(0x41, 0x5F)  # POP R15
            self.asm.emit_bytes(0x41, 0x5E)  # POP R14
            self.asm.emit_bytes(0x41, 0x5D)  # POP R13
            self.asm.emit_bytes(0x41, 0x5C)  # POP R12
            self.asm.emit_pop_rbx()
            
            # Move result back to RAX for return
            self.asm.emit_mov_rax_rcx()
            
            print("DEBUG: Heap allocation completed")
            return True
            
        except Exception as e:
            print(f"ERROR: Allocate compilation failed: {str(e)}")
            raise
    
    
    def compile_deallocate(self, node):
        """Compile memory deallocation - handles both Free and Deallocate"""
        try:
            print(f"DEBUG: Compiling {node.function} operation")
            
            # Free(ptr) only needs the pointer, size is tracked elsewhere
            # For now, we'll use munmap with a default size or skip it
            if node.function == 'Free':
                # Simple free - just mark as successful
                # In a real implementation, you'd track allocations
                self.compiler.compile_expression(node.arguments[0])
                # For now, just return success
                self.asm.emit_mov_rax_imm64(0)  # Success
                print("DEBUG: Free completed (simplified)")
                return True
            
            # Deallocate(address, size) - full munmap
            if len(node.arguments) < 2:
                raise ValueError("Deallocate requires 2 arguments (address, size)")
            
            # Get address in RAX
            self.compiler.compile_expression(node.arguments[0])
            self.asm.emit_push_rax()  # Save address
            
            # Get size in RAX
            self.compiler.compile_expression(node.arguments[1])
            self.asm.emit_bytes(0x48, 0x89, 0xC6)  # MOV RSI, RAX (size)
            
            # Get address back
            self.asm.emit_pop_rdi()  # POP RDI (address)
            
            # RAX = 11 (munmap syscall)
            self.asm.emit_mov_rax_imm64(11)
            
            # Make syscall
            self.asm.emit_syscall()
            
            # Return result (0 on success)
            print("DEBUG: Heap deallocation completed")
            return True
            
        except Exception as e:
            print(f"ERROR: Deallocate compilation failed: {str(e)}")
            raise

    def compile_memory_copy(self, node):
        """Compile memory copy operation"""
        try:
            print(f"DEBUG: Compiling MemoryCopy operation")
            
            if len(node.arguments) < 3:
                raise ValueError("MemoryCopy requires 3 arguments (dest, src, size)")
            
            # Evaluate arguments
            # Destination address
            self.compiler.compile_expression(node.arguments[0])
            self.asm.emit_push_rax()  # Save dest
            
            # Source address
            self.compiler.compile_expression(node.arguments[1])
            self.asm.emit_push_rax()  # Save src
            
            # Size
            self.compiler.compile_expression(node.arguments[2])
            self.asm.emit_mov_rcx_rax()  # Size to RCX
            
            # Pop addresses
            self.asm.emit_pop_rsi()   # Source to RSI
            self.asm.emit_pop_rdi()   # Dest to RDI
            
            # Simple byte-by-byte copy loop
            copy_loop = self.asm.create_label()
            copy_end = self.asm.create_label()
            
            # Check if size is zero
            self.asm.emit_bytes(0x48, 0x83, 0xF9, 0x00)  # CMP RCX, 0
            self.asm.emit_jump_to_label(copy_end, "JE")
            
            # Copy loop
            self.asm.mark_label(copy_loop)
            self.asm.emit_bytes(0x8A, 0x06)              # MOV AL, [RSI]
            self.asm.emit_bytes(0x88, 0x07)              # MOV [RDI], AL
            self.asm.emit_bytes(0x48, 0xFF, 0xC6)        # INC RSI
            self.asm.emit_bytes(0x48, 0xFF, 0xC7)        # INC RDI
            self.asm.emit_bytes(0x48, 0xFF, 0xC9)        # DEC RCX
            self.asm.emit_jump_to_label(copy_loop, "JNE")
            
            self.asm.mark_label(copy_end)
            
            # Return success
            self.asm.emit_mov_rax_imm64(1)
            
            print("DEBUG: MemoryCopy operation completed")
            return True
            
        except Exception as e:
            print(f"ERROR: MemoryCopy compilation failed: {str(e)}")
            raise
    
    def compile_memory_set(self, node):
        """Compile memory set operation"""
        try:
            print(f"DEBUG: Compiling MemorySet operation")
            
            if len(node.arguments) < 3:
                raise ValueError("MemorySet requires 3 arguments (address, value, size)")
            
            # Evaluate arguments
            # Address
            self.compiler.compile_expression(node.arguments[0])
            self.asm.emit_mov_rdi_rax()  # Address to RDI
            
            # Value
            self.compiler.compile_expression(node.arguments[1])
            self.asm.emit_push_rax()  # Save value
            
            # Size
            self.compiler.compile_expression(node.arguments[2])
            self.asm.emit_mov_rcx_rax()  # Size to RCX
            
            # Get value
            self.asm.emit_pop_rax()  # Value to AL
            
            # Simple set loop
            set_loop = self.asm.create_label()
            set_end = self.asm.create_label()
            
            # Check if size is zero
            self.asm.emit_bytes(0x48, 0x83, 0xF9, 0x00)  # CMP RCX, 0
            self.asm.emit_jump_to_label(set_end, "JE")
            
            # Set loop
            self.asm.mark_label(set_loop)
            self.asm.emit_bytes(0x88, 0x07)              # MOV [RDI], AL
            self.asm.emit_bytes(0x48, 0xFF, 0xC7)        # INC RDI
            self.asm.emit_bytes(0x48, 0xFF, 0xC9)        # DEC RCX
            self.asm.emit_jump_to_label(set_loop, "JNE")
            
            self.asm.mark_label(set_end)
            
            # Return success
            self.asm.emit_mov_rax_imm64(1)
            
            print("DEBUG: MemorySet operation completed")
            return True
            
        except Exception as e:
            print(f"ERROR: MemorySet compilation failed: {str(e)}")
            raise
    
    def compile_memory_compare(self, node):
        """Compile memory compare operation"""
        try:
            print(f"DEBUG: Compiling MemoryCompare operation")
            
            if len(node.arguments) < 3:
                raise ValueError("MemoryCompare requires 3 arguments (addr1, addr2, size)")
            
            # Evaluate arguments
            # Address 1
            self.compiler.compile_expression(node.arguments[0])
            self.asm.emit_push_rax()  # Save addr1
            
            # Address 2
            self.compiler.compile_expression(node.arguments[1])
            self.asm.emit_push_rax()  # Save addr2
            
            # Size
            self.compiler.compile_expression(node.arguments[2])
            self.asm.emit_mov_rcx_rax()  # Size to RCX
            
            # Pop addresses
            self.asm.emit_pop_rsi()   # addr2 to RSI
            self.asm.emit_pop_rdi()   # addr1 to RDI
            
            # Compare loop
            cmp_loop = self.asm.create_label()
            cmp_end = self.asm.create_label()
            cmp_not_equal = self.asm.create_label()
            
            # Check if size is zero
            self.asm.emit_bytes(0x48, 0x83, 0xF9, 0x00)  # CMP RCX, 0
            self.asm.emit_jump_to_label(cmp_end, "JE")
            
            # Compare loop
            self.asm.mark_label(cmp_loop)
            self.asm.emit_bytes(0x8A, 0x07)              # MOV AL, [RDI]
            self.asm.emit_bytes(0x8A, 0x1E)              # MOV BL, [RSI]
            self.asm.emit_bytes(0x38, 0xD8)              # CMP AL, BL
            self.asm.emit_jump_to_label(cmp_not_equal, "JNE")
            self.asm.emit_bytes(0x48, 0xFF, 0xC7)        # INC RDI
            self.asm.emit_bytes(0x48, 0xFF, 0xC6)        # INC RSI
            self.asm.emit_bytes(0x48, 0xFF, 0xC9)        # DEC RCX
            self.asm.emit_jump_to_label(cmp_loop, "JNE")
            
            # Equal - return 0
            self.asm.mark_label(cmp_end)
            self.asm.emit_mov_rax_imm64(0)
            self.asm.emit_jump_to_label(cmp_not_equal + 1, "JMP")  # Skip to end
            
            # Not equal - return 1
            self.asm.mark_label(cmp_not_equal)
            self.asm.emit_mov_rax_imm64(1)
            
            print("DEBUG: MemoryCompare operation completed")
            return True
            
        except Exception as e:
            print(f"ERROR: MemoryCompare compilation failed: {str(e)}")
            raise

    def compile_storevalue(self, node):
        """Compile StoreValue operation - write value to memory address"""
        # --- REWRITE to be byte-aware by default ---
        print("DEBUG: Compiling StoreValue")
        if len(node.arguments) < 2:
            raise ValueError("StoreValue requires address and value")
        
        # Check if value is a small constant (byte-sized)
        is_byte_value = False
        if isinstance(node.arguments[1], Number):
            val = int(node.arguments[1].value)
            if 0 <= val <= 255:
                is_byte_value = True
        
        # Compile address
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_push_rax()  # Save address
        
        # Compile value
        self.compiler.compile_expression(node.arguments[1])
        self.asm.emit_mov_rbx_rax()  # Value in RBX
        
        # Restore address
        self.asm.emit_pop_rax()
        
        # Store based on value size
        if is_byte_value:
            self.asm.emit_bytes(0x88, 0x18)  # MOV [RAX], BL (byte)
        else:
            self.asm.emit_bytes(0x48, 0x89, 0x18)  # MOV [RAX], RBX (qword)
        
        print(f"DEBUG: StoreValue completed ({'byte' if is_byte_value else 'qword'})")
        return True
    
       
    
    def compile_atomic_operation(self, node):
        """Compile atomic operations"""
        try:
            print(f"DEBUG: Compiling {node.function} operation")
            
            if node.function == 'AtomicRead':
                if len(node.arguments) < 1:
                    raise ValueError("AtomicRead requires 1 argument (address)")
                
                # Compile address expression
                self.compiler.compile_expression(node.arguments[0])
                
                # Atomic read is just a regular MOV in x86-64 for aligned data
                self.asm.emit_dereference_qword()
                
            elif node.function == 'AtomicWrite':
                if len(node.arguments) < 2:
                    raise ValueError("AtomicWrite requires 2 arguments (address, value)")
                
                # Compile value expression
                self.compiler.compile_expression(node.arguments[1])
                self.asm.emit_push_rax()  # Save value
                
                # Compile address expression
                self.compiler.compile_expression(node.arguments[0])
                self.asm.emit_mov_rbx_rax()  # Address to RBX
                
                # Get value
                self.asm.emit_pop_rax()   # Value to RAX
                
                # Atomic write (XCHG is implicitly locked)
                self.asm.emit_bytes(0x48, 0x87, 0x03)  # XCHG [RBX], RAX
                
            elif node.function == 'AtomicAdd':
                if len(node.arguments) < 2:
                    raise ValueError("AtomicAdd requires 2 arguments (address, value)")
                
                # Get address
                address = 0x1000  # Default address
                if hasattr(node.arguments[0], 'value'):
                    address = int(node.arguments[0].value)
                
                # Get value
                value = 1  # Default increment
                if len(node.arguments) > 1 and hasattr(node.arguments[1], 'value'):
                    value = int(node.arguments[1].value)
                
                # Atomic add
                self.asm.emit_atomic_add(address, value)
                
            elif node.function == 'AtomicCompareSwap':
                if len(node.arguments) < 3:
                    raise ValueError("AtomicCompareSwap requires 3 arguments (address, expected, new)")
                
                # This is complex - simplified implementation
                # In real code, this would use LOCK CMPXCHG
                
                # Load expected value into RAX
                self.compiler.compile_expression(node.arguments[1])
                
                # Load new value into RBX
                self.asm.emit_push_rax()
                self.compiler.compile_expression(node.arguments[2])
                self.asm.emit_mov_rbx_rax()
                self.asm.emit_pop_rax()
                
                # Get address
                address = 0x1000  # Default
                if hasattr(node.arguments[0], 'value'):
                    address = int(node.arguments[0].value)
                
                # Atomic compare and exchange
                self.asm.emit_atomic_compare_exchange(address)
            
            print(f"DEBUG: {node.function} operation completed")
            return True
            
        except Exception as e:
            print(f"ERROR: {node.function} compilation failed: {str(e)}")
            raise
    
    def compile_mmio_operation(self, node):
        """Compile memory-mapped I/O operations"""
        try:
            print(f"DEBUG: Compiling {node.function} operation")
            
            if node.function == 'MMIORead':
                if len(node.arguments) < 1:
                    raise ValueError("MMIORead requires 1 argument (address)")
                
                # Compile address expression
                self.compiler.compile_expression(node.arguments[0])
                
                # Size hint
                size = "qword"
                if len(node.arguments) > 1 and hasattr(node.arguments[1], 'value'):
                    size = str(node.arguments[1].value).lower()
                
                # MMIO read (volatile memory access)
                if size == "byte":
                    self.asm.emit_dereference_byte()
                elif size == "word":
                    self.asm.emit_dereference_word()
                elif size == "dword":
                    self.asm.emit_dereference_dword()
                else:
                    self.asm.emit_dereference_qword()
                
                # Memory barrier to ensure ordering
                self.asm.emit_memory_fence()
                
            elif node.function == 'MMIOWrite':
                if len(node.arguments) < 2:
                    raise ValueError("MMIOWrite requires 2 arguments (address, value)")
                
                # Compile value expression
                self.compiler.compile_expression(node.arguments[1])
                self.asm.emit_push_rax()  # Save value
                
                # Compile address expression
                self.compiler.compile_expression(node.arguments[0])
                self.asm.emit_mov_rbx_rax()  # Address to RBX
                
                # Get value
                self.asm.emit_pop_rax()   # Value to RAX
                
                # Size hint
                size = "qword"
                if len(node.arguments) > 2 and hasattr(node.arguments[2], 'value'):
                    size = str(node.arguments[2].value).lower()
                
                # Memory barrier before write
                self.asm.emit_memory_fence()
                
                # MMIO write
                if size == "byte":
                    self.asm.emit_store_to_pointer_byte("RAX")
                elif size == "qword":
                    self.asm.emit_store_to_pointer_qword("RAX")
                # Add other sizes as needed
                
                # Memory barrier after write
                self.asm.emit_memory_fence()
            
            print(f"DEBUG: {node.function} operation completed")
            return True
            
        except Exception as e:
            print(f"ERROR: {node.function} compilation failed: {str(e)}")
            raise
    
    def compile_hardware_register(self, node):
        """Compile hardware register access"""
        try:
            print(f"DEBUG: Compiling HardwareRegister operation")
            
            if len(node.arguments) < 2:
                raise ValueError("HardwareRegister requires 2 arguments (register, operation)")
            
            # Get register name
            register = "CR0"  # Default
            if hasattr(node.arguments[0], 'value'):
                register = str(node.arguments[0].value)
            elif hasattr(node.arguments[0], 'name'):
                register = node.arguments[0].name
            
            # Get operation
            operation = "read"  # Default
            if hasattr(node.arguments[1], 'value'):
                operation = str(node.arguments[1].value).lower()
            elif hasattr(node.arguments[1], 'name'):
                operation = node.arguments[1].name.lower()
            
            # Handle control registers
            if register.upper().startswith('CR'):
                cr_num = int(register[2:]) if len(register) > 2 else 0
                
                if operation == "read":
                    self.asm.emit_read_cr(cr_num)
                elif operation == "write":
                    # Value should be in RAX
                    if len(node.arguments) > 2:
                        self.compiler.compile_expression(node.arguments[2])
                    self.asm.emit_write_cr(cr_num)
                else:
                    raise ValueError(f"Invalid operation for control register: {operation}")
            
            # Handle MSRs
            elif register.upper() == "MSR":
                if operation == "read":
                    # MSR number should be in ECX
                    if len(node.arguments) > 2:
                        self.compiler.compile_expression(node.arguments[2])
                        self.asm.emit_bytes(0x89, 0xC1)  # MOV ECX, EAX
                    self.asm.emit_read_msr()
                elif operation == "write":
                    # MSR number in ECX, value in EDX:EAX
                    if len(node.arguments) > 2:
                        self.compiler.compile_expression(node.arguments[2])  # MSR number
                        self.asm.emit_bytes(0x89, 0xC1)  # MOV ECX, EAX
                    if len(node.arguments) > 3:
                        self.compiler.compile_expression(node.arguments[3])  # Value
                        # Split 64-bit value into EDX:EAX
                        self.asm.emit_bytes(0x48, 0x89, 0xC2)  # MOV RDX, RAX
                        self.asm.emit_bytes(0x48, 0xC1, 0xEA, 0x20)  # SHR RDX, 32
                    self.asm.emit_write_msr()
            
            else:
                print(f"WARNING: Unsupported register type: {register}")
                # Just return 0 for unsupported registers
                self.asm.emit_mov_rax_imm64(0)
            
            print(f"DEBUG: HardwareRegister operation completed")
            return True
            
        except Exception as e:
            print(f"ERROR: HardwareRegister compilation failed: {str(e)}")
            raise
    
    def compile_interrupt_control(self, node):
        """Compile interrupt control statements"""
        try:
            if isinstance(node, InterruptControl):
                if node.operation == "enable":
                    self.asm.emit_sti()
                    print("DEBUG: Enabled interrupts")
                elif node.operation == "disable":
                    self.asm.emit_cli()
                    print("DEBUG: Disabled interrupts")
                elif node.operation == "trigger":
                    if node.interrupt_number:
                        # Compile interrupt number
                        self.compiler.compile_expression(node.interrupt_number)
                        # For simplicity, just use INT 0x80 (common Linux syscall)
                        self.asm.emit_int(0x80)
                    else:
                        self.asm.emit_int(0x80)  # Default
                    print("DEBUG: Triggered software interrupt")
                
                return True
            return False
            
        except Exception as e:
            print(f"ERROR: Interrupt control compilation failed: {str(e)}")
            raise
    
    def compile_address_of_ast(self, node):
        """Compile AddressOf AST node directly"""
        try:
            print(f"DEBUG: Compiling AddressOf AST node")
            
            if not hasattr(node, 'variable'):
                raise ValueError("AddressOf node missing variable attribute")
            
            # Get variable name
            if hasattr(node.variable, 'name'):
                var_name = node.variable.name
                
                # FIRST: Check if it's a function
                if hasattr(self.compiler, 'user_functions'):
                    # Look in the actual user_functions dictionary
                    if var_name in self.compiler.user_functions.user_functions:
                        # Get function info and label
                        func_info = self.compiler.user_functions.user_functions[var_name]
                        label = func_info['label']
                        self.asm.emit_load_label_address('rax', label)
                        print(f"DEBUG: Got function address for {var_name} with label {label}")
                        return True
                    
                    # Also try with Function. prefix stripped (in case of Function.TestFunc)
                    if var_name.startswith("Function."):
                        clean_target = var_name[9:]
                        if clean_target in self.compiler.user_functions.user_functions:
                            func_info = self.compiler.user_functions.user_functions[clean_target]
                            label = func_info['label']
                            self.asm.emit_load_label_address('rax', label)
                            print(f"DEBUG: Got function address for {clean_target} with label {label}")
                            return True
                
                # SECOND: Check if it's a variable (your existing code)
                resolved_name = self.compiler.resolve_acronym_identifier(var_name)
                
                if resolved_name in self.compiler.variables:
                    # Get stack offset for variable
                    offset = self.compiler.variables[resolved_name]
                    
                    # Emit LEA RAX, [RBP - offset] directly with correct bytes
                    # LEA RAX, [RBP + disp8] = 48 8D 45 disp8
                    if -128 <= -offset <= 127:
                        self.asm.emit_bytes(0x48, 0x8D, 0x45)
                        # Two's complement for negative offset
                        if offset > 0:
                            self.asm.emit_bytes((256 - offset) & 0xFF)
                        else:
                            self.asm.emit_bytes((-offset) & 0xFF)
                    else:
                        # Use 32-bit displacement for larger offsets
                        self.asm.emit_bytes(0x48, 0x8D, 0x85)
                        self.asm.emit_bytes(*struct.pack('<i', -offset))
                    
                    print(f"DEBUG: Got address of variable {resolved_name} at [RBP - {offset}]")
                else:
                    raise ValueError(f"Undefined variable: {var_name} (resolved: {resolved_name})")
            else:
                raise ValueError("AddressOf requires an identifier argument")
            
            print("DEBUG: AddressOf AST compilation completed")
            return True
            
        except Exception as e:
            print(f"ERROR: AddressOf AST compilation failed: {str(e)}")
            raise
    
    def compile_dereference_ast(self, node):
        """Compile Dereference AST node directly"""
        try:
            print(f"DEBUG: Compiling Dereference AST node")
            
            if not hasattr(node, 'pointer'):
                raise ValueError("Dereference node missing pointer attribute")
            
            # Compile pointer expression to get address in RAX
            self.compiler.compile_expression(node.pointer)
            
            # Get size hint - default to qword for backward compatibility
            size_hint = getattr(node, 'size_hint', 'qword') # Default to qword for integers/pointers
            if size_hint is None or size_hint == '':
                size_hint = 'qword' # Default to qword
            
            # Normalize size hint
            size_hint = str(size_hint).lower().strip('"').strip("'")
            
            # Perform dereference based on size
            if size_hint == "byte":
                # MOVZX RAX, BYTE [RAX] - proper zero-extend
                self.asm.emit_bytes(0x48, 0x0F, 0xB6, 0x00)  # MOVZX RAX, BYTE [RAX]
            elif size_hint == "word":
                self.asm.emit_dereference_word()
            elif size_hint == "dword":
                self.asm.emit_dereference_dword()
            elif size_hint == "qword":
                self.asm.emit_dereference_qword()
            else:
                # Default to qword for unknown hints
                self.asm.emit_dereference_qword()
                print(f"DEBUG: Unknown size hint '{size_hint}', defaulting to qword")
            
            print(f"DEBUG: Dereferenced as {size_hint}")
            return True
            
        except Exception as e:
            print(f"ERROR: Dereference AST compilation failed: {str(e)}")
            raise
                
        
    
    def compile_sizeof_ast(self, node):
        """Compile SizeOf AST node directly"""
        try:
            print(f"DEBUG: Compiling SizeOf AST node")
            
            if not hasattr(node, 'target'):
                raise ValueError("SizeOf node missing target attribute")
            
            # Simple type size mapping
            type_sizes = {
                'Integer': 8, 'Int64': 8, 'QWord': 8,
                'Int32': 4, 'DWord': 4,
                'Int16': 2, 'Word': 2,
                'Int8': 1, 'Byte': 1,
                'UInt64': 8, 'UInt32': 4, 'UInt16': 2, 'UInt8': 1,
                'FloatingPoint': 8,
                'Text': 8,  # Pointer to string
                'Boolean': 1,
                'Address': 8,  # 64-bit pointer
                'Pointer': 8   # 64-bit pointer
            }
            
            size = 8  # Default size
            
            if hasattr(node.target, 'name'):
                type_name = node.target.name
                size = type_sizes.get(type_name, 8)
                print(f"DEBUG: Size of type {type_name} is {size} bytes")
            elif hasattr(node.target, 'type_name'):
                type_name = node.target.type_name
                size = type_sizes.get(type_name, 8)
                print(f"DEBUG: Size of type {type_name} is {size} bytes")
            else:
                # For variables, assume 8 bytes (qword)
                size = 8
                print(f"DEBUG: Default size assumption: {size} bytes")
            
            # Load size into RAX
            self.asm.emit_mov_rax_imm64(size)
            
            print("DEBUG: SizeOf AST compilation completed")
            return True
            
        except Exception as e:
            print(f"ERROR: SizeOf AST compilation failed: {str(e)}")
            raise
    
    def compile_inline_assembly(self, node):
        """Compile inline assembly blocks"""
        try:
            if isinstance(node, InlineAssembly):
                print(f"DEBUG: Compiling inline assembly: {node.assembly_code}")
                
                # Emit the assembly code
                self.asm.emit_inline_assembly(node.assembly_code)
                
                print("DEBUG: Inline assembly compilation completed")
                return True
            return False
            
        except Exception as e:
            print(f"ERROR: Inline assembly compilation failed: {str(e)}")
            raise
    
    def compile_system_call(self, node):
        """Compile system call statements"""
        try:
            if isinstance(node, SystemCall):
                print(f"DEBUG: Compiling system call")
                
                # Compile call number into RAX
                self.compiler.compile_expression(node.call_number)
                
                # Handle arguments (limited to first few for simplicity)
                if len(node.arguments) > 0:
                    # First argument goes to RDI
                    self.asm.emit_push_rax()  # Save syscall number
                    self.compiler.compile_expression(node.arguments[0])
                    self.asm.emit_mov_rdi_rax()
                    self.asm.emit_pop_rax()   # Restore syscall number
                
                if len(node.arguments) > 1:
                    # Second argument goes to RSI
                    self.asm.emit_push_rax()
                    self.asm.emit_push_rdi()
                    self.compiler.compile_expression(node.arguments[1])
                    self.asm.emit_mov_rsi_rax()
                    self.asm.emit_pop_rdi()
                    self.asm.emit_pop_rax()
                
                if len(node.arguments) > 2:
                    # Third argument goes to RDX
                    self.asm.emit_push_rax()
                    self.asm.emit_push_rdi()
                    self.asm.emit_push_rsi()
                    self.compiler.compile_expression(node.arguments[2])
                    self.asm.emit_mov_rdx_rax()
                    self.asm.emit_pop_rsi()
                    self.asm.emit_pop_rdi()
                    self.asm.emit_pop_rax()
                
                # Make the system call
                self.asm.emit_syscall()
                
                print("DEBUG: System call compilation completed")
                return True
            return False
            
        except Exception as e:
            print(f"ERROR: System call compilation failed: {str(e)}")
            raise