#!/usr/bin/env python3
"""
Low-Level Operations Module for AILANG Compiler
Handles systems programming operations: pointers, hardware access, atomic operations
FIXED: compile_operation now handles both AST nodes and FunctionCall nodes
"""

import struct
from ailang.parser.ailang_ast import *

class LowLevelOps:
    """Handles low-level systems programming operations"""
    
    def __init__(self, compiler_context):
        self.compiler = compiler_context
        self.asm = compiler_context.asm
    
    def compile_operation(self, node):
        """Compile low-level operations - handles both AST nodes and FunctionCalls"""
        try:
            # Handle direct AST nodes (AddressOf, Dereference, SizeOf) - NEW FIX
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
            
            # Handle FunctionCall nodes (original functionality)
            if hasattr(node, 'function'):
                if node.function == 'Dereference':
                    return self.compile_dereference(node)
                elif node.function == 'AddressOf':
                    return self.compile_address_of(node)
                elif node.function == 'SizeOf':
                    return self.compile_sizeof(node)
                elif node.function == 'Allocate':
                    return self.compile_allocate(node)
                elif node.function == 'Deallocate':
                    return self.compile_deallocate(node)
                elif node.function == 'MemoryCopy':
                    return self.compile_memory_copy(node)
                elif node.function == 'MemorySet':
                    return self.compile_memory_set(node)
                elif node.function == 'MemoryCompare':
                    return self.compile_memory_compare(node)
                elif node.function in ['PortRead', 'PortWrite']:
                    return self.compile_port_operation(node)
                elif node.function in ['AtomicRead', 'AtomicWrite', 'AtomicAdd', 'AtomicCompareSwap']:
                    return self.compile_atomic_operation(node)
                elif node.function in ['MMIORead', 'MMIOWrite']:
                    return self.compile_mmio_operation(node)
                elif node.function == 'HardwareRegister':
                    return self.compile_hardware_register(node)
                else:
                    return False  # Not a low-level operation
            
            return False  # Not a recognized low-level operation
                
        except Exception as e:
            print(f"ERROR: Low-level operation compilation failed: {str(e)}")
            raise
    
    def compile_dereference(self, node):
        """Compile pointer dereference operation"""
        try:
            print(f"DEBUG: Compiling Dereference operation")
            
            if len(node.arguments) < 1:
                raise ValueError("Dereference requires at least 1 argument (pointer)")
            
            # Compile pointer expression to get address in RAX
            self.compiler.compile_expression(node.arguments[0])
            
            # Determine size hint (default to qword)
            size_hint = "qword"
            if len(node.arguments) > 1:
                if hasattr(node.arguments[1], 'value'):
                    size_hint = str(node.arguments[1].value).lower()
            
            # Perform dereference based on size
            if size_hint == "byte":
                self.asm.emit_dereference_byte()
                print(f"DEBUG: Dereferenced as byte")
            elif size_hint == "word":
                self.asm.emit_dereference_word()
                print(f"DEBUG: Dereferenced as word")
            elif size_hint == "dword":
                self.asm.emit_dereference_dword()
                print(f"DEBUG: Dereferenced as dword")
            else:  # qword or default
                self.asm.emit_dereference_qword()
                print(f"DEBUG: Dereferenced as qword")
            
            print("DEBUG: Dereference operation completed")
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
        """Compile memory allocation (simplified using stack allocation)"""
        try:
            print(f"DEBUG: Compiling Allocate operation")
            
            if len(node.arguments) < 1:
                raise ValueError("Allocate requires 1 argument (size)")
            
            # Compile size expression
            self.compiler.compile_expression(node.arguments[0])
            
            # For simplicity, use stack allocation
            # In a real implementation, this would call malloc or similar
            print("DEBUG: Using simplified stack allocation")
            
            # Save size in RBX
            self.asm.emit_mov_rbx_rax()
            
            # Allocate space on stack: SUB RSP, size
            # Align to 16-byte boundary for performance
            self.asm.emit_push_rax()  # Save original size
            
            # Align size to 16 bytes: size = (size + 15) & ~15
            self.asm.emit_bytes(0x48, 0x83, 0xC0, 0x0F)  # ADD RAX, 15
            self.asm.emit_bytes(0x48, 0x83, 0xE0, 0xF0)  # AND RAX, ~15
            
            # SUB RSP, RAX (allocate aligned space)
            self.asm.emit_bytes(0x48, 0x29, 0xC4)  # SUB RSP, RAX
            
            # Return stack pointer as allocated address
            self.asm.emit_mov_rax_rsp()
            
            print("DEBUG: Allocate operation completed (stack-based)")
            return True
            
        except Exception as e:
            print(f"ERROR: Allocate compilation failed: {str(e)}")
            raise
    
    def compile_deallocate(self, node):
        """Compile memory deallocation (simplified - just return success)"""
        try:
            print(f"DEBUG: Compiling Deallocate operation")
            
            # In this simplified implementation, we can't actually free stack allocations
            # In a real implementation, this would call free() or similar
            
            # For now, just return success (1)
            self.asm.emit_mov_rax_imm64(1)
            
            print("DEBUG: Deallocate operation completed (simplified)")
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
    
    def compile_port_operation(self, node):
        """Compile port I/O operations"""
        try:
            print(f"DEBUG: Compiling {node.function} operation")
            
            if node.function == 'PortRead':
                if len(node.arguments) < 2:
                    raise ValueError("PortRead requires 2 arguments (port, size)")
                
                # Port number
                self.compiler.compile_expression(node.arguments[0])
                port = int(node.arguments[0].value) if hasattr(node.arguments[0], 'value') else 0x80
                
                # Size
                size = "byte"  # default
                if len(node.arguments) > 1 and hasattr(node.arguments[1], 'value'):
                    size = str(node.arguments[1].value).lower()
                
                # Perform port read
                self.asm.emit_port_read(port, size)
                
            elif node.function == 'PortWrite':
                if len(node.arguments) < 3:
                    raise ValueError("PortWrite requires 3 arguments (port, value, size)")
                
                # Port number
                port = int(node.arguments[0].value) if hasattr(node.arguments[0], 'value') else 0x80
                
                # Value to write
                self.compiler.compile_expression(node.arguments[1])
                
                # Size
                size = "byte"  # default
                if len(node.arguments) > 2 and hasattr(node.arguments[2], 'value'):
                    size = str(node.arguments[2].value).lower()
                
                # Perform port write
                self.asm.emit_port_write(port, size)
            
            print(f"DEBUG: {node.function} operation completed")
            return True
            
        except Exception as e:
            print(f"ERROR: {node.function} compilation failed: {str(e)}")
            raise
    
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
            
            # Determine size hint
            size_hint = getattr(node, 'size_hint', 'qword')
            
            # Perform dereference based on size
            if size_hint == "byte":
                self.asm.emit_dereference_byte()
                print(f"DEBUG: Dereferenced as byte")
            elif size_hint == "word":
                self.asm.emit_dereference_word()
                print(f"DEBUG: Dereferenced as word")
            elif size_hint == "dword":
                self.asm.emit_dereference_dword()
                print(f"DEBUG: Dereferenced as dword")
            else:  # qword or default
                self.asm.emit_dereference_qword()
                print(f"DEBUG: Dereferenced as qword")
            
            print("DEBUG: Dereference AST compilation completed")
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
            print(f"ERROR: System call compilation failed: {str(e)}")            raise
