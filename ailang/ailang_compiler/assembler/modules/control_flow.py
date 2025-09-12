# ailang_compiler/assembler/modules/control_flow.py
"""Control flow operations - jumps, labels, calls"""

import struct

class ControlFlowOperations:
    """Jump, call, and label management"""
    
    def create_label(self):
        """Generate unique label name"""
        if not hasattr(self, '_label_counter'):
            self._label_counter = 0
        self._label_counter += 1
        return f"L{self._label_counter}"
    
    def mark_label(self, label_name, is_local=False):
        """Mark a label at current position"""
        position = len(self.code)
        self.jump_manager.add_label(label_name, position, is_local)
        
        # Initialize labels dict if needed
        if not hasattr(self, 'labels'):
            self.labels = {}
        if not hasattr(self, 'pending_jumps'):
            self.pending_jumps = []
        
        self.labels[label_name] = position
        print(f"DEBUG: Marked label {label_name} at position {position}")
    
    def emit_jump_to_label(self, label_name, jump_type, is_local=False):
        """Emit a conditional or unconditional jump to a label"""
        position = len(self.code)
        
        # Emit jump opcode based on type
        if jump_type == "JE" or jump_type == "JZ":
            self.emit_bytes(0x0F, 0x84, 0x00, 0x00, 0x00, 0x00)
        elif jump_type == "JNE" or jump_type == "JNZ":
            self.emit_bytes(0x0F, 0x85, 0x00, 0x00, 0x00, 0x00)
        elif jump_type == "JMP":
            self.emit_bytes(0xE9, 0x00, 0x00, 0x00, 0x00)
        elif jump_type == "JL":
            self.emit_bytes(0x0F, 0x8C, 0x00, 0x00, 0x00, 0x00)
        elif jump_type == "JGE":
            self.emit_bytes(0x0F, 0x8D, 0x00, 0x00, 0x00, 0x00)
        elif jump_type == "JG":
            self.emit_bytes(0x0F, 0x8F, 0x00, 0x00, 0x00, 0x00)
        elif jump_type == "JLE":
            self.emit_bytes(0x0F, 0x8E, 0x00, 0x00, 0x00, 0x00)
        elif jump_type == "JNS":
            self.emit_bytes(0x0F, 0x89, 0x00, 0x00, 0x00, 0x00)
        elif jump_type == "JB":
            self.emit_bytes(0x0F, 0x82, 0x00, 0x00, 0x00, 0x00)
        elif jump_type == "JA":
            self.emit_bytes(0x0F, 0x87, 0x00, 0x00, 0x00, 0x00)
        elif jump_type == "JAE":  # Jump if above or equal (unsigned >=)
            self.emit_bytes(0x0F, 0x83, 0x00, 0x00, 0x00, 0x00)
        elif jump_type == "JBE":  # Jump if below or equal (unsigned <=)
            self.emit_bytes(0x0F, 0x86, 0x00, 0x00, 0x00, 0x00)
        else:
            raise ValueError(f"Unknown jump type: {jump_type}")
        
        # Register with jump manager
        self.jump_manager.add_jump(position, label_name, jump_type, is_local)
        print(f"DEBUG: Emitted 32-bit {jump_type} to {label_name} at position {position}")
    
    def emit_call_to_label(self, label):
        """Emit CALL to a label"""
        current_pos = len(self.code)
        
        # Emit CALL opcode
        self.emit_bytes(0xE8)
        
        # Calculate and emit offset if label is already known
        if label in self.labels:
            target_pos = self.labels[label]
            offset = target_pos - (current_pos + 5)  # CALL uses next instruction address
            self.emit_bytes(*struct.pack('<i', offset))
        else:
            # Emit placeholder - will be fixed in resolve phase
            self.emit_bytes(0x00, 0x00, 0x00, 0x00)
            # Add to pending calls for resolution
            if not hasattr(self, 'pending_calls'):
                self.pending_calls = []
            self.pending_calls.append((current_pos, label))
        
        print(f"DEBUG: Emitted CALL to label {label}")
    
    def resolve_jumps(self):
        """Resolve all global jumps"""
        jump_count = len(self.jump_manager.global_jumps)
        if jump_count > 0:
            self.jump_manager.resolve_global_jumps(self.code)
            print(f"DEBUG: Successfully resolved {jump_count} global jumps")
    
    # === GUARDED OPERATIONS ===
    
    def emit_write_guarded_rdi_rsi_size_in_rdx(self):
        """Guarded write(fd=RDI, buf=RSI, size=RDX). Skip if buf==NULL or size==0."""
        skip = self.create_label()
        cont = self.create_label()
        
        self.emit_bytes(0x48, 0x85, 0xF6)  # TEST RSI,RSI
        self.emit_jump_to_label(skip, "JZ")
        
        self.emit_bytes(0x48, 0x85, 0xD2)  # TEST RDX,RDX
        self.emit_jump_to_label(skip, "JZ")
        
        self.emit_mov_rax_imm64(1)
        self.emit_syscall()
        self.emit_jump_to_label(cont, "JMP")
        
        self.mark_label(skip)
        self.emit_mov_rax_imm64(0)
        self.mark_label(cont)