# ailang_compiler/jump_manager.py
from dataclasses import dataclass
from typing import Dict, List, Optional
import struct

@dataclass
class Jump:
    position: int          # Where the jump instruction is
    target_label: str      # Where it's jumping to
    instruction_type: str  # JE, JMP, JNE, etc.
    size: int             # 2 (short), 5 (near JMP), or 6 (near conditional)
    context: str          # 'local' or 'global'

@dataclass  
class Label:
    name: str
    position: int
    context: str  # 'local' or 'global'

class JumpManager:
    """Manages all jump resolution with proper scoping"""
    
    def __init__(self):
        self.global_jumps: List[Jump] = []
        self.global_labels: Dict[str, Label] = {}
        self.local_context_stack = []
        self.label_counter = 0
        
    def enter_local_context(self, name: str):
        """Enter a function or local scope"""
        self.local_context_stack.append({
            'name': name,
            'jumps': [],
            'labels': {},
            'start_position': None
        })
    
    def exit_local_context(self, code_buffer: bytearray) -> None:
        """Exit local scope and resolve all local jumps immediately"""
        if not self.local_context_stack:
            return
            
        context = self.local_context_stack.pop()
        
        # Resolve all local jumps NOW, not later
        for jump in context['jumps']:
            if jump.target_label not in context['labels']:
                raise ValueError(f"Undefined local label: {jump.target_label}")
                
            target = context['labels'][jump.target_label]
            self._resolve_single_jump(jump, target, code_buffer)
    
    def add_jump(self, position: int, target_label: str, 
                 instruction_type: str, is_local: bool = False):
        """Add a jump to be resolved"""
        
        # Determine jump size based on instruction type
        if instruction_type == "JMP":
            size = 5  # E9 + 4-byte offset
        else:  # JE, JNE, JL, JGE, etc.
            size = 6  # 0F 8x + 4-byte offset
            
        jump = Jump(position, target_label, instruction_type, size, 
                   'local' if is_local else 'global')
        
        if is_local and self.local_context_stack:
            self.local_context_stack[-1]['jumps'].append(jump)
        else:
            self.global_jumps.append(jump)
    
    def add_label(self, name: str, position: int, is_local: bool = False):
        """Mark a label position"""
        label = Label(name, position, 'local' if is_local else 'global')
        
        if is_local and self.local_context_stack:
            self.local_context_stack[-1]['labels'][name] = label
        else:
            self.global_labels[name] = label
    
    def _resolve_single_jump(self, jump: Jump, label: Label, 
                            code_buffer: bytearray) -> None:
        """Resolve a single jump by patching the code buffer"""
        
        # Calculate offset from end of jump instruction
        jump_end = jump.position + jump.size
        offset = label.position - jump_end
        print(f"DEBUG: Jump at {jump.position} to label at {label.position}: offset={offset}")
        
        # Validate offset fits in 32 bits
        if not (-2147483648 <= offset <= 2147483647):
            raise ValueError(f"Jump offset {offset} exceeds 32-bit range")
        
        # Pack as 32-bit signed integer
        offset_bytes = struct.pack('<i', offset)
        
        # Patch the code - the offset starts after the opcode(s)
        if jump.instruction_type == "JMP":
            offset_position = jump.position + 1  # After E9
        else:
            offset_position = jump.position + 2  # After 0F 8x
            
        # CRITICAL: Ensure buffer is large enough
        if offset_position + 4 > len(code_buffer):
            # Extend buffer if needed
            code_buffer.extend([0x90] * (offset_position + 4 - len(code_buffer)))
            
        # Now safely write the offset
        code_buffer[offset_position:offset_position+4] = offset_bytes
    
    def resolve_global_jumps(self, code_buffer: bytearray) -> None:
        """Resolve all remaining global jumps"""
        for jump in self.global_jumps:
            if jump.target_label not in self.global_labels:
                raise ValueError(f"Undefined global label: {jump.target_label}")
                
            label = self.global_labels[jump.target_label]
            self._resolve_single_jump(jump, label, code_buffer)
        
        # Clear after resolution
        self.global_jumps.clear()
    
    def create_unique_label(self) -> str:
        """Generate a unique label name"""
        self.label_counter += 1
        return f"L_{self.label_counter}"
