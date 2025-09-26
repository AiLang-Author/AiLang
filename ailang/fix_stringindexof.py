#!/usr/bin/env python3
"""
Correct fix for StringIndexOf - fixing both naming and register issues
"""

from pathlib import Path

def fix_stringindexof():
    string_ops_file = Path("/mnt/c/Users/Sean/Documents/Ailang/ailang/ailang_compiler/modules/string_ops.py")
    
    if not string_ops_file.exists():
        print(f"ERROR: {string_ops_file} not found!")
        return False
        
    content = string_ops_file.read_text()
    
    # Backup
    backup = string_ops_file.with_suffix('.py.correct_backup')
    backup.write_text(content)
    print(f"Backed up to {backup}")
    
    # First, add the correct function name alias
    # Find compile_string_index_of and add an alias right after it
    
    # The correct implementation with proper register usage
    correct_function = '''    def compile_stringindexof(self, node):
        """Find the index of a substring within a string - CORRECTED VERSION"""
        if len(node.arguments) != 2:
            raise ValueError("StringIndexOf requires 2 arguments: haystack, needle")
            
        print("DEBUG: Compiling StringIndexOf (FIXED)")
        
        # Save registers
        self.asm.emit_push_rbx()
        self.asm.emit_push_rcx()
        self.asm.emit_push_rdx()
        self.asm.emit_push_rsi()
        self.asm.emit_push_rdi()
        self.asm.emit_push_r8()
        self.asm.emit_push_r9()
        
        # Get haystack (string to search in) first
        self.compiler.compile_expression(node.arguments[0])
        self.asm.emit_mov_rdi_rax()  # Haystack in RDI
        self.asm.emit_push_rdi()     # Save original for index calculation
        
        # Get needle (substring to find)
        self.compiler.compile_expression(node.arguments[1])
        self.asm.emit_bytes(0x49, 0x89, 0xC1)  # MOV R9, RAX (needle in R9)
        
        # Labels
        outer_loop = self.asm.create_label()
        inner_loop = self.asm.create_label()
        found = self.asm.create_label()
        not_found = self.asm.create_label()
        continue_outer = self.asm.create_label()
        done = self.asm.create_label()
        
        # Outer loop - check each position in haystack
        self.asm.mark_label(outer_loop)
        self.asm.emit_bytes(0x8A, 0x07)  # MOV AL, [RDI]
        self.asm.emit_bytes(0x84, 0xC0)  # TEST AL, AL
        self.asm.emit_jump_to_label(not_found, "JZ")  # End of haystack
        
        # Set up for inner loop
        self.asm.emit_bytes(0x49, 0x89, 0xF8)  # MOV R8, RDI (current haystack pos)
        self.asm.emit_bytes(0x4C, 0x89, 0xCE)  # MOV RSI, R9 (needle pointer)
        
        # Inner loop - compare strings
        self.asm.mark_label(inner_loop)
        self.asm.emit_bytes(0x8A, 0x06)        # MOV AL, [RSI] (needle char)
        self.asm.emit_bytes(0x41, 0x8A, 0x18)  # MOV BL, [R8] (haystack char)
        
        self.asm.emit_bytes(0x84, 0xC0)  # TEST AL, AL
        self.asm.emit_jump_to_label(found, "JZ")  # End of needle = match!
        
        self.asm.emit_bytes(0x38, 0xD8)  # CMP AL, BL
        self.asm.emit_jump_to_label(continue_outer, "JNE")  # Mismatch
        
        # Match so far, continue
        self.asm.emit_bytes(0x49, 0xFF, 0xC0)  # INC R8
        self.asm.emit_bytes(0x48, 0xFF, 0xC6)  # INC RSI
        self.asm.emit_jump_to_label(inner_loop, "JMP")
        
        # No match at this position
        self.asm.mark_label(continue_outer)
        self.asm.emit_bytes(0x48, 0xFF, 0xC7)  # INC RDI
        self.asm.emit_jump_to_label(outer_loop, "JMP")
        
        # Found it!
        self.asm.mark_label(found)
        self.asm.emit_pop_rbx()  # Original haystack
        self.asm.emit_mov_rax_rdi()
        self.asm.emit_bytes(0x48, 0x29, 0xD8)  # SUB RAX, RBX
        self.asm.emit_jump_to_label(done, "JMP")
        
        # Not found
        self.asm.mark_label(not_found)
        self.asm.emit_pop_rbx()  # Clean stack
        self.asm.emit_mov_rax_imm64(-1)
        
        self.asm.mark_label(done)
        
        # Restore registers
        self.asm.emit_pop_r9()
        self.asm.emit_pop_r8()
        self.asm.emit_pop_rdi()
        self.asm.emit_pop_rsi()
        self.asm.emit_pop_rdx()
        self.asm.emit_pop_rcx()
        self.asm.emit_pop_rbx()
        
        print("DEBUG: StringIndexOf completed (FIXED)")
        return True
'''
    
    # Find where to insert - right after compile_string_index_of
    index_of_pos = content.find('def compile_string_index_of(self, node):')
    if index_of_pos == -1:
        print("ERROR: Could not find compile_string_index_of")
        return False
        
    # Find the end of that function
    next_def_pos = content.find('\n    def ', index_of_pos + 10)
    if next_def_pos == -1:
        next_def_pos = len(content)
        
    # Insert the correct function
    content = content[:next_def_pos] + '\n' + correct_function + '\n' + content[next_def_pos:]
    print("Added compile_stringindexof (without underscores)")
    
    # Write back
    string_ops_file.write_text(content)
    print(f"Fixed {string_ops_file}")
    return True

if __name__ == "__main__":
    print("Applying correct fix for StringIndexOf")
    print("=" * 50)
    
    if fix_stringindexof():
        print("\n✓ Fix applied!")
        print("\nTest with:")
        print("  python3 main.py test_stringindexof.ailang")
        print("  ./test_stringindexof_exec")