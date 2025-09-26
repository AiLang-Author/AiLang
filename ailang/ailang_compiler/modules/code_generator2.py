# ailang_compiler/code_generator2.py
from .symbol_table import SymbolTable, SymbolType
from ailang_parser.ailang_ast import *
from ..ast_visitor import ASTVisitor

class CodeGenerator(ASTVisitor):
    def __init__(self, compiler_context, symbol_table: SymbolTable):
        self.compiler = compiler_context
        self.asm = compiler_context.asm
        self.symbols = symbol_table
        
    def generate(self, ast: Program):
        """Generate code - ALL symbols already known"""
        print("Phase 2: Code Generation Starting")
        
        # Emit program prologue
        stack_size = self.symbols.get_stack_size()
        self.emit_prologue(stack_size)
        
        # Allocate the global pool table for FixedPool variables
        # This is the new, centralized home for this logic.
        print("DEBUG: Allocating global pool table...")
        pool_size = 4096 # 4KB for up to 512 pool variables
        self.asm.emit_mov_rax_imm64(9)  # sys_mmap
        self.asm.emit_mov_rdi_imm64(0)  # addr = NULL
        self.asm.emit_mov_rsi_imm64(pool_size)  # size
        self.asm.emit_mov_rdx_imm64(3)  # PROT_READ | PROT_WRITE
        self.asm.emit_mov_r10_imm64(0x22)  # MAP_PRIVATE | MAP_ANONYMOUS
        self.asm.emit_mov_r8_imm64(-1)  # fd = -1
        self.asm.emit_mov_r9_imm64(0)  # offset = 0
        self.asm.emit_syscall()
    
        # Store the pool table's base address in R15 for global access
        self.asm.emit_bytes(0x49, 0x89, 0xC7)  # MOV R15, RAX
        print(f"DEBUG: Pool table allocated, base address stored in R15")

        # Visit AST and generate code
        self.visit(ast)
        
        # Emit epilogue
        self.emit_epilogue()
        
        print("Code Generation Complete")
        
    def handle_Program(self, node: Program):
        """Emit program code"""
        for decl in node.declarations or []:
            self.visit(decl)
            
    def handle_generic(self, node):
        """Default emitter for nodes that don't generate top-level code (like Pool)."""
        super().handle_generic(node) # Continue traversal for children

    def handle_SubRoutine(self, node):
        """Emit code for a SubRoutine."""
        print(f"DEBUG: CodeGen: Compiling SubRoutine.{node.name}")
        
        # The SemanticAnalyzer has already registered the symbol and its label.
        symbol = self.symbols.lookup(node.name)
        if not symbol:
            raise RuntimeError(f"Symbol for SubRoutine {node.name} not found.")

        # Jump over the subroutine body in the main code flow.
        skip_label = self.asm.create_label()
        self.asm.emit_jump_to_label(skip_label, "JMP")

        # Mark the subroutine's entry point.
        self.asm.mark_label(symbol.metadata['label'])

        # --- Subroutine Prologue ---
        self.asm.emit_push_rbp()
        self.asm.emit_mov_rbp_rsp()
        # Note: Subroutines in this design don't have locals/params, so no stack allocation.

        # Use the standard visitor to compile the body.
        # This replaces the old manual 'for stmt in node.body:' loop.
        self.visit(node.body)

        # --- Subroutine Epilogue ---
        self.asm.emit_mov_rsp_rbp()
        self.asm.emit_pop_rbp()
        self.asm.emit_ret()

        # Mark the label to skip to for the main code flow.
        self.asm.mark_label(skip_label)
        return True


    def handle_Assignment(self, node):
        """Emit assignment - symbol MUST exist"""
        symbol = self.symbols.lookup(node.target)
        if not symbol:
            raise RuntimeError(f"Symbol {node.target} not found - semantic analysis failed")
            
        # Generate value
        self.visit(node.value)

        # Handle DynamicPool member assignment
        if '.' in symbol.name:
            parts = symbol.name.split('.')
            if len(parts) == 3 and parts[0] == 'DynamicPool':
                pool_name = f"{parts[0]}.{parts[1]}"
                member_key = parts[2]
                pool_symbol = self.symbols.lookup(pool_name)
                if pool_symbol and pool_symbol.metadata and pool_symbol.metadata.get('pool_type') == 'Dynamic':
                    print(f"DEBUG: Storing to dynamic pool var {symbol.name}")
                    self.asm.emit_push_rax() # Save value
                    member_offset = pool_symbol.metadata['members'][member_key]
                    self.emit_dynamic_pool_store(pool_symbol.offset, member_offset)
                    return
        
        # Check if it's a pool or stack variable
        if symbol.offset & self.symbols.POOL_MARKER:
            pool_index = symbol.offset & ~self.symbols.POOL_MARKER
            print(f"DEBUG: Storing to pool var {symbol.name} at pool index {pool_index}")
            self.asm.emit_bytes(0x49, 0x89, 0x87)  # MOV [R15 + disp32], RAX
            self.asm.emit_bytes(*struct.pack('<i', pool_index * 8))
        else:
            # Store to known stack offset
            print(f"DEBUG: Storing to stack var {symbol.name} at [RBP-{symbol.offset}]")
            self.asm.emit_bytes(0x48, 0x89, 0x85)  # MOV [RBP-offset], RAX
            self.asm.emit_bytes(*struct.pack('<i', -symbol.offset))
        
    def handle_Identifier(self, node):
        """Load identifier - symbol MUST exist"""
        symbol = self.symbols.lookup(node.name)
        if not symbol:
            raise RuntimeError(f"Symbol {node.name} not found")

        # Handle DynamicPool member access
        if '.' in symbol.name:
            parts = symbol.name.split('.')
            if len(parts) == 3 and parts[0] == 'DynamicPool':
                pool_name = f"{parts[0]}.{parts[1]}"
                member_key = parts[2]
                pool_symbol = self.symbols.lookup(pool_name)
                if pool_symbol and pool_symbol.metadata and pool_symbol.metadata.get('pool_type') == 'Dynamic':
                    print(f"DEBUG: Loading from dynamic pool var {symbol.name}")
                    member_offset = pool_symbol.metadata['members'][member_key]
                    self.emit_dynamic_pool_load(pool_symbol.offset, member_offset)
                    return


        if symbol.offset & self.symbols.POOL_MARKER:
            pool_index = symbol.offset & ~self.symbols.POOL_MARKER
            print(f"DEBUG: Loading from pool var {symbol.name} at pool index {pool_index}")
            self.asm.emit_bytes(0x49, 0x8B, 0x87)  # MOV RAX, [R15 + disp32]
            self.asm.emit_bytes(*struct.pack('<i', pool_index * 8))
        else:
            # Load from known stack offset
            print(f"DEBUG: Loading from stack var {symbol.name} at [RBP-{symbol.offset}]")
            self.asm.emit_bytes(0x48, 0x8B, 0x85)  # MOV RAX, [RBP-offset]
            self.asm.emit_bytes(*struct.pack('<i', -symbol.offset))
        
    def emit_prologue(self, stack_size):
        """Emit function prologue with known stack size"""
        self.asm.emit_push_rbp()
        self.asm.emit_bytes(0x48, 0x89, 0xE5)  # MOV RBP, RSP
        if stack_size > 0:
            aligned = (stack_size + 15) & ~15
            self.asm.emit_bytes(0x48, 0x81, 0xEC)  # SUB RSP, size
            self.asm.emit_bytes(*struct.pack('<I', aligned))
            
    def emit_epilogue(self):
        """Emit function epilogue"""
        self.asm.emit_bytes(0x48, 0x89, 0xEC)  # MOV RSP, RBP
        self.asm.emit_pop_rbp()
        self.asm.emit_mov_rax_imm64(60)  # exit
        self.asm.emit_xor_edi_edi()
        self.asm.emit_syscall()

    def emit_dynamic_pool_store(self, pool_stack_offset, member_offset):
        """Helper to emit code for storing a value in a dynamic pool member."""
        # Assumes value to store is on the stack
        # Load pool base pointer from stack into RCX
        self.asm.emit_bytes(0x48, 0x8B, 0x8D, *struct.pack('<i', -pool_stack_offset)) # MOV RCX, [RBP - offset]
        # Add member offset to base pointer
        self.asm.emit_bytes(0x48, 0x81, 0xC1, *struct.pack('<i', member_offset)) # ADD RCX, member_offset
        # Pop value from stack into RAX and store it
        self.asm.emit_pop_rax()
        self.asm.emit_bytes(0x48, 0x89, 0x01) # MOV [RCX], RAX

    def emit_dynamic_pool_load(self, pool_stack_offset, member_offset):
        """Helper to emit code for loading a value from a dynamic pool member."""
        # Load pool base pointer from stack into RAX
        self.asm.emit_bytes(0x48, 0x8B, 0x85, *struct.pack('<i', -pool_stack_offset)) # MOV RAX, [RBP - offset]
        # Add member offset to base pointer
        self.asm.emit_bytes(0x48, 0x81, 0xC0, *struct.pack('<i', member_offset)) # ADD RAX, member_offset
        # Dereference the final address
        self.asm.emit_bytes(0x48, 0x8B, 0x00) # MOV RAX, [RAX]