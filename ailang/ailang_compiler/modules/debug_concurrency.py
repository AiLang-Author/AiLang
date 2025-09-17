# ailang_compiler/modules/debug_concurrency.py
"""
Concurrency debugging: deadlock and race detection
"""

class ConcurrencyDebugOps:
    """Debug operations for concurrent code"""
    
    def __init__(self, compiler):
        self.compiler = compiler
        self.asm = compiler.asm
        self.debug_ops = compiler.debug_ops
        
        # Tracking structures
        self.lock_graph = {}  # For deadlock detection
        self.memory_access_log = {}  # For race detection
        
    def emit_deadlock_detector(self):
        """Emit runtime deadlock detection code"""
        self.asm.define_label('__debug_deadlock_check')
        
        # Maintain a wait-for graph
        # When actor A waits for actor B, add edge A->B
        # Check for cycles periodically
        
        # RDI = waiting actor ID
        # RSI = waited-for actor ID
        
        # Add edge to wait-for graph
        self._emit_add_wait_edge()
        
        # Check for cycle (deadlock)
        self._emit_cycle_detection()
        
        # If cycle found, report deadlock
        self.asm.emit_test_rax_rax()
        no_deadlock = self.asm.create_label()
        self.asm.emit_jump_to_label(no_deadlock, "JZ")
        
        # Deadlock detected!
        self._emit_deadlock_report()
        
        self.asm.mark_label(no_deadlock)
        self.asm.emit_ret()
        
    def _emit_add_wait_edge(self):
        """Add edge to wait-for graph"""
        # Store in graph structure
        # Format: [actor_id] -> [list of actors it waits for]
        
        # Calculate graph entry address
        self.asm.emit_mov_rax_rdi()  # Waiting actor
        self.asm.emit_bytes(0x48, 0xC1, 0xE0, 0x04)  # SHL RAX, 4 (16 bytes per entry)
        
        # Add to graph base
        graph_base = self.asm.add_data('__wait_graph', 4096)  # Static allocation
        self.asm.emit_load_data_address('rbx', graph_base)
        self.asm.emit_add_rax_rbx()
        
        # Store waited-for actor
        self.asm.emit_bytes(0x48, 0x89, 0x30)  # MOV [RAX], RSI
        
    def _emit_cycle_detection(self):
        """Emit DFS cycle detection"""
        # Use depth-first search to find cycles
        # This is simplified - real implementation would be more complex
        
        # Allocate visited array
        self.asm.emit_sub_rsp_imm32(256)  # Space for 256 actors
        self.asm.emit_mov_rdi_rsp()  # Visited array
        
        # Clear visited array
        self.asm.emit_xor_rax_rax()
        self.asm.emit_mov_rcx_imm64(32)  # 256/8 = 32 qwords
        self.asm.emit_bytes(0xF3, 0x48, 0xAB)  # REP STOSQ
        
        # Run DFS from each node
        self.asm.emit_xor_rbx_rbx()  # Start node = 0
        
        dfs_loop = self.asm.create_label()
        dfs_done = self.asm.create_label()
        
        self.asm.mark_label(dfs_loop)
        
        # Check if node < max_actors
        self.asm.emit_cmp_rbx_imm32(self.compiler.scheduler.max_actors)
        self.asm.emit_jump_to_label(dfs_done, "JAE")
        
        # DFS from this node
        self.asm.emit_mov_rdi_rbx()  # Current node
        self.asm.emit_call('__debug_dfs_check')
        
        # If cycle found (RAX != 0), exit
        self.asm.emit_test_rax_rax()
        self.asm.emit_jump_to_label(dfs_done, "JNZ")
        
        # Next node
        self.asm.emit_inc_rbx()
        self.asm.emit_jump_to_label(dfs_loop)
        
        self.asm.mark_label(dfs_done)
        
        # Clean up stack
        self.asm.emit_add_rsp_imm32(256)
        
    def _emit_deadlock_report(self):
        """Report detected deadlock"""
        msg = "\n*** DEADLOCK DETECTED ***\n"
        msg_offset = self.asm.add_string(msg)
        
        self.asm.emit_mov_rax_imm64(1)  # sys_write
        self.asm.emit_mov_rdi_imm64(2)  # stderr
        self.asm.emit_load_data_address('rsi', msg_offset)
        self.asm.emit_mov_rdx_imm64(len(msg))
        self.asm.emit_syscall()
        
        # Print actors involved
        self._emit_print_deadlock_actors()
        
        # Optional: break into debugger
        if self.compiler.debug_ops.debug_level >= 4:
            self.asm.emit_byte(0xCC)  # INT3
            
    def emit_race_detector(self):
        """Emit runtime race condition detection"""
        self.asm.define_label('__debug_race_check')
        
        # Called before memory access
        # RDI = memory address
        # RSI = access type (0=read, 1=write)
        # RDX = actor/thread ID
        
        # Hash address to index
        self.asm.emit_mov_rax_rdi()
        self.asm.emit_bytes(0x48, 0x25, 0xFF, 0x0F, 0x00, 0x00)  # AND RAX, 0xFFF
        
        # Get access log entry
        log_base = self.asm.add_data('__access_log', 32768)  # 4K entries * 8 bytes
        self.asm.emit_load_data_address('rbx', log_base)
        self.asm.emit_bytes(0x48, 0xC1, 0xE0, 0x03)  # SHL RAX, 3
        self.asm.emit_add_rax_rbx()
        
        # Load previous access info
        self.asm.emit_bytes(0x48, 0x8B, 0x08)  # MOV RCX, [RAX]
        
        # Extract previous actor ID and access type
        self.asm.emit_mov_r8_rcx()
        self.asm.emit_bytes(0x49, 0xC1, 0xE8, 0x20)  # SHR R8, 32 (actor ID)
        self.asm.emit_bytes(0x48, 0x81, 0xE1, 0xFF, 0xFF, 0xFF, 0xFF)  # AND RCX, 0xFFFFFFFF (access type)
        
        # Check for race condition:
        # 1. Different actor accessing same location
        # 2. At least one access is a write
        
        self.asm.emit_cmp_r8_rdx()  # Compare actor IDs
        same_actor = self.asm.create_label()
        self.asm.emit_jump_to_label(same_actor, "JE")
        
        # Different actors - check if either access is write
        self.asm.emit_or_rcx_rsi()  # OR previous and current access types
        self.asm.emit_test_rcx_rcx()
        no_race = self.asm.create_label()
        self.asm.emit_jump_to_label(no_race, "JZ")  # Both reads = no race
        
        # Race condition detected!
        self._emit_race_report()
        
        self.asm.mark_label(no_race)
        self.asm.mark_label(same_actor)
        
        # Update access log with current access
        self.asm.emit_bytes(0x48, 0xC1, 0xE2, 0x20)  # SHL RDX, 32 (actor ID to high bits)
        self.asm.emit_or_rdx_rsi()  # OR with access type
        self.asm.emit_bytes(0x48, 0x89, 0x10)  # MOV [RAX], RDX
        
        self.asm.emit_ret()
        
    def _emit_race_report(self):
        """Report detected race condition"""
        self.asm.emit_push_rdi()  # Save address
        self.asm.emit_push_r8()   # Save actor 1
        self.asm.emit_push_rdx()  # Save actor 2
        
        msg = "\n*** RACE CONDITION DETECTED ***\n"
        msg_offset = self.asm.add_string(msg)
        
        self.asm.emit_mov_rax_imm64(1)  # sys_write
        self.asm.emit_mov_rdi_imm64(2)  # stderr
        self.asm.emit_load_data_address('rsi', msg_offset)
        self.asm.emit_mov_rdx_imm64(len(msg))
        self.asm.emit_syscall()
        
        # Print details
        detail = "Address: "
        detail_offset = self.asm.add_string(detail)
        self.asm.emit_mov_rax_imm64(1)
        self.asm.emit_mov_rdi_imm64(2)
        self.asm.emit_load_data_address('rsi', detail_offset)
        self.asm.emit_mov_rdx_imm64(len(detail))
        self.asm.emit_syscall()
        
        # Print address in hex
        self.asm.emit_pop_rdi()  # Address
        self.compiler.debug_ops.emit_print_hex()
        
        # Print actors involved
        actors = "\nActors: "
        actors_offset = self.asm.add_string(actors)
        self.asm.emit_mov_rax_imm64(1)
        self.asm.emit_mov_rdi_imm64(2)
        self.asm.emit_load_data_address('rsi', actors_offset)
        self.asm.emit_mov_rdx_imm64(len(actors))
        self.asm.emit_syscall()
        
        self.asm.emit_pop_rdi()  # Actor 1
        self.compiler.debug_ops.emit_print_hex()
        
        comma = ", "
        comma_offset = self.asm.add_string(comma)
        self.asm.emit_mov_rax_imm64(1)
        self.asm.emit_mov_rdi_imm64(2)
        self.asm.emit_load_data_address('rsi', comma_offset)
        self.asm.emit_mov_rdx_imm64(2)
        self.asm.emit_syscall()
        
        self.asm.emit_pop_rdi()  # Actor 2
        self.compiler.debug_ops.emit_print_hex()
        
        newline = "\n"
        newline_offset = self.asm.add_string(newline)
        self.asm.emit_mov_rax_imm64(1)
        self.asm.emit_mov_rdi_imm64(2)
        self.asm.emit_load_data_address('rsi', newline_offset)
        self.asm.emit_mov_rdx_imm64(1)
        self.asm.emit_syscall()
        
        # Restore registers
        self.asm.emit_pop_rdx()
        self.asm.emit_pop_r8()
        self.asm.emit_pop_rdi()