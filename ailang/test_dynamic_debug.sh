#!/bin/bash
# Temporarily add debug to see all compile_pool calls

cp ailang_compiler/modules/memory_manager.py ailang_compiler/modules/memory_manager.py.backup

# Add debug at start of compile_pool
sed -i '506a\        import traceback; print(f"\\n=== COMPILE_POOL CALLED: {node.pool_type}.{node.name}, pre_pass={pre_pass_only} ==="); traceback.print_stack(limit=5)' ailang_compiler/modules/memory_manager.py

# Compile test
python3 main.py UnitTestCode/test_memory_complete.ailang 2>&1 | grep -A8 "COMPILE_POOL CALLED.*Dynamic"

# Restore
mv ailang_compiler/modules/memory_manager.py.backup ailang_compiler/modules/memory_manager.py
