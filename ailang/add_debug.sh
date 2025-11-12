#!/bin/bash

cp ailang_compiler/modules/expression_compiler.py ailang_compiler/modules/expression_compiler.py.backup

# Add debug before the load
sed -i '/pool_stack_offset = self.compiler.variables\[pool_name\]/a\                                    print(f"DEBUG READ: Loading DynamicPool {pool_name} from stack offset {pool_stack_offset}")' ailang_compiler/modules/expression_compiler.py

# Compile and run test
python3 main.py UnitTestCode/test_memory_complete.ailang > /tmp/compile.log 2>&1
./UnitTestCode/test_memory_complete_exec 2>&1 | head -80

# Restore
mv ailang_compiler/modules/expression_compiler.py.backup ailang_compiler/modules/expression_compiler.py
