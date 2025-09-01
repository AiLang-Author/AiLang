#!/bin/bash
# Save as run_benchmarks.sh

echo "=== AiLang vs C Benchmark Suite ==="
echo ""

# Create nested loop test files
cat > nested_loop.ailang << 'EOF'
sum = 0
i = 0
WhileLoop LessThan(i, 100) {
    j = 0
    WhileLoop LessThan(j, 100) {
        sum = Add(sum, 1)
        j = Add(j, 1)
    }
    i = Add(i, 1)
}
PrintMessage(sum)
EOF

cat > nested_loop.c << 'EOF'
#include <stdio.h>
int main() {
    int sum = 0;
    for(int i = 0; i < 100; i++) {
        for(int j = 0; j < 100; j++) {
            sum++;
        }
    }
    printf("%d\n", sum);
    return 0;
}
EOF

# Create condition test files
cat > conditions.ailang << 'EOF'
counter = 0
value = 0
WhileLoop LessThan(counter, 10000) {
    If GreaterThan(counter, 5000) {
        value = Add(value, 2)
    } Else {
        value = Add(value, 1)
    }
    counter = Add(counter, 1)
}
PrintMessage(value)
EOF

cat > conditions.c << 'EOF'
#include <stdio.h>
int main() {
    int counter = 0;
    int value = 0;
    while(counter < 10000) {
        if(counter > 5000) {
            value += 2;
        } else {
            value += 1;
        }
        counter++;
    }
    printf("%d\n", value);
    return 0;
}
EOF

# Compile C versions
echo "Compiling C versions..."
gcc -O2 nested_loop.c -o nested_loop_c
gcc -O2 conditions.c -o conditions_c

# Compile AiLang versions
echo "Compiling AiLang versions..."
cd ailang_compiler

# Nested loop
cat > compile_nested.py << 'EOF'
import sys, os
sys.path.insert(0, '..')
sys.path.insert(0, '../ailang_parser')
from x64_assembler import X64Assembler
from elf_generator import ELFGenerator
sys.path.append('../ailang_parser')
from compiler import AILANGCompiler
import ailang_compiler
from ailang_compiler import AILANGToX64Compiler

with open('../nested_loop.ailang', 'r') as f:
    source = f.read()
parser = AILANGCompiler()
ast = parser.compile(source)
compiler = AILANGToX64Compiler()
executable = compiler.compile(ast)
with open('../nested_loop_ailang', 'wb') as f:
    f.write(executable)
os.chmod('../nested_loop_ailang', 0o755)
print("Compiled nested_loop")
EOF

python3 compile_nested.py

# Conditions
cat > compile_conditions.py << 'EOF'
import sys, os
sys.path.insert(0, '..')
sys.path.insert(0, '../ailang_parser')
from x64_assembler import X64Assembler
from elf_generator import ELFGenerator
sys.path.append('../ailang_parser')
from compiler import AILANGCompiler
import ailang_compiler
from ailang_compiler import AILANGToX64Compiler

with open('../conditions.ailang', 'r') as f:
    source = f.read()
parser = AILANGCompiler()
ast = parser.compile(source)
compiler = AILANGToX64Compiler()
executable = compiler.compile(ast)
with open('../conditions_ailang', 'wb') as f:
    f.write(executable)
os.chmod('../conditions_ailang', 0o755)
print("Compiled conditions")
EOF

python3 compile_conditions.py

cd ..

echo ""
echo "=== TEST 1: Nested Loops (10,000 iterations) ==="
echo "Verifying correctness:"
echo -n "C result: "
./nested_loop_c
echo -n "AiLang result: "
./nested_loop_ailang

echo ""
echo "Performance (100 runs):"
echo -n "C:      "
time -p bash -c 'for i in {1..100}; do ./nested_loop_c > /dev/null; done' 2>&1 | grep real
echo -n "AiLang: "
time -p bash -c 'for i in {1..100}; do ./nested_loop_ailang > /dev/null; done' 2>&1 | grep real

echo ""
echo "=== TEST 2: Conditional Logic (10,000 branches) ==="
echo "Verifying correctness:"
echo -n "C result: "
./conditions_c
echo -n "AiLang result: "
./conditions_ailang

echo ""
echo "Performance (100 runs):"
echo -n "C:      "
time -p bash -c 'for i in {1..100}; do ./conditions_c > /dev/null; done' 2>&1 | grep real
echo -n "AiLang: "
time -p bash -c 'for i in {1..100}; do ./conditions_ailang > /dev/null; done' 2>&1 | grep real

echo ""
echo "=== Benchmark Complete ==="