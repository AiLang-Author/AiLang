#!/bin/bash
# Benchmark Script: Fibonacci in C vs AiLang
# Usage: ./run_fibonacci_bench.sh

set -e

echo "=== Fibonacci(50) Benchmark ==="

# Recompile C version
echo "[*] Compiling C version..."
gcc -O2 fibonacci_c.c -o fibonacci_c

# Recompile AiLang version
echo "[*] Compiling AiLang version..."
pushd ailang_compiler > /dev/null
python3 compile_fib.py
popd > /dev/null

# Ensure AiLang binary is executable
chmod +x fibonacci_ailang

# Single run tests
echo
echo "Fibonacci(50) single run:"
echo "C version:"
time ./fibonacci_c

echo
echo "AiLang version:"
time ./fibonacci_ailang

# Stress test with 100 iterations
echo
echo "=== 100x Fibonacci(50) Benchmark ==="
echo "C version:"
time for i in {1..100}; do ./fibonacci_c > /dev/null; done

echo
echo "AiLang version:"
time for i in {1..100}; do ./fibonacci_ailang > /dev/null; done

echo
echo "=== Benchmark Complete ==="
