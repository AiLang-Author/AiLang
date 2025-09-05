import sys, os
sys.path.insert(0, '..')
sys.path.insert(0, '../ailang_parser')
from compiler import AILANGCompiler

source = open('../test_just_allocate.ailang').read()
print(f"Testing: {source}")

try:
    parser = AILANGCompiler()
    ast = parser.compile(source)
    print("✓ PoolAllocate parsed successfully")
except Exception as e:
    print(f"✗ PoolAllocate parse error: {e}")
