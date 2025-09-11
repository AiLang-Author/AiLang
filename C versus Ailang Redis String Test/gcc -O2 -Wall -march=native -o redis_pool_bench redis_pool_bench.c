gcc -O2 -Wall -march=native -o redis_pool_bench redis_pool_bench.c
=========================================
       AILANG Pool Allocator Results     
=========================================
./test_pool_real_exec
Real Pool Allocator Test
========================

Initializing 1MB string pool...
[PERF] pool_init: 18144 cycles
Pool initialized successfully

Test 1: Regular StringConcat (10x)       
[PERF] regular_concat: 175176 cycles     
Result:
ABBBBBBBBBB

Test 2: Pooled StringConcat (1000x)
[PERF] pooled_concat: 17424 cycles
Result:
A

Pool statistics:
Bytes used:
0
of 1048576 available

Test 3: 1000 concatenations
[PERF] pooled_1000x: 8676 cycles

Final pool usage:
0
bytes

Performance Analysis:
- Regular: 1010 mmap syscalls
- Pooled: 1 mmap syscall
- Syscalls eliminated: 1009
- Estimated cycles saved: ~30,000,000+

Test complete!

=========================================
    C/Redis-style Allocator Results
=========================================
./redis_pool_bench
=== Redis-style vs AILANG Pool Allocator Benchmark ===

1. Standard C strcat (malloc each time):
   Result: ABBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
   Cycles: 88272
   Allocations: 1001

2. Redis-style SDS (pre-allocation):
   Result: ABBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
   Cycles: 9720
   Allocations: 1-2 (with pre-allocation)
   Memory used: 501, free: 9

3. Pool Allocator (AILANG-style):
Pool overflow: offset 65340, total 362, size 65536
make: *** [benchmark.mak:30: benchmark] Segmentation fault (core dumped)
