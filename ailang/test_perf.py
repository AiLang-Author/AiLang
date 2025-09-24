#!/usr/bin/env python3
# redis_test_enhanced.py - Comprehensive Redis server test suite
import socket
import time
import threading
import statistics
import json
import random
from typing import List, Dict, Any

class RedisTestClient:
    """Enhanced Redis test client with RESP protocol support"""
    
    def __init__(self, host='localhost', port=6379):
        self.host = host
        self.port = port
        self.sock = None
        
    def connect(self):
        """Establish connection to Redis server"""
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.sock.connect((self.host, self.port))
        
    def disconnect(self):
        """Close connection"""
        if self.sock:
            self.sock.close()
            
    def send_command(self, *args) -> bytes:
        """Send RESP array command and return response"""
        # Build RESP array
        cmd = f'*{len(args)}\r\n'
        for arg in args:
            arg_str = str(arg)
            cmd += f'${len(arg_str)}\r\n{arg_str}\r\n'
        
        self.sock.send(cmd.encode())
        return self.sock.recv(4096)
    
    def parse_response(self, response: bytes) -> Any:
        """Parse RESP response"""
        if not response:
            return None
            
        resp_type = chr(response[0])
        data = response[1:].decode('utf-8', errors='ignore')
        
        if resp_type == '+':  # Simple string
            return data.split('\r\n')[0]
        elif resp_type == '-':  # Error
            return f"ERROR: {data.split('\r\n')[0]}"
        elif resp_type == ':':  # Integer
            return int(data.split('\r\n')[0])
        elif resp_type == '$':  # Bulk string
            length = int(data.split('\r\n')[0])
            if length == -1:
                return None
            return data.split('\r\n')[1]
        elif resp_type == '*':  # Array
            # Simple array parsing (not recursive)
            return data
        else:
            return response

class RedisBenchmark:
    """Comprehensive Redis benchmark suite"""
    
    def __init__(self, host='localhost', port=6379):
        self.host = host
        self.port = port
        self.results = {}
        
    def benchmark_ping(self, iterations=1000):
        """Benchmark PING command"""
        client = RedisTestClient(self.host, self.port)
        client.connect()
        
        # Warmup
        for _ in range(10):
            client.send_command('PING')
            
        times = []
        for _ in range(iterations):
            start = time.perf_counter()
            response = client.send_command('PING')
            end = time.perf_counter()
            
            if response != b'+PONG\r\n':
                print(f"Unexpected response: {response}")
                
            times.append((end - start) * 1000)  # Convert to ms
            
        client.disconnect()
        
        return {
            'command': 'PING',
            'iterations': iterations,
            'avg_ms': statistics.mean(times),
            'median_ms': statistics.median(times),
            'min_ms': min(times),
            'max_ms': max(times),
            'stdev_ms': statistics.stdev(times) if len(times) > 1 else 0,
            'p95_ms': statistics.quantiles(times, n=20)[18] if len(times) > 20 else max(times),
            'p99_ms': statistics.quantiles(times, n=100)[98] if len(times) > 100 else max(times),
        }
    
    def benchmark_set_get(self, iterations=1000):
        """Benchmark SET/GET commands"""
        client = RedisTestClient(self.host, self.port)
        client.connect()
        
        set_times = []
        get_times = []
        
        for i in range(iterations):
            key = f'bench:key:{i}'
            value = f'value_{i}'
            
            # SET
            start = time.perf_counter()
            client.send_command('SET', key, value)
            end = time.perf_counter()
            set_times.append((end - start) * 1000)
            
            # GET
            start = time.perf_counter()
            response = client.send_command('GET', key)
            end = time.perf_counter()
            get_times.append((end - start) * 1000)
            
        client.disconnect()
        
        return {
            'SET': {
                'iterations': iterations,
                'avg_ms': statistics.mean(set_times),
                'median_ms': statistics.median(set_times),
                'min_ms': min(set_times),
                'max_ms': max(set_times),
            },
            'GET': {
                'iterations': iterations,
                'avg_ms': statistics.mean(get_times),
                'median_ms': statistics.median(get_times),
                'min_ms': min(get_times),
                'max_ms': max(get_times),
            }
        }
    
    def benchmark_incr(self, iterations=1000):
        """Benchmark INCR command"""
        client = RedisTestClient(self.host, self.port)
        client.connect()
        
        # Initialize counter
        client.send_command('SET', 'counter', '0')
        
        times = []
        for _ in range(iterations):
            start = time.perf_counter()
            response = client.send_command('INCR', 'counter')
            end = time.perf_counter()
            times.append((end - start) * 1000)
            
        # Verify final count
        final = client.parse_response(client.send_command('GET', 'counter'))
        print(f"Final counter value: {final}")
        
        client.disconnect()
        
        return {
            'command': 'INCR',
            'iterations': iterations,
            'avg_ms': statistics.mean(times),
            'median_ms': statistics.median(times),
            'min_ms': min(times),
            'max_ms': max(times),
        }
    
    def benchmark_list_ops(self, iterations=500):
        """Benchmark LPUSH, RPUSH, and LRANGE commands"""
        client = RedisTestClient(self.host, self.port)
        client.connect()
        
        lpush_times = []
        rpush_times = []
        lrange_times = []
        
        list_key = 'bench:list'
        client.send_command('DEL', list_key) # Clean up before start
        
        for i in range(iterations):
            # LPUSH
            start = time.perf_counter()
            client.send_command('LPUSH', list_key, f'item_{i}')
            end = time.perf_counter()
            lpush_times.append((end - start) * 1000)
            
            # RPUSH
            start = time.perf_counter()
            client.send_command('RPUSH', list_key, f'item_{i}')
            end = time.perf_counter()
            rpush_times.append((end - start) * 1000)
            
        # LRANGE
        for i in range(iterations):
            start_index = i % 100
            end_index = start_index + 10
            start = time.perf_counter()
            client.send_command('LRANGE', list_key, start_index, end_index)
            end = time.perf_counter()
            lrange_times.append((end - start) * 1000)
            
        client.disconnect()
        
        return {
            'LPUSH': {
                'iterations': iterations,
                'avg_ms': statistics.mean(lpush_times),
                'median_ms': statistics.median(lpush_times),
            },
            'RPUSH': {
                'iterations': iterations,
                'avg_ms': statistics.mean(rpush_times),
                'median_ms': statistics.median(rpush_times),
            },
            'LRANGE': {
                'iterations': iterations,
                'avg_ms': statistics.mean(lrange_times),
                'median_ms': statistics.median(lrange_times),
            }
        }

    def benchmark_pipeline(self, commands_per_pipeline=10, pipelines=100):
        """Test pipelining (send multiple commands before reading responses)"""
        client = RedisTestClient(self.host, self.port)
        client.connect()
        
        times = []
        
        for p in range(pipelines):
            # Build pipeline commands
            commands = b''
            for i in range(commands_per_pipeline):
                cmd = f'*2\r\n$3\r\nSET\r\n$8\r\npipe:{p}:{i}\r\n$5\r\nvalue\r\n'
                commands += cmd.encode()
            
            start = time.perf_counter()
            client.sock.send(commands)
            
            # Read all responses
            for _ in range(commands_per_pipeline):
                client.sock.recv(1024)
            
            end = time.perf_counter()
            times.append((end - start) * 1000)
            
        client.disconnect()
        
        return {
            'pipeline': f'{commands_per_pipeline} commands',
            'pipelines': pipelines,
            'avg_ms': statistics.mean(times),
            'median_ms': statistics.median(times),
            'throughput_ops': (commands_per_pipeline * pipelines) / (sum(times) / 1000),
        }
    
    def concurrent_benchmark(self, threads=10, iterations_per_thread=100):
        """Multi-threaded concurrent benchmark"""
        results = []
        
        def worker():
            times = []
            client = RedisTestClient(self.host, self.port)
            client.connect()
            
            for _ in range(iterations_per_thread):
                start = time.perf_counter()
                client.send_command('PING')
                end = time.perf_counter()
                times.append((end - start) * 1000)
                
            client.disconnect()
            results.append(times)
        
        # Start threads
        thread_list = []
        start_time = time.time()
        
        for _ in range(threads):
            t = threading.Thread(target=worker)
            t.start()
            thread_list.append(t)
            
        # Wait for completion
        for t in thread_list:
            t.join()
            
        total_time = time.time() - start_time
        all_times = [t for sublist in results for t in sublist]
        
        return {
            'concurrent': True,
            'threads': threads,
            'total_operations': threads * iterations_per_thread,
            'total_time_s': total_time,
            'throughput_ops': (threads * iterations_per_thread) / total_time,
            'avg_ms': statistics.mean(all_times),
            'median_ms': statistics.median(all_times),
        }
    
    def run_full_benchmark(self):
        """Run complete benchmark suite"""
        print("=" * 60)
        print("AILang Redis Server Benchmark Suite")
        print("=" * 60)
        
        # PING benchmark
        print("\n1. PING Benchmark...")
        ping_results = self.benchmark_ping(1000)
        print(f"   Average: {ping_results['avg_ms']:.3f}ms")
        print(f"   Median:  {ping_results['median_ms']:.3f}ms")
        print(f"   P95:     {ping_results['p95_ms']:.3f}ms")
        print(f"   P99:     {ping_results['p99_ms']:.3f}ms")
        
        # SET/GET benchmark
        print("\n2. SET/GET Benchmark...")
        setget_results = self.benchmark_set_get(500)
        print(f"   SET Average: {setget_results['SET']['avg_ms']:.3f}ms")
        print(f"   GET Average: {setget_results['GET']['avg_ms']:.3f}ms")
        
        # INCR benchmark
        print("\n3. INCR Benchmark...")
        incr_results = self.benchmark_incr(1000)
        print(f"   Average: {incr_results['avg_ms']:.3f}ms")

        # List Ops benchmark
        print("\n4. List (LPUSH/RPUSH/LRANGE) Benchmark...")
        list_results = self.benchmark_list_ops(500)
        print(f"   LPUSH Average: {list_results['LPUSH']['avg_ms']:.3f}ms")
        print(f"   RPUSH Average: {list_results['RPUSH']['avg_ms']:.3f}ms")
        print(f"   LRANGE Average: {list_results['LRANGE']['avg_ms']:.3f}ms")
        
        # Pipeline benchmark
        print("\n5. Pipeline Benchmark...")
        pipeline_results = self.benchmark_pipeline(10, 100)
        print(f"   Average: {pipeline_results['avg_ms']:.3f}ms per pipeline")
        print(f"   Throughput: {pipeline_results['throughput_ops']:.0f} ops/sec")
        
        # Concurrent benchmark
        print("\n6. Concurrent Benchmark...")
        concurrent_results = self.concurrent_benchmark(10, 100)
        print(f"   Threads: {concurrent_results['threads']}")
        print(f"   Throughput: {concurrent_results['throughput_ops']:.0f} ops/sec")
        print(f"   Average: {concurrent_results['avg_ms']:.3f}ms")
        
        print("\n" + "=" * 60)
        print("Benchmark Complete!")
        
        return {
            'ping': ping_results,
            'set_get': setget_results,
            'incr': incr_results,
            'list_ops': list_results,
            'pipeline': pipeline_results,
            'concurrent': concurrent_results,
        }

    def run_randomized_stress_test(self, duration_seconds=15):
        """
        Runs a randomized sequence of commands to stress the server and
        find unexpected behavior (fuzz testing).
        """
        print(f"\n--- Starting Randomized Stress Test for {duration_seconds} seconds ---")
        
        # Explicitly seed the random number generator to ensure different runs are different.
        # This uses the system time to initialize the generator.
        random.seed()
        
        client = RedisTestClient(self.host, self.port)
        client.connect()
        
        # Pool of commands to choose from, weighted to have more writes
        commands = [
            'SET', 'SET', 'GET', 'INCR', 'LPUSH', 'LPUSH', 'RPUSH', 'LRANGE', 'DEL', 'EXISTS', 'STRLEN'
        ]
        
        # Keep track of keys we've created to make GET/DEL more effective
        keys_in_use = set()
        list_keys_in_use = set()
        
        operations = 0
        errors = 0
        start_time = time.time()
        
        while time.time() - start_time < duration_seconds:
            command = random.choice(commands)
            
            try:
                if command == 'SET':
                    key = f"rand:{random.randint(0, 100)}"
                    value = f"val:{random.random()}"
                    client.send_command(command, key, value)
                    keys_in_use.add(key)
                
                elif command in ('GET', 'DEL', 'EXISTS', 'STRLEN', 'INCR', 'DECR'):
                    if not keys_in_use: continue
                    key = random.choice(list(keys_in_use))
                    client.send_command(command, key)
                    if command == 'DEL' and key in keys_in_use:
                        keys_in_use.remove(key)

                elif command in ('LPUSH', 'RPUSH'):
                    key = f"randlist:{random.randint(0, 20)}"
                    value = f"item:{random.random()}"
                    client.send_command(command, key, value)
                    list_keys_in_use.add(key)

                elif command == 'LRANGE':
                    if not list_keys_in_use: continue
                    key = random.choice(list(list_keys_in_use))
                    client.send_command(command, key, 0, -1) # Get full list

                operations += 1
            except Exception as e:
                print(f"ERROR during stress test on command {command}: {e}")
                errors += 1
        
        client.disconnect()
        print("--- Randomized Stress Test Complete ---")
        print(f"Total operations: {operations}")
        print(f"Operations per second: {operations / duration_seconds:.2f}")
        if errors > 0:
            print(f"Total errors: {errors}")

def main():
    """Main entry point"""
    import sys
    
    # Quick test or full benchmark
    if len(sys.argv) > 1 and sys.argv[1] == 'full':
        bench = RedisBenchmark()
        results = bench.run_full_benchmark()
        
        # Save results
        with open('redis_benchmark_results.json', 'w') as f:
            json.dump(results, f, indent=2)
            print(f"\nResults saved to redis_benchmark_results.json")
    elif len(sys.argv) > 1 and sys.argv[1] == 'random':
        bench = RedisBenchmark()
        bench.run_randomized_stress_test(duration_seconds=15)

    else:
        # Quick test
        print("Quick Redis Test (use 'full' argument for complete benchmark)")
        print("-" * 40)
        
        client = RedisTestClient()
        client.connect()
        
        # Add FLUSHDB to ensure a clean state for each test run
        flush_resp = client.parse_response(client.send_command('FLUSHDB'))
        print(f"FLUSHDB  -> {flush_resp}")
        
        # Test basic commands
        tests = [
            ('PING', []),
            ('SET', ['test', 'value']),
            ('GET', ['test']),
            ('EXISTS', ['test']),
            ('INCR', ['counter']),
            ('DECR', ['counter']),
            ('LPUSH', ['mylist', 'world', 'hello']),
            ('RPUSH', ['mylist', '!']),
            ('LRANGE', ['mylist', 0, -1]),
        ]
        
        for cmd, args in tests:
            response = client.send_command(cmd, *args)
            parsed = client.parse_response(response)
            print(f"{cmd:8} -> {parsed}")
            
        # Quick performance test
        print("\nQuick Performance Test (100 PINGs):")
        start = time.time()
        for _ in range(100):
            client.send_command('PING')
        elapsed = time.time() - start
        
        print(f"100 PINGs: {elapsed*1000:.2f}ms total")
        print(f"Average: {elapsed*10:.2f}ms per PING")
        
        client.disconnect()

if __name__ == '__main__':
    main()