#!/usr/bin/env python3
"""
Comprehensive Redis Benchmark Suite for AILANG Redis Server
Combines functionality tests with performance benchmarks
Includes proper RESP parsing to handle all response types correctly
"""

import socket
import time
import sys
import argparse

class RedisClient:
    """Redis client with full RESP protocol support"""
    
    def __init__(self, host='localhost', port=6379):
        self.host = host
        self.port = port
        self.sock = None
        self.buffer = b""
        self.connect()
    
    def connect(self):
        """Establish connection to Redis server"""
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.sock.connect((self.host, self.port))
        self.sock.settimeout(1.0)
        self.buffer = b""  # Clear any leftover data
    
    def send_command(self, *args):
        """Send command using RESP protocol"""
        cmd = f"*{len(args)}\r\n"
        for arg in args:
            arg = str(arg)
            cmd += f"${len(arg)}\r\n{arg}\r\n"
        self.sock.sendall(cmd.encode())
    
    def _read_until(self, delimiter):
        """Read from socket until delimiter is found"""
        while delimiter not in self.buffer:
            try:
                chunk = self.sock.recv(4096)
                if not chunk:
                    break
                self.buffer += chunk
            except socket.timeout:
                break
        
        pos = self.buffer.find(delimiter)
        if pos == -1:
            return None
        
        result = self.buffer[:pos]
        self.buffer = self.buffer[pos + len(delimiter):]
        return result
    
    def read_response(self):
        """Read and parse a complete RESP response"""
        # Read the type indicator
        type_byte = self._read_until(b"\r\n")
        if not type_byte:
            return "ERROR: No response"
        
        type_char = chr(type_byte[0])
        content = type_byte[1:].decode()
        
        if type_char == '+':  # Simple string
            return content
        elif type_char == '-':  # Error
            return f"ERROR: {content}"
        elif type_char == ':':  # Integer
            return int(content)
        elif type_char == '$':  # Bulk string
            length = int(content)
            if length == -1:
                return None
            data = self._read_until(b"\r\n")
            return data.decode() if data else ""
        elif type_char == '*':  # Array
            count = int(content)
            if count == -1:
                return None
            elements = []
            for _ in range(count):
                elem = self.read_response()
                elements.append(elem)
            return elements
        else:
            return f"Unknown type: {type_char}"
    
    def close(self):
        """Close connection"""
        if self.sock:
            self.sock.close()

def print_header(text):
    """Print formatted header"""
    print("\n" + "=" * 60)
    print(f"  {text}")
    print("=" * 60)

def print_section(text):
    """Print section header"""
    print(f"\n{text}")
    print("-" * 40)

def test_command(client, description, *args, expected=None):
    """Test a single command and verify response"""
    client.send_command(*args)
    response = client.read_response()
    
    status = "✓" if expected is None or response == expected else "✗"
    
    if isinstance(response, list):
        print(f"  {status} {description}: Array[{len(response)}]")
        if len(response) <= 5:
            for item in response:
                print(f"      - {item}")
    else:
        print(f"  {status} {description}: {response}")
    
    if expected is not None and response != expected:
        print(f"      Expected: {expected}")
    
    return response

def benchmark_operation(client, operation, *args, iterations=1000, modify_key=False):
    """Benchmark a Redis operation"""
    times = []
    
    for i in range(iterations):
        # Prepare arguments
        current_args = list(args) if args else []
        
        # Modify arguments based on operation type
        if modify_key and operation == "SET":
            current_args[0] = f"key{i}"
        elif operation == "SADD":
            current_args[1] = f"member{i}"
        elif operation == "LPUSH":
            current_args[1] = f"item{i}"
        elif operation == "ZADD":
            current_args[0] = str(i)  # score
            current_args[1] = f"member{i}"
        
        start = time.perf_counter()
        client.send_command(operation, *current_args)
        response = client.read_response()
        end = time.perf_counter()
        
        times.append((end - start) * 1000)
        
        if i == 0:
            if isinstance(response, list):
                print(f"    First response: Array[{len(response)}]")
            else:
                print(f"    First response: {response}")
    
    if not times:
        return 0
    
    avg = sum(times) / len(times)
    min_time = min(times)
    max_time = max(times)
    p99 = sorted(times)[int(len(times) * 0.99)]
    
    ops_per_sec = 1000 / avg if avg > 0 else 0
    
    print(f"    Average: {avg:.3f}ms")
    print(f"    Min: {min_time:.3f}ms, Max: {max_time:.3f}ms, P99: {p99:.3f}ms")
    print(f"    Throughput: {ops_per_sec:.0f} ops/sec")
    
    return avg

def run_functionality_tests(client):
    """Run comprehensive functionality tests"""
    print_header("FUNCTIONALITY TESTS")
    
    # Clean start
    client.send_command("FLUSHDB")
    client.read_response()
    
    # 1. String Commands
    print_section("1. String Commands")
    test_command(client, "SET key value", "SET", "key1", "value1", expected="OK")
    test_command(client, "GET key", "GET", "key1", expected="value1")
    test_command(client, "GET non-existent", "GET", "nokey", expected="")
    test_command(client, "STRLEN", "STRLEN", "key1", expected=6)
    test_command(client, "APPEND", "APPEND", "key1", " more", expected=11)
    test_command(client, "GET after APPEND", "GET", "key1", expected="value1 more")
    test_command(client, "INCR", "SET", "counter", "10")
    test_command(client, "INCR by 1", "INCR", "counter", expected=11)
    test_command(client, "DECR by 1", "DECR", "counter", expected=10)
    test_command(client, "MSET", "MSET", "mkey1", "mval1", "mkey2", "mval2", expected="OK")
    test_command(client, "MGET", "MGET", "mkey1", "mkey2", "nokey")
    
    # 2. Hash Commands
    print_section("2. Hash Commands")
    test_command(client, "HSET single", "HSET", "hash1", "field1", "value1", expected=1)
    test_command(client, "HSET multiple", "HSET", "hash1", "f2", "v2", "f3", "v3", expected=2)
    test_command(client, "HGET", "HGET", "hash1", "field1", expected="value1")
    test_command(client, "HMSET", "HMSET", "hash2", "f1", "v1", "f2", "v2", expected="OK")
    test_command(client, "HMGET", "HMGET", "hash2", "f1", "f3", "f2")
    test_command(client, "HKEYS", "HKEYS", "hash1")
    test_command(client, "HVALS", "HVALS", "hash1")
    test_command(client, "HGETALL", "HGETALL", "hash1")
    test_command(client, "HLEN", "HLEN", "hash1", expected=3)
    test_command(client, "HEXISTS true", "HEXISTS", "hash1", "field1", expected=1)
    test_command(client, "HEXISTS false", "HEXISTS", "hash1", "nofield", expected=0)
    test_command(client, "HDEL", "HDEL", "hash1", "field1", expected=1)
    test_command(client, "HINCRBY", "HINCRBY", "hash1", "counter", "5", expected=5)
    test_command(client, "HINCRBY again", "HINCRBY", "hash1", "counter", "3", expected=8)
    
    # 3. List Commands
    print_section("3. List Commands")
    test_command(client, "LPUSH multiple", "LPUSH", "list1", "c", "b", "a", expected=3)
    test_command(client, "LLEN", "LLEN", "list1", expected=3)
    test_command(client, "LRANGE all", "LRANGE", "list1", "0", "-1")
    test_command(client, "LPOP", "LPOP", "list1", expected="a")
    test_command(client, "LINDEX", "LINDEX", "list1", "0", expected="b")
    test_command(client, "RPOP", "RPOP", "list1", expected="c")
    test_command(client, "RPUSH", "RPUSH", "list1", "d", expected=2)
    test_command(client, "LRANGE after ops", "LRANGE", "list1", "0", "-1")
    
    # 4. Set Commands
    print_section("4. Set Commands")
    test_command(client, "SADD multiple", "SADD", "set1", "a", "b", "c", "a", expected=3)
    test_command(client, "SCARD", "SCARD", "set1", expected=3)
    test_command(client, "SISMEMBER true", "SISMEMBER", "set1", "b", expected=1)
    test_command(client, "SISMEMBER false", "SISMEMBER", "set1", "x", expected=0)
    test_command(client, "SMEMBERS", "SMEMBERS", "set1")
    test_command(client, "SREM", "SREM", "set1", "b", expected=1)
    test_command(client, "SCARD after SREM", "SCARD", "set1", expected=2)
    
    # 5. Sorted Set Commands
    print_section("5. Sorted Set Commands")
    test_command(client, "ZADD multiple", "ZADD", "zset1", "1", "one", "2", "two", "3", "three", expected=3)
    test_command(client, "ZCARD", "ZCARD", "zset1", expected=3)
    test_command(client, "ZSCORE", "ZSCORE", "zset1", "two", expected="2")
    test_command(client, "ZRANGE", "ZRANGE", "zset1", "0", "-1")
    test_command(client, "ZREM", "ZREM", "zset1", "two", expected=1)
    test_command(client, "ZRANGE WITHSCORES", "ZRANGE", "zset1", "0", "-1", "WITHSCORES")
    
    # 6. Key Management Commands
    print_section("6. Key Management Commands")
    test_command(client, "EXISTS true", "EXISTS", "key1", expected=1)
    test_command(client, "EXISTS false", "EXISTS", "nokey", expected=0)
    test_command(client, "TYPE string", "TYPE", "key1", expected="string")
    test_command(client, "TYPE hash", "TYPE", "hash1", expected="hash")
    test_command(client, "TYPE list", "TYPE", "list1", expected="list")
    test_command(client, "TYPE set", "TYPE", "set1", expected="set")
    test_command(client, "TYPE zset", "TYPE", "zset1", expected="zset")
    test_command(client, "RENAME", "RENAME", "key1", "key2", expected="OK")
    test_command(client, "GET renamed key", "GET", "key2", expected="value1 more")
    test_command(client, "DEL single", "DEL", "key2", expected=1)
    test_command(client, "EXISTS after DEL", "EXISTS", "key2", expected=0)
    
    # 7. Expiration Commands
    print_section("7. Expiration Commands")
    test_command(client, "SETEX", "SETEX", "expkey", "10", "expvalue", expected="OK")
    ttl_response = test_command(client, "TTL", "TTL", "expkey")
    print(f"      TTL is {ttl_response} (should be ~10)")
    test_command(client, "PTTL", "PTTL", "expkey")
    test_command(client, "PERSIST", "PERSIST", "expkey", expected=1)
    test_command(client, "TTL after PERSIST", "TTL", "expkey", expected=-1)
    
    # 8. Database Commands
    print_section("8. Database Commands")
    dbsize = test_command(client, "DBSIZE", "DBSIZE")
    print(f"      Database has {dbsize} keys")
    test_command(client, "KEYS pattern", "KEYS", "*")
    test_command(client, "RANDOMKEY", "RANDOMKEY")
    test_command(client, "FLUSHDB", "FLUSHDB", expected="OK")
    test_command(client, "DBSIZE after flush", "DBSIZE", expected=0)
    
    # 9. Transaction Commands
    print_section("9. Transaction Commands")
    test_command(client, "SET for transaction", "SET", "tx_counter", "0")
    test_command(client, "MULTI", "MULTI", expected="OK")
    test_command(client, "INCR queued", "INCR", "tx_counter", expected="QUEUED")
    test_command(client, "INCR queued", "INCR", "tx_counter", expected="QUEUED")
    test_command(client, "INCR queued", "INCR", "tx_counter", expected="QUEUED")
    exec_result = test_command(client, "EXEC", "EXEC")
    print(f"      EXEC returned {len(exec_result) if isinstance(exec_result, list) else 0} results")
    test_command(client, "GET tx_counter", "GET", "tx_counter")
    
    # 10. Server Commands
    print_section("10. Server Commands")
    test_command(client, "PING", "PING", expected="PONG")
    test_command(client, "ECHO", "ECHO", "Hello Redis!", expected="Hello Redis!")
    test_command(client, "SELECT 0", "SELECT", "0", expected="OK")
    test_command(client, "SELECT invalid", "SELECT", "99")
    test_command(client, "CONFIG GET", "CONFIG", "GET", "*")
    
    # 11. Stream Commands
    print_section("11. Stream Commands")
    xadd_id = test_command(client, "XADD", "XADD", "stream1", "*", "field1", "value1")
    print(f"      XADD generated ID: {xadd_id}")
    test_command(client, "XREAD", "XREAD", "STREAMS", "stream1", "0-0")

def run_performance_benchmarks(client):
    """Run performance benchmarks"""
    print_header("PERFORMANCE BENCHMARKS")
    
    # Clean start
    client.send_command("FLUSHDB")
    client.read_response()
    
    results = {}
    
    # 1. Basic Operations
    print_section("1. Basic Operations (1000 iterations each)")
    
    print("\nPING:")
    results['ping'] = benchmark_operation(client, "PING", iterations=1000)
    
    print("\nSET:")
    results['set'] = benchmark_operation(client, "SET", "key", "value", iterations=1000, modify_key=True)
    
    print("\nGET:")
    client.send_command("SET", "getkey", "value")
    client.read_response()
    results['get'] = benchmark_operation(client, "GET", "getkey", iterations=1000)
    
    print("\nINCR:")
    client.send_command("SET", "counter", "0")
    client.read_response()
    results['incr'] = benchmark_operation(client, "INCR", "counter", iterations=1000)
    
    # 2. Hash Operations
    print_section("2. Hash Operations (1000 iterations each)")
    
    print("\nHSET:")
    results['hset'] = benchmark_operation(client, "HSET", "bench_hash", "field", "value", iterations=1000)
    
    print("\nHGET:")
    results['hget'] = benchmark_operation(client, "HGET", "bench_hash", "field", iterations=1000)
    
    print("\nHINCRBY:")
    client.send_command("HSET", "bench_hash", "counter", "0")
    client.read_response()
    results['hincrby'] = benchmark_operation(client, "HINCRBY", "bench_hash", "counter", "1", iterations=1000)
    
    # Populate hash with 100 fields for bulk operations
    for i in range(100):
        client.send_command("HSET", "bulk_hash", f"field{i}", f"value{i}")
        client.read_response()
    
    print("\nHKEYS (100 fields):")
    start = time.perf_counter()
    client.send_command("HKEYS", "bulk_hash")
    response = client.read_response()
    hkeys_time = (time.perf_counter() - start) * 1000
    print(f"    Time: {hkeys_time:.3f}ms")
    results['hkeys_100'] = hkeys_time
    
    print("\nHVALS (100 fields):")
    start = time.perf_counter()
    client.send_command("HVALS", "bulk_hash")
    response = client.read_response()
    hvals_time = (time.perf_counter() - start) * 1000
    print(f"    Time: {hvals_time:.3f}ms")
    results['hvals_100'] = hvals_time
    
    print("\nHGETALL (100 fields):")
    start = time.perf_counter()
    client.send_command("HGETALL", "bulk_hash")
    response = client.read_response()
    hgetall_time = (time.perf_counter() - start) * 1000
    print(f"    Time: {hgetall_time:.3f}ms")
    results['hgetall_100'] = hgetall_time
    
    # 3. List Operations
    print_section("3. List Operations (1000 iterations each)")
    
    print("\nLPUSH:")
    results['lpush'] = benchmark_operation(client, "LPUSH", "bench_list", "item", iterations=1000)
    
    print("\nLRANGE (100 items):")
    start = time.perf_counter()
    client.send_command("LRANGE", "bench_list", "0", "99")
    response = client.read_response()
    lrange_time = (time.perf_counter() - start) * 1000
    print(f"    Time: {lrange_time:.3f}ms")
    results['lrange_100'] = lrange_time
    
    print("\nLPOP:")
    results['lpop'] = benchmark_operation(client, "LPOP", "bench_list", iterations=1000)
    
    # 4. Set Operations
    print_section("4. Set Operations")
    
    print("\nSADD (1000 unique members):")
    sadd_times = []
    for i in range(1000):
        start = time.perf_counter()
        client.send_command("SADD", "bench_set", f"member{i}")
        response = client.read_response()
        sadd_times.append((time.perf_counter() - start) * 1000)
    results['sadd'] = sum(sadd_times) / len(sadd_times)
    print(f"    Average: {results['sadd']:.3f}ms")
    print(f"    Throughput: {1000/results['sadd']:.0f} ops/sec")
    
    print("\nSMEMBERS (1000 members):")
    start = time.perf_counter()
    client.send_command("SMEMBERS", "bench_set")
    response = client.read_response()
    smembers_time = (time.perf_counter() - start) * 1000
    print(f"    Time: {smembers_time:.3f}ms")
    results['smembers_1000'] = smembers_time
    
    # 5. Sorted Set Operations
    print_section("5. Sorted Set Operations")
    
    print("\nZADD (1000 members):")
    zadd_times = []
    for i in range(1000):
        start = time.perf_counter()
        client.send_command("ZADD", "bench_zset", str(i), f"member{i}")
        response = client.read_response()
        zadd_times.append((time.perf_counter() - start) * 1000)
    results['zadd'] = sum(zadd_times) / len(zadd_times)
    print(f"    Average: {results['zadd']:.3f}ms")
    print(f"    Throughput: {1000/results['zadd']:.0f} ops/sec")
    
    print("\nZRANGE (100 items):")
    start = time.perf_counter()
    client.send_command("ZRANGE", "bench_zset", "0", "99")
    response = client.read_response()
    zrange_time = (time.perf_counter() - start) * 1000
    print(f"    Time: {zrange_time:.3f}ms")
    results['zrange_100'] = zrange_time
    
    # 6. Mixed Operations
    print_section("6. Mixed Operations")
    
    print("\n1000 operations (250 each: SET/GET/INCR/HSET):")
    start = time.perf_counter()
    for i in range(250):
        client.send_command("SET", f"mix_key{i}", f"value{i}")
        client.read_response()
        client.send_command("GET", f"mix_key{i}")
        client.read_response()
        client.send_command("INCR", "mix_counter")
        client.read_response()
        client.send_command("HSET", "mix_hash", f"field{i}", f"value{i}")
        client.read_response()
    mixed_time = (time.perf_counter() - start) * 1000
    mixed_avg = mixed_time / 1000
    print(f"    Total time: {mixed_time:.3f}ms")
    print(f"    Average per op: {mixed_avg:.3f}ms")
    print(f"    Throughput: {1000/mixed_avg:.0f} ops/sec")
    results['mixed'] = mixed_avg
    
    return results

def run_stress_test(client, duration=10):
    """Run stress test for specified duration"""
    print_header(f"STRESS TEST ({duration} seconds)")
    
    client.send_command("FLUSHDB")
    client.read_response()
    
    operations = 0
    errors = 0
    start_time = time.time()
    
    while time.time() - start_time < duration:
        try:
            # Mix of operations
            op = operations % 4
            if op == 0:
                client.send_command("SET", f"stress_key{operations}", f"value{operations}")
            elif op == 1:
                client.send_command("GET", f"stress_key{operations-1}")
            elif op == 2:
                client.send_command("INCR", "stress_counter")
            else:
                client.send_command("LPUSH", "stress_list", f"item{operations}")
            
            response = client.read_response()
            if isinstance(response, str) and response.startswith("ERROR"):
                errors += 1
            operations += 1
            
        except Exception as e:
            errors += 1
            print(f"    Error at operation {operations}: {e}")
    
    elapsed = time.time() - start_time
    ops_per_sec = operations / elapsed
    
    print(f"  Completed: {operations} operations in {elapsed:.2f} seconds")
    print(f"  Throughput: {ops_per_sec:.0f} ops/sec")
    print(f"  Errors: {errors}")
    
    # Check final state
    client.send_command("DBSIZE")
    dbsize = client.read_response()
    print(f"  Final DB size: {dbsize} keys")

def print_summary(results):
    """Print performance summary"""
    print_header("PERFORMANCE SUMMARY")
    
    print("\nCore Operations:")
    print(f"  PING:     {results.get('ping', 0):.3f}ms ({1000/results.get('ping', 1):.0f} ops/sec)")
    print(f"  SET:      {results.get('set', 0):.3f}ms ({1000/results.get('set', 1):.0f} ops/sec)")
    print(f"  GET:      {results.get('get', 0):.3f}ms ({1000/results.get('get', 1):.0f} ops/sec)")
    print(f"  INCR:     {results.get('incr', 0):.3f}ms ({1000/results.get('incr', 1):.0f} ops/sec)")
    
    print("\nHash Operations:")
    print(f"  HSET:     {results.get('hset', 0):.3f}ms ({1000/results.get('hset', 1):.0f} ops/sec)")
    print(f"  HGET:     {results.get('hget', 0):.3f}ms ({1000/results.get('hget', 1):.0f} ops/sec)")
    print(f"  HINCRBY:  {results.get('hincrby', 0):.3f}ms ({1000/results.get('hincrby', 1):.0f} ops/sec)")
    print(f"  HKEYS (100 fields):   {results.get('hkeys_100', 0):.3f}ms")
    print(f"  HVALS (100 fields):   {results.get('hvals_100', 0):.3f}ms")
    print(f"  HGETALL (100 fields): {results.get('hgetall_100', 0):.3f}ms")
    
    print("\nList Operations:")
    print(f"  LPUSH:    {results.get('lpush', 0):.3f}ms ({1000/results.get('lpush', 1):.0f} ops/sec)")
    print(f"  LPOP:     {results.get('lpop', 0):.3f}ms ({1000/results.get('lpop', 1):.0f} ops/sec)")
    print(f"  LRANGE (100 items):   {results.get('lrange_100', 0):.3f}ms")
    
    print("\nSet Operations:")
    print(f"  SADD:     {results.get('sadd', 0):.3f}ms ({1000/results.get('sadd', 1):.0f} ops/sec)")
    print(f"  SMEMBERS (1000 items): {results.get('smembers_1000', 0):.3f}ms")
    
    print("\nSorted Set Operations:")
    print(f"  ZADD:     {results.get('zadd', 0):.3f}ms ({1000/results.get('zadd', 1):.0f} ops/sec)")
    print(f"  ZRANGE (100 items):   {results.get('zrange_100', 0):.3f}ms")
    
    print("\nMixed Operations:")
    print(f"  Average:  {results.get('mixed', 0):.3f}ms ({1000/results.get('mixed', 1):.0f} ops/sec)")
    
    # Performance rating
    ping_time = results.get('ping', float('inf'))
    print("\nPerformance Rating:")
    if ping_time < 0.3:
        print("  ⭐⭐⭐ EXCELLENT - Production Ready!")
    elif ping_time < 0.5:
        print("  ⭐⭐ VERY GOOD - Suitable for most use cases")
    elif ping_time < 1.0:
        print("  ⭐ GOOD - Acceptable performance")
    else:
        print("  ⚠️ NEEDS OPTIMIZATION")

def main():
    parser = argparse.ArgumentParser(description='Redis Server Benchmark Suite')
    parser.add_argument('--host', default='localhost', help='Redis server host')
    parser.add_argument('--port', type=int, default=6379, help='Redis server port')
    parser.add_argument('--mode', choices=['all', 'func', 'perf', 'stress'], 
                        default='all', help='Test mode to run')
    parser.add_argument('--stress-duration', type=int, default=10, 
                        help='Duration for stress test in seconds')
    
    args = parser.parse_args()
    
    print("=" * 60)
    print("  AILANG REDIS SERVER - COMPREHENSIVE BENCHMARK SUITE")
    print("=" * 60)
    print(f"  Host: {args.host}:{args.port}")
    print(f"  Mode: {args.mode}")
    
    try:
        client = RedisClient(args.host, args.port)
        
        # Warm up
        print("\nWarming up...")
        for _ in range(100):
            client.send_command("PING")
            client.read_response()
        
        results = {}
        
        if args.mode in ['all', 'func']:
            run_functionality_tests(client)
        
        if args.mode in ['all', 'perf']:
            results = run_performance_benchmarks(client)
        
        if args.mode in ['all', 'stress']:
            run_stress_test(client, args.stress_duration)
        
        if results:
            print_summary(results)
        
        client.close()
        print("\n✅ Benchmark completed successfully!")
        
    except Exception as e:
        print(f"\n❌ Error: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()