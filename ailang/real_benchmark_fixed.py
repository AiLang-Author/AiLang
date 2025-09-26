#!/usr/bin/env python3
# real_benchmark_fixed.py - Fixed Redis benchmark with proper RESP parsing

import socket
import time
import sys

class RedisClient:
    def __init__(self, host='localhost', port=6379):
        self.host = host
        self.port = port
        self.sock = None
        self.buffer = b""
        self.connect()
    
    def connect(self):
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
            chunk = self.sock.recv(4096)
            if not chunk:
                break
            self.buffer += chunk
        
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
        if self.sock:
            self.sock.close()

def benchmark_operation(client, operation, *args, iterations=1000):
    """Benchmark a Redis operation"""
    times = []
    
    for i in range(iterations):
        # Prepare arguments
        current_args = [operation] + list(args)
        if operation == "SET":
            current_args[1] = f"key{i}"
        elif operation in ["SADD", "LPUSH", "ZADD"]:
            # For these, modify the value not the key
            if operation == "SADD":
                current_args[2] = f"member{i}"
            elif operation == "LPUSH":
                current_args[2] = f"item{i}"
            elif operation == "ZADD":
                current_args[2] = str(i)  # score
                current_args[3] = f"member{i}"
        
        start = time.perf_counter()
        client.send_command(*current_args)
        response = client.read_response()
        end = time.perf_counter()
        
        times.append((end - start) * 1000)
        
        if i == 0:
            if isinstance(response, list):
                print(f"    First response: Array with {len(response)} elements")
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

def main():
    print("=" * 60)
    print("  AILANG Redis Server - FIXED Benchmark")
    print("  (With proper RESP parsing)")
    print("=" * 60)
    print()
    
    # Connect to Redis
    client = RedisClient()
    
    # Warm up
    print("Warming up...")
    for _ in range(100):
        client.send_command("PING")
        client.read_response()
    
    # Clean database
    client.send_command("FLUSHDB")
    client.read_response()
    
    print("Starting benchmarks...\n")
    
    # 1. Test SET commands specifically
    print("1. SET Commands Test:")
    print("-" * 40)
    
    # SADD
    print("SADD (adding 3 members):")
    client.send_command("SADD", "testset", "a", "b", "c")
    response = client.read_response()
    print(f"  Response: {response} (should be integer 3)")
    
    # SMEMBERS
    print("SMEMBERS testset:")
    client.send_command("SMEMBERS", "testset")
    response = client.read_response()
    if isinstance(response, list):
        print(f"  Response: Array with {len(response)} elements: {response}")
    else:
        print(f"  Response: {response} (ERROR - should be array!)")
    
    # SCARD
    print("SCARD testset:")
    client.send_command("SCARD", "testset")
    response = client.read_response()
    print(f"  Response: {response} (should be integer 3)")
    print()
    
    # 2. Test HASH commands
    print("2. HASH Commands Test:")
    print("-" * 40)
    
    # HSET
    print("HSET (adding 3 fields):")
    client.send_command("HSET", "testhash", "f1", "v1", "f2", "v2", "f3", "v3")
    response = client.read_response()
    print(f"  Response: {response} (should be integer 3)")
    
    # HKEYS
    print("HKEYS testhash:")
    client.send_command("HKEYS", "testhash")
    response = client.read_response()
    if isinstance(response, list):
        print(f"  Response: Array with {len(response)} elements: {response}")
    else:
        print(f"  Response: {response} (ERROR - should be array!)")
    
    # HVALS
    print("HVALS testhash:")
    client.send_command("HVALS", "testhash")
    response = client.read_response()
    if isinstance(response, list):
        print(f"  Response: Array with {len(response)} elements: {response}")
    else:
        print(f"  Response: {response} (ERROR - should be array!)")
    
    # HGETALL
    print("HGETALL testhash:")
    client.send_command("HGETALL", "testhash")
    response = client.read_response()
    if isinstance(response, list):
        print(f"  Response: Array with {len(response)} elements: {response}")
    else:
        print(f"  Response: {response} (ERROR - should be array!)")
    print()
    
    # 3. Test LIST commands
    print("3. LIST Commands Test:")
    print("-" * 40)
    
    # LPUSH
    print("LPUSH (adding 3 items):")
    client.send_command("LPUSH", "testlist", "a", "b", "c")
    response = client.read_response()
    print(f"  Response: {response} (should be integer 3)")
    
    # LRANGE
    print("LRANGE testlist 0 -1:")
    client.send_command("LRANGE", "testlist", "0", "-1")
    response = client.read_response()
    if isinstance(response, list):
        print(f"  Response: Array with {len(response)} elements: {response}")
    else:
        print(f"  Response: {response} (ERROR - should be array!)")
    
    # LPOP
    print("LPOP testlist:")
    client.send_command("LPOP", "testlist")
    response = client.read_response()
    print(f"  Response: {response} (should be 'c')")
    print()
    
    # 4. Performance benchmarks
    print("4. Performance Benchmarks:")
    print("-" * 40)
    
    # Clean up first
    client.send_command("FLUSHDB")
    client.read_response()
    
    # PING
    print("PING (1000 iterations):")
    ping_time = benchmark_operation(client, "PING", iterations=1000)
    print()
    
    # SET
    print("SET (1000 iterations):")
    set_time = benchmark_operation(client, "SET", "key", "value", iterations=1000)
    print()
    
    # GET
    client.send_command("SET", "testkey", "testvalue")
    client.read_response()
    print("GET (1000 iterations):")
    get_time = benchmark_operation(client, "GET", "testkey", iterations=1000)
    print()
    
    # Clean up
    client.send_command("FLUSHDB")
    client.read_response()
    client.close()
    
    # Summary
    print("=" * 60)
    print("  SUMMARY")
    print("=" * 60)
    if ping_time < 0.5:
        print("  🏆 EXCELLENT PERFORMANCE!")
    else:
        print("  ✅ Good performance")

if __name__ == "__main__":
    main()
