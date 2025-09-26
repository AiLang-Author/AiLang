#!/usr/bin/env python3
"""
Verify that the PERSIST fix is actually applied
"""

import socket
import time

def send_redis_command(sock, *args):
    """Send Redis command using raw RESP protocol"""
    cmd = f"*{len(args)}\r\n"
    for arg in args:
        arg_str = str(arg)
        cmd += f"${len(arg_str)}\r\n{arg_str}\r\n"
    sock.send(cmd.encode())
    response = sock.recv(4096).decode()
    return response.strip()

def parse_integer(response):
    """Parse integer from RESP response"""
    if response.startswith(':'):
        return int(response[1:].split('\r')[0])
    return None

print("Testing if PERSIST is actually setting expiration to 0...")
print("=" * 60)

sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
sock.connect(('localhost', 6379))

# Clean slate
send_redis_command(sock, 'DEL', 'test_key')

# Test 1: Key without expiration should have TTL = -1
print("\nTest 1: Key without expiration")
send_redis_command(sock, 'SET', 'test_key', 'value')
resp = send_redis_command(sock, 'TTL', 'test_key')
ttl = parse_integer(resp)
print(f"TTL of key without expiration: {ttl}")
assert ttl == -1, f"Expected -1, got {ttl}"

# Test 2: Key with expiration should have TTL > 0
print("\nTest 2: Key with expiration")
send_redis_command(sock, 'EXPIRE', 'test_key', '100')
resp = send_redis_command(sock, 'TTL', 'test_key')
ttl = parse_integer(resp)
print(f"TTL after EXPIRE 100: {ttl}")
assert ttl > 0, f"Expected > 0, got {ttl}"

# Test 3: PERSIST should return 1 and set TTL to -1
print("\nTest 3: PERSIST on key with expiration")
resp = send_redis_command(sock, 'PERSIST', 'test_key')
persist_result = parse_integer(resp)
print(f"PERSIST result: {persist_result}")

resp = send_redis_command(sock, 'TTL', 'test_key')
ttl = parse_integer(resp)
print(f"TTL after PERSIST: {ttl}")

if ttl == -1:
    print("\n✓ SUCCESS: PERSIST is working correctly!")
else:
    print(f"\n✗ FAILURE: TTL is {ttl} instead of -1")
    print("\nDiagnosis:")
    print("The PERSIST command returns 1 (success) but doesn't clear expiration.")
    print("This indicates the StoreValue(Add(wrapper, 8), 0) line is not working.")
    print("\nPossible causes:")
    print("1. The compiled binary doesn't have the latest code")
    print("2. There's a bug in StoreValue or memory addressing")
    print("3. The wrapper structure layout is different than expected")
    
sock.close()