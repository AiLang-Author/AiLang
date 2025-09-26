#!/usr/bin/env python3
"""
Debug script specifically for the PERSIST/TTL issue
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

def test_persist_ttl_detailed():
    """Detailed test of PERSIST/TTL interaction"""
    print("=" * 60)
    print("DETAILED PERSIST/TTL DEBUG TEST")
    print("=" * 60)
    
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.connect(('localhost', 6379))
    
    # Clean up first
    print("\n1. Cleanup...")
    send_redis_command(sock, 'DEL', 'debug_key')
    
    # Set key with no expiration
    print("\n2. Setting key without expiration...")
    resp = send_redis_command(sock, 'SET', 'debug_key', 'test_value')
    print(f"   SET response: {resp}")
    
    # Check TTL (should be -1)
    resp = send_redis_command(sock, 'TTL', 'debug_key')
    ttl = parse_integer(resp)
    print(f"   TTL without expiration: {ttl} (should be -1)")
    
    # Set expiration
    print("\n3. Setting 10 second expiration...")
    resp = send_redis_command(sock, 'EXPIRE', 'debug_key', '10')
    print(f"   EXPIRE response: {resp}")
    
    # Check TTL (should be ~10)
    resp = send_redis_command(sock, 'TTL', 'debug_key')
    ttl = parse_integer(resp)
    print(f"   TTL with expiration: {ttl} (should be ~10)")
    
    # Use PERSIST
    print("\n4. Calling PERSIST...")
    resp = send_redis_command(sock, 'PERSIST', 'debug_key')
    persist_result = parse_integer(resp)
    print(f"   PERSIST response: {persist_result} (should be 1)")
    
    # Check TTL immediately
    resp = send_redis_command(sock, 'TTL', 'debug_key')
    ttl = parse_integer(resp)
    print(f"   TTL after PERSIST: {ttl} (should be -1)")
    
    # Check key still exists
    resp = send_redis_command(sock, 'GET', 'debug_key')
    print(f"   GET key: {resp}")
    
    # Try setting expiration again and then PERSIST
    print("\n5. Testing SETEX then PERSIST...")
    send_redis_command(sock, 'DEL', 'debug_key')
    resp = send_redis_command(sock, 'SETEX', 'debug_key', '10', 'test_value')
    print(f"   SETEX response: {resp}")
    
    resp = send_redis_command(sock, 'TTL', 'debug_key')
    ttl_before = parse_integer(resp)
    print(f"   TTL before PERSIST: {ttl_before}")
    
    resp = send_redis_command(sock, 'PERSIST', 'debug_key')
    persist_result = parse_integer(resp)
    print(f"   PERSIST response: {persist_result}")
    
    resp = send_redis_command(sock, 'TTL', 'debug_key')
    ttl_after = parse_integer(resp)
    print(f"   TTL after PERSIST: {ttl_after} (should be -1)")
    
    # Test PERSIST on key without expiration
    print("\n6. Testing PERSIST on key without expiration...")
    send_redis_command(sock, 'SET', 'no_expire_key', 'value')
    resp = send_redis_command(sock, 'PERSIST', 'no_expire_key')
    persist_result = parse_integer(resp)
    print(f"   PERSIST on non-expiring key: {persist_result} (should be 0)")
    
    sock.close()
    
    print("\n" + "=" * 60)
    if ttl_after == -1:
        print("✓ PERSIST/TTL working correctly!")
    else:
        print(f"✗ PERSIST/TTL ISSUE: TTL is {ttl_after} instead of -1")
        print("\nPossible issues:")
        print("1. PERSIST is not setting expiration_ms to 0")
        print("2. TTL is not checking for expiration_ms == 0")
        print("3. There's a timing issue with the fake_system_time")
    print("=" * 60)

if __name__ == "__main__":
    test_persist_ttl_detailed()