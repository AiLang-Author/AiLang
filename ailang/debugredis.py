#!/usr/bin/env python3
"""
Focused debug test scripts for specific Redis server issues
Run each test individually to isolate the problem
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

def parse_array(response):
    """Parse array response and return items"""
    lines = response.split('\r\n')
    if not lines[0].startswith('*'):
        return None
    count = int(lines[0][1:])
    items = []
    i = 1
    for _ in range(count):
        if i >= len(lines):
            break
        if lines[i].startswith(':'):
            items.append(('int', int(lines[i][1:])))
            i += 1
        elif lines[i].startswith('+'):
            items.append(('str', lines[i][1:]))
            i += 1
        elif lines[i].startswith('$'):
            length = int(lines[i][1:])
            i += 1
            if length < 0:
                items.append(('null', None))
            elif i < len(lines):
                items.append(('bulk', lines[i]))
                i += 1
        else:
            i += 1
    return items

# =============================================================================
# Test 1: FLUSHDB Issue
# =============================================================================
def test_flushdb():
    """Test FLUSHDB - should clear all keys"""
    print("=" * 60)
    print("TEST 1: FLUSHDB Issue")
    print("=" * 60)
    
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.connect(('localhost', 6379))
    
    # First, add some keys
    print("\n1. Adding test keys...")
    for i in range(5):
        resp = send_redis_command(sock, 'SET', f'test_key_{i}', f'value_{i}')
        print(f"   SET test_key_{i}: {resp}")
    
    # Check DBSIZE before flush
    resp = send_redis_command(sock, 'DBSIZE')
    size_before = parse_integer(resp)
    print(f"\n2. DBSIZE before FLUSHDB: {size_before}")
    
    # Execute FLUSHDB
    resp = send_redis_command(sock, 'FLUSHDB')
    print(f"\n3. FLUSHDB response: {resp}")
    
    # Check DBSIZE after flush
    resp = send_redis_command(sock, 'DBSIZE')
    size_after = parse_integer(resp)
    print(f"\n4. DBSIZE after FLUSHDB: {size_after}")
    
    # Try to GET the keys
    print("\n5. Trying to GET flushed keys:")
    for i in range(5):
        resp = send_redis_command(sock, 'GET', f'test_key_{i}')
        print(f"   GET test_key_{i}: {resp}")
    
    # Check what keys remain
    resp = send_redis_command(sock, 'KEYS', '*')
    print(f"\n6. KEYS * after FLUSHDB: {resp[:200]}...")  # First 200 chars
    
    sock.close()
    print(f"\n{'✓' if size_after == 0 else '✗'} FLUSHDB test {'passed' if size_after == 0 else 'FAILED'}")

# =============================================================================
# Test 2: TTL/PERSIST Issue
# =============================================================================
def test_ttl_persist():
    """Test TTL after PERSIST - should return -1"""
    print("\n" + "=" * 60)
    print("TEST 2: TTL/PERSIST Issue")
    print("=" * 60)
    
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.connect(('localhost', 6379))
    
    # Clean up first
    send_redis_command(sock, 'DEL', 'ttl_test_key')
    
    # Set key with expiration
    print("\n1. Setting key with 10 second expiration...")
    resp = send_redis_command(sock, 'SETEX', 'ttl_test_key', '10', 'test_value')
    print(f"   SETEX: {resp}")
    
    # Check initial TTL
    resp = send_redis_command(sock, 'TTL', 'ttl_test_key')
    ttl_before = parse_integer(resp)
    print(f"\n2. TTL before PERSIST: {ttl_before}")
    
    # Execute PERSIST
    resp = send_redis_command(sock, 'PERSIST', 'ttl_test_key')
    persist_result = parse_integer(resp)
    print(f"\n3. PERSIST response: {persist_result} (1=success, 0=no expiry or key not found)")
    
    # Check TTL after PERSIST
    resp = send_redis_command(sock, 'TTL', 'ttl_test_key')
    ttl_after = parse_integer(resp)
    print(f"\n4. TTL after PERSIST: {ttl_after} (should be -1)")
    
    # Verify key still exists
    resp = send_redis_command(sock, 'GET', 'ttl_test_key')
    print(f"\n5. GET key after PERSIST: {resp}")
    
    sock.close()
    print(f"\n{'✓' if ttl_after == -1 else '✗'} TTL/PERSIST test {'passed' if ttl_after == -1 else 'FAILED'}")

# =============================================================================
# Test 3: ZREM Issue
# =============================================================================
def test_zrem():
    """Test ZREM - should return 1 when removing existing member"""
    print("\n" + "=" * 60)
    print("TEST 3: ZREM Issue")
    print("=" * 60)
    
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.connect(('localhost', 6379))
    
    # Clean up first
    send_redis_command(sock, 'DEL', 'zset_test')
    
    # Add members to sorted set
    print("\n1. Adding members to sorted set...")
    resp = send_redis_command(sock, 'ZADD', 'zset_test', '1', 'one', '2', 'two', '3', 'three')
    added_count = parse_integer(resp)
    print(f"   ZADD response: {added_count} members added")
    
    # Check current members
    resp = send_redis_command(sock, 'ZRANGE', 'zset_test', '0', '-1')
    print(f"\n2. ZRANGE before removal: {resp}")
    
    # Remove existing member
    print("\n3. Removing 'two'...")
    resp = send_redis_command(sock, 'ZREM', 'zset_test', 'two')
    removed_count = parse_integer(resp)
    print(f"   ZREM response: {removed_count} (should be 1)")
    
    # Check members after removal
    resp = send_redis_command(sock, 'ZRANGE', 'zset_test', '0', '-1')
    print(f"\n4. ZRANGE after removal: {resp}")
    
    # Try to remove non-existent member
    print("\n5. Trying to remove non-existent member...")
    resp = send_redis_command(sock, 'ZREM', 'zset_test', 'nonexistent')
    removed_count2 = parse_integer(resp)
    print(f"   ZREM response: {removed_count2} (should be 0)")
    
    sock.close()
    print(f"\n{'✓' if removed_count == 1 else '✗'} ZREM test {'passed' if removed_count == 1 else 'FAILED'}")

# =============================================================================
# Test 4: Transaction EXEC Issue
# =============================================================================
def test_transaction():
    """Test EXEC returning actual results instead of nulls"""
    print("\n" + "=" * 60)
    print("TEST 4: Transaction EXEC Issue")
    print("=" * 60)
    
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.connect(('localhost', 6379))
    
    # Clean up and setup
    send_redis_command(sock, 'DEL', 'tx_counter')
    resp = send_redis_command(sock, 'SET', 'tx_counter', '0')
    print(f"\n1. Initial setup - SET tx_counter 0: {resp}")
    
    # Start transaction
    resp = send_redis_command(sock, 'MULTI')
    print(f"\n2. MULTI: {resp}")
    
    # Queue commands
    print("\n3. Queueing commands...")
    resp1 = send_redis_command(sock, 'INCR', 'tx_counter')
    print(f"   INCR tx_counter: {resp1}")
    
    resp2 = send_redis_command(sock, 'SET', 'tx_key', 'tx_value')
    print(f"   SET tx_key tx_value: {resp2}")
    
    resp3 = send_redis_command(sock, 'GET', 'tx_counter')
    print(f"   GET tx_counter: {resp3}")
    
    resp4 = send_redis_command(sock, 'INCR', 'tx_counter')
    print(f"   INCR tx_counter: {resp4}")
    
    # Execute transaction
    print("\n4. Executing transaction...")
    resp = send_redis_command(sock, 'EXEC')
    print(f"   EXEC raw response:\n{resp}\n")
    
    # Parse EXEC response
    results = parse_array(resp)
    if results:
        print("5. Parsed EXEC results:")
        for i, result in enumerate(results):
            print(f"   Command {i+1}: {result}")
            if result[0] == 'null':
                print(f"      ^ This should NOT be null!")
    
    # Verify final state
    print("\n6. Verifying final state:")
    resp = send_redis_command(sock, 'GET', 'tx_counter')
    print(f"   GET tx_counter: {resp} (should be '2')")
    
    resp = send_redis_command(sock, 'GET', 'tx_key')
    print(f"   GET tx_key: {resp} (should be 'tx_value')")
    
    sock.close()
    
    has_nulls = results and any(r[0] == 'null' for r in results)
    print(f"\n{'✗' if has_nulls else '✓'} Transaction test {'FAILED - returns nulls' if has_nulls else 'passed'}")

# =============================================================================
# Main
# =============================================================================
if __name__ == "__main__":
    print("REDIS SERVER DEBUG TESTS")
    print("Run with debug flag (-D 1 or higher) to see server debug output")
    print("")
    
    try:
        test_flushdb()
        time.sleep(1)
        
        test_ttl_persist()
        time.sleep(1)
        
        test_zrem()
        time.sleep(1)
        
        test_transaction()
        
        print("\n" + "=" * 60)
        print("ALL TESTS COMPLETED")
        print("=" * 60)
        
    except ConnectionRefusedError:
        print("Error: Could not connect to Redis server on localhost:6379")
        print("Make sure the server is running with debug flags enabled")
    except Exception as e:
        print(f"Error: {e}")
        import traceback
        traceback.print_exc()