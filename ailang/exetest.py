#!/usr/bin/env python3
"""
Complete test script for Redis transaction functionality.
Tests MULTI, EXEC, DISCARD and verifies state management.
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
    
    # Read response with timeout
    sock.settimeout(2.0)
    try:
        response = sock.recv(4096).decode()
    except socket.timeout:
        return "[TIMEOUT]"
    return response

def parse_resp_response(response):
    """Parse a single RESP response"""
    if not response or response == "[TIMEOUT]":
        return ("error", "timeout")
    
    lines = response.split('\r\n')
    if not lines:
        return ("error", "empty")
    
    first_line = lines[0]
    if first_line.startswith('+'):
        return ("simple", first_line[1:])
    elif first_line.startswith('-'):
        return ("error", first_line[1:])
    elif first_line.startswith(':'):
        return ("int", int(first_line[1:]))
    elif first_line.startswith('$'):
        length = int(first_line[1:])
        if length < 0:
            return ("null", None)
        elif len(lines) > 1:
            return ("bulk", lines[1])
        else:
            return ("bulk", "")
    elif first_line.startswith('*'):
        count = int(first_line[1:])
        results = []
        i = 1
        for _ in range(count):
            if i >= len(lines):
                break
            if lines[i].startswith(':'):
                results.append(('int', int(lines[i][1:])))
                i += 1
            elif lines[i].startswith('+'):
                results.append(('str', lines[i][1:]))
                i += 1
            elif lines[i].startswith('$'):
                length = int(lines[i][1:])
                i += 1
                if length >= 0 and i < len(lines):
                    results.append(('bulk', lines[i]))
                    i += 1
                else:
                    results.append(('null', None))
            else:
                i += 1
        return ("array", results)
    return ("unknown", response)

def test_transactions():
    """Run comprehensive transaction tests"""
    
    print("=" * 70)
    print("COMPREHENSIVE REDIS TRANSACTION TESTS")
    print("=" * 70)
    
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.connect(('localhost', 6379))
    
    # Test 1: Basic MULTI/EXEC
    print("\nTest 1: Basic MULTI/EXEC")
    print("-" * 40)
    
    resp = send_redis_command(sock, 'FLUSHDB')
    print(f"FLUSHDB: {parse_resp_response(resp)}")
    
    resp = send_redis_command(sock, 'SET', 'counter', '0')
    print(f"SET counter 0: {parse_resp_response(resp)}")
    
    resp = send_redis_command(sock, 'MULTI')
    print(f"MULTI: {parse_resp_response(resp)}")
    
    resp = send_redis_command(sock, 'INCR', 'counter')
    print(f"INCR counter: {parse_resp_response(resp)}")
    
    resp = send_redis_command(sock, 'INCR', 'counter')
    print(f"INCR counter: {parse_resp_response(resp)}")
    
    resp = send_redis_command(sock, 'SET', 'key', 'value')
    print(f"SET key value: {parse_resp_response(resp)}")
    
    resp = send_redis_command(sock, 'GET', 'counter')
    print(f"GET counter: {parse_resp_response(resp)}")
    
    resp = send_redis_command(sock, 'EXEC')
    parsed = parse_resp_response(resp)
    print(f"EXEC: {parsed}")
    if parsed[0] == "array":
        for i, result in enumerate(parsed[1]):
            print(f"  Result {i+1}: {result}")
    
    # Verify state after EXEC
    resp = send_redis_command(sock, 'PING')
    print(f"PING after EXEC: {parse_resp_response(resp)}")
    
    resp = send_redis_command(sock, 'GET', 'counter')
    print(f"GET counter: {parse_resp_response(resp)}")
    
    resp = send_redis_command(sock, 'GET', 'key')
    print(f"GET key: {parse_resp_response(resp)}")
    
    # Test 2: DISCARD
    print("\n\nTest 2: DISCARD")
    print("-" * 40)
    
    resp = send_redis_command(sock, 'GET', 'counter')
    print(f"GET counter before: {parse_resp_response(resp)}")
    
    resp = send_redis_command(sock, 'MULTI')
    print(f"MULTI: {parse_resp_response(resp)}")
    
    resp = send_redis_command(sock, 'INCR', 'counter')
    print(f"INCR counter: {parse_resp_response(resp)}")
    
    resp = send_redis_command(sock, 'SET', 'discarded', 'should_not_exist')
    print(f"SET discarded: {parse_resp_response(resp)}")
    
    resp = send_redis_command(sock, 'DISCARD')
    print(f"DISCARD: {parse_resp_response(resp)}")
    
    # Verify state after DISCARD
    resp = send_redis_command(sock, 'PING')
    print(f"PING after DISCARD: {parse_resp_response(resp)}")
    
    resp = send_redis_command(sock, 'GET', 'counter')
    print(f"GET counter after: {parse_resp_response(resp)} (should be unchanged)")
    
    resp = send_redis_command(sock, 'GET', 'discarded')
    print(f"GET discarded: {parse_resp_response(resp)} (should be null)")
    
    # Test 3: Empty transaction
    print("\n\nTest 3: Empty Transaction")
    print("-" * 40)
    
    resp = send_redis_command(sock, 'MULTI')
    print(f"MULTI: {parse_resp_response(resp)}")
    
    resp = send_redis_command(sock, 'EXEC')
    print(f"EXEC (empty): {parse_resp_response(resp)}")
    
    resp = send_redis_command(sock, 'PING')
    print(f"PING after empty EXEC: {parse_resp_response(resp)}")
    
    # Test 4: Error cases
    print("\n\nTest 4: Error Cases")
    print("-" * 40)
    
    resp = send_redis_command(sock, 'EXEC')
    print(f"EXEC without MULTI: {parse_resp_response(resp)}")
    
    resp = send_redis_command(sock, 'DISCARD')
    print(f"DISCARD without MULTI: {parse_resp_response(resp)}")
    
    resp = send_redis_command(sock, 'MULTI')
    print(f"MULTI: {parse_resp_response(resp)}")
    
    resp = send_redis_command(sock, 'MULTI')
    print(f"Nested MULTI: {parse_resp_response(resp)}")
    
    resp = send_redis_command(sock, 'DISCARD')
    print(f"DISCARD: {parse_resp_response(resp)}")
    
    # Test 5: Mixed command types in transaction
    print("\n\nTest 5: Mixed Commands in Transaction")
    print("-" * 40)
    
    resp = send_redis_command(sock, 'FLUSHDB')
    print(f"FLUSHDB: {parse_resp_response(resp)}")
    
    resp = send_redis_command(sock, 'MULTI')
    print(f"MULTI: {parse_resp_response(resp)}")
    
    resp = send_redis_command(sock, 'SET', 'string_key', 'hello')
    print(f"SET string_key: {parse_resp_response(resp)}")
    
    resp = send_redis_command(sock, 'LPUSH', 'list_key', 'item1')
    print(f"LPUSH list_key: {parse_resp_response(resp)}")
    
    resp = send_redis_command(sock, 'SADD', 'set_key', 'member1')
    print(f"SADD set_key: {parse_resp_response(resp)}")
    
    resp = send_redis_command(sock, 'HSET', 'hash_key', 'field1', 'value1')
    print(f"HSET hash_key: {parse_resp_response(resp)}")
    
    resp = send_redis_command(sock, 'INCR', 'counter')
    print(f"INCR counter: {parse_resp_response(resp)}")
    
    resp = send_redis_command(sock, 'EXEC')
    parsed = parse_resp_response(resp)
    print(f"EXEC: {parsed}")
    if parsed[0] == "array":
        for i, result in enumerate(parsed[1]):
            print(f"  Result {i+1}: {result}")
    
    # Verify all operations succeeded
    resp = send_redis_command(sock, 'GET', 'string_key')
    print(f"GET string_key: {parse_resp_response(resp)}")
    
    resp = send_redis_command(sock, 'LLEN', 'list_key')
    print(f"LLEN list_key: {parse_resp_response(resp)}")
    
    resp = send_redis_command(sock, 'SCARD', 'set_key')
    print(f"SCARD set_key: {parse_resp_response(resp)}")
    
    resp = send_redis_command(sock, 'HGET', 'hash_key', 'field1')
    print(f"HGET hash_key field1: {parse_resp_response(resp)}")
    
    resp = send_redis_command(sock, 'GET', 'counter')
    print(f"GET counter: {parse_resp_response(resp)}")
    
    sock.close()
    
    print("\n" + "=" * 70)
    print("ALL TRANSACTION TESTS COMPLETED")
    print("=" * 70)

if __name__ == "__main__":
    try:
        test_transactions()
    except ConnectionRefusedError:
        print("Error: Could not connect to Redis server on localhost:6379")
        print("Make sure the Redis server is running.")
    except Exception as e:
        print(f"Error: {e}")
        import traceback
        traceback.print_exc()