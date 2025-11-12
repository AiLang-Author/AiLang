#!/bin/bash
# debug_hash_commands.sh - Debug why HGET and other commands are failing

echo "====================================="
echo "  Debugging Hash Commands"
echo "====================================="
echo ""

# 1. Test HSET/HGET directly with verbose output
echo "1. Testing HSET/HGET with verbose output:"
echo "-----------------------------------------"
echo "Setting user:test name John..."
redis-cli HSET user:test name John
echo ""

echo "Getting user:test name..."
result=$(redis-cli HGET user:test name 2>&1)
echo "Raw result: '$result'"
echo "Result length: ${#result}"
echo ""

# 2. Check if the key exists and its type
echo "2. Checking key existence and type:"
echo "------------------------------------"
echo "EXISTS user:test:"
redis-cli EXISTS user:test

echo "TYPE user:test:"
redis-cli TYPE user:test
echo ""

# 3. Try HLEN to see if hash has entries
echo "3. Testing HLEN:"
echo "----------------"
echo "HLEN user:test:"
redis-cli HLEN user:test
echo ""

# 4. Check if it's a wrapper/type issue
echo "4. Testing with fresh key:"
echo "--------------------------"
redis-cli DEL testkey 2>/dev/null
echo "HSET testkey field1 value1:"
redis-cli HSET testkey field1 value1

echo "HGET testkey field1:"
redis-cli HGET testkey field1
echo ""

# 5. Test if regular Redis commands still work
echo "5. Testing regular commands:"
echo "-----------------------------"
echo "SET normalkey normalvalue:"
redis-cli SET normalkey normalvalue

echo "GET normalkey:"
redis-cli GET normalkey
echo ""

# 6. Check server logs for errors
echo "6. Checking for compilation warnings:"
echo "--------------------------------------"
grep -n "HGET\|HSET\|HashMap" redis_server.ailang | head -5
echo ""

# 7. Look for the actual HGET implementation
echo "7. Checking HGET implementation:"
echo "---------------------------------"
echo "Looking for HGET command handler..."
awk '/is_hget = StringCompare.*"HGET"/,/command_handled = 1/' redis_server.ailang | head -20
echo ""

echo "====================================="
echo "  Diagnosis"
echo "====================================="
echo ""
echo "If HSET returns 1 but HGET returns empty, possible issues:"
echo "1. HGET is not retrieving from the correct hash pointer"
echo "2. HashMap.HGet is returning 0 instead of the value"
echo "3. RESP.BulkString(0) is being called for non-null values"
echo "4. The value is not being properly stored in the hash"