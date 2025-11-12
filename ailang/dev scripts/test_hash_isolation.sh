#!/bin/bash
# test_hash_isolation.sh - Test hash commands with detailed output

echo "====================================="
echo "  Testing Hash Commands Step by Step"
echo "====================================="
echo ""

# Clean slate
redis-cli FLUSHDB > /dev/null

# Test 1: Basic HSET
echo "Test 1: HSET"
echo "-------------"
echo "Command: HSET myhash field1 value1"
echo -n "Result: "
redis-cli HSET myhash field1 value1
echo ""

# Test 2: Check key exists
echo "Test 2: Key Exists"
echo "------------------"
echo "Command: EXISTS myhash"
echo -n "Result: "
redis-cli EXISTS myhash
echo ""

# Test 3: Check type
echo "Test 3: Type Check"
echo "------------------"
echo "Command: TYPE myhash"
echo -n "Result: "
redis-cli TYPE myhash
echo ""

# Test 4: Try HGET
echo "Test 4: HGET"
echo "------------"
echo "Command: HGET myhash field1"
echo -n "Result: '"
redis-cli HGET myhash field1
echo "'"
echo ""

# Test 5: Raw RESP Protocol (with timeout and proper connection handling)
echo "Test 5: Raw RESP Protocol"
echo "--------------------------"
echo "Sending: HGET myhash field1"
result=$(echo -e "*3\r\n\$4\r\nHGET\r\n\$6\r\nmyhash\r\n\$6\r\nfield1\r\n" | timeout 1 nc localhost 6379)
echo "Raw response: $result"
echo ""

# Test 6: Compare with working GET/SET
echo "Test 6: Regular GET/SET (for comparison)"
echo "-----------------------------------------"
redis-cli SET testkey testvalue > /dev/null
echo "GET testkey: $(redis-cli GET testkey)"
echo ""

# Test 7: Check if HLEN works
echo "Test 7: HLEN"
echo "------------"
echo "Command: HLEN myhash"
echo -n "Result: "
redis-cli HLEN myhash
echo ""

# Test 8: Check HEXISTS
echo "Test 8: HEXISTS"
echo "---------------"
echo "Command: HEXISTS myhash field1"
echo -n "Result: "
redis-cli HEXISTS myhash field1
echo ""

echo "====================================="
echo "  Analysis"
echo "====================================="
if [ "$(redis-cli HSET test f v 2>/dev/null)" = "1" ] && [ -z "$(redis-cli HGET test f 2>/dev/null)" ]; then
    echo "❌ HSET works but HGET returns empty"
    echo "   Likely issue: HashMap.HGet returns value but response handling fails"
elif [ "$(redis-cli HSET test f v 2>/dev/null)" = "1" ] && [ "$(redis-cli --raw HGET test f 2>/dev/null)" = "v" ]; then
    echo "✅ Hash commands working correctly!"
else
    echo "❓ Unexpected behavior - needs investigation"
fi