#!/bin/bash
# test_hashmap_chunks.sh - Test each chunk as you add it

echo "Testing HashMap Integration"
echo "==========================="

# Test Chunk 1: Basic setup
echo ""
echo "CHUNK 1: Testing basic setup"
echo "-----------------------------"
redis-cli SET testkey "testvalue" | grep -q "OK" && echo "✓ SET works" || echo "✗ SET failed"
redis-cli TYPE testkey | grep -q "string" && echo "✓ TYPE works" || echo "✗ TYPE failed"
redis-cli DEL testkey | grep -q "1" && echo "✓ DEL works" || echo "✗ DEL failed"
redis-cli FLUSHDB | grep -q "OK" && echo "✓ FLUSHDB works" || echo "✗ FLUSHDB failed"

# Test Chunk 2: HSET/HGET
echo ""
echo "CHUNK 2: Testing HSET/HGET"
echo "-----------------------------"
result=$(redis-cli HSET user:1 name "Alice" 2>/dev/null)
if [[ "$result" == "1" ]]; then
    echo "✓ HSET new field returns 1"
else
    echo "✗ HSET failed (got: $result)"
fi

result=$(redis-cli HGET user:1 name 2>/dev/null)
if [[ "$result" == "Alice" ]]; then
    echo "✓ HGET returns correct value"
else
    echo "✗ HGET failed (got: $result)"
fi

result=$(redis-cli HSET user:1 name "Bob" 2>/dev/null)
if [[ "$result" == "0" ]]; then
    echo "✓ HSET update returns 0"
else
    echo "✗ HSET update failed (got: $result)"
fi

result=$(redis-cli HGET user:1 missing 2>/dev/null)
if [[ -z "$result" ]]; then
    echo "✓ HGET missing field returns nil"
else
    echo "✗ HGET missing failed (got: $result)"
fi

# Test TYPE on hash
result=$(redis-cli TYPE user:1 2>/dev/null)
if [[ "$result" == "hash" ]]; then
    echo "✓ TYPE returns 'hash' for hash keys"
else
    echo "✗ TYPE failed (got: $result)"
fi

# Test Chunk 3: HLEN/HEXISTS/HDEL
echo ""
echo "CHUNK 3: Testing HLEN/HEXISTS/HDEL"
echo "-----------------------------"

redis-cli HSET user:1 age "30" > /dev/null 2>&1
redis-cli HSET user:1 email "alice@example.com" > /dev/null 2>&1

result=$(redis-cli HLEN user:1 2>/dev/null)
if [[ "$result" == "3" ]]; then
    echo "✓ HLEN returns correct count"
else
    echo "✗ HLEN failed (got: $result)"
fi

result=$(redis-cli HEXISTS user:1 name 2>/dev/null)
if [[ "$result" == "1" ]]; then
    echo "✓ HEXISTS returns 1 for existing field"
else
    echo "✗ HEXISTS existing failed (got: $result)"
fi

result=$(redis-cli HEXISTS user:1 missing 2>/dev/null)
if [[ "$result" == "0" ]]; then
    echo "✓ HEXISTS returns 0 for missing field"
else
    echo "✗ HEXISTS missing failed (got: $result)"
fi

result=$(redis-cli HDEL user:1 email 2>/dev/null)
if [[ "$result" == "1" ]]; then
    echo "✓ HDEL returns 1 for deleted field"
else
    echo "✗ HDEL failed (got: $result)"
fi

result=$(redis-cli HDEL user:1 missing 2>/dev/null)
if [[ "$result" == "0" ]]; then
    echo "✓ HDEL returns 0 for missing field"
else
    echo "✗ HDEL missing failed (got: $result)"
fi

echo ""
echo "==========================="
echo "Basic HashMap tests complete!"