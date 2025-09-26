#!/bin/bash
# test_redis_hash.sh - Test Redis hash commands in AILANG Redis server

HOST="localhost"
PORT="6379"
REDIS_CLI="redis-cli -h $HOST -p $PORT --raw"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo "=========================================="
echo "  Testing Redis Hash Commands"
echo "=========================================="

# Clean slate
echo "Preparing tests..."
$REDIS_CLI FLUSHDB

echo ""
echo "--- Basic Hash Operations ---"

# HSET/HGET tests
echo -en "${YELLOW}[TEST 1]${NC} HSET user:1 name Alice ... "
result=$($REDIS_CLI HSET user:1 name Alice)
if [[ "$result" == "1" ]]; then
    echo -e "${GREEN}PASS${NC} (returned 1 for new field)"
else
    echo -e "${RED}FAIL${NC} (expected 1, got $result)"
fi

echo -en "${YELLOW}[TEST 2]${NC} HGET user:1 name ... "
result=$($REDIS_CLI HGET user:1 name)
if [[ "$result" == "Alice" ]]; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC} (expected Alice, got $result)"
fi

echo -en "${YELLOW}[TEST 3]${NC} HSET user:1 name Bob (update) ... "
result=$($REDIS_CLI HSET user:1 name Bob)
if [[ "$result" == "0" ]]; then
    echo -e "${GREEN}PASS${NC} (returned 0 for update)"
else
    echo -e "${RED}FAIL${NC} (expected 0, got $result)"
fi

echo -en "${YELLOW}[TEST 4]${NC} HGET non-existent field ... "
result=$($REDIS_CLI HGET user:1 missing)
if [[ -z "$result" ]]; then
    echo -e "${GREEN}PASS${NC} (nil)"
else
    echo -e "${RED}FAIL${NC} (expected nil, got $result)"
fi

echo ""
echo "--- Multiple Field Operations ---"

echo -en "${YELLOW}[TEST 5]${NC} HMSET multiple fields ... "
result=$($REDIS_CLI HMSET user:2 name Charlie age 30 email charlie@example.com)
if [[ "$result" == "OK" ]]; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC} (expected OK, got $result)"
fi

echo -en "${YELLOW}[TEST 6]${NC} HMGET multiple fields ... "
result=$($REDIS_CLI HMGET user:2 name age missing email)
expected=$'Charlie\n30\n\ncharlie@example.com'
if [[ "$result" == "$expected" ]]; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    echo "Expected: $expected"
    echo "Got: $result"
fi

echo ""
echo "--- Hash Management ---"

echo -en "${YELLOW}[TEST 7]${NC} HLEN user:2 ... "
result=$($REDIS_CLI HLEN user:2)
if [[ "$result" == "3" ]]; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC} (expected 3, got $result)"
fi

echo -en "${YELLOW}[TEST 8]${NC} HEXISTS user:2 email ... "
result=$($REDIS_CLI HEXISTS user:2 email)
if [[ "$result" == "1" ]]; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC} (expected 1, got $result)"
fi

echo -en "${YELLOW}[TEST 9]${NC} HDEL user:2 email ... "
result=$($REDIS_CLI HDEL user:2 email)
if [[ "$result" == "1" ]]; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC} (expected 1, got $result)"
fi

echo -en "${YELLOW}[TEST 10]${NC} HDEL non-existent field ... "
result=$($REDIS_CLI HDEL user:2 missing)
if [[ "$result" == "0" ]]; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC} (expected 0, got $result)"
fi

echo ""
echo "--- Enumeration Commands ---"

echo -en "${YELLOW}[TEST 11]${NC} HKEYS user:2 ... "
result=$($REDIS_CLI HKEYS user:2 | sort)
expected=$'age\nname'
if [[ "$result" == "$expected" ]]; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    echo "Expected (sorted): $expected"
    echo "Got (sorted): $result"
fi

echo -en "${YELLOW}[TEST 12]${NC} HVALS user:2 ... "
# Note: values may come in any order
result=$($REDIS_CLI HVALS user:2)
if [[ "$result" == *"Charlie"* ]] && [[ "$result" == *"30"* ]]; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC} (expected to contain Charlie and 30)"
fi

echo -en "${YELLOW}[TEST 13]${NC} HGETALL user:2 ... "
result=$($REDIS_CLI HGETALL user:2)
# Should contain name, Charlie, age, 30 in some order
if [[ "$result" == *"name"* ]] && [[ "$result" == *"Charlie"* ]] && [[ "$result" == *"age"* ]] && [[ "$result" == *"30"* ]]; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
fi

echo ""
echo "--- Increment Operations ---"

echo -en "${YELLOW}[TEST 14]${NC} HINCRBY new counter by 5 ... "
result=$($REDIS_CLI HINCRBY counter:1 visits 5)
if [[ "$result" == "5" ]]; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC} (expected 5, got $result)"
fi

echo -en "${YELLOW}[TEST 15]${NC} HINCRBY existing by 3 ... "
result=$($REDIS_CLI HINCRBY counter:1 visits 3)
if [[ "$result" == "8" ]]; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC} (expected 8, got $result)"
fi

echo -en "${YELLOW}[TEST 16]${NC} HINCRBY by negative ... "
result=$($REDIS_CLI HINCRBY counter:1 visits -2)
if [[ "$result" == "6" ]]; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC} (expected 6, got $result)"
fi

echo ""
echo "--- Type Checking ---"

echo -en "${YELLOW}[TEST 17]${NC} TYPE of hash key ... "
result=$($REDIS_CLI TYPE user:1)
if [[ "$result" == "hash" ]]; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC} (expected hash, got $result)"
fi

echo ""
echo "--- Wrong Type Errors ---"

$REDIS_CLI SET string_key "I am a string" > /dev/null

echo -en "${YELLOW}[TEST 18]${NC} HGET on string key ... "
result=$($REDIS_CLI HGET string_key field 2>&1)
if [[ "$result" == *"WRONGTYPE"* ]]; then
    echo -e "${GREEN}PASS${NC} (correct error)"
else
    echo -e "${RED}FAIL${NC} (expected WRONGTYPE error)"
fi

echo ""
echo "--- Cleanup ---"

echo -en "${YELLOW}[TEST 19]${NC} DEL hash key ... "
result=$($REDIS_CLI DEL user:1)
if [[ "$result" == "1" ]]; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC} (expected 1, got $result)"
fi

echo -en "${YELLOW}[TEST 20]${NC} EXISTS after DEL ... "
result=$($REDIS_CLI EXISTS user:1)
if [[ "$result" == "0" ]]; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC} (expected 0, got $result)"
fi

echo ""
echo "=========================================="
echo "  Hash Command Tests Complete!"
echo "=========================================="

# Performance benchmark
echo ""
echo "--- Performance Benchmark ---"
echo "Running 1000 HSET operations..."
start_time=$(date +%s.%N)
for i in {1..1000}; do
    $REDIS_CLI HSET bench:hash field$i value$i > /dev/null
done
end_time=$(date +%s.%N)
duration=$(echo "$end_time - $start_time" | bc -l 2>/dev/null || echo "N/A")
if [[ "$duration" != "N/A" ]]; then
    avg=$(echo "scale=3; $duration * 1000 / 1000" | bc -l)
    echo -e "${GREEN}Average HSET time: ${avg}ms${NC}"
fi

hlen=$($REDIS_CLI HLEN bench:hash)
echo -e "${GREEN}Final hash size: $hlen fields${NC}"

# Cleanup
$REDIS_CLI FLUSHDB > /dev/null
echo "Database cleaned."