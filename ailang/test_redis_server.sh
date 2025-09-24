#!/bin/bash
# test_redis_server.sh - Comprehensive test and benchmark suite for AILANG Redis server

# --- Configuration ---
HOST="localhost"
PORT="6379"
REDIS_CLI="redis-cli -h $HOST -p $PORT --raw"

# --- Colors for output ---
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# --- Test counters ---
PASSED=0
FAILED=0
TEST_NO=0

# --- Helper Functions ---

# The core test function.
# Compares command output with an expected string. Handles multi-line output.
test_command() {
	local description="$1"
	local command="$2"
	local expected_raw="$3"

	TEST_NO=$((TEST_NO + 1))
	echo -en "${YELLOW}[TEST $TEST_NO]${NC} Testing: $description ... "

	# Execute the command. Using a subshell with the pipe allows us to test commands like `SMEMBERS | sort`.
	result_raw=$(/bin/bash -c "$REDIS_CLI $command" 2>&1 | tr -d '\r')

	# Normalize by removing leading/trailing whitespace from each line.
	# This makes comparisons more robust to slight formatting differences.
	# For raw output, (nil) is an empty string, which awk will collapse, so we handle it specially.
	if [[ -z "$result_raw" && "$expected_raw" == "" ]]; then
		result_normalized=""
		expected_normalized=""
	else
	result_normalized=$(echo -n "$result_raw" | awk '{$1=$1};1')
	expected_normalized=$(echo -n "$expected_raw" | awk '{$1=$1};1')
	fi

	if [[ "$result_normalized" == "$expected_normalized" ]]; then
		echo -e "${GREEN}PASS${NC}"
		PASSED=$((PASSED + 1))
	else
		echo -e "${RED}FAIL${NC}"
		echo -e "  ${YELLOW}COMMAND:${NC}  $REDIS_CLI $command"
		echo -e "  ${GREEN}EXPECTED:${NC}\n---\n$expected_raw\n---"
		echo -e "  ${RED}GOT:${NC}\n---\n$result_raw\n---"
		FAILED=$((FAILED + 1))
	fi
}

# Function to run a benchmark and check if it completes successfully.
test_benchmark() {
	local description="$1"
	local benchmark_cmd="$2"

	echo ""
	echo -e "${YELLOW}--- Running Benchmark: $description ---${NC}"

	# We just check for successful execution and that it doesn't crash.
	output=$(redis-benchmark -h $HOST -p $PORT $benchmark_cmd 2>&1)

	if [[ $? -eq 0 && "$output" == *"requests per second"* ]]; then
		echo -e "${GREEN}Benchmark completed successfully.${NC}"
		echo "$output" | grep "requests per second"
	else
		echo -e "${RED}Benchmark FAILED to run or crashed.${NC}"
		echo "  COMMAND: redis-benchmark -h $HOST -p $PORT $benchmark_cmd"
		echo "  OUTPUT:"
		echo "$output"
	fi
	echo -e "${YELLOW}------------------------------------------${NC}"
}

# --- Main Test Execution ---

echo "=========================================="
echo "  AILANG Redis Server Test Suite"
echo "=========================================="
echo ""

# 1. Initial Setup: Clean the database
echo "--- Preparing for tests (FLUSHDB) ---"
$REDIS_CLI FLUSHDB >/dev/null
echo ""

# 2. Run All Tests

echo "--- Testing Connection Commands ---"
test_command "PING" "PING" "PONG"
test_command "ECHO hello" "ECHO hello" "hello"
echo ""

echo "--- Testing String Commands ---"
test_command "SET key1 val1" "SET key1 val1" "OK"
test_command "GET key1" "GET key1" "val1"
test_command "GET non-existent key" "GET key-does-not-exist" ""
test_command "STRLEN of key1" "STRLEN key1" "4"
test_command "APPEND to key1" "APPEND key1 ' more'" "9"
test_command "GET appended key1" "GET key1" "val1 more"
test_command "SET counter 10" "SET counter 10" "OK"
test_command "INCR counter" "INCR counter" "11"
test_command "DECR counter" "DECR counter" "10"
echo ""

echo "--- Testing Multi-key String Commands (MSET/MGET) ---"
test_command "MSET k1 v1 k2 v2" "MSET mkey1 v1 mkey2 v2" "OK"
test_command "MGET all keys" "MGET mkey1 mkey2" $'v1\nv2'
test_command "MGET with non-existent key" "MGET mkey1 no-key mkey2" $'v1\n\nv2'
echo ""

echo "--- Testing Key Commands (DEL, EXISTS, TYPE, EXPIRE, TTL) ---"
test_command "EXISTS counter" "EXISTS counter" "1"
test_command "DEL key1" "DEL key1" "1"
test_command "EXISTS key1 after DEL" "EXISTS key1" "0"
test_command "TYPE of string key" "TYPE counter" "string"
test_command "TYPE of non-existent key" "TYPE no-key" "none"
test_command "SETEX with 10s expiry" "SETEX expiry_key 10 'will expire'" "OK"
test_command "TTL of expiry_key" "TTL expiry_key" "10"
echo ""

echo "--- Testing List Commands (LPUSH, RPUSH, LLEN, LRANGE) ---"
test_command "LPUSH to mylist" "LPUSH mylist c b a" "3"
test_command "LLEN of mylist" "LLEN mylist" "3"
test_command "LRANGE of mylist" "LRANGE mylist 0 -1" $'a\nb\nc'
test_command "TYPE of list key" "TYPE mylist" "list"
echo ""

echo "--- Testing Set Commands (SADD, SMEMBERS, SCARD, SISMEMBER) ---"
test_command "SADD to myset" "SADD myset a b c a" "3"
test_command "SCARD of myset" "SCARD myset" "3"
test_command "SISMEMBER a in myset" "SISMEMBER myset a" "1"
test_command "SISMEMBER d in myset" "SISMEMBER myset d" "0"
test_command "SMEMBERS of myset (sorted)" "'SMEMBERS myset' | sort" $'a\nb\nc'
test_command "TYPE of set key" "TYPE myset" "set"
echo ""

echo "--- Testing Stream Commands (XADD, XREAD) ---"
test_command "XADD to mystream" "XADD mystream '*' f1 v1" "1-0"
test_command "XADD to mystream again" "XADD mystream '*' f2 v2" "2-0"
test_command "TYPE of stream key" "TYPE mystream" "stream"
test_command "XREAD from mystream" "XREAD STREAMS mystream 1-0" $'2-0\nf2\nv2'
echo ""

echo "--- Testing Server Commands ---"
# Count of keys: counter, mkey1, mkey2, expiry_key, mylist, myset, mystream = 7
test_command "DBSIZE" "DBSIZE" "7"
test_command "FLUSHDB" "FLUSHDB" "OK"
test_command "DBSIZE after FLUSHDB" "DBSIZE" "0"
echo ""

# 3. Final Summary
echo "=========================================="
echo "  Test Summary"
echo "=========================================="
echo "Total tests: $TEST_NO"
echo -e "  ${GREEN}Passed: $PASSED${NC}"
echo -e "  ${RED}Failed: $FAILED${NC}"
echo "=========================================="

# 4. Run Benchmarks
if [[ $FAILED -eq 0 ]]; then
	echo ""
	echo "All tests passed. Proceeding to benchmarks."
	test_benchmark "PING command" "-n 10000 -c 50 PING"
	test_benchmark "SET/GET commands" "-t SET,GET -n 10000 -c 50 -r 10000"
	test_benchmark "LPUSH/LRANGE" "-n 10000 -c 50 -t LPUSH,LRANGE"
else
	echo ""
	echo "Skipping benchmarks due to test failures."
fi

# Exit with status code based on test failures
exit $FAILED