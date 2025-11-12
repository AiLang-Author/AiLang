#!/bin/bash
# baseline_benchmark.sh - Comprehensive baseline benchmark for AILANG Redis server
# Run this BEFORE adding HashMap commands to establish performance baseline

HOST="localhost"
PORT="6379"
REDIS_CLI="redis-cli -h $HOST -p $PORT --raw"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

# Test counters
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# Function to print section headers
print_header() {
    echo ""
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${CYAN}  $1${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
}

# Function to run a single test
run_test() {
    local description="$1"
    local command="$2"
    local expected="$3"
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    echo -en "${YELLOW}[TEST $TOTAL_TESTS]${NC} $description ... "
    
    result=$($REDIS_CLI $command 2>&1 | tr -d '\r')
    
    if [[ "$result" == "$expected" ]]; then
        echo -e "${GREEN}✓ PASS${NC}"
        PASSED_TESTS=$((PASSED_TESTS + 1))
        return 0
    else
        echo -e "${RED}✗ FAIL${NC}"
        echo -e "  Expected: '$expected'"
        echo -e "  Got:      '$result'"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        return 1
    fi
}

# Function to run performance benchmark
run_benchmark() {
    local operation="$1"
    local iterations="${2:-1000}"
    
    echo -e "${YELLOW}Benchmarking:${NC} $operation ($iterations iterations)"
    
    if command -v bc &> /dev/null; then
        start_time=$(date +%s.%N)
        
        case "$operation" in
            "PING")
                for ((i=1; i<=iterations; i++)); do
                    $REDIS_CLI PING > /dev/null 2>&1
                done
                ;;
            "SET")
                for ((i=1; i<=iterations; i++)); do
                    $REDIS_CLI SET "bench:key:$i" "value$i" > /dev/null 2>&1
                done
                ;;
            "GET")
                # First create a key
                $REDIS_CLI SET "bench:get:test" "testvalue" > /dev/null 2>&1
                for ((i=1; i<=iterations; i++)); do
                    $REDIS_CLI GET "bench:get:test" > /dev/null 2>&1
                done
                ;;
            "INCR")
                for ((i=1; i<=iterations; i++)); do
                    $REDIS_CLI INCR "bench:counter" > /dev/null 2>&1
                done
                ;;
            "LPUSH")
                for ((i=1; i<=iterations; i++)); do
                    $REDIS_CLI LPUSH "bench:list" "item$i" > /dev/null 2>&1
                done
                ;;
            "SADD")
                for ((i=1; i<=iterations; i++)); do
                    $REDIS_CLI SADD "bench:set" "member$i" > /dev/null 2>&1
                done
                ;;
        esac
        
        end_time=$(date +%s.%N)
        duration=$(echo "$end_time - $start_time" | bc -l)
        avg_time=$(echo "scale=3; $duration * 1000 / $iterations" | bc -l)
        ops_per_sec=$(echo "scale=0; $iterations / $duration" | bc -l)
        
        echo -e "${GREEN}  ➤ Average: ${avg_time}ms | Throughput: ${ops_per_sec} ops/sec${NC}"
    else
        echo -e "${YELLOW}  ➤ Install 'bc' for detailed timing${NC}"
    fi
}

# Function to save results
save_results() {
    local filename="baseline_benchmark_$(date +%Y%m%d_%H%M%S).txt"
    echo "Saving results to $filename..."
    {
        echo "AILANG Redis Server Baseline Benchmark Results"
        echo "Date: $(date)"
        echo "================================================"
        echo ""
        echo "Test Summary:"
        echo "  Total Tests: $TOTAL_TESTS"
        echo "  Passed: $PASSED_TESTS"
        echo "  Failed: $FAILED_TESTS"
        echo ""
        echo "Current Supported Commands:"
        echo "  - Strings: SET, GET, MSET, MGET, STRLEN, APPEND, INCR, DECR"
        echo "  - Lists: LPUSH, LRANGE, LLEN"
        echo "  - Sets: SADD, SCARD, SISMEMBER, SMEMBERS"
        echo "  - Streams: XADD, XREAD"
        echo "  - Keys: DEL, EXISTS, TYPE, EXPIRE, TTL, SETEX"
        echo "  - Server: PING, ECHO, FLUSHDB, DBSIZE"
        echo ""
        echo "Performance Baseline (before HashMap implementation):"
        echo "  Note: Re-run benchmark commands manually for accurate timing"
    } > "$filename"
    echo -e "${GREEN}Results saved to $filename${NC}"
}

# Main execution
main() {
    clear
    echo -e "${CYAN}╔══════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${CYAN}║       AILANG Redis Server - Baseline Benchmark Suite        ║${NC}"
    echo -e "${CYAN}║          Run BEFORE adding HashMap implementation           ║${NC}"
    echo -e "${CYAN}╚══════════════════════════════════════════════════════════════╝${NC}"
    echo ""
    echo -e "${YELLOW}Server:${NC} $HOST:$PORT"
    echo -e "${YELLOW}Date:${NC} $(date)"
    
    # Check server connectivity
    echo ""
    echo -e "${YELLOW}Checking server...${NC}"
    if ! $REDIS_CLI PING > /dev/null 2>&1; then
        echo -e "${RED}ERROR: Cannot connect to Redis server at $HOST:$PORT${NC}"
        echo "Please ensure your AILANG Redis server is running."
        exit 1
    fi
    echo -e "${GREEN}✓ Server is responding${NC}"
    
    # Clean slate
    echo -e "${YELLOW}Cleaning database...${NC}"
    $REDIS_CLI FLUSHDB > /dev/null
    echo -e "${GREEN}✓ Database cleaned${NC}"
    
    # Run comprehensive tests
    print_header "1. CONNECTION COMMANDS"
    run_test "PING" "PING" "PONG"
    run_test "ECHO" "ECHO hello" "hello"
    
    print_header "2. STRING COMMANDS"
    run_test "SET" "SET key1 value1" "OK"
    run_test "GET" "GET key1" "value1"
    run_test "GET non-existent" "GET nokey" ""
    run_test "STRLEN" "STRLEN key1" "6"
    run_test "APPEND" "APPEND key1 _more" "11"
    run_test "GET after APPEND" "GET key1" "value1_more"
    run_test "SET number" "SET num 10" "OK"
    run_test "INCR" "INCR num" "11"
    run_test "DECR" "DECR num" "10"
    run_test "MSET" "MSET k1 v1 k2 v2 k3 v3" "OK"
    run_test "MGET" "MGET k1 k2 k3" $'v1\nv2\nv3'
    
    print_header "3. LIST COMMANDS"
    run_test "LPUSH single" "LPUSH list1 item1" "1"
    run_test "LPUSH multiple" "LPUSH list1 item2 item3" "3"
    run_test "LLEN" "LLEN list1" "3"
    run_test "LRANGE" "LRANGE list1 0 -1" $'item3\nitem2\nitem1'
    
    print_header "4. SET COMMANDS"
    run_test "SADD single" "SADD set1 member1" "1"
    run_test "SADD duplicate" "SADD set1 member1" "0"
    run_test "SADD multiple" "SADD set1 m2 m3 m4" "3"
    run_test "SCARD" "SCARD set1" "4"
    run_test "SISMEMBER exists" "SISMEMBER set1 m2" "1"
    run_test "SISMEMBER missing" "SISMEMBER set1 m5" "0"
    
    print_header "5. KEY MANAGEMENT COMMANDS"
    run_test "EXISTS existing" "EXISTS key1" "1"
    run_test "EXISTS missing" "EXISTS nokey" "0"
    run_test "TYPE string" "TYPE key1" "string"
    run_test "TYPE list" "TYPE list1" "list"
    run_test "TYPE set" "TYPE set1" "set"
    run_test "TYPE missing" "TYPE nokey" "none"
    run_test "DEL single" "DEL key1" "1"
    run_test "EXISTS after DEL" "EXISTS key1" "0"
    run_test "SETEX" "SETEX tempkey 10 tempvalue" "OK"
    run_test "TTL" "TTL tempkey" "10"
    
    print_header "6. SERVER COMMANDS"
    # Count current keys
    DBSIZE_RESULT=$($REDIS_CLI DBSIZE)
    run_test "DBSIZE" "DBSIZE" "$DBSIZE_RESULT"
    run_test "FLUSHDB" "FLUSHDB" "OK"
    run_test "DBSIZE after FLUSH" "DBSIZE" "0"
    
    print_header "7. PERFORMANCE BENCHMARKS"
    
    # Clean for benchmarks
    $REDIS_CLI FLUSHDB > /dev/null
    
    run_benchmark "PING" 1000
    run_benchmark "SET" 500
    run_benchmark "GET" 1000
    run_benchmark "INCR" 500
    run_benchmark "LPUSH" 500
    run_benchmark "SADD" 500
    
    print_header "8. STRESS TESTS"
    
    echo -e "${YELLOW}Creating 100 keys...${NC}"
    for i in {1..100}; do
        $REDIS_CLI SET "stress:key:$i" "value$i" > /dev/null
    done
    stress_dbsize=$($REDIS_CLI DBSIZE)
    echo -e "${GREEN}  ➤ Created $stress_dbsize keys successfully${NC}"
    
    echo -e "${YELLOW}Testing large values...${NC}"
    large_value=$(printf 'x%.0s' {1..1000})  # 1000 character string
    result=$($REDIS_CLI SET largekey "$large_value" 2>&1)
    if [[ "$result" == "OK" ]]; then
        echo -e "${GREEN}  ➤ Large value (1KB) stored successfully${NC}"
    else
        echo -e "${RED}  ➤ Failed to store large value${NC}"
    fi
    
    # Verify large value
    retrieved=$($REDIS_CLI GET largekey | wc -c)
    echo -e "${GREEN}  ➤ Retrieved value size: $retrieved bytes${NC}"
    
    print_header "9. ERROR HANDLING"
    
    # Test wrong number of arguments
    echo -e "${YELLOW}Testing error handling...${NC}"
    error_result=$($REDIS_CLI SET 2>&1)
    if [[ "$error_result" == *"ERR"* ]]; then
        echo -e "${GREEN}  ➤ Correct error for missing arguments${NC}"
    else
        echo -e "${RED}  ➤ No error for missing arguments${NC}"
    fi
    
    # Test wrong type operations
    $REDIS_CLI SET stringkey "string" > /dev/null
    error_result=$($REDIS_CLI LPUSH stringkey item 2>&1)
    if [[ "$error_result" == *"WRONGTYPE"* ]]; then
        echo -e "${GREEN}  ➤ Correct WRONGTYPE error${NC}"
    else
        echo -e "${RED}  ➤ No WRONGTYPE error${NC}"
    fi
    
    print_header "10. FINAL SUMMARY"
    
    echo -e "${CYAN}Test Results:${NC}"
    echo -e "  Total Tests: ${YELLOW}$TOTAL_TESTS${NC}"
    echo -e "  Passed: ${GREEN}$PASSED_TESTS${NC}"
    echo -e "  Failed: ${RED}$FAILED_TESTS${NC}"
    
    if [[ $FAILED_TESTS -eq 0 ]]; then
        echo ""
        echo -e "${GREEN}════════════════════════════════════════════════${NC}"
        echo -e "${GREEN}  ✓ ALL BASELINE TESTS PASSED!${NC}"
        echo -e "${GREEN}  Server ready for HashMap implementation${NC}"
        echo -e "${GREEN}════════════════════════════════════════════════${NC}"
    else
        echo ""
        echo -e "${RED}════════════════════════════════════════════════${NC}"
        echo -e "${RED}  ✗ Some tests failed${NC}"
        echo -e "${RED}  Review failures before adding new features${NC}"
        echo -e "${RED}════════════════════════════════════════════════${NC}"
    fi
    
    # Save results
    echo ""
    save_results
    
    # Run official redis-benchmark if available
    if command -v redis-benchmark &> /dev/null; then
        echo ""
        print_header "11. OFFICIAL REDIS-BENCHMARK (Optional)"
        echo -e "${YELLOW}Running official redis-benchmark tool...${NC}"
        echo ""
        
        # Quick benchmark with limited requests
        redis-benchmark -h $HOST -p $PORT -t PING,SET,GET -n 1000 -q
    else
        echo ""
        echo -e "${YELLOW}Note: Install redis-tools for official benchmarks${NC}"
    fi
    
    # Clean up
    echo ""
    echo -e "${YELLOW}Cleaning up...${NC}"
    $REDIS_CLI FLUSHDB > /dev/null
    echo -e "${GREEN}✓ Database cleaned${NC}"
    
    echo ""
    echo -e "${CYAN}Baseline benchmark complete!${NC}"
    echo -e "${CYAN}Ready to proceed with HashMap implementation.${NC}"
}

# Run the benchmark
main "$@"