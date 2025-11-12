#!/bin/bash
# stress_test_bobs_bank_v2.sh - ACTUALLY WORKS WITH YOUR API

API="http://localhost:8080/anything"
RESULTS_DIR="./stress_test_results"
mkdir -p "$RESULTS_DIR"

echo "========================================"
echo "BOB'S BANK STRESS TEST - WORKING VERSION"
echo "========================================"
echo ""

log() {
    echo "[$(date '+%H:%M:%S')] $1"
}

# Test 1: Create accounts that actually work
test_account_creation() {
    log "TEST 1: Creating 100 test accounts..."
    
    start=$(date +%s.%N)
    
    for i in {1..100}; do
        account_id=$(printf "TEST%03d" $i)
        curl -s -X POST "$API" \
            -H "Content-Type: application/json" \
            -d "{\"program\":\"BANKING\",\"data\":{\"operation\":\"CREATE\",\"account_id\":\"$account_id\",\"account_name\":\"Test User $i\",\"amount\":1000}}" > /dev/null &
        
        # Batch in groups of 10 to avoid overwhelming
        if [ $((i % 10)) -eq 0 ]; then
            wait
        fi
    done
    
    wait
    
    end=$(date +%s.%N)
    duration=$(echo "$end - $start" | bc)
    
    log "✓ Created 100 accounts in ${duration}s"
    echo "100 accounts, ${duration}s" > "$RESULTS_DIR/account_creation.txt"
}

# Test 2: Deposits on accounts that exist
test_deposit_flood() {
    log "TEST 2: 1000 deposits to existing accounts..."
    
    start=$(date +%s.%N)
    success=0
    
    for i in {1..1000}; do
        account_num=$((RANDOM % 100 + 1))
        account_id=$(printf "TEST%03d" $account_num)
        amount=$((RANDOM % 500 + 50))
        
        result=$(curl -s -X POST "$API" \
            -H "Content-Type: application/json" \
            -d "{\"program\":\"BANKING\",\"data\":{\"operation\":\"DEPOSIT\",\"account_id\":\"$account_id\",\"amount\":$amount}}")
        
        if [[ "$result" != *"ERROR"* ]]; then
            success=$((success + 1))
        fi
        
        # Batch in groups of 20
        if [ $((i % 20)) -eq 0 ]; then
            wait
            echo -ne "\r  Progress: $i/1000"
        fi &
    done
    
    wait
    echo ""
    
    end=$(date +%s.%N)
    duration=$(echo "$end - $start" | bc)
    rate=$(echo "1000 / $duration" | bc -l)
    
    log "✓ 1000 deposits: ${success} succeeded in ${duration}s (${rate} tx/sec)"
    echo "1000 deposits, ${success} succeeded, ${duration}s, ${rate} tx/sec" > "$RESULTS_DIR/deposit_flood.txt"
}

# Test 3: Mixed realistic operations
test_mixed_workload() {
    log "TEST 3: Mixed workload (deposit, withdraw, balance checks)..."
    
    start=$(date +%s.%N)
    deposits=0
    withdraws=0
    checks=0
    
    for op_num in {1..500}; do
        account_num=$((RANDOM % 100 + 1))
        account_id=$(printf "TEST%03d" $account_num)
        
        case $((RANDOM % 3)) in
            0) # Deposit
                curl -s -X POST "$API" \
                    -H "Content-Type: application/json" \
                    -d "{\"program\":\"BANKING\",\"data\":{\"operation\":\"DEPOSIT\",\"account_id\":\"$account_id\",\"amount\":100}}" > /dev/null &
                deposits=$((deposits + 1))
                ;;
            1) # Withdraw
                curl -s -X POST "$API" \
                    -H "Content-Type: application/json" \
                    -d "{\"program\":\"BANKING\",\"data\":{\"operation\":\"WITHDRAW\",\"account_id\":\"$account_id\",\"amount\":50}}" > /dev/null &
                withdraws=$((withdraws + 1))
                ;;
            2) # Balance
                curl -s -X POST "$API" \
                    -H "Content-Type: application/json" \
                    -d "{\"program\":\"BANKING\",\"data\":{\"operation\":\"BALANCE\",\"account_id\":\"$account_id\"}}" > /dev/null &
                checks=$((checks + 1))
                ;;
        esac
        
        if [ $((op_num % 25)) -eq 0 ]; then
            wait
            echo -ne "\r  Progress: $op_num/500"
        fi
    done
    
    wait
    echo ""
    
    end=$(date +%s.%N)
    duration=$(echo "$end - $start" | bc)
    rate=$(echo "500 / $duration" | bc -l)
    
    log "✓ 500 mixed ops in ${duration}s (${rate} ops/sec)"
    log "  Deposits: $deposits, Withdraws: $withdraws, Checks: $checks"
    echo "500 mixed ops, ${duration}s, ${rate} ops/sec, D:$deposits W:$withdraws C:$checks" > "$RESULTS_DIR/mixed_workload.txt"
}

# Test 4: Data integrity - concurrent operations on same account
test_race_conditions() {
    log "TEST 4: Race condition test (100 concurrent $10 deposits)..."
    
    account_id="RACE001"
    
    # Create the account
    curl -s -X POST "$API" \
        -H "Content-Type: application/json" \
        -d "{\"program\":\"BANKING\",\"data\":{\"operation\":\"CREATE\",\"account_id\":\"$account_id\",\"account_name\":\"Race Test Account\",\"amount\":0}}" > /dev/null
    
    sleep 1  # Let it persist
    
    start=$(date +%s.%N)
    
    # 100 concurrent $10 deposits
    for i in {1..100}; do
        curl -s -X POST "$API" \
            -H "Content-Type: application/json" \
            -d "{\"program\":\"BANKING\",\"data\":{\"operation\":\"DEPOSIT\",\"account_id\":\"$account_id\",\"amount\":10}}" > /dev/null &
    done
    
    wait
    
    end=$(date +%s.%N)
    duration=$(echo "$end - $start" | bc)
    
    # Check final balance
    balance_response=$(curl -s -X POST "$API" \
        -H "Content-Type: application/json" \
        -d "{\"program\":\"BANKING\",\"data\":{\"operation\":\"BALANCE\",\"account_id\":\"$account_id\"}}")
    
    log "✓ Race test complete in ${duration}s"
    log "Expected balance: \$1000, Response: $balance_response"
    
    echo "100 concurrent deposits, ${duration}s, Response: $balance_response" > "$RESULTS_DIR/race_conditions.txt"
}

# Test 5: Memory usage monitoring
test_memory_usage() {
    log "TEST 5: Memory usage monitoring..."
    
    # Get initial memory
    if [ -f "/proc/$(pgrep JCL_http_gateway)/status" ]; then
        initial_gateway=$(grep VmRSS /proc/$(pgrep JCL_http_gateway)/status | awk '{print $2}')
    else
        initial_gateway=0
    fi
    
    if [ -f "/proc/$(pgrep JCL_daemon)/status" ]; then
        initial_daemon=$(grep VmRSS /proc/$(pgrep JCL_daemon)/status | awk '{print $2}')
    else
        initial_daemon=0
    fi
    
    log "Initial memory: Gateway=${initial_gateway}KB, Daemon=${initial_daemon}KB"
    
    # Run 500 operations
    for i in {1..500}; do
        account_num=$((RANDOM % 100 + 1))
        account_id=$(printf "TEST%03d" $account_num)
        
        curl -s -X POST "$API" \
            -H "Content-Type: application/json" \
            -d "{\"program\":\"BANKING\",\"data\":{\"operation\":\"DEPOSIT\",\"account_id\":\"$account_id\",\"amount\":10}}" > /dev/null &
        
        if [ $((i % 50)) -eq 0 ]; then
            wait
        fi
    done
    
    wait
    sleep 2
    
    # Get final memory
    if [ -f "/proc/$(pgrep JCL_http_gateway)/status" ]; then
        final_gateway=$(grep VmRSS /proc/$(pgrep JCL_http_gateway)/status | awk '{print $2}')
    else
        final_gateway=0
    fi
    
    if [ -f "/proc/$(pgrep JCL_daemon)/status" ]; then
        final_daemon=$(grep VmRSS /proc/$(pgrep JCL_daemon)/status | awk '{print $2}')
    else
        final_daemon=0
    fi
    
    log "Final memory: Gateway=${final_gateway}KB, Daemon=${final_daemon}KB"
    
    gateway_growth=$((final_gateway - initial_gateway))
    daemon_growth=$((final_daemon - initial_daemon))
    
    log "Growth: Gateway=${gateway_growth}KB, Daemon=${daemon_growth}KB"
    
    echo "Gateway: $initial_gateway -> $final_gateway KB (+${gateway_growth}KB)" > "$RESULTS_DIR/memory_usage.txt"
    echo "Daemon: $initial_daemon -> $final_daemon KB (+${daemon_growth}KB)" >> "$RESULTS_DIR/memory_usage.txt"
}

# Test 6: Different programs (not just BANKING)
test_other_programs() {
    log "TEST 6: Testing other programs (TESTAPI, TXNVALID)..."
    
    start=$(date +%s.%N)
    
    # Test TESTAPI
    for i in {1..50}; do
        curl -s -X POST "$API" \
            -H "Content-Type: application/json" \
            -d "{\"program\":\"TESTAPI\",\"data\":{\"name\":\"User$i\",\"message\":\"Test message\"}}" > /dev/null &
    done
    
    # Test TXNVALID with various amounts
    for amount in 100 5000 50000 100000; do
        curl -s -X POST "$API" \
            -H "Content-Type: application/json" \
            -d "{\"program\":\"TXNVALID\",\"data\":{\"amount\":$amount}}" > /dev/null &
    done
    
    wait
    
    end=$(date +%s.%N)
    duration=$(echo "$end - $start" | bc)
    
    log "✓ Other programs tested in ${duration}s"
    echo "54 other program calls, ${duration}s" > "$RESULTS_DIR/other_programs.txt"
}

# Main execution
main() {
    log "Starting comprehensive stress tests..."
    log "Make sure JCL gateway and daemon are running!"
    echo ""
    
    # Check if services are running
    if ! pgrep -x JCL_http_gateway > /dev/null; then
        log "ERROR: JCL_http_gateway not running!"
        exit 1
    fi
    
    if ! pgrep -x JCL_daemon > /dev/null; then
        log "ERROR: JCL_daemon not running!"
        exit 1
    fi
    
    log "Services detected, beginning tests..."
    echo ""
    
    test_account_creation
    echo ""
    
    test_deposit_flood
    echo ""
    
    test_mixed_workload
    echo ""
    
    test_race_conditions
    echo ""
    
    test_memory_usage
    echo ""
    
    test_other_programs
    echo ""
    
    log "========================================"
    log "ALL TESTS COMPLETE"
    log "========================================"
    log "Results saved to: $RESULTS_DIR/"
    
    echo ""
    echo "SUMMARY:"
    cat "$RESULTS_DIR"/*.txt
}

main