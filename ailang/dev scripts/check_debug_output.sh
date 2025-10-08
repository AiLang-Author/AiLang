#!/bin/bash
# check_debug_output.sh - Find all PrintMessage calls in redis_server.ailang

echo "Checking for debug output in redis_server.ailang..."
echo "================================================"

# Count total PrintMessage calls
total_prints=$(grep -c "PrintMessage" redis_server.ailang 2>/dev/null || echo 0)
echo "Total PrintMessage calls: $total_prints"

# Count PrintMessage calls inside Debug blocks
# This is approximate - counts lines between Debug{ and }
in_debug=$(awk '/Debug\(.*level=/{flag=1} flag && /PrintMessage/{count++} /^[[:space:]]*\}/ && flag{flag=0} END{print count+0}' redis_server.ailang)
echo "PrintMessage inside Debug blocks: ~$in_debug"

# Find PrintMessage calls NOT in Debug blocks
echo ""
echo "PrintMessage calls outside Debug blocks:"
echo "----------------------------------------"

awk '
/Debug\(.*level=/{in_debug=1; brace_count=0}
in_debug && /{/{brace_count++}
in_debug && /}/{brace_count--; if(brace_count==0) in_debug=0}
!in_debug && /PrintMessage/ {print NR": "$0}
' redis_server.ailang | head -20

echo ""
echo "First 20 potential non-debug PrintMessage calls shown."
echo ""

# Check for specific problem areas
echo "Checking for error messages outside debug blocks:"
grep -n "ERROR\|Error\|error" redis_server.ailang | grep PrintMessage | head -10

echo ""
echo "Compilation command check:"
echo "--------------------------"
echo "To compile WITHOUT debug output:"
echo "  python3 main.py redis_server.ailang"
echo ""
echo "To compile WITH debug output:"
echo "  python3 main.py -D redis_server.ailang    # Level 1"
echo "  python3 main.py -D2 redis_server.ailang   # Level 2"
echo "  python3 main.py -D3 redis_server.ailang   # Level 3"

# Quick performance test
echo ""
echo "Quick PING test (5 pings):"
for i in {1..5}; do
    start=$(date +%s.%N)
    redis-cli PING > /dev/null 2>&1
    end=$(date +%s.%N)
    duration=$(echo "scale=3; ($end - $start) * 1000" | bc 2>/dev/null || echo "N/A")
    echo "  PING $i: ${duration}ms"
done