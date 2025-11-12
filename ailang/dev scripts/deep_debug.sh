#!/bin/bash
# deep_debug.sh - Find the real performance bottleneck

echo "========================================"
echo "  Deep Performance Debugging"
echo "========================================"

# 1. Check if server is actually compiled without debug
echo ""
echo "1. Checking binary for debug symbols..."
echo "----------------------------------------"
strings redis_server_exec | grep -c "DEBUG" || echo "0 DEBUG strings found"
strings redis_server_exec | grep -c "PrintMessage" || echo "0 PrintMessage strings found"
file_size=$(stat -c%s redis_server_exec)
echo "Binary size: $file_size bytes"
if [ $file_size -gt 150000 ]; then
    echo "⚠️ Binary seems large (>150KB) - might have debug code"
else
    echo "✅ Binary size looks normal"
fi

# 2. Check if there's file I/O on every request
echo ""
echo "2. Monitoring file access during PING..."
echo "----------------------------------------"
# Start server in background
pkill -f redis_server_exec 2>/dev/null
sleep 1
./redis_server_exec > server.log 2>&1 &
SERVER_PID=$!
sleep 2

# Use strace to monitor system calls during PING
echo "Running strace on 5 PINGs..."
for i in {1..5}; do
    strace -c -p $SERVER_PID 2>&1 &
    STRACE_PID=$!
    sleep 0.1
    redis-cli PING > /dev/null 2>&1
    sleep 0.1
    kill $STRACE_PID 2>/dev/null
    wait $STRACE_PID 2>/dev/null
done 2>&1 | grep -E "write|read|open|close" | head -10

# 3. Check for memory allocation issues
echo ""
echo "3. Testing for memory allocation overhead..."
echo "----------------------------------------"
# Monitor with time command
{ time redis-cli PING > /dev/null 2>&1; } 2>&1 | grep real
{ time redis-cli SET testkey testvalue > /dev/null 2>&1; } 2>&1 | grep real
{ time redis-cli GET testkey > /dev/null 2>&1; } 2>&1 | grep real

# 4. Test raw TCP connection speed
echo ""
echo "4. Testing raw TCP connection speed..."
echo "----------------------------------------"
python3 << 'EOF'
import socket
import time

sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
sock.connect(('localhost', 6379))

# Test raw RESP protocol PING
ping_cmd = b"*1\r\n$4\r\nPING\r\n"

times = []
for i in range(10):
    start = time.perf_counter()
    sock.send(ping_cmd)
    response = sock.recv(1024)
    end = time.perf_counter()
    times.append((end - start) * 1000)
    
    if i == 0:
        print(f"First response: {response}")

avg = sum(times) / len(times)
print(f"Average raw PING: {avg:.3f}ms")
print(f"Min: {min(times):.3f}ms")
print(f"Max: {max(times):.3f}ms")

sock.close()
EOF

# 5. Check CPU usage
echo ""
echo "5. CPU usage during operations..."
echo "----------------------------------------"
top -b -n 1 -p $SERVER_PID | tail -2

# 6. Check for blocking operations
echo ""
echo "6. Testing for blocking operations..."
echo "----------------------------------------"
# Send multiple rapid PINGs
start_time=$(date +%s.%N)
for i in {1..20}; do
    redis-cli PING > /dev/null 2>&1 &
done
wait
end_time=$(date +%s.%N)
total_time=$(echo "scale=3; ($end_time - $start_time) * 1000" | bc)
echo "20 parallel PINGs took: ${total_time}ms"
avg_time=$(echo "scale=3; $total_time / 20" | bc)
echo "Average per PING: ${avg_time}ms"

# 7. Check server.log for any output
echo ""
echo "7. Checking server output..."
echo "----------------------------------------"
if [ -s server.log ]; then
    echo "Server log (first 10 lines):"
    head -10 server.log
else
    echo "No server output (good!)"
fi

# Kill the server
kill $SERVER_PID 2>/dev/null

echo ""
echo "========================================"
echo "  Analysis Summary"
echo "========================================"
echo ""
echo "If you see:"
echo "- Many file operations per PING -> File I/O issue"
echo "- High CPU usage -> Computation issue"
echo "- Server output -> PrintMessage still active"
echo "- Parallel PINGs slow -> Blocking/serialization issue"
echo ""

# 8. Alternative: Try a minimal test server
echo "8. Creating minimal test server for comparison..."
echo "----------------------------------------"
cat > minimal_server.c << 'EOF'
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>

int main() {
    int server_fd = socket(AF_INET, SOCK_STREAM, 0);
    struct sockaddr_in addr = {0};
    addr.sin_family = AF_INET;
    addr.sin_port = htons(6380);
    addr.sin_addr.s_addr = INADDR_ANY;
    
    bind(server_fd, (struct sockaddr*)&addr, sizeof(addr));
    listen(server_fd, 1);
    printf("Minimal server on port 6380\n");
    
    while(1) {
        int client = accept(server_fd, NULL, NULL);
        char buf[1024];
        int n = read(client, buf, sizeof(buf));
        if(n > 0 && strstr(buf, "PING")) {
            write(client, "+PONG\r\n", 7);
        }
        close(client);
    }
    return 0;
}
EOF

gcc -O2 minimal_server.c -o minimal_server 2>/dev/null
if [ -f minimal_server ]; then
    ./minimal_server &
    MINIMAL_PID=$!
    sleep 1
    
    echo "Testing minimal C server on port 6380..."
    start=$(date +%s.%N)
    echo -e "*1\r\n\$4\r\nPING\r\n" | nc localhost 6380 > /dev/null
    end=$(date +%s.%N)
    c_time=$(echo "scale=3; ($end - $start) * 1000" | bc)
    echo "Minimal C server PING: ${c_time}ms"
    
    kill $MINIMAL_PID 2>/dev/null
    echo "This is the baseline TCP performance on your system"
fi

rm -f minimal_server minimal_server.c server.log