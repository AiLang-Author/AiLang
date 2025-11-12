#!/bin/bash
# Kill all AILANG-related processes

echo "=== Finding all running processes ==="

# Show what's running
ps aux | grep -E "redis|cobol|_exec|ailang" | grep -v grep

echo ""
echo "=== Killing Redis servers ==="
pkill -9 -f redis_server

echo "=== Killing COBOL daemons ==="
pkill -9 -f cobol-daemon_exec
pkill -9 -f cobol_daemon

echo "=== Killing test executables ==="
pkill -9 -f test_connect_primitive_exec
pkill -9 -f "_exec$"

echo "=== Killing any processes on common ports ==="
# Find and kill anything on port 6379 (Redis)
fuser -k 6379/tcp 2>/dev/null

# Find and kill anything on port 5432 (PostgreSQL)
fuser -k 5432/tcp 2>/dev/null

# Find and kill anything on port 8080
fuser -k 8080/tcp 2>/dev/null

echo ""
echo "=== Checking what's still listening on ports ==="
ss -tlnp | grep -E "6379|5432|8080"

echo ""
echo "=== Cleanup complete! ==="
echo "Remaining processes:"
ps aux | grep -E "redis|cobol|_exec|ailang" | grep -v grep || echo "None!"