#!/bin/bash
# wsl_io_test.sh - Test I/O performance difference between Windows and Linux filesystems

echo "WSL I/O Performance Comparison Test"
echo "===================================="
echo ""

# Test file
TEST_FILE="test_io_perf.tmp"
SIZE="1M"

echo "Test 1: Windows Filesystem (/mnt/c/...)"
echo "-----------------------------------------"
cd /mnt/c/Users/Sean/Documents/Ailang/ailang 2>/dev/null || cd /mnt/c/tmp

echo "Writing $SIZE file..."
time_start=$(date +%s.%N)
dd if=/dev/zero of=$TEST_FILE bs=$SIZE count=1 2>/dev/null
time_end=$(date +%s.%N)
write_time=$(echo "scale=3; ($time_end - $time_start) * 1000" | bc)
echo "Write time: ${write_time}ms"

echo "Reading $SIZE file..."
time_start=$(date +%s.%N)
dd if=$TEST_FILE of=/dev/null bs=$SIZE 2>/dev/null
time_end=$(date +%s.%N)
read_time=$(echo "scale=3; ($time_end - $time_start) * 1000" | bc)
echo "Read time: ${read_time}ms"

rm -f $TEST_FILE

echo ""
echo "Test 2: Linux Filesystem (~)"
echo "-----------------------------------------"
cd ~

echo "Writing $SIZE file..."
time_start=$(date +%s.%N)
dd if=/dev/zero of=$TEST_FILE bs=$SIZE count=1 2>/dev/null
time_end=$(date +%s.%N)
write_time=$(echo "scale=3; ($time_end - $time_start) * 1000" | bc)
echo "Write time: ${write_time}ms"

echo "Reading $SIZE file..."
time_start=$(date +%s.%N)
dd if=$TEST_FILE of=/dev/null bs=$SIZE 2>/dev/null
time_end=$(date +%s.%N)
read_time=$(echo "scale=3; ($time_end - $time_start) * 1000" | bc)
echo "Read time: ${read_time}ms"

rm -f $TEST_FILE

echo ""
echo "===================================="
echo "If Windows filesystem is 5-50x slower,"
echo "that's your Redis performance problem!"