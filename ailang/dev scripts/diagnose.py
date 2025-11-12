#!/usr/bin/env python3
import subprocess
import time
import sys
from collections import Counter

print("Starting parser with debug output capture...")

process = subprocess.Popen(
    ["python3", "cobol_frontend/cobol_integration.py", 
     "cobol_frontend/tests/BANKING.cbl", "--ailang-only", 
     "--io-backend", "jcl", "-o", "BANKING"],
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE,
    universal_newlines=True,
    bufsize=1
)

stdout_lines = []
stderr_lines = []
start = time.time()

# Read both stdout and stderr
import select
while time.time() - start < 2.0:  # 2 second timeout
    if process.poll() is not None:
        # Process finished
        remaining_out = process.stdout.read()
        remaining_err = process.stderr.read()
        if remaining_out:
            stdout_lines.extend(remaining_out.splitlines())
        if remaining_err:
            stderr_lines.extend(remaining_err.splitlines())
        break
    
    # Check if data available
    ready = select.select([process.stdout, process.stderr], [], [], 0.01)
    
    if process.stdout in ready[0]:
        line = process.stdout.readline()
        if line:
            stdout_lines.append(line.rstrip())
    
    if process.stderr in ready[0]:
        line = process.stderr.readline()
        if line:
            stderr_lines.append(line.rstrip())

if process.poll() is None:
    process.kill()
    process.wait()
    print(f"⏱️  TIMEOUT after 2s")
    print(f"Captured: {len(stdout_lines)} stdout, {len(stderr_lines)} stderr lines")
else:
    print(f"✅ Completed normally")
    print(f"Captured: {len(stdout_lines)} stdout, {len(stderr_lines)} stderr lines")

# Debug lines are in stdout, not stderr
debug_lines = stdout_lines

# Look for repeated debug lines (loop indicator)
loops = []
if debug_lines:
    all_lines = debug_lines[-500:] if len(debug_lines) > 500 else debug_lines
    counter = Counter(all_lines)
    loops = [(line, count) for line, count in counter.most_common(10) if count > 5]
    
    if loops:
        print("\n🔁 LOOP DETECTED - Repeated debug lines:")
        for line, count in loops[:5]:
            print(f"  {count:4d}x: {line[:100]}")

print(f"\n=== FIRST 100 DEBUG LINES (start of parsing) ===")
for i, line in enumerate(debug_lines[:100]):
    print(f"{i:5d}: {line}")

# Find where loop starts
if loops:
    loop_line = loops[0][0]
    try:
        first_occurrence = debug_lines.index(loop_line)
        print(f"\n=== 50 LINES BEFORE LOOP (first repeat at line {first_occurrence}) ===")
        start_idx = max(0, first_occurrence - 50)
        for i, line in enumerate(debug_lines[start_idx:first_occurrence + 10], start=start_idx):
            marker = " <-- LOOP STARTS" if i == first_occurrence else ""
            print(f"{i:5d}: {line[:120]}{marker}")
    except ValueError:
        pass

print(f"\n=== LAST 50 DEBUG LINES (end of capture) ===")
for i, line in enumerate(debug_lines[-50:], start=len(debug_lines)-50):
    print(f"{i:5d}: {line[:120]}")

# Save to file
with open('/tmp/parser_debug.log', 'w') as f:
    f.write("=== STDERR ===\n")
    f.write('\n'.join(stderr_lines))
    f.write("\n\n=== STDOUT ===\n")
    f.write('\n'.join(stdout_lines))

print(f"\nFull log saved to: /tmp/parser_debug.log")