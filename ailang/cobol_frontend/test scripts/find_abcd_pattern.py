#!/usr/bin/env python3
"""
Find where 'ABCD' pattern comes from in original file
"""
import sys
sys.path.insert(0, 'cobol_frontend')

# Read original file
with open('tests/newcob.cbl', 'r') as f:
    original_lines = f.readlines()

print("Searching for 'ABCD' pattern in original file...")
print("=" * 80)

found_abcd = []
for i, line in enumerate(original_lines, 1):
    if 'ABCD' in line:
        found_abcd.append((i, line))

print(f"Found {len(found_abcd)} lines containing 'ABCD'")
print()

for line_num, line in found_abcd[:10]:  # Show first 10
    print(f"Line {line_num}:")
    print(f"  {repr(line)}")
    
    # Show context
    if line_num > 1:
        print(f"  Prev: {repr(original_lines[line_num-2][:80])}")
    if line_num < len(original_lines):
        print(f"  Next: {repr(original_lines[line_num][:80])}")
    print()

print("\n" + "=" * 80)
print("Searching for lone quote character lines...")
print("=" * 80)

# Look for lines that are mostly spaces with just a quote
for i, line in enumerate(original_lines, 1):
    if len(line) >= 72:
        code_area = line[6:72]
        stripped = code_area.strip()
        if stripped == '"' or stripped == "'":
            print(f"Line {i}: Code area is just a quote!")
            print(f"  Full line: {repr(line)}")
            
            # Show context
            if i > 1:
                print(f"  Prev: {repr(original_lines[i-2])}")
            if i < len(original_lines):
                print(f"  Next: {repr(original_lines[i])}")
            print()

print("\n" + "=" * 80)
print("Searching for string continuations with quotes...")
print("=" * 80)

# Find unclosed strings followed by continuation lines
for i in range(len(original_lines) - 1):
    line = original_lines[i]
    next_line = original_lines[i+1]
    
    if len(line) >= 72 and len(next_line) >= 7:
        code_area = line[6:72]
        next_indicator = next_line[6]
        
        if next_indicator == '-':
            quote_count = code_area.count('"')
            if quote_count % 2 == 1:
                print(f"Lines {i+1}-{i+2}: String continuation")
                print(f"  Line {i+1}: {repr(line[:80])}")
                print(f"  Line {i+2}: {repr(next_line[:80])}")
                print()