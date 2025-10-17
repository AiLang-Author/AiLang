#!/usr/bin/env python3
"""
Find all lines with odd number of quotes after preprocessing
"""
import sys
sys.path.insert(0, 'cobol_frontend')

from cobol_lexer import preprocess_continuation_lines

# Read the file
with open('tests/newcob.cbl', 'r') as f:
    original = f.read()

print("Preprocessing full file...")
preprocessed = preprocess_continuation_lines(original)

print("Searching for lines with odd number of quotes...")
print("=" * 80)

lines = preprocessed.split('\n')
odd_quote_lines = []

for i, line in enumerate(lines, 1):
    quote_count = line.count('"')
    if quote_count % 2 == 1:
        odd_quote_lines.append((i, line, quote_count))

print(f"Found {len(odd_quote_lines)} lines with odd number of quotes")
print()

# Show first 20
for i, (line_num, line, count) in enumerate(odd_quote_lines[:20], 1):
    print(f"{i}. Line {line_num} ({count} quotes):")
    print(f"   {repr(line[:80])}")
    
    # Show context
    if line_num > 1:
        print(f"   Previous line: {repr(lines[line_num-2][:80])}")
    if line_num < len(lines):
        print(f"   Next line: {repr(lines[line_num][:80])}")
    print()

# Check specifically around line 119156
print("\n" + "=" * 80)
print("Lines 119154-119160:")
for i in range(119153, 119160):
    if i < len(lines):
        quote_count = lines[i].count('"')
        marker = " ⚠️" if quote_count % 2 == 1 else ""
        print(f"Line {i+1} ({quote_count} quotes){marker}: {repr(lines[i][:80])}")