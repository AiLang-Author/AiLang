#!/usr/bin/env python3
"""
Trace where line 119156 came from in the original file
"""
import sys
sys.path.insert(0, 'cobol_frontend')

# Read original file
with open('tests/newcob.cbl', 'r') as f:
    original_lines = f.readlines()

# Show original lines around 119156 (±10 lines)
print("ORIGINAL FILE - Lines 119146-119166:")
print("=" * 80)
for i in range(119145, 119166):
    if i < len(original_lines):
        line = original_lines[i]
        # Check for continuation indicators
        indicator = line[6] if len(line) > 6 else ' '
        marker = ""
        if indicator == '-':
            marker = " ← CONTINUATION"
        elif indicator in '*/':
            marker = " ← COMMENT"
        elif indicator == 'D':
            marker = " ← DEBUG"
        
        print(f"Line {i+1} (col7='{indicator}'){marker}:")
        print(f"  {repr(line)}")
        
        # Show if line has quotes
        if '"' in line:
            positions = [j for j, c in enumerate(line) if c == '"']
            print(f"  → Quotes at positions: {positions}")
        print()

print("\n" + "=" * 80)
print("Looking for string continuation patterns...")
print("=" * 80)

# Find any line ending with unclosed quote before 119156
for i in range(119145, 119157):
    if i < len(original_lines):
        line = original_lines[i]
        if len(line) > 72:
            code_area = line[6:72]
            quote_count = code_area.count('"')
            if quote_count % 2 == 1:
                print(f"Line {i+1}: Unclosed quote in code area")
                print(f"  Code: {repr(code_area)}")
                
                # Check if next line is continuation
                if i+1 < len(original_lines):
                    next_line = original_lines[i+1]
                    if len(next_line) > 6 and next_line[6] == '-':
                        print(f"  → Next line IS continuation")
                    else:
                        print(f"  → Next line NOT continuation")