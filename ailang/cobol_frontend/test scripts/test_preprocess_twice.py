#!/usr/bin/env python3
"""
Test if preprocessing is deterministic
"""
import sys
sys.path.insert(0, 'cobol_frontend')

from cobol_lexer import preprocess_continuation_lines

# Read the file
with open('tests/newcob.cbl', 'r') as f:
    original = f.read()

print("Testing if preprocessing is deterministic...")
print("=" * 80)

# Preprocess once
preprocessed1 = preprocess_continuation_lines(original)
print(f"First preprocessing: {len(preprocessed1)} chars, {len(preprocessed1.split(chr(10)))} lines")

# Preprocess twice (preprocessing the already preprocessed!)
preprocessed2 = preprocess_continuation_lines(preprocessed1)
print(f"Second preprocessing: {len(preprocessed2)} chars, {len(preprocessed2.split(chr(10)))} lines")

# Check if they're the same
if preprocessed1 == preprocessed2:
    print("\n✓ Preprocessing is idempotent (preprocessing twice gives same result)")
else:
    print("\n✗ PREPROCESSING IS NOT IDEMPOTENT!")
    print("   This means preprocessing corrupts already-preprocessed data!")
    
    # Find first difference
    lines1 = preprocessed1.split('\n')
    lines2 = preprocessed2.split('\n')
    
    for i, (line1, line2) in enumerate(zip(lines1, lines2), 1):
        if line1 != line2:
            print(f"\nFirst difference at line {i}:")
            print(f"  After 1st pass: {repr(line1[:80])}")
            print(f"  After 2nd pass: {repr(line2[:80])}")
            break

# Now check line 119157 specifically
print("\n" + "=" * 80)
print("Checking line 119157 specifically:")
lines1 = preprocessed1.split('\n')
if len(lines1) > 119156:
    print(f"Line 119157 (1st pass): {repr(lines1[119156][:80])}")
else:
    print(f"Line 119157 doesn't exist after 1st pass (only {len(lines1)} lines)")

# Check for quote characters near line 119157
print("\nSearching for lone quotes near line 119157...")
for i in range(max(0, 119150), min(len(lines1), 119165)):
    line = lines1[i]
    quote_count = line.count('"')
    if quote_count % 2 == 1:
        print(f"  Line {i+1}: ODD number of quotes ({quote_count})")
        print(f"    Content: {repr(line[:80])}")