#!/usr/bin/env python3
"""
Dump the preprocessed file to disk and verify line numbers
"""
import sys
sys.path.insert(0, 'cobol_frontend')

from cobol_lexer import preprocess_continuation_lines

# Read original file
print("Reading original file...")
with open('tests/newcob.cbl', 'r') as f:
    original = f.read()

print(f"Original: {len(original)} chars, {len(original.split(chr(10)))} lines")

# Preprocess
print("Preprocessing...")
preprocessed = preprocess_continuation_lines(original)

print(f"Preprocessed: {len(preprocessed)} chars, {len(preprocessed.split(chr(10)))} lines")

# Write to file
output_file = 'newcob_preprocessed.txt'
with open(output_file, 'w') as f:
    f.write(preprocessed)

print(f"\nWrote preprocessed output to: {output_file}")

# Show lines around 119156
lines = preprocessed.split('\n')
print("\n" + "=" * 80)
print("Lines 119150-119165 in PREPROCESSED file:")
print("=" * 80)

for i in range(119149, min(119166, len(lines))):
    line = lines[i]
    quote_count = line.count('"')
    marker = " ⚠️ ODD QUOTES" if quote_count % 2 == 1 else ""
    print(f"Line {i+1} ({len(line)} chars, {quote_count} quotes){marker}:")
    print(f"  {repr(line)}")

# Also show byte-by-byte for line 119156
if len(lines) > 119155:
    print("\n" + "=" * 80)
    print("Line 119156 byte-by-byte:")
    print("=" * 80)
    line = lines[119155]
    for j, ch in enumerate(line):
        print(f"  Position {j}: {repr(ch)} (ord={ord(ch)})")
    
    print("\n" + "=" * 80)
    print("Now try tokenizing starting from line 119150...")
    print("=" * 80)
    
    # Try to manually tokenize line by line
    from cobol_lexer import COBOLLexer
    
    # Create a small section
    section = '\n'.join(lines[119149:119160])
    print(f"Section to tokenize ({len(section)} chars):")
    print(section)
    print("\n" + "=" * 80)
    
    try:
        lexer = COBOLLexer(section)
        tokens = lexer.tokenize()
        print(f"✓ Tokenized successfully! {len(tokens)} tokens")
    except Exception as e:
        print(f"✗ Tokenization failed: {e}")
        import traceback
        traceback.print_exc()