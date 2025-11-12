#!/usr/bin/env python3
"""
Get more context around the problematic line
"""

import sys
sys.path.insert(0, '/mnt/c/Users/Sean/Documents/AiLang/ailang')

from cobol_frontend.cobol_lexer import preprocess_continuation_lines

with open('cobol_frontend/tests/newcob.cbl', 'r') as f:
    source = f.read()

preprocessed = preprocess_continuation_lines(source)
lines = preprocessed.split('\n')

print(f"Showing lines 107450-107480 for full context:")
print("="*80)

for i in range(107448, min(107482, len(lines))):
    line = lines[i][:100]
    marker = ">>>" if i == 107463 else "   "
    print(f"{marker} {i+1:6d}: {line}")

print("="*80)