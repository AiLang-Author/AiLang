#!/usr/bin/env python3
"""
Find the unterminated string at line 107464 in newcob.cbl
"""

with open('cobol_frontend/tests/newcob.cbl', 'r') as f:
    lines = f.readlines()

print(f"Total lines in file: {len(lines)}")
print(f"\nScanning for unclosed quotes around line 107464...")
print("=" * 80)

# Check a range around the reported line
start = 95000
end = 120000

suspicious = []

for i in range(start, min(len(lines), end)):
    line = lines[i].rstrip()
    
    # Skip blank lines and comments
    if not line or (len(line) > 6 and line[6] == '*'):
        continue
    
    # Simple quote check (not perfect but catches obvious issues)
    # Count unescaped quotes
    single_quotes = 0
    double_quotes = 0
    
    j = 0
    while j < len(line):
        if line[j] == "'" and (j == len(line)-1 or line[j+1] != "'"):
            single_quotes += 1
            j += 1
        elif line[j] == '"' and (j == len(line)-1 or line[j+1] != '"'):
            double_quotes += 1
            j += 1
        else:
            j += 1
    
    # Odd number of quotes = likely unclosed
    if single_quotes % 2 != 0 or double_quotes % 2 != 0:
        suspicious.append((i+1, line, single_quotes, double_quotes))

print(f"\nFound {len(suspicious)} suspicious lines:\n")

for line_num, content, s, d in suspicious[:20]:  # Show first 20
    print(f"Line {line_num:6d} (single={s}, double={d}):")
    print(f"  {content[:100]}")
    print()

if len(suspicious) > 20:
    print(f"... and {len(suspicious) - 20} more")

print("\n" + "=" * 80)
print("Run this script from /mnt/c/Users/Sean/Documents/AiLang/ailang:")
print("  python3 /tmp/find_bad_string.py")