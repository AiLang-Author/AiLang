#!/usr/bin/env python3
# investigate_duplicate.py - Find out why there are TWO enhanced_load_source functions

import re

print("=" * 60)
print("  Investigating Duplicate Functions")
print("=" * 60)

with open("import_resolver.py", "r") as f:
    content = f.read()
    lines = content.split('\n')

# Find all occurrences of 'def enhanced_load_source'
occurrences = []
for i, line in enumerate(lines):
    if 'def enhanced_load_source' in line:
        occurrences.append((i + 1, line.strip()))

print(f"\nFound {len(occurrences)} definitions of enhanced_load_source:")
for line_num, line_text in occurrences:
    print(f"  Line {line_num}: {line_text}")

# Extract both function bodies
print("\n" + "=" * 60)
print("  Analyzing Each Function")
print("=" * 60)

for idx, (line_num, _) in enumerate(occurrences):
    print(f"\n--- Function #{idx + 1} at line {line_num} ---")
    
    # Extract function body (from def to next def or end)
    start = line_num - 1
    end = start + 1
    
    # Find the end of this function
    indent_level = len(lines[start]) - len(lines[start].lstrip())
    while end < len(lines):
        if lines[end].strip() and not lines[end].startswith(' ' * (indent_level + 1)):
            if not lines[end].startswith(' ' * indent_level):
                break
        end += 1
    
    func_body = '\n'.join(lines[start:end])
    
    # Check what it returns
    returns = re.findall(r'^\s*return (.+)$', func_body, re.MULTILINE)
    print(f"Returns: {returns}")
    
    # Check if it's a wrapper
    if 'enhanced_load_source' in func_body[50:]:  # Skip the def line
        print("⚠️ This appears to be a WRAPPER calling another enhanced_load_source!")
    
    # Check signature
    sig_match = re.search(r'def enhanced_load_source\((.*?)\)', func_body)
    if sig_match:
        print(f"Parameters: {sig_match.group(1)}")

print("\n" + "=" * 60)
print("  The Problem")
print("=" * 60)

print("""
WHAT'S HAPPENING:
1. There are TWO enhanced_load_source functions
2. One is likely the original (returns single value)
3. One is likely our wrapper (returns tuple)
4. The wrapper was added to the END of the file
5. Python uses the LAST definition

BUT main.py expects single value, so either:
- The wrapper is wrong, OR
- main.py needs updating

Let's check main.py to see what it expects...
""")

# Check main.py
print("\n" + "=" * 60)
print("  Checking main.py")
print("=" * 60)

with open("main.py", "r") as f:
    main_lines = f.readlines()

for i, line in enumerate(main_lines):
    if 'enhanced_load_source' in line:
        print(f"Line {i+1}: {line.strip()}")
        
        # Check if it's unpacking a tuple
        if ',' in line and '=' in line and 'enhanced_load_source' in line:
            print("  ✓ This line expects a TUPLE")
        elif '=' in line and 'enhanced_load_source' in line:
            print("  ✗ This line expects a SINGLE value")

print("\n" + "=" * 60)
print("  Solution")
print("=" * 60)

print("""
THE FIX:
1. If the wrapper at the end returns tuple but main.py expects single value:
   - Update main.py to unpack the tuple
   
2. If main.py already expects tuple but getting error:
   - The wrapper might be malformed
   
3. Clean solution:
   - Remove duplicate function
   - Have ONE enhanced_load_source that returns (source, aliases)
   - Update main.py to match
""")

# Save findings
with open("duplicate_function_analysis.txt", "w") as f:
    f.write("DUPLICATE FUNCTION ANALYSIS\n")
    f.write("=" * 40 + "\n\n")
    f.write(f"Found {len(occurrences)} definitions of enhanced_load_source\n")
    for line_num, line_text in occurrences:
        f.write(f"  Line {line_num}: {line_text}\n")
    f.write("\nPython uses the LAST definition in the file!\n")
    f.write("\nThis explains the confusion:\n")
    f.write("- Test shows it returns tuple\n")
    f.write("- But main.py might be expecting single value\n")
    f.write("- Or vice versa\n")

print("\nAnalysis saved to: duplicate_function_analysis.txt")