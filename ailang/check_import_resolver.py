#!/usr/bin/env python3
# check_import_resolver.py - Figure out what enhanced_load_source returns

import os
import ast
import re

print("=" * 60)
print("  Checking enhanced_load_source")
print("=" * 60)
print()

# 1. Find and analyze import_resolver.py
print("1. Analyzing import_resolver.py...")
print("-" * 40)

with open("import_resolver.py", "r") as f:
    content = f.read()

# Find enhanced_load_source function
pattern = r'def enhanced_load_source\([^)]*\):.*?(?=\ndef|\nclass|\Z)'
match = re.search(pattern, content, re.DOTALL)

if match:
    func_code = match.group(0)
    print("Found enhanced_load_source function")
    
    # Look for return statements
    returns = re.findall(r'return .*', func_code)
    print(f"\nReturn statements found: {len(returns)}")
    for ret in returns:
        print(f"  {ret.strip()}")
    
    # Check if it returns a tuple
    if any("," in ret for ret in returns):
        print("\n✓ Function DOES return multiple values")
    else:
        print("\n✗ Function returns SINGLE value")
        print("  This explains the error!")

print()
print("2. Looking for alias_mappings handling...")
print("-" * 40)

# Search for alias_mappings in the file
alias_mentions = re.findall(r'.*alias.*', content, re.IGNORECASE)
print(f"Found {len(alias_mentions)} mentions of 'alias'")

# Check if there's a global or class variable for mappings
if "self.alias_mappings" in content or "alias_mappings =" in content:
    print("✓ Found alias_mappings variable")
else:
    print("✗ No alias_mappings variable found")

print()
print("3. Checking how imports are processed...")
print("-" * 40)

# Look for the import processing logic
if "random" in content and "prefix" in content:
    print("✓ Found random prefix generation")
    
    # Find where prefixes are used
    prefix_pattern = r'prefix\s*=.*?[A-Z0-9]{6,8}'
    if re.search(prefix_pattern, content):
        print("✓ Found prefix assignment")
    
    # Check if mappings are saved
    if "mapping" in content.lower():
        print("✓ Found mapping logic")
    else:
        print("✗ No mapping storage found")

print()
print("4. THE FIX NEEDED:")
print("-" * 40)
print("""
We need to modify enhanced_load_source to return BOTH:
1. The processed source code
2. The alias mappings

Current enhanced_load_source probably just returns:
    return processed_source

It should return:
    return processed_source, alias_mappings

Where alias_mappings is a dict like:
    {'RANDOM_Module': 'OriginalModule', ...}
""")

print()
print("5. Creating fixed version...")
print("-" * 40)

# Create a wrapper function that fixes the issue
fix_code = '''# Add this to import_resolver.py or create a new file

def enhanced_load_source_with_mappings(filepath):
    """Wrapper that properly returns mappings"""
    
    # Call original function
    processed_source = enhanced_load_source(filepath)
    
    # Extract mappings from the processed source
    # Look for the random prefixes that were added
    import re
    
    alias_mappings = {}
    
    # Find patterns like XXXXXX_ModuleName
    pattern = r'([A-Z0-9]{6,8}_\w+)'
    matches = re.findall(pattern, processed_source)
    
    for match in matches:
        if '_' in match:
            prefix, original = match.split('_', 1)
            # Only if it looks like a random prefix
            if len(prefix) >= 6 and prefix.isupper():
                alias_mappings[match] = original
    
    return processed_source, alias_mappings

# OR modify enhanced_load_source directly to track and return mappings
'''

print(fix_code)

# Save the analysis
with open("import_resolver_analysis.txt", "w") as f:
    f.write("IMPORT RESOLVER ANALYSIS\n")
    f.write("=" * 40 + "\n\n")
    f.write("enhanced_load_source currently returns: SINGLE VALUE\n")
    f.write("Need to modify it to return: (source, mappings)\n\n")
    f.write("Quick fix options:\n")
    f.write("1. Modify enhanced_load_source to track and return mappings\n")
    f.write("2. Create wrapper function that extracts mappings\n")
    f.write("3. Store mappings globally and access separately\n")

print("Analysis saved to import_resolver_analysis.txt")

print()
print("=" * 60)
print("  QUICK FIX")
print("=" * 60)
print()
print("Restore main.py first:")
print("  mv main.py.backup main.py")
print()
print("Then either:")
print("1. Keep using single value for now:")
print("   source_code = enhanced_load_source(filepath)")
print()
print("2. Or fix import_resolver.py to return tuple")
print("   (see suggestions above)")
print()
print("For now, to get Redis working, just restore the backup!")