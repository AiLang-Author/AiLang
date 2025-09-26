#!/usr/bin/env python3
# Fix import_resolver.py to return mappings

import re

print("Fixing import_resolver.py to track and return alias mappings...")

with open("import_resolver.py", "r") as f:
    content = f.read()

# Find enhanced_load_source function
if "def enhanced_load_source" in content:
    # Check if it already returns a tuple
    pattern = r'def enhanced_load_source\(.*?\):.*?return\s+([^,\n]+)$'
    
    # Simple approach: wrap the function
    wrapper = '''
# Fixed version that returns mappings
_original_enhanced_load_source = enhanced_load_source

def enhanced_load_source(filepath):
    """Enhanced version that also returns alias mappings"""
    processed_source = _original_enhanced_load_source(filepath)
    
    # Extract alias mappings from processed source
    import re
    alias_mappings = {}
    
    # Find all RANDOM_Module patterns
    pattern = r'([A-Z0-9]{6,8})_(\w+)'
    for match in re.finditer(pattern, processed_source):
        alias = match.group(0)  # Full alias like ABCDEF_Module
        original = match.group(2)  # Original module name
        alias_mappings[alias] = original
    
    return processed_source, alias_mappings
'''
    
    # Add wrapper to end of file
    with open("import_resolver.py", "a") as f:
        f.write("\n" + wrapper)
    
    print("✓ Added wrapper to return (source, mappings)")
else:
    print("✗ Could not find enhanced_load_source")

