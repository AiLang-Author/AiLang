#!/bin/bash
# restore_and_fix.sh - Restore backups and apply the correct fix

echo "========================================"
echo "  Restoring and Fixing Correctly"
echo "========================================"
echo ""

# 1. Restore main.py
echo "1. Restoring main.py..."
if [ -f main.py.backup ]; then
    mv main.py.backup main.py
    echo "  ✓ Restored main.py"
else
    echo "  ⚠️ No backup found"
fi

# 2. Check what enhanced_load_source returns
echo ""
echo "2. Checking enhanced_load_source..."
python3 -c "
from import_resolver import enhanced_load_source
import inspect
sig = inspect.signature(enhanced_load_source)
print(f'  Function signature: {sig}')

# Test what it returns
try:
    result = enhanced_load_source('test_fix.ailang')
    print(f'  Returns type: {type(result)}')
    if isinstance(result, tuple):
        print(f'  Returns tuple with {len(result)} elements')
    else:
        print('  Returns single value (not a tuple)')
except Exception as e:
    print(f'  Error testing: {e}')
"

echo ""
echo "3. Applying the CORRECT fix..."
echo ""

# Create the correct fix based on what enhanced_load_source actually does
cat > correct_fix.py << 'EOF'
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

EOF

python3 correct_fix.py

echo ""
echo "4. Testing..."
python3 main.py test_fix.ailang 2>&1 | head -10

echo ""
echo "========================================"
echo "  Instructions"
echo "========================================"
echo ""
echo "If this doesn't work, the simplest workaround is:"
echo ""
echo "1. Don't use imports for now:"
echo "   Copy HashMap module directly into redis_server.ailang"
echo ""
echo "2. Or fix import_resolver.py manually:"
echo "   Make enhanced_load_source return (source, mappings)"
echo ""
echo "3. Or disable import resolution:"
echo "   Just use the original source without aliasing"