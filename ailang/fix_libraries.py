#!/usr/bin/env python3
"""
Batch fix common syntax errors in AILANG libraries
"""

import os
import re
import sys

def fix_library_syntax(filepath):
    """Fix common syntax issues in a library file"""
    
    with open(filepath, 'r') as f:
        content = f.read()
    
    original = content
    fixes_applied = []
    
    # Fix 1: Add colons after ThenBlock/ElseBlock
    if 'ThenBlock {' in content:
        content = re.sub(r'ThenBlock\s*{', 'ThenBlock: {', content)
        fixes_applied.append("Added colons after ThenBlock")
    
    if 'ElseBlock {' in content:
        content = re.sub(r'ElseBlock\s*{', 'ElseBlock: {', content)
        fixes_applied.append("Added colons after ElseBlock")
    
    # Fix 2: Fix pool definitions (dash to equals)
    # Pattern: ElementType-X should be ElementType=X
    content = re.sub(r'ElementType-(\w+)', r'ElementType=\1', content)
    content = re.sub(r'MaximumLength-(\d+)', r'MaximumLength=\1', content)
    content = re.sub(r'Initialize-(\w+)', r'Initialize=\1', content)
    content = re.sub(r'CanChange-(\w+)', r'CanChange=\1', content)
    if 'ElementType=' in content and 'ElementType-' not in content:
        fixes_applied.append("Fixed pool parameter syntax (- to =)")
    
    # Fix 3: Fix MaximumLength missing colon
    content = re.sub(r'MaximumLength=(\d+),', r'MaximumLength: \1,', content)
    if 'MaximumLength:' in content:
        fixes_applied.append("Added colons after MaximumLength")
    
    # Fix 4: Fix CatchError blocks
    content = re.sub(r'CatchError\.(\w+)\s*{', r'CatchError.\1: {', content)
    if 'CatchError.' in content and ': {' in content:
        fixes_applied.append("Added colons after CatchError blocks")
    
    # Fix 5: Fix FinallyBlock
    if 'FinallyBlock {' in content:
        content = re.sub(r'FinallyBlock\s*{', 'FinallyBlock: {', content)
        fixes_applied.append("Added colon after FinallyBlock")
    
    # Fix 6: Multiple Input parameters on separate lines to combined
    # This is trickier - look for consecutive Input: lines
    lines = content.split('\n')
    new_lines = []
    i = 0
    while i < len(lines):
        line = lines[i]
        if 'Input:' in line and not '(' in line:
            # Check if next line also has Input:
            params = []
            while i < len(lines) and 'Input:' in lines[i]:
                # Extract parameter from line
                match = re.search(r'Input:\s*(\w+):\s*(\w+)', lines[i])
                if match:
                    params.append(f"{match.group(1)}: {match.group(2)}")
                i += 1
            if len(params) > 1:
                new_lines.append(f"    Input: ({', '.join(params)})")
                fixes_applied.append("Combined multiple Input parameters")
            elif params:
                new_lines.append(f"    Input: {params[0]}")
            i -= 1  # Back up one since we'll increment at end
        else:
            new_lines.append(line)
        i += 1
    
    if fixes_applied and len(params) > 1:
        content = '\n'.join(new_lines)
    
    # Only write if changes were made
    if content != original:
        print(f"\n📝 {os.path.basename(filepath)}:")
        for fix in fixes_applied:
            print(f"   ✓ {fix}")
        
        # Backup original
        backup_path = filepath + '.backup'
        with open(backup_path, 'w') as f:
            f.write(original)
        
        # Write fixed version
        with open(filepath, 'w') as f:
            f.write(content)
        
        return True
    else:
        print(f"   ⏭️  {os.path.basename(filepath)}: No fixes needed")
        return False

def main():
    """Fix all libraries"""
    
    library_dir = "Librarys"
    
    if not os.path.exists(library_dir):
        print(f"❌ Directory {library_dir} not found")
        sys.exit(1)
    
    print("🔧 AILANG Library Syntax Fixer")
    print("=" * 40)
    
    fixed_count = 0
    
    # Get all .ailang files
    for filename in os.listdir(library_dir):
        if filename.endswith('.ailang'):
            filepath = os.path.join(library_dir, filename)
            if fix_library_syntax(filepath):
                fixed_count += 1
    
    print("\n" + "=" * 40)
    print(f"✅ Fixed {fixed_count} libraries")
    print("💾 Backups created with .backup extension")
    print("\n🧪 Run ./test_all_libraries.sh to verify fixes")

if __name__ == "__main__":
    main()