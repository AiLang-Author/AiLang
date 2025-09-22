#!/usr/bin/env python3
"""
Fix syntax issues in AILANG test files
"""

import os
import re
import sys

def fix_test_syntax(filepath):
    """Fix common syntax issues in test files"""
    
    with open(filepath, 'r') as f:
        content = f.read()
    
    original = content
    fixes_applied = []
    
    # Fix 1: Remove Body: from WhileLoop
    # Pattern: WhileLoop condition { Body: { ... } }
    # Should be: WhileLoop condition { ... }
    if 'WhileLoop' in content and 'Body:' in content:
        # Use regex to find and fix WhileLoop blocks with Body:
        pattern = r'(WhileLoop\s+[^{]+\s*{\s*)Body:\s*{'
        content = re.sub(pattern, r'\1', content)
        
        # Also need to remove the extra closing brace
        # This is trickier - we need to count braces
        lines = content.split('\n')
        new_lines = []
        in_while = False
        while_depth = 0
        body_found = False
        
        for i, line in enumerate(lines):
            if 'WhileLoop' in line and '{' in line:
                in_while = True
                while_depth = 1
                body_found = 'Body:' in lines[i:i+3]  # Check next few lines
            elif in_while:
                while_depth += line.count('{') - line.count('}')
                if while_depth == 0:
                    in_while = False
                    if body_found and line.strip() == '}':
                        # Skip extra closing brace from Body:
                        body_found = False
                        continue
            new_lines.append(line)
        
        if len(new_lines) != len(lines):
            content = '\n'.join(new_lines)
            fixes_applied.append("Removed Body: from WhileLoop blocks")
    
    # Fix 2: Add colons after Input/Output/Body in functions
    # Pattern: Input (params) or Output Type or Body {
    content = re.sub(r'(\s+Input)\s+([^:\n]+)$', r'\1: \2', content, flags=re.MULTILINE)
    content = re.sub(r'(\s+Output)\s+([^:\n]+)$', r'\1: \2', content, flags=re.MULTILINE)
    content = re.sub(r'(\s+Body)\s*{', r'\1: {', content)
    if 'Input:' in content or 'Output:' in content or 'Body:' in content:
        fixes_applied.append("Added colons after Input/Output/Body")
    
    # Fix 3: Add colons after ThenBlock/ElseBlock
    content = re.sub(r'ThenBlock\s*{', 'ThenBlock: {', content)
    content = re.sub(r'ElseBlock\s*{', 'ElseBlock: {', content)
    if 'ThenBlock:' in content or 'ElseBlock:' in content:
        fixes_applied.append("Added colons after ThenBlock/ElseBlock")
    
    # Fix 4: Fix IfCondition without ThenBlock (old syntax)
    # Pattern: IfCondition condition { ... } should be IfCondition condition ThenBlock: { ... }
    pattern = r'(IfCondition\s+[^{]+)\s*{'
    if re.search(pattern, content) and 'ThenBlock' not in content:
        content = re.sub(pattern, r'\1 ThenBlock: {', content)
        fixes_applied.append("Added ThenBlock to IfCondition")
    
    # Fix 5: Fix CatchError blocks
    content = re.sub(r'CatchError\.(\w+)\s*{', r'CatchError.\1: {', content)
    if 'CatchError.' in content and ': {' in content:
        fixes_applied.append("Added colons after CatchError blocks")
    
    # Only write if changes were made
    if content != original:
        filename = os.path.basename(filepath)
        print(f"📝 {filename}:")
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
        return False

def main():
    """Fix all test files"""
    
    test_dir = "tests"
    
    if not os.path.exists(test_dir):
        print(f"❌ Directory {test_dir} not found")
        print("Looking for test files in current directory...")
        test_dir = "."
    
    print("🔧 AILANG Test File Syntax Fixer")
    print("=" * 40)
    
    fixed_count = 0
    checked_count = 0
    
    # Get all test .ailang files
    for filename in os.listdir(test_dir):
        if filename.startswith('test_') and filename.endswith('.ailang'):
            filepath = os.path.join(test_dir, filename)
            checked_count += 1
            if fix_test_syntax(filepath):
                fixed_count += 1
            else:
                print(f"   ⏭️  {filename}: No fixes needed")
    
    print("\n" + "=" * 40)
    print(f"✅ Fixed {fixed_count}/{checked_count} test files")
    if fixed_count > 0:
        print("💾 Backups created with .backup extension")
        print("\n🧪 Run ./run_function_tests.sh to verify fixes")

if __name__ == "__main__":
    main()