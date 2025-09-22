#!/usr/bin/env python3
"""
Fix double closing braces in WhileLoop blocks for all test files
"""

import os
import re

def fix_double_braces(content):
    """
    Fix double closing braces in WhileLoop blocks.
    The pattern is: WhileLoop has an extra indented closing brace.
    """
    lines = content.split('\n')
    fixed_lines = []
    i = 0
    
    while i < len(lines):
        line = lines[i]
        
        # Check for the specific pattern of double closing braces
        # Pattern: current line has spaces + }, next line has spaces + }
        if re.match(r'^\s+\}$', line):
            # This line is an indented closing brace
            next_line_idx = i + 1
            
            # Check if next non-empty line is also just a closing brace
            while next_line_idx < len(lines) and lines[next_line_idx].strip() == '':
                next_line_idx += 1
                
            if next_line_idx < len(lines) and re.match(r'^\s+\}$', lines[next_line_idx]):
                # We have double closing braces
                # Check context - is this after a WhileLoop?
                # Look backwards for WhileLoop
                found_while = False
                for j in range(max(0, i - 30), i):
                    if 'WhileLoop' in lines[j]:
                        found_while = True
                        break
                
                if found_while:
                    # Skip the first closing brace (keep the second one)
                    print(f"   - Removed extra brace at line {i+1}")
                    i += 1
                    continue
        
        fixed_lines.append(line)
        i += 1
    
    return '\n'.join(fixed_lines)

def process_file(filepath):
    """Process a single file to fix double braces"""
    print(f"\n📝 Processing {os.path.basename(filepath)}:")
    
    try:
        with open(filepath, 'r') as f:
            original = f.read()
    except FileNotFoundError:
        print(f"   ❌ File not found")
        return False
    
    # Create backup
    backup_path = filepath + '.brace_backup'
    with open(backup_path, 'w') as f:
        f.write(original)
    
    # Fix the content
    fixed = fix_double_braces(original)
    
    # Check if changes were made
    if fixed != original:
        with open(filepath, 'w') as f:
            f.write(fixed)
        
        # Count how many fixes were made
        original_lines = original.count('\n')
        fixed_lines = fixed.count('\n')
        removed = original_lines - fixed_lines
        
        print(f"   ✓ Fixed {removed} extra closing brace(s)")
        print(f"   💾 Backup saved as {backup_path}")
        return True
    else:
        print(f"   ℹ️  No double braces found")
        return False

def main():
    """Main function to fix all test files"""
    print("=" * 50)
    print("🔧 AILANG Test File Double Brace Fixer")
    print("=" * 50)
    
    # List of test files that are known to have issues
    problem_files = [
        'test_pool_overflow.ailang',
        'test_pool_tracking.ailang',
        'test_loops_comprehensive.ailang',
        'test_loop_patterns.ailang',
        'test_loop_simple.ailang',
        'test_loopmain_structure.ailang',
        'test_subroutine_basic.ailang',
        'test_loop_special.ailang',
        'test_loop_syntax_forms.ailang',
        'test_loop_edge_cases.ailang',
        'test_fileio_minimal.ailang'
    ]
    
    # Also scan for any other test files
    all_test_files = []
    for f in os.listdir('.'):
        if f.startswith('test_') and f.endswith('.ailang'):
            all_test_files.append(f)
    
    # Combine and deduplicate
    files_to_fix = list(set(problem_files + all_test_files))
    files_to_fix.sort()
    
    print(f"Found {len(files_to_fix)} test files to check")
    
    fixed_count = 0
    for filepath in files_to_fix:
        if os.path.exists(filepath):
            if process_file(filepath):
                fixed_count += 1
    
    print("\n" + "=" * 50)
    print(f"✅ Fixed {fixed_count} files")
    print(f"💾 All backups created with .brace_backup extension")
    print("\nTo restore a backup: cp filename.brace_backup filename")
    print("\n🧪 Run ./run_function_tests.sh to verify fixes")

if __name__ == "__main__":
    main()