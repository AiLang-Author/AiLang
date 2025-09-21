#!/usr/bin/env python3
"""
fix_library_syntax.py
Automatically fixes common syntax issues in AILANG libraries
"""

import re
from pathlib import Path
import shutil
from datetime import datetime

def fix_function_input_syntax(content):
    """Fix Input/Output/Body missing colons"""
    # Fix "Input (" to "Input: ("
    content = re.sub(r'(\s+Input)\s+\(', r'\1: (', content)
    # Fix "Output" without colon
    content = re.sub(r'(\s+Output)\s+([A-Za-z])', r'\1: \2', content)
    # Fix "Body {" to "Body: {"
    content = re.sub(r'(\s+Body)\s+\{', r'\1: {', content)
    return content

def fix_pool_syntax(content):
    """Fix pool declarations missing DOT"""
    # Fix pool types without DOT
    pool_types = ['FixedPool', 'DynamicPool', 'TemporalPool', 'NeuralPool', 
                  'KernelPool', 'ActorPool', 'SecurityPool', 'ConstrainedPool', 'FilePool']
    for pool_type in pool_types:
        # Fix "PoolType PoolName" to "PoolType.PoolName"
        pattern = f'({pool_type})\\s+([A-Z][A-Za-z0-9_]*)'
        replacement = r'\1.\2'
        content = re.sub(pattern, replacement, content)
    return content

def fix_multiline_strings(content):
    """Fix strings that span multiple lines"""
    # Find strings that might span lines and fix them
    lines = content.split('\n')
    fixed_lines = []
    in_string = False
    string_buffer = []
    
    for line in lines:
        # Count quotes in line
        quote_count = line.count('"') - line.count('\\"')
        
        if in_string:
            string_buffer.append(line)
            if quote_count % 2 == 1:  # Odd number means string ends
                # Combine the buffered lines
                combined = ' '.join(string_buffer).replace('\n', ' ')
                fixed_lines.append(combined)
                string_buffer = []
                in_string = False
        else:
            if quote_count % 2 == 1:  # Odd number means string starts but doesn't end
                string_buffer.append(line)
                in_string = True
            else:
                fixed_lines.append(line)
    
    # Handle unclosed string at end
    if string_buffer:
        fixed_lines.append(' '.join(string_buffer))
    
    return '\n'.join(fixed_lines)

def fix_else_block_indentation(content):
    """Fix ElseBlock appearing as expression due to indentation"""
    lines = content.split('\n')
    fixed_lines = []
    
    for i, line in enumerate(lines):
        if 'ElseBlock' in line and i > 0:
            # Check if previous line ends with }
            prev_line = lines[i-1].strip()
            if prev_line.endswith('}'):
                # Ensure ElseBlock is at same indentation as matching IfCondition
                # Find the matching IfCondition
                indent_level = len(line) - len(line.lstrip())
                # Adjust if needed
                if line.strip().startswith('ElseBlock'):
                    # Make sure it's properly indented
                    fixed_line = ' ' * (indent_level - 4) + line.strip() if indent_level >= 4 else line.strip()
                    fixed_lines.append(fixed_line)
                    continue
        fixed_lines.append(line)
    
    return '\n'.join(fixed_lines)

def fix_library_syntax(file_path):
    """Apply all fixes to a library file"""
    with open(file_path, 'r') as f:
        content = f.read()
    
    original_content = content
    
    # Apply fixes
    content = fix_function_input_syntax(content)
    content = fix_pool_syntax(content)
    content = fix_multiline_strings(content)
    content = fix_else_block_indentation(content)
    
    # Additional specific fixes
    # Fix "Function StringLength" to "Function.StringLength"
    content = re.sub(r'Function\s+([A-Z][A-Za-z0-9_]*)\s*\{', r'Function.\1 {', content)
    content = re.sub(r'SubRoutine\s+([A-Z][A-Za-z0-9_]*)\s*\{', r'SubRoutine.\1 {', content)
    
    return content, content != original_content

def main():
    # Backup directory
    backup_dir = Path("Librarys_backup_" + datetime.now().strftime("%Y%m%d_%H%M%S"))
    
    # Get all library files
    library_dir = Path("Librarys")
    if not library_dir.exists():
        print("Error: Librarys directory not found")
        return
    
    library_files = list(library_dir.glob("Library.*.ailang"))
    
    print(f"Found {len(library_files)} library files")
    print(f"Creating backup in {backup_dir}")
    
    # Create backup
    shutil.copytree(library_dir, backup_dir)
    print("Backup created")
    
    fixed_count = 0
    
    for lib_file in library_files:
        print(f"\nProcessing {lib_file.name}...")
        
        try:
            fixed_content, was_changed = fix_library_syntax(lib_file)
            
            if was_changed:
                # Write fixed content
                with open(lib_file, 'w') as f:
                    f.write(fixed_content)
                print(f"  ✓ Fixed and saved")
                fixed_count += 1
            else:
                print(f"  - No changes needed")
                
        except Exception as e:
            print(f"  ✗ Error: {e}")
    
    print(f"\n{'='*50}")
    print(f"Fixed {fixed_count} out of {len(library_files)} libraries")
    print(f"Backup saved to: {backup_dir}")
    print("\nRun test_all_libraries.sh to verify fixes")

if __name__ == "__main__":
    main()