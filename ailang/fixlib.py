#!/usr/bin/env python3
"""
Fix all occurrences of 'Libraries' to 'Librarys' in the compiler
"""

import os
import re

def fix_library_paths():
    files_to_fix = [
        'ailang_compiler/ailang_compiler.py',
        'ailang_compiler/modules/library_inliner.py',  # May also have library paths
        'ailang_parser/compiler.py',
        'main.py',  # Check if main.py has any library references
    ]
    
    for filepath in files_to_fix:
        if not os.path.exists(filepath):
            print(f"⚠️  {filepath} not found")
            continue
            
        print(f"Checking {filepath}...")
        
        with open(filepath, 'r') as f:
            content = f.read()
        
        original = content
        
        # Replace variations of Libraries with Librarys
        # Case 1: 'Libraries' in strings
        content = re.sub(r"'Libraries'", "'Librarys'", content)
        content = re.sub(r'"Libraries"', '"Librarys"', content)
        
        # Case 2: Libraries/ in paths
        content = re.sub(r"Libraries/", "Librarys/", content)
        
        # Case 3: os.path.join with Libraries
        content = re.sub(r"'Libraries',", "'Librarys',", content)
        content = re.sub(r'"Libraries",', '"Librarys",', content)
        
        # Case 4: Comment references
        content = re.sub(r"# (.*?)Libraries", r"# \1Librarys", content)
        
        if content != original:
            # Backup
            with open(filepath + '.bak', 'w') as f:
                f.write(original)
            
            # Write fixed
            with open(filepath, 'w') as f:
                f.write(content)
            
            print(f"  ✓ Fixed {filepath}")
            
            # Show what changed
            for i, (old_line, new_line) in enumerate(zip(original.split('\n'), content.split('\n'))):
                if old_line != new_line:
                    print(f"    Line {i+1}: Libraries -> Librarys")
        else:
            print(f"  - No changes needed")

if __name__ == "__main__":
    fix_library_paths()  # Add the parentheses