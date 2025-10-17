#!/usr/bin/env python3
from pathlib import Path
import re

def smart_dedent(content):
    """Only dedent method definitions and their bodies, not control flow"""
    lines = content.splitlines(keepends=True)
    fixed = []
    in_method = False
    method_indent = 0
    
    for i, line in enumerate(lines):
        stripped = line.lstrip()
        current_indent = len(line) - len(stripped)
        
        # Detect method definition (starts with 'def ')
        if stripped.startswith('def '):
            in_method = True
            method_indent = current_indent
            # Dedent the method definition by 4 spaces
            if current_indent >= 4:
                fixed.append(line[4:])
            else:
                fixed.append(line)
            continue
        
        # Detect monkey-patch line (ends method)
        if in_method and 'COBOLToAilangMultiProgramConverter.' in line or 'COBOLMultiProgramParser.' in line:
            in_method = False
            # Don't dedent the monkey-patch line
            fixed.append(line)
            continue
        
        # Inside a method - dedent by 4 if indented
        if in_method and current_indent >= 4:
            fixed.append(line[4:])
        else:
            fixed.append(line)
    
    result = ''.join(fixed)
    
    # Fix mangled monkey-patch lines
    result = re.sub(r'\)(COBOL[A-Za-z]+)\.', r')\n\1.', result)
    
    return result

# Fix all modules
modules = {
    'parser': ['division_parsers.py', 'statement_parsers.py', 'expression_parsers.py'],
    'converter': ['type_system.py', 'decimal_support.py', 'expression_converter.py', 'statement_converter.py']
}

for dir_name, files in modules.items():
    dir_path = Path(dir_name)
    for filename in files:
        filepath = dir_path / filename
        if filepath.exists():
            print(f"Fixing {dir_name}/{filename}...")
            content = filepath.read_text()
            fixed = smart_dedent(content)
            filepath.write_text(fixed)
            print(f"  ✓ Done")

print("\n✓ All files fixed with smart dedenting!")
