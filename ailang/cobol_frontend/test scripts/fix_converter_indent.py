#!/usr/bin/env python3
from pathlib import Path
import re

def fix_file(filepath):
    lines = filepath.read_text().splitlines(keepends=True)
    fixed_lines = []
    
    for line in lines:
        # Don't touch blank lines or lines with no indentation
        if not line.strip() or not line.startswith(' '):
            fixed_lines.append(line)
            continue
        
        # Remove 4 spaces if line starts with them
        if line.startswith('    '):
            fixed_lines.append(line[4:])
        else:
            fixed_lines.append(line)
    
    content = ''.join(fixed_lines)
    
    # Fix mangled monkey-patch lines
    content = re.sub(r'\)COBOLToAilangMultiProgramConverter', ')\nCOBOLToAilangMultiProgramConverter', content)
    content = re.sub(r'\)def ', ')\n\ndef ', content)
    
    return content

# Fix converter modules
converter_dir = Path("converter")
files = ["type_system.py", "decimal_support.py", "expression_converter.py", "statement_converter.py"]

for filename in files:
    filepath = converter_dir / filename
    if filepath.exists():
        print(f"Fixing {filename}...")
        filepath.write_text(fix_file(filepath))
        print(f"  ✓ Done")

print("\n✓ All converter files fixed!")
