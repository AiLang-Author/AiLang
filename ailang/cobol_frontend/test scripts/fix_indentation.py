#!/usr/bin/env python3
from pathlib import Path

def fix_file_indentation(filepath):
    lines = filepath.read_text().splitlines(keepends=True)
    fixed_lines = []
    
    for line in lines:
        if not line.strip() or not line.startswith(' '):
            fixed_lines.append(line)
            continue
        
        if line.startswith('    '):
            fixed_lines.append(line[4:])
        else:
            fixed_lines.append(line)
    
    return ''.join(fixed_lines)

parser_dir = Path("parser")
for filename in ["division_parsers.py", "statement_parsers.py", "expression_parsers.py"]:
    filepath = parser_dir / filename
    if filepath.exists():
        print(f"Fixing {filename}...")
        filepath.write_text(fix_file_indentation(filepath))
        print(f"  ✓ Done")

print("\n✓ All files fixed!")
