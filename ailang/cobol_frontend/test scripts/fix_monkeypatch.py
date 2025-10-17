#!/usr/bin/env python3
from pathlib import Path
import re

def fix_monkey_patch_lines(content):
    """Fix mangled monkey-patch lines like ')COBOLMultiProgramParser'"""
    # Pattern: ")COBOLMultiProgramParser" should be ")\nCOBOLMultiProgramParser"
    content = re.sub(r'\)COBOLMultiProgramParser', ')\nCOBOLMultiProgramParser', content)
    
    # Also fix any other common issues
    content = re.sub(r'\)def ', ')\n\ndef ', content)
    
    return content

parser_dir = Path("parser")
for filename in ["division_parsers.py", "statement_parsers.py", "expression_parsers.py"]:
    filepath = parser_dir / filename
    if filepath.exists():
        print(f"Fixing {filename}...")
        content = filepath.read_text()
        fixed = fix_monkey_patch_lines(content)
        filepath.write_text(fixed)
        print(f"  ✓ Done")

print("\n✓ All monkey-patch lines fixed!")
