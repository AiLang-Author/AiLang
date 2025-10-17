#!/usr/bin/env python3
"""Fix by re-extracting methods with correct dedenting"""
from pathlib import Path

def dedent_method(method_lines):
    """Remove exactly 4 spaces from the start of each line"""
    result = []
    for line in method_lines:
        if line.startswith('    '):  # Has at least 4 spaces
            result.append(line[4:])  # Remove first 4 spaces
        elif line.strip() == '':  # Blank line
            result.append(line)
        else:  # Less than 4 spaces (shouldn't happen for class methods)
            result.append(line)
    return result

def process_module(filepath):
    """Process a module file to fix indentation"""
    content = filepath.read_text()
    lines = content.splitlines(keepends=True)
    
    fixed = []
    i = 0
    
    while i < len(lines):
        line = lines[i]
        
        # Check if this is a method definition
        if line.lstrip().startswith('def ') and line.startswith('    '):
            # Found a class method that needs dedenting
            method_lines = []
            base_indent = 4  # Class method indent
            
            # Collect the entire method
            method_lines.append(line)
            i += 1
            
            while i < len(lines):
                curr = lines[i]
                
                # Check if we hit a monkey-patch line or next method
                if 'COBOLMultiProgramParser.' in curr or 'COBOLToAilangMultiProgramConverter.' in curr:
                    # Don't include monkey-patch in method
                    break
                elif curr.lstrip().startswith('def ') and curr.startswith('    '):
                    # Next method
                    break
                elif curr.lstrip().startswith('# Monkey-patch'):
                    # Comment before monkey-patch
                    break
                
                method_lines.append(curr)
                i += 1
            
            # Dedent and add
            fixed.extend(dedent_method(method_lines))
        else:
            # Not a method definition, keep as-is
            fixed.append(line)
            i += 1
    
    return ''.join(fixed)

# Process all extension modules
for module_dir in ['parser', 'converter']:
    dir_path = Path(module_dir)
    for py_file in dir_path.glob('*.py'):
        if py_file.name in ['__init__.py', 'ast_nodes.py', 'parser_core.py', 'converter_core.py']:
            continue  # Skip these
        
        print(f"Processing {module_dir}/{py_file.name}...")
        fixed = process_module(py_file)
        py_file.write_text(fixed)
        print(f"  ✓ Fixed")

print("\n✓ All files processed!")
