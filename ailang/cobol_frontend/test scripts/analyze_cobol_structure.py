#!/usr/bin/env python3
"""
Analyze COBOL parser and converter files to plan modularization
"""
import os
import re
from collections import defaultdict

def analyze_file(filepath):
    """Analyze a Python file and extract structure information"""
    
    with open(filepath, 'r') as f:
        content = f.read()
        lines = content.split('\n')
    
    info = {
        'total_lines': len(lines),
        'blank_lines': sum(1 for line in lines if not line.strip()),
        'comment_lines': sum(1 for line in lines if line.strip().startswith('#')),
        'classes': [],
        'functions': [],
        'imports': []
    }
    
    # Find classes
    class_pattern = re.compile(r'^class\s+(\w+)[\(:]')
    for i, line in enumerate(lines):
        match = class_pattern.match(line)
        if match:
            class_name = match.group(1)
            # Count methods in this class
            methods = []
            indent_level = len(line) - len(line.lstrip())
            
            for j in range(i+1, len(lines)):
                next_line = lines[j]
                if not next_line.strip():
                    continue
                next_indent = len(next_line) - len(next_line.lstrip())
                
                # Check if we're still inside the class
                if next_indent <= indent_level and next_line.strip():
                    break
                
                # Look for methods
                if next_line.strip().startswith('def '):
                    method_match = re.match(r'\s+def\s+(\w+)\s*\(', next_line)
                    if method_match:
                        methods.append(method_match.group(1))
            
            info['classes'].append({
                'name': class_name,
                'line': i + 1,
                'methods': methods,
                'method_count': len(methods)
            })
    
    # Find top-level functions (not in classes)
    func_pattern = re.compile(r'^def\s+(\w+)\s*\(')
    for i, line in enumerate(lines):
        match = func_pattern.match(line)
        if match:
            info['functions'].append({
                'name': match.group(1),
                'line': i + 1
            })
    
    # Find imports
    for line in lines[:50]:  # Check first 50 lines
        if line.startswith('import ') or line.startswith('from '):
            info['imports'].append(line.strip())
    
    # Calculate code lines
    info['code_lines'] = info['total_lines'] - info['blank_lines'] - info['comment_lines']
    
    return info

def find_dataclasses(filepath):
    """Find all @dataclass definitions"""
    with open(filepath, 'r') as f:
        lines = f.readlines()
    
    dataclasses = []
    for i, line in enumerate(lines):
        if '@dataclass' in line:
            # Next non-empty line should have the class name
            for j in range(i+1, min(i+5, len(lines))):
                if lines[j].strip().startswith('class '):
                    match = re.match(r'class\s+(\w+)', lines[j].strip())
                    if match:
                        dataclasses.append({
                            'name': match.group(1),
                            'line': i + 1
                        })
                        break
    
    return dataclasses

def suggest_modularization(parser_info, converter_info):
    """Suggest how to split the files into modules"""
    
    print("\n" + "="*80)
    print("MODULARIZATION PLAN")
    print("="*80)
    
    print("\n📁 PROPOSED STRUCTURE:")
    print("""
cobol_frontend/
├── __init__.py
├── cobol_integration.py          (orchestrator - keep as is)
├── cobol_lexer.py                (lexer - keep as is)
│
├── parser/
│   ├── __init__.py
│   ├── ast_nodes.py              ← All @dataclass definitions
│   ├── parser_core.py            ← COBOLMultiProgramParser class
│   ├── division_parsers.py       ← parse_identification, parse_data, etc.
│   ├── statement_parsers.py      ← parse_move, parse_add, parse_if, etc.
│   └── expression_parsers.py     ← parse_expression, parse_primary, etc.
│
├── converter/
│   ├── __init__.py
│   ├── converter_core.py         ← Main converter class
│   ├── expression_converter.py   ← Expression conversion
│   ├── statement_converter.py    ← Statement conversion
│   ├── type_system.py            ← PIC clause handling, type inference
│   └── decimal_support.py        ← Decimal arithmetic handling
│
└── tests/
    ├── hello.cbl
    ├── calculate.cbl
    └── ...
""")
    
    return

def main():
    # Analyze files
    cobol_dir = '.'
    
    parser_file = os.path.join(cobol_dir, 'cobol_parser.py')
    converter_file = os.path.join(cobol_dir, 'cobol_ast_converter.py')
    
    print("="*80)
    print("COBOL TRANSPILER FILE ANALYSIS")
    print("="*80)
    
    if os.path.exists(parser_file):
        print(f"\n📄 Analyzing {parser_file}...")
        parser_info = analyze_file(parser_file)
        dataclasses = find_dataclasses(parser_file)
        
        print(f"\n  Total Lines:    {parser_info['total_lines']:,}")
        print(f"  Code Lines:     {parser_info['code_lines']:,}")
        print(f"  Blank Lines:    {parser_info['blank_lines']:,}")
        print(f"  Comment Lines:  {parser_info['comment_lines']:,}")
        print(f"\n  Classes:        {len(parser_info['classes'])}")
        
        for cls in parser_info['classes']:
            print(f"    - {cls['name']} ({cls['method_count']} methods)")
            if cls['method_count'] > 0:
                print(f"      Methods: {', '.join(cls['methods'][:5])}", end='')
                if cls['method_count'] > 5:
                    print(f" ... +{cls['method_count']-5} more")
                else:
                    print()
        
        print(f"\n  Dataclasses:    {len(dataclasses)}")
        for dc in dataclasses[:10]:
            print(f"    - {dc['name']}")
        if len(dataclasses) > 10:
            print(f"    ... +{len(dataclasses)-10} more")
        
        print(f"\n  Top-level Functions: {len(parser_info['functions'])}")
        for func in parser_info['functions']:
            print(f"    - {func['name']}")
    else:
        print(f"❌ File not found: {parser_file}")
        parser_info = None
    
    print("\n" + "="*80)
    
    if os.path.exists(converter_file):
        print(f"\n📄 Analyzing {converter_file}...")
        converter_info = analyze_file(converter_file)
        
        print(f"\n  Total Lines:    {converter_info['total_lines']:,}")
        print(f"  Code Lines:     {converter_info['code_lines']:,}")
        print(f"  Blank Lines:    {converter_info['blank_lines']:,}")
        print(f"  Comment Lines:  {converter_info['comment_lines']:,}")
        print(f"\n  Classes:        {len(converter_info['classes'])}")
        
        for cls in converter_info['classes']:
            print(f"    - {cls['name']} ({cls['method_count']} methods)")
            if cls['method_count'] > 0:
                print(f"      Methods: {', '.join(cls['methods'][:5])}", end='')
                if cls['method_count'] > 5:
                    print(f" ... +{cls['method_count']-5} more")
                else:
                    print()
        
        print(f"\n  Top-level Functions: {len(converter_info['functions'])}")
        for func in converter_info['functions'][:10]:
            print(f"    - {func['name']}")
        if len(converter_info['functions']) > 10:
            print(f"    ... +{len(converter_info['functions'])-10} more")
    else:
        print(f"❌ File not found: {converter_file}")
        converter_info = None
    
    # Suggest modularization
    if parser_info and converter_info:
        suggest_modularization(parser_info, converter_info)

if __name__ == '__main__':
    main()