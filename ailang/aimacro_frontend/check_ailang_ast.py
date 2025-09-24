#!/usr/bin/env python3
"""
Script to inspect what's actually available in the ailang_ast module
"""

import sys
import os

# Add parent directory to path
project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
sys.path.insert(0, project_root)

try:
    import ailang_parser.ailang_ast as ailang_ast
    
    print("Successfully imported ailang_ast module")
    print("\nAvailable classes and functions:")
    
    for name in dir(ailang_ast):
        if not name.startswith('_'):
            obj = getattr(ailang_ast, name)
            if isinstance(obj, type):
                print(f"  Class: {name}")
            elif callable(obj):
                print(f"  Function: {name}")
    
    print("\nChecking for specific classes we need:")
    required_classes = [
        'Program', 'Function', 'If', 'While', 'ForEvery', 
        'ReturnValue', 'Assignment', 'FunctionCall', 
        'Identifier', 'Number', 'String', 'LibraryImport'
    ]
    
    for class_name in required_classes:
        if hasattr(ailang_ast, class_name):
            print(f"  ✓ {class_name} found")
        else:
            print(f"  ✗ {class_name} NOT FOUND")
            # Try to find similar names
            similar = [n for n in dir(ailang_ast) if class_name.lower() in n.lower()]
            if similar:
                print(f"    Similar names: {similar}")

except ImportError as e:
    print(f"Failed to import ailang_ast: {e}")
    sys.exit(1)