#!/usr/bin/env python3
"""
Verification script to show exactly which methods are in which files.
Produces a detailed mapping of original methods to split file locations.
"""

import ast
from pathlib import Path
from typing import Dict, Set, List, Tuple
from collections import defaultdict


def extract_methods_with_lines(file_path: Path) -> Dict[str, Tuple[int, str]]:
    """
    Extract methods/classes with their line numbers and source preview.
    
    Returns: Dict[method_name, (line_number, first_line_of_source)]
    """
    methods = {}
    
    try:
        content = file_path.read_text()
        tree = ast.parse(content)
        
        for node in ast.walk(tree):
            if isinstance(node, (ast.FunctionDef, ast.ClassDef)):
                # Get first line of source (up to 80 chars)
                source_segment = ast.get_source_segment(content, node)
                if source_segment:
                    first_line = source_segment.split('\n')[0][:80]
                else:
                    first_line = f"<no source available>"
                
                methods[node.name] = (node.lineno, first_line)
    
    except Exception as e:
        print(f"Warning: Could not parse {file_path}: {e}")
    
    return methods


def extract_from_directory_detailed(dir_path: Path) -> Dict[str, Dict[str, Tuple[int, str]]]:
    """
    Extract methods from all files in directory with line numbers.
    
    Returns: Dict[filename, Dict[method_name, (line_number, source_preview)]]
    """
    result = {}
    
    for file_path in sorted(dir_path.glob("*.py")):
        if file_path.name == "__init__.py":
            continue
        
        methods = extract_methods_with_lines(file_path)
        if methods:
            result[file_path.name] = methods
    
    return result


def create_reverse_index(split_methods: Dict[str, Dict[str, Tuple[int, str]]]) -> Dict[str, str]:
    """
    Create a reverse index: method_name -> filename
    """
    reverse = {}
    for filename, methods in split_methods.items():
        for method_name in methods.keys():
            if method_name in reverse:
                # Duplicate method name across files
                reverse[method_name] = f"DUPLICATE: {reverse[method_name]}, {filename}"
            else:
                reverse[method_name] = filename
    
    return reverse


def verify_methods(original_file: Path, split_dir: Path, package_name: str):
    """
    Detailed verification showing where each method ended up.
    """
    print(f"\n{'='*100}")
    print(f"METHOD VERIFICATION: {package_name}")
    print(f"{'='*100}\n")
    
    # Extract original methods
    original_methods = extract_methods_with_lines(original_file)
    
    # Extract split methods
    split_methods_by_file = extract_from_directory_detailed(split_dir)
    
    # Create reverse index
    reverse_index = create_reverse_index(split_methods_by_file)
    
    # Statistics
    total = len(original_methods)
    found = 0
    missing = []
    
    print(f"Original file: {original_file.name}")
    print(f"Split directory: {split_dir.name}")
    print(f"Total methods in original: {total}\n")
    
    # Show where each method went
    print(f"{'METHOD NAME':<40} | {'LINE':>5} | {'DESTINATION FILE':<30} | {'STATUS':<10}")
    print(f"{'-'*40}-+-{'-'*5}-+-{'-'*30}-+-{'-'*10}")
    
    for method_name, (line_no, source_preview) in sorted(original_methods.items()):
        if method_name in reverse_index:
            dest_file = reverse_index[method_name]
            
            if dest_file.startswith("DUPLICATE"):
                status = "⚠️ DUP"
                found += 1
            else:
                status = "✅ OK"
                found += 1
            
            print(f"{method_name:<40} | {line_no:>5} | {dest_file:<30} | {status:<10}")
        else:
            missing.append((method_name, line_no, source_preview))
            print(f"{method_name:<40} | {line_no:>5} | {'MISSING':<30} | {'❌ MISS':<10}")
    
    # Show missing methods with source preview
    if missing:
        print(f"\n{'='*100}")
        print(f"MISSING METHODS ({len(missing)}):")
        print(f"{'='*100}\n")
        
        for method_name, line_no, source_preview in missing:
            print(f"❌ {method_name} (line {line_no})")
            print(f"   Source: {source_preview}")
            print()
    
    # Show file distribution
    print(f"\n{'='*100}")
    print(f"SPLIT FILE DISTRIBUTION:")
    print(f"{'='*100}\n")
    
    for filename, methods in sorted(split_methods_by_file.items()):
        print(f"📁 {filename}: {len(methods)} methods")
        
        # Group by type (class vs function)
        classes = []
        functions = []
        
        for method_name in sorted(methods.keys()):
            # Simple heuristic: starts with capital = class
            if method_name[0].isupper():
                classes.append(method_name)
            else:
                functions.append(method_name)
        
        if classes:
            print(f"   Classes: {', '.join(classes)}")
        if functions:
            # Show first 5 functions, then count
            if len(functions) <= 5:
                print(f"   Methods: {', '.join(functions)}")
            else:
                print(f"   Methods: {', '.join(functions[:5])} ... (+{len(functions)-5} more)")
        print()
    
    # Show extra methods (not in original)
    all_split_methods = set(reverse_index.keys())
    all_original_methods = set(original_methods.keys())
    extra_methods = all_split_methods - all_original_methods
    
    if extra_methods:
        print(f"{'='*100}")
        print(f"NEW HELPER CLASSES/METHODS ({len(extra_methods)}):")
        print(f"{'='*100}\n")
        
        for method_name in sorted(extra_methods):
            filename = reverse_index[method_name]
            print(f"   ➕ {method_name:<40} in {filename}")
    
    # Summary
    print(f"\n{'='*100}")
    print(f"SUMMARY:")
    print(f"{'='*100}")
    print(f"✅ Methods found:    {found:>4} / {total} ({100*found/total:.1f}%)")
    print(f"❌ Methods missing:  {len(missing):>4} / {total} ({100*len(missing)/total:.1f}%)")
    print(f"➕ New helpers:      {len(extra_methods):>4}")
    print(f"📁 Split files:      {len(split_methods_by_file):>4}")
    
    if len(missing) == 0:
        print("\n🎉 PERFECT! All methods accounted for!")
    elif len(missing) <= 3:
        print(f"\n✨ EXCELLENT! Only {len(missing)} method(s) to add.")
    else:
        print(f"\n⚠️  ATTENTION NEEDED: {len(missing)} methods missing.")
    
    print(f"{'='*100}\n")


def main():
    print("🔍 Starting Method Verification...")
    print("This will show exactly where each method is located.\n")
    
    # Verify parser
    if Path("cobol_parser.py").exists() and Path("parser").exists():
        verify_methods(
            Path("cobol_parser.py"),
            Path("parser"),
            "COBOL Parser"
        )
    else:
        print("⚠️  Parser files not found\n")
    
    # Verify converter
    if Path("cobol_ast_converter.py").exists() and Path("converter").exists():
        verify_methods(
            Path("cobol_ast_converter.py"),
            Path("converter"),
            "COBOL AST Converter"
        )
    else:
        print("⚠️  Converter files not found\n")
    
    print("="*100)
    print("Verification complete!")
    print("="*100)


if __name__ == "__main__":
    main()