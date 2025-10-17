#!/usr/bin/env python3
"""
Deep comparison of original vs split files to verify identical functionality.
Checks: method signatures, variable names, call patterns, logic preservation.

This is a pure analysis tool - it only imports standard library modules.
"""

import ast
from pathlib import Path
from typing import Dict, Set, List
from collections import defaultdict
import difflib


class MethodAnalyzer:
    """Extract detailed information about methods/functions."""
    
    def __init__(self, file_path: Path):
        self.file_path = file_path
        self.content = file_path.read_text()
        self.tree = None
        try:
            self.tree = ast.parse(self.content)
        except SyntaxError as e:
            print(f"Warning: Could not parse {file_path}: {e}")
    
    def extract_method_info(self) -> Dict[str, dict]:
        """Extract detailed info for each method/function."""
        methods = {}
        
        if not self.tree:
            return methods
        
        for node in ast.walk(self.tree):
            if isinstance(node, (ast.FunctionDef, ast.ClassDef)):
                info = {
                    'name': node.name,
                    'type': 'function' if isinstance(node, ast.FunctionDef) else 'class',
                    'line': node.lineno,
                    'params': [],
                    'variables': set(),
                    'calls': set(),
                    'source': ast.get_source_segment(self.content, node) or ''
                }
                
                if isinstance(node, ast.FunctionDef):
                    # Extract parameters
                    info['params'] = [arg.arg for arg in node.args.args]
                    
                    # Extract variable assignments
                    for child in ast.walk(node):
                        if isinstance(child, ast.Name) and isinstance(child.ctx, ast.Store):
                            info['variables'].add(child.id)
                        elif isinstance(child, ast.Call):
                            if isinstance(child.func, ast.Name):
                                info['calls'].add(child.func.id)
                            elif isinstance(child.func, ast.Attribute):
                                info['calls'].add(child.func.attr)
                
                methods[node.name] = info
        
        return methods


def extract_all_methods(file_path: Path) -> Dict[str, dict]:
    """Extract method information from a file."""
    analyzer = MethodAnalyzer(file_path)
    return analyzer.extract_method_info()


def extract_from_directory(dir_path: Path) -> Dict[str, Dict[str, dict]]:
    """Extract method info from all .py files in directory."""
    all_methods = {}
    
    for file_path in dir_path.glob("*.py"):
        if file_path.name == "__init__.py":
            continue
        
        methods = extract_all_methods(file_path)
        for name, info in methods.items():
            info['file'] = file_path.name
            all_methods[name] = info
    
    return all_methods


def compare_method_signatures(original: dict, split: dict, method_name: str) -> List[str]:
    """Compare method signatures and return differences."""
    differences = []
    
    # Compare parameters
    orig_params = set(original.get('params', []))
    split_params = set(split.get('params', []))
    
    if orig_params != split_params:
        differences.append(f"  ❌ Parameter mismatch:")
        differences.append(f"     Original: {sorted(orig_params)}")
        differences.append(f"     Split:    {sorted(split_params)}")
    
    # Compare variable names
    orig_vars = original.get('variables', set())
    split_vars = split.get('variables', set())
    
    missing_vars = orig_vars - split_vars
    extra_vars = split_vars - orig_vars
    
    if missing_vars or extra_vars:
        differences.append(f"  ⚠️  Variable name differences:")
        if missing_vars:
            differences.append(f"     Missing: {sorted(missing_vars)}")
        if extra_vars:
            differences.append(f"     Extra:   {sorted(extra_vars)}")
    
    # Compare function calls
    orig_calls = original.get('calls', set())
    split_calls = split.get('calls', set())
    
    missing_calls = orig_calls - split_calls
    extra_calls = split_calls - orig_calls
    
    if missing_calls or extra_calls:
        differences.append(f"  ⚠️  Function call differences:")
        if missing_calls:
            differences.append(f"     Missing: {sorted(missing_calls)}")
        if extra_calls:
            differences.append(f"     Extra:   {sorted(extra_calls)}")
    
    return differences


def compute_source_similarity(orig_source: str, split_source: str) -> float:
    """Compute similarity ratio between two source code strings."""
    # Normalize whitespace
    orig_normalized = ' '.join(orig_source.split())
    split_normalized = ' '.join(split_source.split())
    
    matcher = difflib.SequenceMatcher(None, orig_normalized, split_normalized)
    return matcher.ratio()


def deep_compare(original_file: Path, split_dir: Path, package_name: str):
    """Perform deep comparison of original vs split files."""
    print(f"\n{'='*80}")
    print(f"DEEP COMPARISON: {package_name}")
    print(f"{'='*80}\n")
    
    # Extract method info
    original_methods = extract_all_methods(original_file)
    split_methods = extract_from_directory(split_dir)
    
    # Track statistics
    total_methods = len(original_methods)
    identical_count = 0
    modified_count = 0
    missing_count = 0
    
    print(f"Original file: {original_file.name}")
    print(f"Split directory: {split_dir.name}")
    print(f"Total methods to verify: {total_methods}\n")
    
    # Compare each method
    for method_name, orig_info in sorted(original_methods.items()):
        if method_name not in split_methods:
            print(f"❌ MISSING: {method_name} (line {orig_info['line']})")
            missing_count += 1
            continue
        
        split_info = split_methods[method_name]
        
        # Compare signatures
        differences = compare_method_signatures(orig_info, split_info, method_name)
        
        # Compute source similarity
        similarity = compute_source_similarity(
            orig_info.get('source', ''),
            split_info.get('source', '')
        )
        
        if not differences and similarity > 0.95:
            identical_count += 1
            print(f"✅ {method_name:40s} | {split_info['file']:25s} | Similarity: {similarity:.1%}")
        else:
            modified_count += 1
            print(f"⚠️  {method_name:40s} | {split_info['file']:25s} | Similarity: {similarity:.1%}")
            
            if differences:
                for diff in differences:
                    print(diff)
            
            if similarity < 0.95:
                print(f"  ⚠️  Source code similarity: {similarity:.1%} (< 95%)")
    
    # Check for extra methods in split files
    extra_methods = set(split_methods.keys()) - set(original_methods.keys())
    if extra_methods:
        print(f"\n{'='*80}")
        print("EXTRA METHODS IN SPLIT FILES (new classes/helpers):")
        print(f"{'='*80}")
        for method_name in sorted(extra_methods):
            split_info = split_methods[method_name]
            print(f"  ➕ {method_name:40s} | {split_info['file']:25s} | Type: {split_info['type']}")
    
    # Summary
    print(f"\n{'='*80}")
    print("SUMMARY:")
    print(f"{'='*80}")
    print(f"✅ Identical methods:  {identical_count:3d} / {total_methods} ({100*identical_count/total_methods:.1f}%)")
    print(f"⚠️  Modified methods:  {modified_count:3d} / {total_methods} ({100*modified_count/total_methods:.1f}%)")
    print(f"❌ Missing methods:   {missing_count:3d} / {total_methods} ({100*missing_count/total_methods:.1f}%)")
    print(f"➕ Extra helpers:     {len(extra_methods)}")
    
    if missing_count == 0 and modified_count == 0:
        print("\n🎉 PERFECT SPLIT: All methods extracted with no modifications!")
    elif missing_count == 0 and modified_count < 5:
        print(f"\n✨ EXCELLENT SPLIT: Only {modified_count} minor differences detected.")
    elif missing_count < 5:
        print(f"\n⚠️  GOOD SPLIT: {missing_count} methods need attention.")
    else:
        print(f"\n❌ NEEDS WORK: {missing_count} missing methods require investigation.")


def main():
    print("🔍 Starting Deep Comparison Analysis...")
    print("This will verify that the split preserves all functionality.\n")
    
    # Compare parser
    if Path("cobol_parser.py").exists() and Path("parser").exists():
        deep_compare(
            Path("cobol_parser.py"),
            Path("parser"),
            "COBOL Parser"
        )
    else:
        print("⚠️  Skipping parser comparison (files not found)")
    
    # Compare converter
    if Path("cobol_ast_converter.py").exists() and Path("converter").exists():
        deep_compare(
            Path("cobol_ast_converter.py"),
            Path("converter"),
            "COBOL AST Converter"
        )
    else:
        print("⚠️  Skipping converter comparison (files not found)")
    
    print("\n" + "="*80)
    print("Analysis complete!")
    print("="*80)


if __name__ == "__main__":
    main()