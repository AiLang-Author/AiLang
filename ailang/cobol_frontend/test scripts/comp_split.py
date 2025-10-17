#!/usr/bin/env python3
"""
Compare original cobol_parser.py and cobol_ast_converter.py with split files
to ensure all methods and classes are correctly extracted and grouped.
"""

import re
from pathlib import Path
from typing import Dict, Set, List
from collections import defaultdict

def extract_definitions(file_path: Path) -> Set[str]:
    """Extract top-level method and class names from a Python file."""
    definitions = set()
    try:
        lines = file_path.read_text().splitlines()
        for i, line in enumerate(lines):
            stripped = line.lstrip()
            if stripped.startswith("def ") or stripped.startswith("class "):
                name = stripped.split()[1].split("(")[0].split(":")[0]
                definitions.add(name)
    except FileNotFoundError:
        print(f"Warning: {file_path} not found")
    return definitions

def extract_definitions_from_dir(dir_path: Path) -> Dict[str, Set[str]]:
    """Extract method/class names from all .py files in a directory."""
    result = defaultdict(set)
    for file_path in dir_path.glob("*.py"):
        if file_path.name == "__init__.py":
            continue
        definitions = extract_definitions(file_path)
        result[file_path.name] = definitions
    return result

def compare_definitions(original_defs: Set[str], split_defs: Dict[str, Set[str]], package_name: str) -> None:
    """Compare original definitions with those in split files."""
    print(f"\n=== Comparing {package_name} ===")
    
    # Flatten split definitions
    all_split_defs = set()
    for file_name, defs in split_defs.items():
        all_split_defs.update(defs)
    
    # Find missing and extra definitions
    missing = original_defs - all_split_defs
    extra = all_split_defs - original_defs
    
    print(f"Original definitions ({len(original_defs)}): {sorted(original_defs)}")
    print(f"Split definitions ({len(all_split_defs)}): {sorted(all_split_defs)}")
    
    if missing:
        print(f"Missing definitions ({len(missing)}): {sorted(missing)}")
    else:
        print("No missing definitions")
    
    if extra:
        print(f"Extra definitions ({len(extra)}): {sorted(extra)}")
    else:
        print("No extra definitions")
    
    # Show definitions per file
    print("\nDefinitions per file:")
    for file_name, defs in sorted(split_defs.items()):
        print(f"  {file_name}: {len(defs)} definitions - {sorted(defs)}")

def main():
    # Extract original definitions
    parser_defs = extract_definitions(Path("cobol_parser.py"))
    converter_defs = extract_definitions(Path("cobol_ast_converter.py"))
    
    # Extract split definitions
    parser_split_defs = extract_definitions_from_dir(Path("parser"))
    converter_split_defs = extract_definitions_from_dir(Path("converter"))
    
    # Compare
    compare_definitions(parser_defs, parser_split_defs, "parser")
    compare_definitions(converter_defs, converter_split_defs, "converter")

if __name__ == "__main__":
    main()