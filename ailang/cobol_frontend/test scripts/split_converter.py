#!/usr/bin/env python3
"""
Robust splitter for cobol_ast_converter.py with proper method extraction.
Handles class methods correctly by extracting them separately and monkey-patching.
"""

import shutil
import sys
from pathlib import Path
from typing import List, Dict, Set

# ----------------------------------------------------------------------
# Configuration
# ----------------------------------------------------------------------
METHOD_GROUPS: Dict[str, List[str]] = {
    "converter_core": [
        # Class definition and core orchestration
        "COBOLToAilangMultiProgramConverter",  # Just the class skeleton + __init__
        "flatten_variable_declarations",
        "convert",
        "convert_compilation_unit",
        "convert_cobol_program",
        "make_variable_reference",
        "create_program_subroutine",
        "create_variable_pool",
        "create_paragraph_subroutine",
        "create_variable_initializations",
        "get_initial_value_node",
        "make_assignment_target",
        # Serializer class
        "AILangASTSerializer",
    ],
    "type_system": [
        "calculate_pic_size",
        "get_redefines_values",
        "normalize_name",
        "infer_type",
        "get_storage_info",
        "is_edited_format",
        "parse_edit_format",
        "create_format_function",
        "format_storage_display",
    ],
    "decimal_support": [
        "is_decimal_operation",
        "get_arithmetic_function",
        "convert_decimal_value",
    ],
    "expression_converter": [
        "convert_binary_op",
        "convert_expression",
        "convert_function_call",
        "_is_string_expr",
        "convert_array_subscript",
    ],
    "statement_converter": [
        "convert_statement",
        "_collect_literals_recursive",
        "convert_display",
        "convert_inspect",
        "convert_move",
        "convert_compute",
        "convert_arithmetic",
        "convert_if",
        "convert_call",
        "convert_perform_paragraph",
        "convert_perform_times",
        "convert_perform_until",
        "convert_perform_varying",
        "convert_evaluate",
        "convert_accept",
        "convert_stop_run",
        "convert_goback",
        "convert_unstring",
        "convert_string_concat",
    ],
}

EXTRA_IMPORTS: Dict[str, List[str]] = {
    "converter_core": [],
    "type_system": ["from .converter_core import COBOLToAilangMultiProgramConverter"],
    "decimal_support": [
        "from .converter_core import COBOLToAilangMultiProgramConverter",
        "from .type_system import *",
    ],
    "expression_converter": [
        "from .converter_core import COBOLToAilangMultiProgramConverter",
        "from .type_system import *",
        "from .decimal_support import *",
    ],
    "statement_converter": [
        "from .converter_core import COBOLToAilangMultiProgramConverter",
        "from .expression_converter import *",
        "from .type_system import *",
        "from .decimal_support import *",
    ],
}

# ----------------------------------------------------------------------
# Configuration
# ----------------------------------------------------------------------
SOURCE_FILE = Path("cobol_ast_converter.py")
HEADER_LINES = 51
MAIN_CLASS = "COBOLToAilangMultiProgramConverter"
SERIALIZER_CLASS = "AILangASTSerializer"

def read_source() -> List[str]:
    """Read source file."""
    if not SOURCE_FILE.exists():
        raise FileNotFoundError(f"{SOURCE_FILE} not found")
    return SOURCE_FILE.read_text().splitlines(keepends=True)

def normalize_indent(lines: List[str]) -> List[str]:
    """Convert tabs to 4 spaces."""
    return [line.replace("\t", "    ") for line in lines]

def find_definitions(lines: List[str]) -> Dict[str, tuple]:
    """
    Find all top-level classes and their methods.
    Returns: {name: (start_line, indent_level, is_class)}
    """
    definitions = {}
    current_class = None
    class_indent = None
    
    for i, line in enumerate(lines):
        stripped = line.lstrip()
        indent = len(line) - len(stripped)
        
        if stripped.startswith("class "):
            if indent == 0:  # Top-level class
                name = stripped.split()[1].split("(")[0].split(":")[0]
                definitions[name] = (i, indent, True)
                current_class = name
                class_indent = indent + 4
                
        elif stripped.startswith("def "):
            if indent == class_indent and current_class:
                # Method of current class
                name = stripped.split()[1].split("(")[0]
                # Skip known inner functions
                if name not in ("search_for_target", "visit"):
                    definitions[name] = (i, indent, False)
            elif indent == 0:
                # Top-level function
                name = stripped.split()[1].split("(")[0]
                definitions[name] = (i, indent, False)
    
    return definitions

def extract_class_skeleton(lines: List[str], start_idx: int, methods_to_include: Set[str]) -> str:
    """
    Extract class definition with only specified methods.
    For converter_core, we want __init__ and core methods only.
    """
    base_indent = len(lines[start_idx]) - len(lines[start_idx].lstrip())
    result = []
    i = start_idx
    
    # Add class definition line
    result.append(lines[i])
    i += 1
    
    # Add docstring if present
    while i < len(lines):
        line = lines[i]
        stripped = line.lstrip()
        indent = len(line) - len(stripped)
        
        if stripped.startswith('"""') or stripped.startswith("'''"):
            result.append(line)
            i += 1
            # Continue until closing quotes
            quote_type = '"""' if stripped.startswith('"""') else "'''"
            while i < len(lines):
                result.append(lines[i])
                if lines[i].rstrip().endswith(quote_type):
                    i += 1
                    break
                i += 1
            break
        elif line.strip():
            break
        else:
            result.append(line)
            i += 1
    
    # Scan for methods and only include those in methods_to_include
    while i < len(lines):
        line = lines[i]
        stripped = line.lstrip()
        indent = len(line) - len(stripped)
        
        # Stop at next class or dedent to class level
        if indent <= base_indent and stripped.startswith("class "):
            break
            
        if stripped.startswith("def ") and indent == base_indent + 4:
            method_name = stripped.split()[1].split("(")[0]
            
            if method_name in methods_to_include:
                # Include this method
                method_block = extract_method_block(lines, i)
                result.extend(method_block)
                # Skip past this method
                i += len(method_block)
            else:
                # Skip this method entirely
                i += 1
                while i < len(lines):
                    cur_line = lines[i]
                    cur_indent = len(cur_line) - len(cur_line.lstrip())
                    # Stop when we hit another method or class at same/lower indent
                    if cur_line.strip() and cur_indent <= base_indent + 4:
                        if cur_line.lstrip().startswith("def ") or cur_line.lstrip().startswith("class "):
                            break
                    i += 1
        else:
            i += 1
    
    return "".join(normalize_indent(result))

def extract_method_block(lines: List[str], start_idx: int) -> List[str]:
    """Extract a complete method including nested functions."""
    base_indent = len(lines[start_idx]) - len(lines[start_idx].lstrip())
    block = [lines[start_idx]]
    i = start_idx + 1
    
    while i < len(lines):
        line = lines[i]
        indent = len(line) - len(line.lstrip())
        
        # Stop at same or lower indent that starts a new definition
        if line.strip() and indent <= base_indent:
            if line.lstrip().startswith("def ") or line.lstrip().startswith("class "):
                break
        
        block.append(line)
        i += 1
    
    return block

def extract_standalone_method(lines: List[str], start_idx: int, class_name: str) -> str:
    """
    Extract a method and format it as a standalone function that will be
    monkey-patched onto the class.
    """
    method_lines = extract_method_block(lines, start_idx)
    
    # Add monkey-patch comment
    result = [f"# Monkey-patch method onto {class_name}\n"]
    result.extend(normalize_indent(method_lines))
    result.append(f"\n{class_name}.{method_lines[0].strip().split()[1].split('(')[0]} = {method_lines[0].strip().split()[1].split('(')[0]}\n\n")
    
    return "".join(result)

def build_converter_core(lines: List[str], definitions: Dict[str, tuple]) -> str:
    """Build the converter_core module with class skeletons."""
    header = "".join(normalize_indent(lines[:HEADER_LINES]))
    
    parts = [header]
    parts.append("\n# Converter Core\n\n")
    
    # Extract main converter class with only core methods
    core_methods = {"__init__", "convert", "convert_compilation_unit", 
                    "convert_cobol_program", "make_variable_reference",
                    "create_program_subroutine", "create_variable_pool",
                    "create_paragraph_subroutine", "create_variable_initializations",
                    "get_initial_value_node", "make_assignment_target",
                    "flatten_variable_declarations"}
    
    print(f"   Extracting {MAIN_CLASS} with methods: {sorted(core_methods)}")
    main_start = definitions[MAIN_CLASS][0]
    parts.append(extract_class_skeleton(lines, main_start, core_methods))
    parts.append("\n\n")
    
    # Extract serializer class completely (all methods)
    if SERIALIZER_CLASS in definitions:
        print(f"   Extracting complete {SERIALIZER_CLASS} class")
        serializer_start = definitions[SERIALIZER_CLASS][0]
        # Find all methods of serializer
        serializer_methods = set()
        for name, (idx, indent, is_class) in definitions.items():
            if not is_class and idx > serializer_start:
                # Check if this is a serializer method (comes after serializer class)
                # and before any other class
                next_class_idx = float('inf')
                for other_name, (other_idx, other_indent, other_is_class) in definitions.items():
                    if other_is_class and other_idx > serializer_start:
                        next_class_idx = min(next_class_idx, other_idx)
                if idx < next_class_idx:
                    serializer_methods.add(name)
        
        parts.append(extract_class_skeleton(lines, serializer_start, serializer_methods))
    
    return "".join(parts)

def build_extension_module(
    lines: List[str],
    definitions: Dict[str, tuple],
    method_names: List[str],
    extra_imports: List[str],
    header_comment: str,
) -> str:
    """Build a module that extends the main class with additional methods."""
    header = "".join(normalize_indent(lines[:HEADER_LINES]))
    
    parts = [header]
    if extra_imports:
        parts.append("\n" + "\n".join(extra_imports) + "\n")
    parts.append(f"\n# {header_comment}\n\n")
    
    seen = set()
    for name in method_names:
        if name in seen:
            continue
        
        if name not in definitions:
            raise KeyError(f"Method '{name}' not found in source")
        
        start_idx, indent, is_class = definitions[name]
        
        if not is_class:
            # Extract as standalone method
            parts.append(extract_standalone_method(lines, start_idx, MAIN_CLASS))
        
        seen.add(name)
    
    return "".join(parts)

def create_init_package(dry_run: bool) -> None:
    """Create __init__.py with proper imports."""
    init_path = Path("converter/__init__.py")
    init_content = """# Import in dependency order
from .converter_core import COBOLToAilangMultiProgramConverter, AILangASTSerializer
from .type_system import *
from .decimal_support import *
from .expression_converter import *
from .statement_converter import *

__all__ = ["COBOLToAilangMultiProgramConverter", "AILangASTSerializer"]
"""
    print(f"  {'(dry-run) ' if dry_run else ''}Writing {init_path}")
    if not dry_run:
        init_path.write_text(init_content)

def write_module(name: str, content: str, dry_run: bool) -> None:
    """Write module to disk."""
    path = Path("converter") / f"{name}.py"
    print(f"  {'(dry-run) ' if dry_run else ''}Writing {path}")
    if not dry_run:
        path.write_text(content)

def main() -> None:
    dry_run = "--dry-run" in sys.argv
    
    print("\n1. Creating backups…")
    for fname in ("cobol_parser.py", "cobol_ast_converter.py"):
        src = Path(fname)
        if src.exists():
            backup = src.with_suffix(".backup")
            if not dry_run:
                shutil.copy2(src, backup)
            print(f"   → {backup}")
    
    print("\n2. Preparing converter/ package…")
    if not dry_run:
        Path("converter").mkdir(exist_ok=True)
    create_init_package(dry_run)
    
    print("\n3. Scanning source…")
    lines = read_source()
    definitions = find_definitions(lines)
    print(f"   Found {len(definitions)} definitions")
    
    print("\n4. Building converter_core.py…")
    core_content = build_converter_core(lines, definitions)
    write_module("converter_core", core_content, dry_run)
    
    print("\n5. Building extension modules…")
    for module_name, methods in METHOD_GROUPS.items():
        if module_name == "converter_core":
            continue
            
        print(f"\n   Building {module_name}.py…")
        extra = EXTRA_IMPORTS.get(module_name, [])
        comment = module_name.replace("_", " ").title()
        content = build_extension_module(
            lines, definitions, methods, extra, comment
        )
        write_module(module_name, content, dry_run)
    
    print("\n" + "="*80)
    print("✓ Converter refactoring complete!")
    print("\nVerify with:")
    print("   python3 -c 'from converter import COBOLToAilangMultiProgramConverter'")
    print("   grep -c 'def ' converter/*.py")
    print("="*80)

if __name__ == "__main__":
    main()