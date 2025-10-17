#!/usr/bin/env python3
"""
Final Fix: Extract AST nodes first, then split parser properly
"""

import shutil
from pathlib import Path

def extract_ast_nodes():
    """Extract all AST node classes from cobol_parser.py"""
    
    source = Path("cobol_parser.py")
    lines = source.read_text().splitlines(keepends=True)
    
    # Find where AST nodes end (class COBOLMultiProgramParser starts)
    parser_class_start = None
    for i, line in enumerate(lines):
        if line.strip().startswith("class COBOLMultiProgramParser"):
            parser_class_start = i
            break
    
    if not parser_class_start:
        print("ERROR: Could not find COBOLMultiProgramParser class")
        return None
    
    # Header (copyright + imports)
    header_end = 16  # Adjust based on your file
    
    # Extract AST section
    ast_content = []
    ast_content.extend(lines[:header_end])  # Header
    ast_content.append("\n# COBOL AST Node Definitions\n\n")
    
    # Find where imports end and AST classes begin
    ast_start = None
    for i in range(header_end, parser_class_start):
        if lines[i].strip().startswith("class ParseContext"):
            ast_start = i
            break
    
    if ast_start:
        ast_content.extend(lines[ast_start:parser_class_start])
    
    return "".join(ast_content)

def create_parser_core():
    """Create parser_core.py with just the class skeleton"""
    
    source = Path("cobol_parser.py")
    lines = source.read_text().splitlines(keepends=True)
    
    # Find class start
    class_start = None
    for i, line in enumerate(lines):
        if line.strip().startswith("class COBOLMultiProgramParser"):
            class_start = i
            break
    
    header_end = 16
    
    core_methods = {
        "__init__", "push_context", "pop_context", "current_context",
        "requires_period", "current_token", "peek_token", "advance",
        "match", "consume", "error", "consume_optional_period",
        "skip_insignificant_tokens", "parse_all_programs", 
        "is_data_only_program", "validate_program", "parse_single_program"
    }
    
    content = []
    content.extend(lines[:header_end])
    content.append("\nfrom .ast_nodes import *\n\n")
    
    # Extract class definition and selected methods
    i = class_start
    content.append(lines[i])  # class line
    i += 1
    
    # Skip to first def
    while i < len(lines) and not lines[i].strip().startswith("def "):
        if '"""' in lines[i]:
            # Include docstring
            content.append(lines[i])
            if lines[i].count('"""') == 1:
                i += 1
                while i < len(lines) and '"""' not in lines[i]:
                    content.append(lines[i])
                    i += 1
                if i < len(lines):
                    content.append(lines[i])
        i += 1
    
    # Extract only core methods
    while i < len(lines):
        line = lines[i]
        
        if line.strip().startswith("def "):
            method_name = line.strip().split("def ")[1].split("(")[0]
            
            if method_name in core_methods:
                # Extract this method
                method_lines = [line]
                i += 1
                base_indent = len(line) - len(line.lstrip())
                
                while i < len(lines):
                    curr_line = lines[i]
                    curr_indent = len(curr_line) - len(curr_line.lstrip())
                    
                    if curr_line.strip() and curr_indent <= base_indent:
                        if curr_line.lstrip().startswith(("def ", "class ")):
                            break
                    
                    method_lines.append(curr_line)
                    i += 1
                
                content.extend(method_lines)
            else:
                # Skip this method
                i += 1
        else:
            i += 1
    
    return "".join(content)

def create_extension_module(module_name, method_list):
    """Create an extension module with monkey-patched methods"""
    
    source = Path("cobol_parser.py")
    lines = source.read_text().splitlines(keepends=True)
    
    header_end = 16
    content = []
    content.extend(lines[:header_end])
    content.append("\nfrom .parser_core import COBOLMultiProgramParser\n")
    content.append("from .ast_nodes import *\n\n")
    content.append(f"# {module_name.replace('_', ' ').title()}\n\n")
    
    # Find and extract each method
    for method_name in method_list:
        i = 0
        found = False
        
        while i < len(lines):
            line = lines[i]
            
            if line.strip().startswith(f"def {method_name}("):
                found = True
                method_lines = [line]
                i += 1
                base_indent = len(line) - len(line.lstrip())
                
                while i < len(lines):
                    curr_line = lines[i]
                    curr_indent = len(curr_line) - len(curr_line.lstrip())
                    
                    if curr_line.strip() and curr_indent <= base_indent:
                        if curr_line.lstrip().startswith(("def ", "class ")):
                            break
                    
                    method_lines.append(curr_line)
                    i += 1
                
                # Add monkey-patch
                content.append(f"# Monkey-patch {method_name}\n")
                content.extend(method_lines)
                content.append(f"COBOLMultiProgramParser.{method_name} = {method_name}\n\n")
                break
            
            i += 1
        
        if not found:
            print(f"  WARNING: Method {method_name} not found")
    
    return "".join(content)

def main():
    print("="*70)
    print("FINAL PARSER SPLIT - Fixing everything")
    print("="*70)
    
    # 1. Backup
    print("\n1. Creating backup...")
    shutil.copy2("cobol_parser.py", "cobol_parser.py.backup")
    
    # 2. Create parser directory
    print("\n2. Creating parser/ directory...")
    Path("parser").mkdir(exist_ok=True)
    
    # 3. Extract AST nodes
    print("\n3. Extracting AST nodes...")
    ast_content = extract_ast_nodes()
    if ast_content:
        Path("parser/ast_nodes.py").write_text(ast_content)
        print("   ✓ Created ast_nodes.py")
    
    # 4. Create parser_core
    print("\n4. Creating parser_core.py...")
    core_content = create_parser_core()
    Path("parser/parser_core.py").write_text(core_content)
    print("   ✓ Created parser_core.py")
    
    # 5. Create extension modules
    print("\n5. Creating extension modules...")
    
    modules = {
        "division_parsers": [
            "parse_identification_division", "parse_environment_division",
            "parse_select_statement", "parse_file_section", "parse_data_division",
            "parse_working_storage", "parse_linkage_section", "parse_fd_entry",
            "parse_variable_decl", "parse_pic_clause", "parse_usage_type",
            "parse_procedure_division", "parse_end_program"
        ],
        "statement_parsers": [
            "parse_go_to", "parse_paragraph", "parse_statement", "parse_read_statement",
            "parse_display", "parse_accept", "parse_move", "parse_compute",
            "parse_add", "parse_subtract", "parse_multiply", "parse_divide",
            "parse_string_statement", "parse_unstring_statement", "parse_inspect",
            "parse_inspect_statement", "parse_if", "parse_perform", "parse_perform_until",
            "parse_perform_varying", "parse_call", "parse_evaluate", "parse_when_clause",
            "parse_stop_run"
        ],
        "expression_parsers": [
            "parse_condition", "parse_or_expression", "parse_and_expression",
            "parse_not_expression", "parse_comparison", "parse_arithmetic_expression",
            "parse_additive", "parse_multiplicative", "parse_primary",
            "parse_function_call", "parse_expression", "parse_string"
        ]
    }
    
    for module_name, methods in modules.items():
        print(f"\n   Creating {module_name}.py...")
        content = create_extension_module(module_name, methods)
        Path(f"parser/{module_name}.py").write_text(content)
        print(f"   ✓ {len(methods)} methods extracted")
    
    # 6. Create __init__.py
    print("\n6. Creating __init__.py...")
    init_content = """# Import in dependency order
from .ast_nodes import *
from .parser_core import COBOLMultiProgramParser, ParserError
from .division_parsers import *
from .statement_parsers import *
from .expression_parsers import *

__all__ = [
    "COBOLMultiProgramParser", "ParserError",
    "ParseContext", "COBOLASTNode", "COBOLCompilationUnit", "COBOLProgram",
    "COBOLDataDivision", "COBOLSelectStatement", "COBOLFileDescriptor",
    "COBOLLinkageSection", "COBOLVariableDecl", "COBOLProcedureDivision",
    "COBOLParagraph", "COBOLDisplay", "COBOLAccept", "COBOLRead",
    "COBOLMove", "COBOLCompute", "COBOLArithmetic", "COBOLIf",
    "COBOLPerformUntil", "COBOLPerformParagraph", "COBOLPerformTimes",
    "COBOLPerformVarying", "COBOLCall", "COBOLWhenClause", "COBOLEvaluate",
    "COBOLStopRun", "COBOLGoback", "COBOLExit", "COBOLBinaryOp",
    "COBOLUnaryOp", "COBOLIdentifier", "COBOLArraySubscript",
    "COBOLNumberLiteral", "COBOLStringLiteral", "COBOLFunctionCall",
    "COBOLStringConcat", "COBOLUnstring", "COBOLInspect"
]
"""
    Path("parser/__init__.py").write_text(init_content)
    
    print("\n" + "="*70)
    print("✓ COMPLETE! Parser successfully split into modules")
    print("="*70)
    print("\nVerify with:")
    print("  python3 -c 'from parser import COBOLMultiProgramParser'")
    print("  ls -la parser/")

if __name__ == "__main__":
    main()