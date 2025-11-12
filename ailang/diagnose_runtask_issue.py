#!/usr/bin/env python3
"""
Diagnostic script to trace RunTask argument generation issue.

This script will help identify where unexpected RunTask arguments are being created
during the COBOL-to-Ailang transpilation process.

Usage: python3 diagnose_runtask_issue_fixed.py <cobol_file>
Example: python3 diagnose_runtask_issue_fixed.py cobol_frontend/tests/EXEC85.cbl
"""

import sys
import os

# Add the project to path - assuming script is run from ailang/ directory
project_root = os.getcwd()
sys.path.insert(0, project_root)

from cobol_frontend.cobol_lexer import COBOLLexer
from cobol_frontend.parser import COBOLMultiProgramParser
from cobol_frontend.converter import COBOLToAilangMultiProgramConverter, AILangASTSerializer

def diagnose_transpilation(cobol_file_path):
    """
    Run transpilation with detailed diagnostics to find RunTask issue.
    """
    print(f"=" * 80)
    print(f"DIAGNOSTIC: Analyzing {cobol_file_path}")
    print(f"=" * 80)
    
    # Read COBOL source
    with open(cobol_file_path, 'r') as f:
        cobol_code = f.read()
    
    print("\n1. Lexing COBOL...")
    try:
        lexer = COBOLLexer(cobol_code)
        tokens = lexer.tokenize()
        print(f"   ✓ Generated {len(tokens)} tokens")
    except Exception as e:
        print(f"   ✗ Lexer error: {e}")
        return None, []
    
    print("\n2. Parsing COBOL...")
    try:
        parser = COBOLMultiProgramParser(tokens, debug=False)  # Set to True for verbose output
        compilation_unit = parser.parse_all_programs()
        
        if not compilation_unit or not compilation_unit.programs:
            print("ERROR: Failed to parse COBOL")
            return None, []
        
        print(f"   ✓ Parsed {len(compilation_unit.programs)} program(s)")
        for prog in compilation_unit.programs:
            print(f"      - {prog.program_id}")
    except Exception as e:
        print(f"   ✗ Parser error: {e}")
        import traceback
        traceback.print_exc()
        return None, []
    
    print("\n3. Converting to Ailang AST...")
    try:
        converter = COBOLToAilangMultiProgramConverter(
            io_backend_type='jcl',  # Fixed: was io_backend, should be io_backend_type
            debug=False,  # Set to True for verbose output
            perf_enabled=False  # Fixed: was perf_profiling, should be perf_enabled
        )
        ailang_ast = converter.convert(compilation_unit)
        print(f"   ✓ Converted to Ailang AST")
    except Exception as e:
        print(f"   ✗ Conversion error: {e}")
        import traceback
        traceback.print_exc()
        return None, []
    
    print("\n4. Serializing Ailang AST...")
    try:
        # Get required libraries from the converter's backend
        required_libs = converter.io_backend.get_required_libraries() if converter.io_backend else []
        serializer = AILangASTSerializer(
            io_backend_type='jcl',
            required_libraries=required_libs
        )
        ailang_code = serializer.serialize(ailang_ast)
        print(f"   ✓ Serialized {len(ailang_code.split(chr(10)))} lines of Ailang code")
    except Exception as e:
        print(f"   ✗ Serialization error: {e}")
        import traceback
        traceback.print_exc()
        return None, []
    
    print("\n5. Analyzing generated code for RunTask patterns...")
    lines = ailang_code.split('\n')
    
    runtask_issues = []
    for i, line in enumerate(lines, 1):
        if 'RunTask' in line:
            # Check if it has unexpected arguments (commas after task name)
            if 'RunTask(' in line and ', ' in line:
                # Extract the full RunTask call
                start = line.find('RunTask(')
                if start >= 0:
                    end = line.find(')', start)
                    if end >= 0:
                        runtask_call = line[start:end+1]
                        # Check if it has => style arguments (named arguments)
                        if '=>' in runtask_call:
                            runtask_issues.append((i, line.strip(), runtask_call))
    
    if runtask_issues:
        print(f"\n⚠️  Found {len(runtask_issues)} RunTask calls with unexpected arguments:")
        print("-" * 80)
        for line_num, line, runtask_call in runtask_issues[:10]:  # Show first 10
            print(f"Line {line_num}: {line}")
            print(f"  Call: {runtask_call}")
            print()
        
        if len(runtask_issues) > 10:
            print(f"  ... and {len(runtask_issues) - 10} more")
    else:
        print("\n✓ No suspicious RunTask calls found")
    
    print("\n6. Checking for SQL generation patterns...")
    sql_lines = [i for i, line in enumerate(lines, 1) if 'PG_Query' in line or 'db_conn' in line]
    
    if sql_lines:
        print(f"   Found SQL generation at {len(sql_lines)} lines")
        print(f"   First few locations: {sql_lines[:10]}")
        print("\n   Sample SQL generation context (showing first occurrence):")
        if sql_lines:
            line_num = sql_lines[0]
            start = max(0, line_num - 3)
            end = min(len(lines), line_num + 3)
            print(f"\n   --- Around line {line_num} ---")
            for i in range(start, end):
                prefix = ">>> " if i == line_num - 1 else "    "
                print(f"   {prefix}{i+1}: {lines[i]}")
    else:
        print("   No SQL generation patterns found")
    
    # Save output to file
    output_file = cobol_file_path.replace('.cbl', '_transpiled.ailang')
    with open(output_file, 'w') as f:
        f.write(ailang_code)
    print(f"\n7. Full transpiled output saved to: {output_file}")
    print(f"   Total lines: {len(lines)}")
    
    return ailang_code, runtask_issues

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python3 diagnose_runtask_issue_fixed.py <cobol_file>")
        print("\nExample:")
        print("  python3 diagnose_runtask_issue_fixed.py cobol_frontend/tests/EXEC85.cbl")
        sys.exit(1)
    
    cobol_file = sys.argv[1]
    
    if not os.path.exists(cobol_file):
        print(f"ERROR: File not found: {cobol_file}")
        sys.exit(1)
    
    ailang_code, issues = diagnose_transpilation(cobol_file)
    
    if ailang_code:
        print("\n" + "=" * 80)
        print("DIAGNOSTIC COMPLETE")
        print("=" * 80)
        if issues:
            print(f"⚠️  Found {len(issues)} RunTask calls with suspicious arguments")
            print("Review the output above for details.")
        else:
            print("✓ No issues found with RunTask calls")
    else:
        print("\n" + "=" * 80)
        print("DIAGNOSTIC FAILED")
        print("=" * 80)
        print("See errors above for details.")