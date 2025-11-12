#!/usr/bin/env python3
"""
Diagnose the COBOLPerformThru issue
"""

import sys
sys.path.insert(0, '.')

def check_imports():
    """Check what's actually imported"""
    print("="*80)
    print("CHECKING IMPORTS")
    print("="*80)
    
    # Check ast_nodes.py
    print("\n1. Checking if COBOLPerformThru is defined in ast_nodes.py:")
    with open('cobol_frontend/parser/ast_nodes.py', 'r') as f:
        content = f.read()
        if 'class COBOLPerformThru' in content:
            print("   ✅ COBOLPerformThru class is defined")
            # Find the definition
            for i, line in enumerate(content.split('\n'), 1):
                if 'class COBOLPerformThru' in line:
                    print(f"   Line {i}: {line.strip()}")
                    # Show next 5 lines
                    lines = content.split('\n')
                    for j in range(i, min(i+5, len(lines))):
                        print(f"   Line {j+1}: {lines[j]}")
                    break
        else:
            print("   ❌ COBOLPerformThru class NOT found")
    
    # Check statement_parsers.py imports
    print("\n2. Checking statement_parsers.py imports:")
    with open('cobol_frontend/parser/statement_parsers.py', 'r') as f:
        content = f.read()
        # Check for import statement
        if 'from .ast_nodes import' in content:
            print("   Found import statement:")
            for line in content.split('\n'):
                if 'from .ast_nodes import' in line:
                    print(f"   {line}")
        
        # Check if COBOLPerformThru is used
        if 'COBOLPerformThru(' in content:
            print("   ✅ COBOLPerformThru is instantiated in statement_parsers.py")
            for i, line in enumerate(content.split('\n'), 1):
                if 'COBOLPerformThru(' in line:
                    print(f"   Line {i}: {line.strip()}")
                    break
        else:
            print("   ❌ COBOLPerformThru is NOT used in statement_parsers.py")
    
    # Check converter_core.py imports
    print("\n3. Checking converter_core.py imports:")
    with open('cobol_frontend/converter/converter_core.py', 'r') as f:
        content = f.read()
        if 'COBOLPerformThru' in content:
            print("   ✅ COBOLPerformThru is mentioned")
            for line in content.split('\n'):
                if 'COBOLPerformThru' in line and 'import' in line:
                    print(f"   {line}")
        else:
            print("   ❌ COBOLPerformThru NOT imported in converter_core.py")

def test_import():
    """Try to actually import COBOLPerformThru"""
    print("\n" + "="*80)
    print("TESTING ACTUAL IMPORT")
    print("="*80)
    
    try:
        from cobol_frontend.parser.ast_nodes import COBOLPerformThru
        print("✅ Successfully imported COBOLPerformThru")
        print(f"   Class: {COBOLPerformThru}")
        print(f"   Module: {COBOLPerformThru.__module__}")
        
        # Try to create an instance
        try:
            obj = COBOLPerformThru(
                from_paragraph="START-PARA",
                thru_paragraph="END-PARA",
                line=1,
                column=1
            )
            print(f"✅ Successfully created instance: {obj}")
        except Exception as e:
            print(f"❌ Failed to create instance: {e}")
            
    except ImportError as e:
        print(f"❌ Failed to import COBOLPerformThru: {e}")
    except Exception as e:
        print(f"❌ Unexpected error: {e}")

def test_converter():
    """Test if converter can handle COBOLPerformThru"""
    print("\n" + "="*80)
    print("TESTING CONVERTER HANDLING")
    print("="*80)
    
    try:
        from cobol_frontend.parser.ast_nodes import COBOLPerformThru
        from cobol_frontend.converter import COBOLToAilangMultiProgramConverter
        
        # Create a simple AST with COBOLPerformThru
        perform_thru = COBOLPerformThru(
            from_paragraph="START-PARA",
            thru_paragraph="END-PARA",
            line=1,
            column=1
        )
        
        print(f"Created node: {perform_thru}")
        
        # Try to convert it
        converter = COBOLToAilangMultiProgramConverter(io_backend='jcl')
        
        # Check if converter has a handler
        print("\nChecking converter methods:")
        converter_methods = [m for m in dir(converter) if not m.startswith('_')]
        perform_methods = [m for m in converter_methods if 'perform' in m.lower()]
        print(f"  Perform-related methods: {perform_methods}")
        
        # Check statement_converter
        from cobol_frontend.converter.statement_converter import StatementConverter
        stmt_converter = StatementConverter(converter)
        
        stmt_methods = [m for m in dir(stmt_converter) if not m.startswith('_')]
        perform_stmt_methods = [m for m in stmt_methods if 'perform' in m.lower()]
        print(f"  StatementConverter perform methods: {perform_stmt_methods}")
        
    except Exception as e:
        print(f"❌ Error: {e}")
        import traceback
        traceback.print_exc()

def find_actual_error():
    """Find where the actual error occurs"""
    print("\n" + "="*80)
    print("FINDING ACTUAL ERROR LOCATION")
    print("="*80)
    
    # Look for the line number mentioned in the error
    print("\nSearching for line 299833 in newval.cbl...")
    
    try:
        with open('cobol_frontend/tests/newval.cbl', 'r') as f:
            lines = f.readlines()
            
        if len(lines) >= 299833:
            start = max(0, 299833 - 5)
            end = min(len(lines), 299833 + 5)
            
            print(f"\nShowing lines {start+1} to {end}:")
            for i in range(start, end):
                marker = ">>>" if i == 299832 else "   "
                print(f"{marker} {i+1:6d}: {lines[i].rstrip()}")
        else:
            print(f"   File only has {len(lines)} lines")
    except FileNotFoundError:
        print("   File not found")

if __name__ == '__main__':
    check_imports()
    test_import()
    test_converter()
    find_actual_error()