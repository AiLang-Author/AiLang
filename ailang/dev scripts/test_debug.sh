#!/usr/bin/env python3
"""
Debug script to identify where AILANG compiler is failing
Run: python debug_compiler.py
"""

import sys
import traceback

def test_imports():
    """Test if modules import correctly"""
    print("1. Testing imports...")
    try:
        import ailang_parser.compiler
        print("   ✓ compiler imports")
    except ImportError as e:
        print(f"   ✗ compiler import failed: {e}")
        return False
    
    try:
        from ailang_parser.parser import Parser
        print("   ✓ Parser imports")
    except ImportError as e:
        print(f"   ✗ Parser import failed: {e}")
        return False
        
    try:
        from ailang_parser.lexer import Lexer
        print("   ✓ Lexer imports")
    except ImportError as e:
        print(f"   ✗ Lexer import failed: {e}")
        return False
        
    try:
        import ailang_parser.ailang_ast
        print("   ✓ AST imports")
    except ImportError as e:
        print(f"   ✗ AST import failed: {e}")
        return False
    
    return True

def test_simple_parse(code, description):
    """Test parsing a simple code snippet"""
    print(f"\n{description}")
    print(f"   Code: {code}")
    
    try:
        from ailang_parser.parser import Parser
        from ailang_parser.lexer import Lexer
        
        lexer = Lexer(code)
        parser = Parser(lexer)
        ast = parser.parse()
        print(f"   ✓ Parse succeeded")
        return True
    except AttributeError as e:
        print(f"   ✗ AttributeError: {e}")
        print(f"      (Method probably doesn't exist or wrong signature)")
        return False
    except TypeError as e:
        print(f"   ✗ TypeError: {e}")
        print(f"      (Probably AST node constructor mismatch)")
        return False
    except Exception as e:
        print(f"   ✗ {type(e).__name__}: {e}")
        return False

def test_ast_nodes():
    """Test if AST nodes have expected structure"""
    print("\n2. Testing AST node structure...")
    
    try:
        from ailang_parser.ailang_ast import FunctionCall, Assignment, Identifier
        
        # Test FunctionCall
        try:
            # Try with string (old style)
            fc = FunctionCall(function="test", arguments=[], line=1, column=1)
            print("   ✓ FunctionCall accepts string function")
        except:
            try:
                # Try with AST node (new style)
                ident = Identifier(name="test", line=1, column=1)
                fc = FunctionCall(function=ident, arguments=[], line=1, column=1)
                print("   ✓ FunctionCall accepts AST node function")
            except Exception as e:
                print(f"   ✗ FunctionCall constructor failed: {e}")
        
        # Test Assignment
        try:
            # Try with string (old style)
            assign = Assignment(target="x", value=Identifier(name="y", line=1, column=1), line=1, column=1)
            print("   ✓ Assignment accepts string target")
        except:
            try:
                # Try with AST node (new style)
                ident = Identifier(name="x", line=1, column=1)
                assign = Assignment(target=ident, value=Identifier(name="y", line=1, column=1), line=1, column=1)
                print("   ✓ Assignment accepts AST node target")
            except Exception as e:
                print(f"   ✗ Assignment constructor failed: {e}")
                
    except ImportError as e:
        print(f"   ✗ Could not import AST nodes: {e}")
        return False
    
    return True

def test_parser_methods():
    """Check which parser methods exist"""
    print("\n3. Checking parser methods...")
    
    try:
        from ailang_parser.parser import Parser
        from ailang_parser.lexer import Lexer
        
        lexer = Lexer("")
        parser = Parser(lexer)
        
        methods_to_check = [
            'parse_expression',
            'parse_strict_expression', 
            'parse_primary',
            'parse_postfix_expression',
            'parse_identifier',
            'parse_qualified_name',
            'parse_function_call_with_base'
        ]
        
        for method_name in methods_to_check:
            if hasattr(parser, method_name):
                print(f"   ✓ {method_name} exists")
            else:
                print(f"   ✗ {method_name} NOT FOUND")
                
    except Exception as e:
        print(f"   ✗ Could not check methods: {e}")
        return False
    
    return True

def main():
    print("=== AILANG Compiler Debug Script ===\n")
    
    # Test imports
    if not test_imports():
        print("\n❌ Import test failed. Fix imports first.")
        return
    
    # Test AST structure
    test_ast_nodes()
    
    # Check parser methods
    test_parser_methods()
    
    # Test parsing simple expressions
    test_cases = [
        ("42", "4. Testing number literal"),
        ("x", "5. Testing identifier"),
        ("x = 5", "6. Testing assignment"),
        ("Add(1, 2)", "7. Testing function call"),
        ("x.y", "8. Testing member access (if supported)"),
    ]
    
    passed = 0
    failed = 0
    
    for code, description in test_cases:
        if test_simple_parse(code, description):
            passed += 1
        else:
            failed += 1
    
    print(f"\n=== Summary ===")
    print(f"Passed: {passed}/{len(test_cases)}")
    print(f"Failed: {failed}/{len(test_cases)}")
    
    if failed > 0:
        print("\n⚠️  The parser changes likely broke compatibility.")
        print("Check if FunctionCall/Assignment expect strings vs AST nodes.")
        print("Make sure parse_postfix_expression exists if parse_expression calls it.")

if __name__ == "__main__":
    main()