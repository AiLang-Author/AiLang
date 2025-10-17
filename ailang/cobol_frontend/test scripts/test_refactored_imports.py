#!/usr/bin/env python3
"""
Test that refactored modules can be imported successfully
Run this AFTER refactoring to verify nothing broke
"""
import sys

def test_parser_imports():
    """Test parser module imports"""
    print("Testing parser imports...")
    try:
        from parser import COBOLMultiProgramParser
        print("  ✓ parser.COBOLMultiProgramParser")
        
        from parser.ast_nodes import COBOLProgram, COBOLDisplay
        print("  ✓ parser.ast_nodes (dataclasses)")
        
        return True
    except Exception as e:
        print(f"  ✗ Parser import failed: {e}")
        return False

def test_converter_imports():
    """Test converter module imports"""
    print("\nTesting converter imports...")
    try:
        from converter import COBOLToAilangMultiProgramConverter, AILangASTSerializer
        print("  ✓ converter.COBOLToAilangMultiProgramConverter")
        print("  ✓ converter.AILangASTSerializer")
        
        return True
    except Exception as e:
        print(f"  ✗ Converter import failed: {e}")
        return False

def test_integration():
    """Test that integration still works"""
    print("\nTesting integration...")
    try:
        from cobol_lexer import COBOLLexer
        from parser import COBOLMultiProgramParser
        from converter import COBOLToAilangMultiProgramConverter
        
        # Try a minimal parse
        code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       PROCEDURE DIVISION.
           STOP RUN.
        """
        
        lexer = COBOLLexer(code)
        tokens = lexer.tokenize()
        parser = COBOLMultiProgramParser(tokens)
        ast = parser.parse_all_programs()
        
        converter = COBOLToAilangMultiProgramConverter()
        result = converter.convert(ast)
        
        print("  ✓ End-to-end pipeline works!")
        return True
        
    except Exception as e:
        print(f"  ✗ Integration test failed: {e}")
        import traceback
        traceback.print_exc()
        return False

def main():
    print("="*70)
    print("REFACTORED MODULE IMPORT TEST")
    print("="*70)
    
    results = []
    results.append(("Parser", test_parser_imports()))
    results.append(("Converter", test_converter_imports()))
    results.append(("Integration", test_integration()))
    
    print("\n" + "="*70)
    print("RESULTS")
    print("="*70)
    
    for name, passed in results:
        status = "✓ PASS" if passed else "✗ FAIL"
        print(f"  {name:20s}: {status}")
    
    all_passed = all(r[1] for r in results)
    
    print("\n" + "="*70)
    if all_passed:
        print("✓ ALL TESTS PASSED - Refactoring successful!")
        return 0
    else:
        print("✗ SOME TESTS FAILED - Check errors above")
        print("\nTo rollback:")
        print("  mv cobol_parser.py.backup cobol_parser.py")
        print("  mv cobol_ast_converter.py.backup cobol_ast_converter.py")
        print("  rm -rf parser/ converter/")
        return 1

if __name__ == '__main__':
    sys.exit(main())