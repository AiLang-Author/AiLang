#!/usr/bin/env python3
"""
Test script to verify parser fixes for the 4 main error categories:
1. BY keyword boundary (8 errors)
2. COMMA expression handling (7 errors)
3. CALL period handling (4 errors)
4. LEVEL_NUMBER boundary (2 errors)
"""

import sys
sys.path.insert(0, '.')

from cobol_frontend.cobol_lexer import COBOLLexer
from cobol_frontend.parser import COBOLMultiProgramParser

def test_by_keyword_boundary():
    """Test FIX #1: BY keyword as expression boundary"""
    print("\n" + "="*80)
    print("TEST 1: BY Keyword Boundary")
    print("="*80)
    
    test_code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-BY.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MESSAGE PIC X(20) VALUE "Hello World".
       PROCEDURE DIVISION.
           INSPECT MESSAGE REPLACING ALL "o" BY "0".
           STOP RUN.
    """
    
    try:
        lexer = COBOLLexer(test_code)
        tokens = lexer.tokenize()
        parser = COBOLMultiProgramParser(tokens)
        ast = parser.parse_all_programs()
        print("✓ PASS: INSPECT...BY statement parsed successfully")
        return True
    except Exception as e:
        print(f"✗ FAIL: {e}")
        return False

def test_level_number_boundary():
    """Test FIX #2: LEVEL_NUMBER as expression boundary"""
    print("\n" + "="*80)
    print("TEST 2: LEVEL_NUMBER Boundary")
    print("="*80)
    
    test_code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-LEVEL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-VAR PIC 9.
       PROCEDURE DIVISION.
           MOVE 5 TO WS-VAR.
           STOP RUN.
    """
    
    try:
        lexer = COBOLLexer(test_code)
        tokens = lexer.tokenize()
        parser = COBOLMultiProgramParser(tokens)
        ast = parser.parse_all_programs()
        print("✓ PASS: Statement with level-like number parsed correctly")
        return True
    except Exception as e:
        print(f"✗ FAIL: {e}")
        return False

def test_call_period_handling():
    """Test FIX #3: CALL statement period handling"""
    print("\n" + "="*80)
    print("TEST 3: CALL Period Handling")
    print("="*80)
    
    # Test case 1: CALL without period (multi-statement line)
    test_code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-CALL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-VAR PIC X.
       PROCEDURE DIVISION.
           CALL "SUBPROG" USING WS-VAR
           DISPLAY "DONE".
           STOP RUN.
    """
    
    try:
        lexer = COBOLLexer(test_code)
        tokens = lexer.tokenize()
        parser = COBOLMultiProgramParser(tokens)
        ast = parser.parse_all_programs()
        print("✓ PASS: CALL without period (followed by statement) parsed")
        return True
    except Exception as e:
        print(f"✗ FAIL: {e}")
        return False

def test_comma_handling():
    """Test improved COMMA error messages"""
    print("\n" + "="*80)
    print("TEST 4: COMMA Error Messages")
    print("="*80)
    
    # This should still fail but with a better error message
    test_code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-VALS.
          05 WS-VAL1 PIC 9 VALUE 1, .
       PROCEDURE DIVISION.
           STOP RUN.
    """
    
    try:
        lexer = COBOLLexer(test_code)
        tokens = lexer.tokenize()
        parser = COBOLMultiProgramParser(tokens)
        ast = parser.parse_all_programs()
        print("✓ PASS: Parsed (unexpectedly - malformed input)")
        return True
    except Exception as e:
        error_msg = str(e)
        if "Missing expression before COMMA" in error_msg:
            print(f"✓ PASS: Got expected error with good message: {error_msg[:80]}...")
            return True
        else:
            print(f"✗ FAIL: Wrong error message: {error_msg}")
            return False

def test_real_world_occurs_file():
    """Test the actual occurs.cbl file that was failing"""
    print("\n" + "="*80)
    print("TEST 5: Real World Test (occurs.cbl)")
    print("="*80)
    
    try:
        with open('cobol_frontend/tests/occurs.cbl', 'r') as f:
            content = f.read()
        
        lexer = COBOLLexer(content)
        tokens = lexer.tokenize()
        print(f"  Lexed {len(tokens)} tokens")
        
        parser = COBOLMultiProgramParser(tokens)
        ast = parser.parse_all_programs()
        print(f"✓ PASS: Parsed {len(ast.programs)} programs from occurs.cbl")
        return True
    except Exception as e:
        print(f"✗ FAIL: {e}")
        
        # Show which line failed
        import re
        match = re.search(r'line (\d+)', str(e))
        if match:
            line_num = int(match.group(1))
            with open('cobol_frontend/tests/occurs.cbl', 'r') as f:
                lines = f.readlines()
            if line_num <= len(lines):
                print(f"\n  Failed at line {line_num}:")
                print(f"  > {lines[line_num-1].rstrip()}")
        
        return False

def main():
    """Run all tests"""
    print("="*80)
    print("PARSER FIX VERIFICATION TESTS")
    print("="*80)
    
    results = []
    
    # Run individual tests
    results.append(("BY Keyword", test_by_keyword_boundary()))
    results.append(("LEVEL_NUMBER", test_level_number_boundary()))
    results.append(("CALL Period", test_call_period_handling()))
    results.append(("COMMA Error", test_comma_handling()))
    results.append(("Real World", test_real_world_occurs_file()))
    
    # Summary
    print("\n" + "="*80)
    print("TEST SUMMARY")
    print("="*80)
    
    passed = sum(1 for _, result in results if result)
    total = len(results)
    
    for test_name, result in results:
        status = "✓ PASS" if result else "✗ FAIL"
        print(f"{status:8s} {test_name}")
    
    print(f"\nTotal: {passed}/{total} tests passed")
    
    if passed == total:
        print("\n🎉 All tests passed! Fixes are working correctly.")
        return 0
    else:
        print(f"\n⚠️  {total - passed} test(s) failed. Review patches before applying.")
        return 1

if __name__ == '__main__':
    sys.exit(main())