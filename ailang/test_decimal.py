#!/usr/bin/env python3
"""
Test MERGE with NIST-style formatting (the actual format from EXEC85.cbl)
"""

import sys
sys.path.insert(0, '.')

from cobol_frontend.cobol_lexer import COBOLLexer
from cobol_frontend.parser import COBOLMultiProgramParser

def test_merge_basic():
    """Test basic MERGE statement"""
    
    print("=" * 70)
    print("TEST 1: Basic MERGE Statement")
    print("=" * 70)
    
    # Simplified version - all on one line like NIST does it
    code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ST301M.
       PROCEDURE DIVISION.
           MERGE SORTFILE ON ASCENDING KEY MYKEY USING FILE1 FILE2 GIVING OUTFILE.
           STOP RUN.
    """
    
    try:
        lexer = COBOLLexer(code)
        tokens = lexer.tokenize()
        
        # Debug: show tokens
        print("\nTokens generated:")
        merge_section = False
        for i, tok in enumerate(tokens):
            if tok.value == 'MERGE':
                merge_section = True
            if merge_section:
                print(f"  [{i}] {tok.type.name:20s} = '{tok.value}'")
                if tok.type.name == 'PERIOD' and tok.value == '.':
                    break
        
        parser = COBOLMultiProgramParser(tokens, debug=True)
        result = parser.parse_all_programs()
        
        # Handle both single program and list of programs
        if isinstance(result, list):
            programs = result
        else:
            # It's a COBOLCompilationUnit
            programs = result.programs if hasattr(result, 'programs') else [result]
        
        print(f"\n✅ SUCCESS: Parsed {len(programs)} program(s)")
        if programs:
            first_prog = programs[0]
            prog_id = first_prog.program_id if hasattr(first_prog, 'program_id') else 'Unknown'
            
            # Handle procedure_division - could be object or list
            if hasattr(first_prog, 'procedure_division'):
                proc_div = first_prog.procedure_division
                if hasattr(proc_div, 'statements'):
                    stmt_count = len(proc_div.statements) if proc_div.statements else 0
                elif isinstance(proc_div, list):
                    stmt_count = len(proc_div)
                else:
                    stmt_count = 'N/A'
            else:
                stmt_count = 0
            
            print(f"   Program: {prog_id}")
            print(f"   Statements: {stmt_count}")
        return True
        
    except Exception as e:
        print(f"\n❌ FAILED: {e}")
        import traceback
        traceback.print_exc()
        return False

def test_merge_multiline():
    """Test MERGE with multi-line format (more readable)"""
    
    print("\n" + "=" * 70)
    print("TEST 2: Multi-line MERGE Statement")
    print("=" * 70)
    
    code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ST301M.
       PROCEDURE DIVISION.
           MERGE SORTFILE
               ON ASCENDING KEY MYKEY
               USING FILE1 FILE2
               GIVING OUTFILE.
           STOP RUN.
    """
    
    try:
        lexer = COBOLLexer(code)
        tokens = lexer.tokenize()
        parser = COBOLMultiProgramParser(tokens, debug=False)
        result = parser.parse_all_programs()
        
        # Handle both single program and list of programs
        programs = result if isinstance(result, list) else [result] if result else []
        
        print(f"\n✅ SUCCESS: Parsed {len(programs)} program(s)")
        return True
        
    except Exception as e:
        print(f"\n❌ FAILED: {e}")
        
        # Show where it failed
        if "Expected" in str(e):
            print("\nParser expected something but didn't find it")
            print("This usually means:")
            print("  - Keyword not recognized (check lexer)")
            print("  - Wrong token order (check parse_merge logic)")
            print("  - Missing required clause")
        
        return False

def test_merge_with_file_section():
    """Test MERGE with proper file declarations (like NIST)"""
    
    print("\n" + "=" * 70)
    print("TEST 3: MERGE with File Section (NIST style)")
    print("=" * 70)
    
    code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ST301M.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SORTFILE ASSIGN TO DISK.
           SELECT FILE1 ASSIGN TO DISK.
           SELECT FILE2 ASSIGN TO DISK.
           SELECT OUTFILE ASSIGN TO DISK.
       DATA DIVISION.
       FILE SECTION.
       SD SORTFILE.
       01 SORT-REC.
           05 MYKEY PIC X(10).
       FD FILE1.
       01 F1-REC PIC X(80).
       FD FILE2.
       01 F2-REC PIC X(80).
       FD OUTFILE.
       01 OUT-REC PIC X(80).
       PROCEDURE DIVISION.
           MERGE SORTFILE ON ASCENDING KEY MYKEY USING FILE1 FILE2 GIVING OUTFILE.
           STOP RUN.
    """
    
    try:
        lexer = COBOLLexer(code)
        tokens = lexer.tokenize()
        parser = COBOLMultiProgramParser(tokens, debug=False)
        result = parser.parse_all_programs()
        
        # Handle both single program and list of programs
        programs = result if isinstance(result, list) else [result] if result else []
        
        print(f"\n✅ SUCCESS: Parsed {len(programs)} program(s)")
        print(f"   This matches NIST EXEC85.cbl format!")
        return True
        
    except Exception as e:
        print(f"\n❌ FAILED: {e}")
        return False

def diagnose_parse_merge():
    """Look at parse_merge implementation"""
    
    print("\n" + "=" * 70)
    print("PARSE_MERGE IMPLEMENTATION CHECK")
    print("=" * 70)
    
    try:
        with open('cobol_frontend/parser/statement_parsers.py', 'r') as f:
            content = f.read()
        
        # Find parse_merge
        if 'def parse_merge' not in content:
            print("❌ parse_merge() not found")
            return
        
        print("✓ parse_merge() exists")
        
        # Check for key parsing sections
        checks = [
            ('ON ASCENDING/DESCENDING', 'ASCENDING' in content and 'DESCENDING' in content),
            ('USING clause', 'USING' in content),
            ('GIVING clause', 'GIVING' in content),
            ('Truth table comments', '# Truth table:' in content or '# TRUTH TABLE' in content),
        ]
        
        for check_name, found in checks:
            status = "✓" if found else "❌"
            print(f"{status} {check_name}")
        
        # Check for phase-based parsing
        if 'PHASE' in content or 'Phase' in content:
            print("✓ Uses phase-based parsing pattern")
        
    except Exception as e:
        print(f"❌ Could not analyze: {e}")

def main():
    """Run all tests"""
    
    print("\n" + "=" * 70)
    print("MERGE STATEMENT COMPREHENSIVE TEST")
    print("=" * 70)
    
    results = []
    
    # Test 1: Basic MERGE
    results.append(("Basic MERGE", test_merge_basic()))
    
    # Test 2: Multi-line MERGE
    results.append(("Multi-line MERGE", test_merge_multiline()))
    
    # Test 3: Full NIST-style
    results.append(("NIST-style MERGE", test_merge_with_file_section()))
    
    # Diagnostic
    diagnose_parse_merge()
    
    # Summary
    print("\n" + "=" * 70)
    print("TEST SUMMARY")
    print("=" * 70)
    
    for test_name, passed in results:
        status = "✅ PASS" if passed else "❌ FAIL"
        print(f"{status}: {test_name}")
    
    if all(r[1] for r in results):
        print("\n🎉 ALL TESTS PASSED - MERGE parser is working!")
        print("\nReady to parse NIST EXEC85.cbl")
    else:
        print("\n⚠️  Some tests failed")
        print("\nLikely issue: parse_merge() logic needs adjustment")
        print("Check: Does your parse_merge() handle all COBOL formats?")
    
    print("=" * 70)
    
    return 0 if all(r[1] for r in results) else 1

if __name__ == '__main__':
    sys.exit(main())