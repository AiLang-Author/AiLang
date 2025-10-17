#!/usr/bin/env python3
"""
NIST COBOL85 Test Suite Ingester
Tests the COBOL parser against real NIST validation code
"""

import sys
import os

# Direct imports to avoid __init__.py issues
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from cobol_lexer import COBOLLexer
from cobol_parser import COBOLMultiProgramParser

# The NIST test suite snippet from EXEC85
NIST_CODE = """CCVS85  VERSION 4.0   01 OCT 1992 0032                                          
*HEADER,COBOL,EXEC85                                                            
000100 IDENTIFICATION DIVISION.                                         EXEC84.2
000200                                                                  EXEC84.2
000400 PROGRAM-ID.                                                      EXEC84.2
000500     EXEC85.                                                      EXEC84.2
000600 INSTALLATION.                                                    EXEC84.2
000700     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".EXEC84.2
000800     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".EXEC84.2
000900 ENVIRONMENT DIVISION.                                            EXEC84.2
001000                                                                  EXEC84.2
001100****************************************************************  EXEC84.2
001200*                                                              *  EXEC84.2
001300*    VALIDATION FOR:-                                          *  EXEC84.2
001400*                                                              *  EXEC84.2
001500*    "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".EXEC84.2
001600*                                                              *  EXEC84.2
001700*    "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".EXEC84.2
001800*                                                              *  EXEC84.2
001900****************************************************************  EXEC84.2
002000 CONFIGURATION SECTION.                                           EXEC84.2
002100                                                                  EXEC84.2
002200 SPECIAL-NAMES.                                                   EXEC84.2
002300 INPUT-OUTPUT SECTION.                                            EXEC84.2
002400 FILE-CONTROL.                                                    EXEC84.2
002500     SELECT  OPTIONAL POPULATION-FILE                             EXEC84.2
002600     ASSIGN TO                                                    EXEC84.2
002700     XXXXX001.                                                    EXEC84.2
002800     SELECT  SOURCE-COBOL-PROGRAMS                                EXEC84.2
002900     ASSIGN TO                                                    EXEC84.2
003000     XXXXX002                                                     EXEC84.2
003100     ORGANIZATION SEQUENTIAL.                                     EXEC84.2
003200     SELECT  UPDATED-POPULATION-FILE                              EXEC84.2
003300     ASSIGN TO                                                    EXEC84.2
003400     XXXXX003.                                                    EXEC84.2
003500     SELECT  PRINT-FILE                                           EXEC84.2
003600     ASSIGN TO                                                    EXEC84.2
003700     XXXXX055.                                                    EXEC84.2
003800     SELECT  CONTROL-CARD-FILE                                    EXEC84.2
003900     ASSIGN TO                                                    EXEC84.2
004000     XXXXX058.                                                    EXEC84.2
004100 DATA DIVISION.                                                   EXEC84.2
004200 FILE SECTION.                                                    EXEC84.2
004300 FD  POPULATION-FILE.                                             EXEC84.2
004400*    RECORD CONTAINS 2400 CHARACTERS.                             EXEC84.2
004500 01  SOURCE-IN-2400.                                              EXEC84.2
004600     02 SOURCE-IN                    PIC X(80).                   EXEC84.2
004700*                                              OCCURS 30.         EXEC84.2
004800 FD  CONTROL-CARD-FILE.                                           EXEC84.2
004900 01  CONTROL-RECORD                  PIC X(80).                   EXEC84.2
005000 FD  PRINT-FILE.                                                  EXEC84.2
005100 01  PRINT-REC.                                                   EXEC84.2
005200   05        FILLER                  PIC X.                       EXEC84.2
005300   05        PRINT-DATA              PIC X(131).                  EXEC84.2
005400 FD  SOURCE-COBOL-PROGRAMS                                        EXEC84.2
005500     BLOCK CONTAINS 1 RECORDS.                                    EXEC84.2
005600 01  CT-OUT.                                                      EXEC84.2
005700     02 FILLER PIC X(72).                                         EXEC84.2
005800     02 FILLER PIC X(8).                                          EXEC84.2
005900 FD  UPDATED-POPULATION-FILE                                      EXEC84.2
006000     RECORD CONTAINS 2400 CHARACTERS.                             EXEC84.2
006100 01  UPDATED-SOURCE-OUT-2400.                                     EXEC84.2
006200     02 UD-SOURCE-OUT                PIC X(80)  OCCURS 30.        EXEC84.2
006300                                                                  EXEC84.2
006400 WORKING-STORAGE SECTION.                                         EXEC84.2
006500                                                                  EXEC84.2
006600 01  FILLER                          PIC X(40)  VALUE             EXEC84.2
006700            "NEWEXEC WORKING-STORAGE STARTS HERE ==->".           EXEC84.2
006800 01  BLOCK-TYPE                      PIC X(5).                    EXEC84.2
006900 01  SUB1                            PIC S9(3)  COMP.             EXEC84.2
007000 01  SUB2                            PIC S9(3)  COMP.             EXEC84.2
007100 01  SUB3                            PIC S9(3)  COMP.             EXEC84.2
007200 01  SUB4                            PIC S9(3)  COMP.             EXEC84.2
007300 01  SUB5                            PIC S9(3)  COMP.             EXEC84.2
007400 01  SUB6                            PIC S9(3)  COMP.             EXEC84.2
007500 01  SUB7                            PIC S9(3)  COMP.             EXEC84.2
007600 01  WA-ERR-IND                      PIC 9 VALUE ZEROES.          EXEC84.2
007700 01  WA-FIRST-IND                    PIC 9 VALUE ZEROES.          EXEC84.2
007800 01  WA-ZCARD-TABLE.                                              EXEC84.2
007900   05        WA-ZCARD                OCCURS 10                    EXEC84.2
008000                                     PIC X(60).                   EXEC84.2
008100 01  WA-TOP-OF-PAGE-LINE.                                         EXEC84.2
008200   05        FILLER                  PIC X(4)   VALUE SPACES.     EXEC84.2
008300   05        WA-VERSION.                                          EXEC84.2
008400     07      WA-VERSION-TEXT         PIC X(22)  VALUE             EXEC84.2
008500            "CCVS85 VERSION NUMBER ".                             EXEC84.2
008600     07      WA-VERSION-NUM          PIC X(3) VALUE SPACES.       EXEC84.2
008700   05        WA-RELEASE.                                          EXEC84.2
008800     07      WA-RELEASE-TEXT         PIC X(14)  VALUE             EXEC84.2
008900            ", RELEASED ON ".                                     EXEC84.2
009000     07      WA-VERSION-DATE         PIC X(11) VALUE SPACES.      EXEC84.2
009100   05        FILLER                  PIC X(4)   VALUE SPACES.     EXEC84.2
009200   05        WA-COMPANY-AND-COMPILER PIC X(30) VALUE SPACES.      EXEC84.2
009300   05        FILLER                  PIC X(5)   VALUE SPACES.     EXEC84.2
009400   05        WA-DATE                 PIC XXBXXBXX.                EXEC84.2
009500   05        FILLER                  PIC X(4)   VALUE SPACES.     EXEC84.2
009600   05        FILLER                  PIC X(5)   VALUE "PAGE ".    EXEC84.2
009700   05        WA-PAGE-CT              PIC Z(5)9.                   EXEC84.2
009800                                                                  EXEC84.2
009900 01  WA-ACCT-LINE-1.                                              EXEC84.2
010000   05        FILLER                  PIC X(19)  VALUE             EXEC84.2
010100            " ** END OF PROGRAM ".                                EXEC84.2
010200   05        WA-CURRENT-PROG         PIC X(6).                    EXEC84.2
010300   05        FILLER                  PIC X(32)  VALUE             EXEC84.2
010400            " FOUND,  COBOL LINES PROCESSED: ".                   EXEC84.2
010500   05        WA-LINES-COBOL          PIC Z(5)9.                   EXEC84.2
010600 01  WA-ACCT-LINE-2.                                              EXEC84.2
010700   05        FILLER                  PIC X(19)  VALUE             EXEC84.2
010800            " ** LINES INSERTED ".                                EXEC84.2
010900   05        WA-LINES-INSERTED       PIC Z(5)9.                   EXEC84.2
011000   05        FILLER                  PIC X(19)  VALUE             EXEC84.2
011100            " ** LINES REPLACED ".                                EXEC84.2
011200   05        WA-LINES-REPLACED       PIC Z(5)9.                   EXEC84.2
011300   05        FILLER                  PIC X(19)  VALUE             EXEC84.2
011400            " ** LINES DELETED  ".                                EXEC84.2
011500   05        WA-LINES-DELETED        PIC Z(5)9.                   EXEC84.2
011600 01  WA-ACCT-LINE-3.                                              EXEC84.2
011700   05        FILLER                  PIC X(18)  VALUE             EXEC84.2
011800            " ** OPTIONAL CODE ".                                 EXEC84.2
011900   05        WA-OPTIONAL-CODE        PIC X(8).                    EXEC84.2
012000   05        WA-CODE-REMOVED         PIC Z(5)9.                   EXEC84.2
012100   05        WA-CODE-KILLED          PIC X(21)  VALUE             EXEC84.2
012200            " ** COMMENTS DELETED ".                              EXEC84.2
012300   05        WA-COMMENTS-DEL         PIC Z(5)9.                   EXEC84.2
012400 01  WA-FINAL-LINE-1.                                             EXEC84.2
012500   05        FILLER                  PIC X(34)  VALUE             EXEC84.2
012600            " ** END OF POPULATION FILE REACHED".                 EXEC84.2
012700   05        FILLER                  PIC X(27)  VALUE             EXEC84.2
012800            " NUMBER OF PROGRAMS FOUND: ".                        EXEC84.2
012900   05        WA-PROGS-FOUND          PIC Z(5)9.                   EXEC84.2
013000 01  WA-FINAL-LINE-2.                                             EXEC84.2
013100   05        FILLER                  PIC X(47)  VALUE             EXEC84.2
013200            " ** NUMBER OF PROGRAMS WRITTEN TO SOURCE FILE: ".    EXEC84.2
013300   05        WA-SOURCE-PROGS         PIC Z(5)9.                   EXEC84.2
013400 01  WA-FINAL-LINE-3.                                             EXEC84.2
013500   05        FILLER                  PIC X(48)  VALUE             EXEC84.2
013600            " ** NUMBER OF PROGRAMS WRITTEN TO NEW POPULATION".   EXEC84.2
013700   05        FILLER                  PIC X(7)   VALUE " FILE: ".  EXEC84.2
013800   05        WA-NEWPOP-PROGS         PIC Z(5)9.                   EXEC84.2
013900 01  WB-CONTROL-DATA.                                             EXEC84.2
"""

def analyze_tokens():
    """Step 1: Tokenize and analyze what tokens we get"""
    print("=" * 80)
    print("STEP 1: TOKENIZING NIST CODE")
    print("=" * 80)
    
    lexer = COBOLLexer(NIST_CODE)
    tokens = lexer.tokenize()
    
    print(f"\n✓ Generated {len(tokens)} tokens")
    
    # Show first 50 tokens
    print("\nFirst 50 tokens:")
    for i, token in enumerate(tokens[:50]):
        print(f"  {i:3d}. Line {token.line:4d} | {token.type.name:20s} = '{token.value}'")
    
    # Count token types
    from collections import Counter
    token_types = Counter(t.type.name for t in tokens)
    print(f"\nToken type distribution:")
    for token_type, count in token_types.most_common(20):
        print(f"  {token_type:25s}: {count:5d}")
    
    return tokens

def test_parse(tokens):
    """Step 2: Try to parse and see what fails"""
    print("\n" + "=" * 80)
    print("STEP 2: PARSING")
    print("=" * 80)
    
    parser = COBOLMultiProgramParser(tokens)
    
    try:
        ast = parser.parse_all_programs()
        print(f"\n✓ PARSE SUCCEEDED!")
        print(f"  Programs found: {len(ast.programs)}")
        
        for i, program in enumerate(ast.programs):
            print(f"\n  Program {i+1}: {program.program_id}")
            if program.data_division:
                ws = program.data_division.working_storage or []
                print(f"    - Working-Storage vars: {len(ws)}")
                if len(ws) > 0:
                    print(f"      First few: {[v.name for v in ws[:5]]}")
            if program.procedure_division:
                paras = program.procedure_division.paragraphs or []
                print(f"    - Paragraphs: {len(paras)}")
        
        return ast, None
        
    except Exception as e:
        print(f"\n✗ PARSE FAILED!")
        print(f"  Error: {e}")
        print(f"  Position: {parser.pos}/{len(tokens)}")
        
        if parser.pos < len(tokens):
            print(f"\n  Failed at token {parser.pos}:")
            for i in range(max(0, parser.pos-3), min(len(tokens), parser.pos+5)):
                marker = " >>> " if i == parser.pos else "     "
                t = tokens[i]
                print(f"{marker}{i:3d}. Line {t.line:4d} | {t.type.name:20s} = '{t.value}'")
        
        return None, e

def find_missing_features(error):
    """Step 3: Analyze the error to find what's missing"""
    print("\n" + "=" * 80)
    print("STEP 3: IDENTIFYING MISSING FEATURES")
    print("=" * 80)
    
    if error is None:
        print("\n✓ No missing features detected - parser succeeded!")
        return
    
    error_msg = str(error).lower()
    
    missing = []
    
    # Check for specific features
    checks = {
        "INSTALLATION paragraph": ["installation"],
        "SPECIAL-NAMES": ["special-names", "special_names"],
        "FILE-CONTROL": ["file-control", "file_control"],
        "SELECT OPTIONAL": ["optional"],
        "ASSIGN TO": ["assign"],
        "ORGANIZATION": ["organization"],
        "BLOCK CONTAINS": ["block contains", "block_contains"],
        "RECORD CONTAINS": ["record contains", "record_contains"],
        "PIC with B (insertion)": ["xxbxxbxx", "pic.*b"],
        "PIC Z (zero suppression)": ["pic z", "pic.*z"],
        "FILLER": ["filler"],
    }
    
    for feature, patterns in checks.items():
        if any(pattern in error_msg for pattern in patterns):
            missing.append(feature)
    
    if missing:
        print("\n⚠ Potentially missing features:")
        for feature in missing:
            print(f"  - {feature}")
    else:
        print("\n? Could not identify specific missing feature from error")
        print("  Raw error:", error)
    
    return missing

def main():
    """Main test harness"""
    print("""
╔═══════════════════════════════════════════════════════════════════════════╗
║              NIST COBOL85 Test Suite Ingestion Report                     ║
║                                                                            ║
║  Testing parser against real NIST validation code (EXEC85)                ║
╚═══════════════════════════════════════════════════════════════════════════╝
""")
    
    # Step 1: Tokenize
    tokens = analyze_tokens()
    
    # Step 2: Parse
    ast, error = test_parse(tokens)
    
    # Step 3: Find missing features
    if error:
        missing = find_missing_features(error)
    
    # Summary
    print("\n" + "=" * 80)
    print("SUMMARY")
    print("=" * 80)
    
    if ast:
        print("\n✓ SUCCESS: Parser can handle NIST COBOL85 test code!")
        print("  Next steps:")
        print("    1. Test conversion to Ailang AST")
        print("    2. Test full compilation to binary")
        print("    3. Run more NIST test cases")
    else:
        print("\n✗ FAILED: Parser needs enhancements")
        print("  Next steps:")
        print("    1. Implement missing features identified above")
        print("    2. Re-run this test")
        print("    3. Iterate until passing")

if __name__ == "__main__":
    main()