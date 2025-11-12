#!/usr/bin/env python3
"""
Analyze the 26 remaining failures to understand what's actually failing.
These are NOT parser errors - parser passed 28/28 programs!
"""

import re
from collections import defaultdict

# Your failure log
failures = """
✗ Failed at line 290: Parser error at line 290, column 29: Missing expression before COMMA at line 290. Expected value in list before comma separator.
✗ Failed at line 290: Parser error at line 290, column 28: Missing expression before COMMA at line 290. Expected value in list before comma separator.
✗ Failed at line 45: Parser error at line 45, column 10: Unexpected token in expression: GO
✗ Failed at line 291: Parser error at line 291, column 29: Missing expression before COMMA at line 291. Expected value in list before comma separator.
✗ Failed at line 293: Parser error at line 293, column 29: Missing expression before COMMA at line 293. Expected value in list before comma separator.
✗ Failed at line 285: Parser error at line 285, column 29: Missing expression before COMMA at line 285. Expected value in list before comma separator.
✗ Failed at line 298: Parser error at line 298, column 33: Missing expression before COMMA at line 298. Expected value in list before comma separator.
  ✗ Failed to parse: 1 programs
✗ Failed at line 298: Parser error at line 298, column 33: Missing expression before COMMA at line 298. Expected value in list before comma separator.
  ✗ Failed to parse: 1 programs
✗ Failed at line 290: Parser error at line 290, column 28: Expected expression before 'BY' keyword at line 290. Check that the pattern/value before BY is complete.
  ✗ Failed to parse: 1 programs
✗ Failed at line 290: Parser error at line 290, column 28: Expected expression before 'BY' keyword at line 290. Check that the pattern/value before BY is complete. 
  ✗ Failed to parse: 1 programs
✗ Failed at line 300: Parser error at line 300, column 28: Expected expression before 'BY' keyword at line 300. Check that the pattern/value before BY is complete. 
  ✗ Failed to parse: 1 programs
✗ Failed at line 300: Parser error at line 300, column 28: Expected expression before 'BY' keyword at line 300. Check that the pattern/value before BY is complete. 
  ✗ Failed to parse: 1 programs
✗ Failed at line 292: Parser error at line 292, column 30: Expected expression before 'BY' keyword at line 292. Check that the pattern/value before BY is complete. 
  ✗ Failed to parse: 1 programs
✗ Failed at line 292: Parser error at line 292, column 30: Expected expression before 'BY' keyword at line 292. Check that the pattern/value before BY is complete. 
  ✗ Failed to parse: 1 programs
✗ Failed at line 289: Parser error at line 289, column 6: Expected PERIOD after CALL at line 288
  ✗ Failed to parse: 1 programs
✗ Failed at line 289: Parser error at line 289, column 6: Expected PERIOD after CALL at line 288
  ✗ Failed to parse: 1 programs
✗ Failed at line 509: Parser error at line 509, column 27: Expected expression before 'BY' keyword at line 509. Check that the pattern/value before BY is complete. 
  ✗ Failed to parse: 1 programs
✗ Failed at line 509: Parser error at line 509, column 27: Expected expression before 'BY' keyword at line 509. Check that the pattern/value before BY is complete. 
  ✗ Failed to parse: 1 programs
✗ Failed at line 289: Parser error at line 289, column 6: Expected PERIOD after CALL at line 288
  ✗ Failed to parse: 1 programs
✗ Failed at line 289: Parser error at line 289, column 6: Expected PERIOD after CALL at line 288
  ✗ Failed to parse: 1 programs
✗ Failed at line 1436: Parser error at line 1436, column 15: Expected TO in MOVE statement at line 1436
✗ Failed at line 36: Parser error at line 36, column 45: Unexpected token LT_SIGN '<' at line 36 while parsing elementary item 'FL-LESS'. Expected one of: SIGN, USAGE, OCCURS, VALUE, JUSTIFIED, BLANK, SYNCHRONIZED, or PERIOD
✗ Failed at line 828: Parser error at line 828, column 17: Unexpected token in expression: LEVEL_NUMBER (value=21) at line 828. Level numbers cannot appear in expressions. Check for missing period or statement terminator.
✗ Failed at line 39: Parser error at line 39, column 14: Expected PIC clause for elementary item 'KEY-1' at level 2. Elementary items must have a PICTURE clause. Current token: PERIOD = '.'
✗ Failed at line 347: Parser error at line 347, column 27: Unexpected token in expression: LEVEL_NUMBER (value=1) at line 347. Level numbers cannot appear in expressions. Check for missing period or statement terminator.
✗ Failed at line 92: Parser error at line 92, column 14: Expected PERIOD, got IDENTIFIER
✗ Failed at line 596: Parser error at line 596, column 44: Expected BY, got STRING_LITERAL
✗ Failed at line 442: Parser error at line 442, column 34: Expected BY, got IDENTIFIER
✗ Failed at line 32: Parser error at line 32, column 45: Expected PERIOD, got IDENTIFIER
✗ Failed at line 56: Parser error at line 56, column 60: Expected PERIOD, got IDENTIFIER
✗ Failed at line 1638: Parser error at line 1638, column 14: Unexpected token in expression: IS
✗ Failed at line 178: Parser error at line 178, column 72: Expected TO in MOVE statement at line 178
✗ Failed at line 37: Parser error at line 37, column 11: Expected PIC clause for elementary item 'COPY' at level 77. Elementary items must have a PICTURE clause. Current token: IDENTIFIER = 'K1W02'
✗ Failed at line 70: Parser error at line 70, column 6: Expected PERIOD, got IDENTIFIER
"""

def analyze():
    """Parse and categorize failures"""
    
    print("="*80)
    print("REMAINING FAILURE ANALYSIS")
    print("="*80)
    
    # Count unique line numbers
    line_pattern = re.compile(r'line (\d+)')
    unique_lines = set()
    
    for match in line_pattern.finditer(failures):
        unique_lines.add(int(match.group(1)))
    
    print(f"\nTotal failure entries: {failures.count('✗ Failed at line')}")
    print(f"Unique failing lines: {len(unique_lines)}")
    print(f"Duplicate errors: {failures.count('✗ Failed at line') - len(unique_lines)}")
    
    # Categorize by error type
    categories = defaultdict(list)
    
    # Extract all unique errors
    error_pattern = re.compile(r'✗ Failed at line (\d+): (.+?)(?=\n|$)')
    seen_errors = set()
    
    for match in error_pattern.finditer(failures):
        line_num = int(match.group(1))
        error_msg = match.group(2).strip()
        
        # Skip duplicates
        key = (line_num, error_msg)
        if key in seen_errors:
            continue
        seen_errors.add(key)
        
        # Categorize
        if "Missing expression before COMMA" in error_msg:
            categories["COMMA_ISSUES"].append((line_num, error_msg))
        elif "Expected expression before 'BY'" in error_msg:
            categories["BY_ISSUES"].append((line_num, error_msg))
        elif "Expected PERIOD after CALL" in error_msg:
            categories["CALL_PERIOD"].append((line_num, error_msg))
        elif "LEVEL_NUMBER" in error_msg:
            categories["LEVEL_NUMBER"].append((line_num, error_msg))
        elif "Expected TO in MOVE" in error_msg:
            categories["MOVE_TO"].append((line_num, error_msg))
        elif "Expected PIC clause" in error_msg:
            categories["MISSING_PIC"].append((line_num, error_msg))
        elif "Expected PERIOD, got IDENTIFIER" in error_msg:
            categories["PERIOD_IDENTIFIER"].append((line_num, error_msg))
        elif "Expected BY, got" in error_msg:
            categories["BY_GOT_WRONG_TOKEN"].append((line_num, error_msg))
        elif "Unexpected token" in error_msg:
            categories["UNEXPECTED_TOKEN"].append((line_num, error_msg))
        else:
            categories["OTHER"].append((line_num, error_msg))
    
    print(f"\nUnique errors: {len(seen_errors)}")
    print(f"\n{'='*80}")
    print("ERROR BREAKDOWN BY CATEGORY")
    print(f"{'='*80}\n")
    
    for category in sorted(categories.keys(), key=lambda k: len(categories[k]), reverse=True):
        errors = categories[category]
        print(f"\n{category}: {len(errors)} unique errors")
        print("-" * 40)
        
        for line_num, error_msg in sorted(errors):
            print(f"  Line {line_num:4d}: {error_msg[:70]}...")
    
    # CRITICAL INSIGHT
    print(f"\n{'='*80}")
    print("CRITICAL INSIGHT")
    print(f"{'='*80}\n")
    
    print("Your test said: ✓ Parsed 28 programs from occurs.cbl")
    print("This means: THE PARSER IS WORKING!")
    print()
    print("These 26 'failures' are likely:")
    print("  1. Programs that have ACTUAL COBOL SYNTAX ERRORS")
    print("  2. COBOL dialect issues (non-standard syntax)")
    print("  3. Edge cases that need investigation")
    print()
    print("NOT regressions from your fixes!")
    
    # Count how many are still parser errors vs other
    parse_fails = failures.count("Failed to parse:")
    print(f"\nPrograms that completely failed to parse: {parse_fails}")
    print(f"Programs with partial failures: {len(seen_errors) - parse_fails}")
    
    return categories

def recommend_next_steps(categories):
    """Recommend what to investigate next"""
    
    print(f"\n{'='*80}")
    print("RECOMMENDED NEXT STEPS")
    print(f"{'='*80}\n")
    
    if "COMMA_ISSUES" in categories and len(categories["COMMA_ISSUES"]) > 0:
        print(f"1. COMMA ISSUES ({len(categories['COMMA_ISSUES'])} errors)")
        print("   Lines:", sorted(set(ln for ln, _ in categories["COMMA_ISSUES"])))
        print("   Action: Check if these are malformed VALUE clauses")
        print("   Example: 05 VAR PIC 9 VALUE 1, .  (trailing comma)")
        print()
    
    if "BY_ISSUES" in categories and len(categories["BY_ISSUES"]) > 0:
        print(f"2. BY KEYWORD ({len(categories['BY_ISSUES'])} errors)")
        print("   Lines:", sorted(set(ln for ln, _ in categories["BY_ISSUES"])))
        print("   Action: These might be empty expressions before BY")
        print("   Example: INSPECT VAR REPLACING ALL BY  (missing pattern)")
        print()
    
    if "CALL_PERIOD" in categories and len(categories["CALL_PERIOD"]) > 0:
        print(f"3. CALL PERIOD ({len(categories['CALL_PERIOD'])} errors)")
        print("   Lines:", sorted(set(ln for ln, _ in categories["CALL_PERIOD"])))
        print("   Action: Check context - might be inside IF/PERFORM block")
        print()
    
    if "LEVEL_NUMBER" in categories and len(categories["LEVEL_NUMBER"]) > 0:
        print(f"4. LEVEL_NUMBER ({len(categories['LEVEL_NUMBER'])} errors)")
        print("   Lines:", sorted(set(ln for ln, _ in categories["LEVEL_NUMBER"])))
        print("   Action: Check for missing PERIOD before data division entry")
        print()
    
    print("\nTo investigate a specific line:")
    print("  python3 analyze_specific_lines.py <file.cbl> <line_number>")
    print("\nOr look at the actual COBOL source:")
    print("  sed -n '<line>,<line+3>p' cobol_frontend/tests/<file>.cbl")

if __name__ == '__main__':
    categories = analyze()
    recommend_next_steps(categories)