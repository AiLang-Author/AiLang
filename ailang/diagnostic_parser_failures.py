#!/usr/bin/env python3
"""
Diagnostic script to analyze parser failures and identify root causes.
Focus on the top error patterns from failures.log
"""

import sys
import re
from collections import defaultdict, Counter
from pathlib import Path

sys.path.insert(0, '.')

from cobol_frontend.cobol_lexer import COBOLLexer
from cobol_frontend.parser import COBOLMultiProgramParser

def load_test_files():
    """Load all COBOL test files that failed"""
    test_dir = Path('cobol_frontend/tests')
    cobol_files = {}
    
    if test_dir.exists():
        for cbl_file in test_dir.glob('*.cbl'):
            try:
                with open(cbl_file, 'r', encoding='utf-8', errors='ignore') as f:
                    cobol_files[cbl_file.name] = f.read()
            except Exception as e:
                print(f"Warning: Could not read {cbl_file}: {e}")
    
    return cobol_files

def extract_line_from_file(filename, line_num, cobol_files):
    """Extract specific line from a COBOL file"""
    if filename not in cobol_files:
        return None
    
    lines = cobol_files[filename].split('\n')
    if 0 < line_num <= len(lines):
        return lines[line_num - 1]
    return None

def analyze_failure_log():
    """Parse failures.log and categorize all errors"""
    
    failures_file = Path('failures.log')
    if not failures_file.exists():
        print("ERROR: failures.log not found")
        print("Run your test suite first to generate failures.log")
        return None
    
    with open(failures_file, 'r', encoding='utf-8', errors='ignore') as f:
        content = f.read()
    
    # Extract all failure entries with file context
    # Pattern: ✗ Failed at line N: error message
    failure_pattern = re.compile(r'✗ Failed at line (\d+): (.+?)(?=\n|$)', re.MULTILINE)
    
    failures = []
    for match in failure_pattern.finditer(content):
        line_num = int(match.group(1))
        error_msg = match.group(2).strip()
        
        failures.append({
            'line': line_num,
            'error': error_msg
        })
    
    print(f"\n{'='*80}")
    print(f"LOADED {len(failures)} FAILURE ENTRIES FROM failures.log")
    print(f"{'='*80}")
    
    return failures

def categorize_errors(failures):
    """Categorize errors by type using pattern matching"""
    
    # Define error categories with regex patterns
    categories = {
        'PERIOD_AFTER_CALL': re.compile(r'Expected PERIOD after CALL'),
        'EXPR_BEFORE_BY': re.compile(r'Expected expression before .BY. keyword'),
        'MISSING_EXPR_COMMA': re.compile(r'Missing expression before COMMA'),
        'BY_GOT_STRING': re.compile(r'Expected BY, got STRING_LITERAL'),
        'BY_GOT_IDENTIFIER': re.compile(r'Expected BY, got IDENTIFIER'),
        'UNEXPECTED_LEVEL_NUMBER': re.compile(r'Unexpected token in expression: LEVEL_NUMBER'),
        'UNEXPECTED_IS': re.compile(r'Unexpected token in expression: IS'),
        'UNEXPECTED_GO': re.compile(r'Unexpected token in expression: GO'),
        'PIC_CLAUSE_MISSING': re.compile(r'Expected PIC clause for elementary item'),
        'UNEXPECTED_LT_SIGN': re.compile(r'Unexpected token LT_SIGN'),
        'PERIOD_GOT_IDENTIFIER': re.compile(r'Expected PERIOD, got IDENTIFIER'),
        'TO_IN_MOVE': re.compile(r'Expected TO in MOVE statement'),
        'FAILED_TO_PARSE': re.compile(r'✗ Failed to parse: \d+ programs'),
    }
    
    categorized = defaultdict(list)
    uncategorized = []
    
    for failure in failures:
        error = failure['error']
        line = failure['line']
        
        matched = False
        for category, pattern in categories.items():
            if pattern.search(error):
                categorized[category].append(failure)
                matched = True
                break
        
        if not matched:
            uncategorized.append(failure)
    
    return categorized, uncategorized

def print_category_summary(categorized, uncategorized):
    """Print summary of error categories"""
    
    print(f"\n{'='*80}")
    print("ERROR CATEGORY SUMMARY (Sorted by frequency)")
    print(f"{'='*80}\n")
    
    # Sort by frequency
    sorted_cats = sorted(categorized.items(), key=lambda x: len(x[1]), reverse=True)
    
    for category, failures in sorted_cats:
        count = len(failures)
        print(f"{category:30s} : {count:3d} occurrences")
        
        # Show first 2 examples
        for i, failure in enumerate(failures[:2]):
            print(f"  └─ Line {failure['line']}: {failure['error'][:70]}...")
        
        if count > 2:
            print(f"  └─ ... and {count - 2} more")
        print()
    
    if uncategorized:
        print(f"{'UNCATEGORIZED':30s} : {len(uncategorized):3d} occurrences")
        for failure in uncategorized[:3]:
            print(f"  └─ Line {failure['line']}: {failure['error'][:70]}...")
        print()

def analyze_by_keyword_pattern(failures):
    """Deep dive into 'Expected expression before BY' errors"""
    
    print(f"\n{'='*80}")
    print("DEEP DIVE: 'Expected expression before BY' errors")
    print(f"{'='*80}\n")
    
    by_errors = [f for f in failures if 'before \'BY\' keyword' in f['error']]
    
    if not by_errors:
        print("No BY keyword errors found")
        return
    
    print(f"Found {len(by_errors)} BY keyword errors\n")
    
    # Analyze common line numbers
    line_nums = [f['line'] for f in by_errors]
    line_counter = Counter(line_nums)
    
    print("Most common error lines:")
    for line_num, count in line_counter.most_common(5):
        print(f"  Line {line_num}: {count} occurrences")
    
    print("\nLikely causes:")
    print("  1. INSPECT/REPLACE statements with BY keyword")
    print("  2. Expression parser not recognizing BY as clause boundary")
    print("  3. Empty expression before BY in statements like:")
    print("     INSPECT VAR REPLACING ALL <expr> BY")
    print("     REPLACE <something> BY")

def analyze_comma_pattern(failures):
    """Deep dive into 'Missing expression before COMMA' errors"""
    
    print(f"\n{'='*80}")
    print("DEEP DIVE: 'Missing expression before COMMA' errors")
    print(f"{'='*80}\n")
    
    comma_errors = [f for f in failures if 'Missing expression before COMMA' in f['error']]
    
    if not comma_errors:
        print("No COMMA errors found")
        return
    
    print(f"Found {len(comma_errors)} COMMA errors\n")
    
    # Extract line numbers
    line_nums = [f['line'] for f in comma_errors]
    line_counter = Counter(line_nums)
    
    print("Most common error lines:")
    for line_num, count in line_counter.most_common(5):
        print(f"  Line {line_num}: {count} occurrences")
    
    print("\nLikely causes:")
    print("  1. Level 88 VALUE clauses with trailing commas")
    print("  2. Empty elements in comma-separated lists")
    print("  3. Multi-line value lists with continuation issues")
    print("  4. Pattern: VALUE 1, 2, , 3  (double comma)")
    print("  5. Pattern: VALUE 1, 2,  (trailing comma)")

def analyze_period_pattern(failures):
    """Deep dive into PERIOD-related errors"""
    
    print(f"\n{'='*80}")
    print("DEEP DIVE: PERIOD-related errors")
    print(f"{'='*80}\n")
    
    period_errors = [f for f in failures if 'PERIOD' in f['error']]
    
    if not period_errors:
        print("No PERIOD errors found")
        return
    
    # Subcategorize
    after_call = [f for f in period_errors if 'after CALL' in f['error']]
    got_identifier = [f for f in period_errors if 'got IDENTIFIER' in f['error']]
    other = [f for f in period_errors if f not in after_call and f not in got_identifier]
    
    print(f"Expected PERIOD after CALL: {len(after_call)} occurrences")
    print(f"Expected PERIOD, got IDENTIFIER: {len(got_identifier)} occurrences")
    print(f"Other PERIOD errors: {len(other)} occurrences\n")
    
    if after_call:
        print("Sample 'after CALL' errors:")
        for f in after_call[:2]:
            print(f"  Line {f['line']}: {f['error'][:70]}...")
    
    print("\nLikely causes:")
    print("  1. Multi-statement lines (CALL X. MOVE Y.)")
    print("  2. Context-sensitive period rules not properly implemented")
    print("  3. Statement boundary detection failing")

def generate_fix_recommendations(categorized):
    """Generate prioritized fix recommendations"""
    
    print(f"\n{'='*80}")
    print("PRIORITIZED FIX RECOMMENDATIONS")
    print(f"{'='*80}\n")
    
    # Sort by frequency to prioritize
    sorted_cats = sorted(categorized.items(), key=lambda x: len(x[1]), reverse=True)
    
    fix_map = {
        'FAILED_TO_PARSE': {
            'impact': 'CRITICAL',
            'file': 'parser/division_parsers.py',
            'fix': 'Recovery after parse errors is failing - programs can\'t be parsed at all',
            'action': 'Review error recovery logic in parse_all_programs()'
        },
        'EXPR_BEFORE_BY': {
            'impact': 'HIGH',
            'file': 'expression_parsers.py',
            'fix': 'Add BY to expression boundary checks in _is_expression_boundary()',
            'action': 'Ensure BY keyword stops expression parsing like TO/FROM/GIVING'
        },
        'PERIOD_AFTER_CALL': {
            'impact': 'HIGH',
            'file': 'statement_parsers.py',
            'fix': 'Relax period requirement for CALL in certain contexts',
            'action': 'Use consume_optional_period() or check _is_statement_boundary()'
        },
        'MISSING_EXPR_COMMA': {
            'impact': 'MEDIUM',
            'file': 'expression_parsers.py',
            'fix': 'Better handling of empty expressions before commas in VALUE lists',
            'action': 'Add validation in _parse_level88_values() to skip empty values'
        },
        'UNEXPECTED_LEVEL_NUMBER': {
            'impact': 'MEDIUM',
            'file': 'expression_parsers.py',
            'fix': 'Add LEVEL_NUMBER to expression boundary checks',
            'action': 'Level numbers should never appear in expression context'
        },
        'BY_GOT_STRING': {
            'impact': 'MEDIUM',
            'file': 'statement_parsers.py',
            'fix': 'INSPECT/REPLACE parsing may have incorrect token consumption',
            'action': 'Review parse_inspect() - may be consuming BY keyword prematurely'
        },
    }
    
    priority = 1
    for category, failures in sorted_cats[:6]:  # Top 6 issues
        if category in fix_map:
            info = fix_map[category]
            count = len(failures)
            
            print(f"[{priority}] {category} ({count} occurrences) - {info['impact']} IMPACT")
            print(f"    File: {info['file']}")
            print(f"    Fix: {info['fix']}")
            print(f"    Action: {info['action']}")
            print()
            priority += 1

def main():
    """Main diagnostic routine"""
    
    print("="*80)
    print("COBOL PARSER FAILURE DIAGNOSTIC TOOL")
    print("="*80)
    
    # Step 1: Load failure log
    failures = analyze_failure_log()
    if not failures:
        return
    
    # Step 2: Categorize errors
    categorized, uncategorized = categorize_errors(failures)
    
    # Step 3: Print summary
    print_category_summary(categorized, uncategorized)
    
    # Step 4: Deep dives into top error patterns
    analyze_by_keyword_pattern(failures)
    analyze_comma_pattern(failures)
    analyze_period_pattern(failures)
    
    # Step 5: Generate fix recommendations
    generate_fix_recommendations(categorized)
    
    print(f"\n{'='*80}")
    print("DIAGNOSTIC COMPLETE")
    print(f"{'='*80}")
    print(f"\nTotal failures analyzed: {len(failures)}")
    print(f"Categorized: {sum(len(v) for v in categorized.values())}")
    print(f"Uncategorized: {len(uncategorized)}")

if __name__ == '__main__':
    main()