#!/usr/bin/env python3
"""
Validate truth table coverage in expression and statement parsers.
Identifies missing boundary checks and incomplete patterns.
"""

import sys
import re
from pathlib import Path
from collections import defaultdict

sys.path.insert(0, '.')

from cobol_frontend.cobol_lexer import COBOLTokenType

def extract_boundary_checks(filepath):
    """Extract all token types checked in _is_expression_boundary()"""
    
    with open(filepath, 'r') as f:
        content = f.read()
    
    # Find the _is_expression_boundary function
    boundary_func = re.search(
        r'def _is_expression_boundary\(self.*?\n(.*?)(?=\n\s*def |\nclass |\Z)',
        content,
        re.DOTALL
    )
    
    if not boundary_func:
        return set(), []
    
    func_body = boundary_func.group(1)
    
    # Extract all COBOLTokenType checks
    token_checks = set()
    for match in re.finditer(r'COBOLTokenType\.(\w+)', func_body):
        token_checks.add(match.group(1))
    
    # Extract comments about what's checked
    comments = re.findall(r'#\s*(.+)', func_body)
    
    return token_checks, comments

def analyze_statement_keywords():
    """Get all statement keywords that should be boundaries"""
    
    # These are keywords that start statements and should stop expressions
    statement_keywords = [
        'MOVE', 'ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE', 'COMPUTE',
        'IF', 'PERFORM', 'DISPLAY', 'ACCEPT', 'CALL', 'GO', 'STOP',
        'EXIT', 'EVALUATE', 'SEARCH', 'INSPECT', 'STRING', 'UNSTRING',
        'READ', 'WRITE', 'OPEN', 'CLOSE', 'DELETE', 'REWRITE',
        'START', 'RETURN', 'RELEASE', 'SORT', 'MERGE',
    ]
    
    # These are clause keywords that should also stop expressions
    clause_keywords = [
        'TO', 'FROM', 'GIVING', 'BY', 'UNTIL', 'VARYING',
        'THEN', 'ELSE', 'WHEN', 'END-IF', 'END-PERFORM',
    ]
    
    return statement_keywords, clause_keywords

def check_boundary_coverage():
    """Check if all necessary keywords are in boundary checks"""
    
    print(f"\n{'='*80}")
    print("TRUTH TABLE COVERAGE ANALYSIS")
    print(f"{'='*80}\n")
    
    # Get what's currently checked
    expr_file = Path('cobol_frontend/parser/expression_parsers.py')
    if not expr_file.exists():
        print("ERROR: expression_parsers.py not found")
        print(f"Looked in: {expr_file.absolute()}")
        return set(), [], set()
    
    checked_tokens, comments = extract_boundary_checks(expr_file)
    
    print("Currently checked in _is_expression_boundary():")
    for token in sorted(checked_tokens):
        print(f"  ✓ {token}")
    print(f"\nTotal: {len(checked_tokens)} token types\n")
    
    # Get what should be checked
    stmt_keywords, clause_keywords = analyze_statement_keywords()
    
    print("Statement keywords that SHOULD be boundaries:")
    missing_stmt = []
    for keyword in stmt_keywords:
        if keyword in checked_tokens:
            print(f"  ✓ {keyword:15s} (covered)")
        else:
            print(f"  ✗ {keyword:15s} (MISSING)")
            missing_stmt.append(keyword)
    
    print(f"\nClause keywords that SHOULD be boundaries:")
    missing_clause = []
    for keyword in clause_keywords:
        if keyword in checked_tokens:
            print(f"  ✓ {keyword:15s} (covered)")
        else:
            print(f"  ✗ {keyword:15s} (MISSING)")
            missing_clause.append(keyword)
    
    return missing_stmt, missing_clause, checked_tokens

def analyze_error_patterns_vs_boundaries(missing_keywords):
    """Cross-reference missing boundaries with actual errors"""
    
    failures_file = Path('failures.log')
    if not failures_file.exists():
        return
    
    with open(failures_file, 'r', encoding='utf-8', errors='ignore') as f:
        content = f.read()
    
    print(f"\n{'='*80}")
    print("CORRELATION: Missing boundaries vs Actual errors")
    print(f"{'='*80}\n")
    
    # Check if errors mention any of the missing keywords
    for keyword in missing_keywords:
        pattern = re.compile(f"before '{keyword}' keyword", re.IGNORECASE)
        matches = pattern.findall(content)
        
        if matches:
            print(f"✗ {keyword:15s} - CAUSES {len(matches)} ERRORS IN PRODUCTION")
        else:
            print(f"  {keyword:15s} - not causing errors (yet)")

def check_parse_primary_boundaries():
    """Check what parse_primary() considers as boundaries"""
    
    print(f"\n{'='*80}")
    print("PARSE_PRIMARY BOUNDARY ANALYSIS")
    print(f"{'='*80}\n")
    
    expr_file = Path('cobol_frontend/expression_parsers.py')
    with open(expr_file, 'r') as f:
        content = f.read()
    
    # Find parse_primary function
    primary_func = re.search(
        r'def parse_primary\(self\).*?\n(.*?)(?=\n\s*def |\nclass |\Z)',
        content,
        re.DOTALL
    )
    
    if not primary_func:
        print("Could not find parse_primary()")
        return
    
    func_body = primary_func.group(1)
    
    # Check for error cases
    error_cases = re.findall(
        r'if token_type == COBOLTokenType\.(\w+):.*?self\.error\((.*?)\)',
        func_body,
        re.DOTALL
    )
    
    print("Token types that cause errors in parse_primary():")
    for token_type, error_msg in error_cases:
        # Clean up the error message
        error_msg = error_msg[:60].replace('\n', ' ').strip()
        print(f"  - {token_type:20s} → {error_msg}")
    
    # Check what's handled
    handled_tokens = set(re.findall(r'if self\.match\(COBOLTokenType\.(\w+)', func_body))
    print(f"\nToken types explicitly handled: {len(handled_tokens)}")
    for token in sorted(handled_tokens)[:15]:
        print(f"  ✓ {token}")
    if len(handled_tokens) > 15:
        print(f"  ... and {len(handled_tokens) - 15} more")

def identify_truth_table_gaps():
    """Identify gaps in truth table coverage"""
    
    print(f"\n{'='*80}")
    print("TRUTH TABLE GAP ANALYSIS")
    print(f"{'='*80}\n")
    
    # Load failures
    failures_file = Path('failures.log')
    if not failures_file.exists():
        print("No failures.log found")
        return
    
    with open(failures_file, 'r', encoding='utf-8', errors='ignore') as f:
        content = f.read()
    
    # Extract unique error patterns
    error_patterns = defaultdict(int)
    
    # Pattern 1: "Expected X before Y"
    for match in re.finditer(r'Expected (\w+) before [\'"](\w+)[\'"]', content):
        expected = match.group(1)
        found = match.group(2)
        pattern = f"Expected {expected} before {found}"
        error_patterns[pattern] += 1
    
    # Pattern 2: "Unexpected token X"
    for match in re.finditer(r'Unexpected token[^:]*:\s*(\w+)', content):
        token = match.group(1)
        pattern = f"Unexpected token: {token}"
        error_patterns[pattern] += 1
    
    # Pattern 3: "Expected X, got Y"
    for match in re.finditer(r'Expected (\w+), got (\w+)', content):
        expected = match.group(1)
        found = match.group(2)
        pattern = f"Expected {expected}, got {found}"
        error_patterns[pattern] += 1
    
    print("Top error patterns (indicating truth table gaps):\n")
    sorted_patterns = sorted(error_patterns.items(), key=lambda x: x[1], reverse=True)
    
    for i, (pattern, count) in enumerate(sorted_patterns[:15], 1):
        print(f"{i:2d}. [{count:3d}x] {pattern}")
    
    # Analyze what these mean
    print(f"\n{'='*80}")
    print("INTERPRETATION")
    print(f"{'='*80}\n")
    
    print("These patterns indicate:")
    print("  1. 'Expected X before Y' → Y should be in expression boundary checks")
    print("  2. 'Unexpected token: X' → X appearing where not expected (boundary issue)")
    print("  3. 'Expected X, got Y' → Parser expected X but truth table doesn't handle Y")

def main():
    """Main diagnostic"""
    
    print("="*80)
    print("TRUTH TABLE VALIDATION TOOL")
    print("="*80)
    
    # Check boundary coverage
    missing_stmt, missing_clause, checked = check_boundary_coverage()
    
    # Correlate with actual errors
    all_missing = missing_stmt + missing_clause
    if all_missing:
        analyze_error_patterns_vs_boundaries(all_missing)
    
    # Check parse_primary boundaries
    check_parse_primary_boundaries()
    
    # Identify gaps
    identify_truth_table_gaps()
    
    print(f"\n{'='*80}")
    print("SUMMARY")
    print(f"{'='*80}\n")
    print(f"Expression boundary checks: {len(checked)} token types")
    print(f"Missing statement keywords: {len(missing_stmt)}")
    print(f"Missing clause keywords: {len(missing_clause)}")
    
    if all_missing:
        print(f"\nCritical missing boundaries:")
        for kw in all_missing[:5]:
            print(f"  - {kw}")

if __name__ == '__main__':
    main()