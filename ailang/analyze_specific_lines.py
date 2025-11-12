#!/usr/bin/env python3
"""
Analyze specific problematic lines from COBOL test files.
Shows token stream, parser state, and context for debugging.
"""

import sys
import re
from pathlib import Path

sys.path.insert(0, '.')

from cobol_frontend.cobol_lexer import COBOLLexer, COBOLTokenType
from cobol_frontend.parser import COBOLMultiProgramParser

def extract_context(lines, target_line, context_size=5):
    """Extract lines around the target with line numbers"""
    start = max(0, target_line - context_size - 1)
    end = min(len(lines), target_line + context_size)
    
    context = []
    for i in range(start, end):
        marker = " >>> " if i == target_line - 1 else "     "
        context.append(f"{marker}{i+1:4d}: {lines[i]}")
    
    return '\n'.join(context)

def tokenize_line(line, line_num):
    """Tokenize a single line and show the token stream"""
    try:
        lexer = COBOLLexer(line)
        tokens = lexer.tokenize()
        
        print(f"\n  Token stream ({len(tokens)} tokens):")
        for i, token in enumerate(tokens):
            print(f"    [{i:2d}] {token.type.name:20s} = '{token.value}'")
        
        return tokens
    except Exception as e:
        print(f"\n  Lexer error: {e}")
        return None

def analyze_by_keyword_context(lines, line_num):
    """Special analysis for BY keyword errors"""
    target_line = lines[line_num - 1]
    
    print("\n  BY KEYWORD ANALYSIS:")
    
    # Check what comes before BY
    by_match = re.search(r'(.{0,30})\s+BY\s+(.{0,30})', target_line, re.IGNORECASE)
    if by_match:
        before_by = by_match.group(1).strip()
        after_by = by_match.group(2).strip()
        
        print(f"    Before BY: '{before_by}'")
        print(f"    After BY:  '{after_by}'")
        
        # Check if this is INSPECT REPLACING
        if 'REPLACING' in target_line.upper():
            print("    → Pattern: INSPECT...REPLACING...BY")
            print("    → Issue: Expression parser consuming past BY keyword")
            print("    → Fix needed: Add BY to _is_expression_boundary()")
        
        # Check if this is REPLACE statement
        elif 'REPLACE' in target_line.upper():
            print("    → Pattern: REPLACE...BY")
            print("    → Issue: REPLACE statement parsing may be broken")
    else:
        print("    → BY keyword not found on this line (may be continuation)")

def analyze_comma_context(lines, line_num):
    """Special analysis for COMMA errors"""
    target_line = lines[line_num - 1]
    
    print("\n  COMMA ANALYSIS:")
    
    # Look for comma patterns
    if ',,' in target_line:
        print("    → Found double comma: ',,'")
        print("    → Empty value in list")
    
    if re.search(r',\s*\.', target_line):
        print("    → Found trailing comma before period: ', .'")
        print("    → Empty trailing value")
    
    # Check for VALUE clause
    if 'VALUE' in target_line.upper():
        value_match = re.search(r'VALUE[S]?\s+(.+)', target_line, re.IGNORECASE)
        if value_match:
            value_part = value_match.group(1)
            print(f"    → VALUE clause: '{value_part}'")
            
            # Count commas
            comma_count = value_part.count(',')
            print(f"    → Comma count: {comma_count}")
            
            # Split by comma and check for empties
            values = [v.strip() for v in value_part.split(',')]
            empty_values = [i for i, v in enumerate(values) if not v or v == '.']
            if empty_values:
                print(f"    → Empty values at positions: {empty_values}")

def analyze_period_context(lines, line_num):
    """Special analysis for PERIOD errors"""
    target_line = lines[line_num - 1]
    
    print("\n  PERIOD ANALYSIS:")
    
    # Check if line has period
    has_period = '.' in target_line
    print(f"    → Line has period: {has_period}")
    
    # Check if this is after CALL
    if 'CALL' in target_line.upper():
        print("    → CALL statement on this line")
        
        # Check if next line starts a statement
        if line_num < len(lines):
            next_line = lines[line_num].strip()
            stmt_keywords = ['MOVE', 'DISPLAY', 'IF', 'PERFORM', 'CALL', 'ADD', 'SUBTRACT']
            starts_with_stmt = any(next_line.upper().startswith(kw) for kw in stmt_keywords)
            
            if starts_with_stmt:
                print(f"    → Next line starts new statement: '{next_line[:40]}'")
                print("    → Issue: Multi-statement line or missing period")
                print("    → Fix: Use consume_optional_period() instead of consume()")

def analyze_specific_line(filename, line_num):
    """Analyze a specific line from a COBOL file"""
    
    filepath = Path('cobol_frontend/tests') / filename
    if not filepath.exists():
        print(f"ERROR: File not found: {filepath}")
        return
    
    with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
        lines = f.readlines()
    
    if line_num < 1 or line_num > len(lines):
        print(f"ERROR: Line {line_num} out of range (file has {len(lines)} lines)")
        return
    
    print(f"\n{'='*80}")
    print(f"ANALYZING: {filename} line {line_num}")
    print(f"{'='*80}")
    
    # Show context
    print("\nCONTEXT:")
    print(extract_context(lines, line_num))
    
    # Tokenize the problematic line
    target_line = lines[line_num - 1]
    print(f"\nTARGET LINE:")
    print(f"  Raw: {target_line.rstrip()}")
    tokens = tokenize_line(target_line, line_num)
    
    return target_line, tokens, lines

def batch_analyze_errors():
    """Analyze all errors from failures.log"""
    
    failures_file = Path('failures.log')
    if not failures_file.exists():
        print("ERROR: failures.log not found")
        return
    
    with open(failures_file, 'r', encoding='utf-8', errors='ignore') as f:
        content = f.read()
    
    # Extract line numbers and errors
    pattern = re.compile(r'Parser error at line (\d+), column \d+: (.+?)(?=\n|Parser error)', re.MULTILINE | re.DOTALL)
    
    errors = []
    for match in pattern.finditer(content):
        line_num = int(match.group(1))
        error_msg = match.group(2).strip()
        errors.append((line_num, error_msg))
    
    if not errors:
        print("No parseable errors found in failures.log")
        return
    
    # Group by error type
    by_errors = [(ln, err) for ln, err in errors if 'before \'BY\' keyword' in err]
    comma_errors = [(ln, err) for ln, err in errors if 'before COMMA' in err]
    period_errors = [(ln, err) for ln, err in errors if 'PERIOD after CALL' in err]
    
    print(f"\n{'='*80}")
    print("BATCH ERROR ANALYSIS")
    print(f"{'='*80}")
    print(f"\nFound {len(errors)} distinct error messages")
    print(f"  - BY keyword errors: {len(by_errors)}")
    print(f"  - COMMA errors: {len(comma_errors)}")
    print(f"  - PERIOD errors: {len(period_errors)}")
    
    # Analyze first few of each type
    if by_errors:
        print(f"\n--- Analyzing first BY keyword error ---")
        for line_num, error_msg in by_errors[:1]:
            print(f"Line {line_num}: {error_msg[:80]}...")
    
    if comma_errors:
        print(f"\n--- Analyzing first COMMA error ---")
        for line_num, error_msg in comma_errors[:1]:
            print(f"Line {line_num}: {error_msg[:80]}...")

def main():
    """Main entry point"""
    
    if len(sys.argv) > 2:
        # Analyze specific line: python analyze_specific_lines.py file.cbl 123
        filename = sys.argv[1]
        line_num = int(sys.argv[2])
        
        target_line, tokens, lines = analyze_specific_line(filename, line_num)
        
        # Do contextual analysis based on error patterns
        if target_line:
            if 'BY' in target_line.upper():
                analyze_by_keyword_context(lines, line_num)
            if ',' in target_line:
                analyze_comma_context(lines, line_num)
            if 'CALL' in target_line.upper():
                analyze_period_context(lines, line_num)
    else:
        # Batch analyze from failures.log
        batch_analyze_errors()
        print("\n" + "="*80)
        print("USAGE: python analyze_specific_lines.py <file.cbl> <line_number>")
        print("       For detailed analysis of a specific line")
        print("="*80)

if __name__ == '__main__':
    main()