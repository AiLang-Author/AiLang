#!/usr/bin/env python3
"""
Analyze the 22 programs that failed to parse
Find common patterns in the failures
"""

import sys
import os
from collections import defaultdict

sys.path.insert(0, os.path.abspath('cobol_frontend'))

from cobol_lexer import COBOLLexer
from parser import COBOLMultiProgramParser

# Your NIST test suite path
test_suite_path = 'cobol_frontend/tests'

def analyze_failure(filepath):
    """Analyze why a specific file failed"""
    
    with open(filepath, 'r') as f:
        source = f.read()
    
    try:
        lexer = COBOLLexer(source)
        tokens = lexer.tokenize()
        
        parser = COBOLMultiProgramParser(tokens, debug=False)
        ast = parser.parse_all_programs()
        
        # Check if it has failures
        if hasattr(ast, 'failed_programs') and ast.failed_programs:
            return {
                'status': 'partial',
                'success_count': len(ast.programs),
                'failure_count': len(ast.failed_programs),
                'errors': [(line, err) for line, err in ast.failed_programs]
            }
        
        return {
            'status': 'success',
            'program_count': len(ast.programs)
        }
        
    except Exception as e:
        return {
            'status': 'failed',
            'error': str(e),
            'error_type': type(e).__name__
        }

def extract_error_pattern(error_msg):
    """Extract the key pattern from an error message"""
    if "Expected PERIOD" in error_msg:
        # Extract what was found instead
        import re
        match = re.search(r'Expected PERIOD, got (\w+)', error_msg)
        if match:
            return f"PERIOD→{match.group(1)}"
        return "PERIOD"
    elif "Expected PIC" in error_msg:
        return "PIC"
    elif "INDEXED" in error_msg:
        return "INDEXED"
    elif "USAGE" in error_msg:
        return "USAGE"
    elif "Expected" in error_msg:
        match = re.search(r'Expected (\w+)', error_msg)
        if match:
            return f"Expected_{match.group(1)}"
    return "OTHER"

def main():
    """Run analysis on NIST test suite"""
    
    print("=" * 80)
    print("ANALYZING FAILED PROGRAMS IN NIST TEST SUITE")
    print("=" * 80)
    print()
    
    # Get all .cbl files
    test_files = []
    for root, dirs, files in os.walk(test_suite_path):
        for file in files:
            if file.endswith('.cbl'):
                test_files.append(os.path.join(root, file))
    
    print(f"Found {len(test_files)} COBOL files in test suite")
    print()
    
    # Analyze each file
    results = {}
    failures = []
    successes = []
    partials = []
    
    for filepath in test_files:
        filename = os.path.basename(filepath)
        result = analyze_failure(filepath)
        results[filename] = result
        
        if result['status'] == 'failed':
            failures.append((filename, result))
        elif result['status'] == 'partial':
            partials.append((filename, result))
        else:
            successes.append(filename)
    
    # Report
    print("=" * 80)
    print("RESULTS SUMMARY")
    print("=" * 80)
    print(f"✓ Fully successful:  {len(successes)}")
    print(f"⚠ Partial success:   {len(partials)}")
    print(f"✗ Complete failures: {len(failures)}")
    print()
    
    if failures:
        print("=" * 80)
        print(f"COMPLETE FAILURES ({len(failures)} files)")
        print("=" * 80)
        
        # Group by error type
        error_types = defaultdict(list)
        for filename, result in failures:
            error_types[result.get('error_type', 'Unknown')].append((filename, result['error']))
        
        for error_type, items in error_types.items():
            print(f"\n{error_type}: {len(items)} files")
            for filename, error in items[:5]:  # Show first 5
                short_error = error[:60] + "..." if len(error) > 60 else error
                print(f"  - {filename}")
                print(f"    {short_error}")
            if len(items) > 5:
                print(f"  ... and {len(items) - 5} more")
    
    if partials:
        print("\n" + "=" * 80)
        print(f"PARTIAL FAILURES ({len(partials)} files)")
        print("=" * 80)
        
        # Analyze error patterns in partial failures
        error_patterns = defaultdict(list)
        
        for filename, result in partials:
            for line, error_msg in result['errors']:
                pattern = extract_error_pattern(error_msg)
                error_patterns[pattern].append((filename, line, error_msg))
        
        # Show most common patterns
        for pattern, items in sorted(error_patterns.items(), key=lambda x: len(x[1]), reverse=True):
            print(f"\n{pattern}: {len(items)} occurrences")
            # Show 3 examples
            for filename, line, error_msg in items[:3]:
                print(f"  - {filename} line {line}")
                print(f"    {error_msg[:80]}")
            if len(items) > 3:
                print(f"  ... and {len(items) - 3} more")
    
    print("\n" + "=" * 80)
    print("RECOMMENDATION")
    print("=" * 80)
    print()
    
    if error_patterns:
        top_pattern = max(error_patterns.items(), key=lambda x: len(x[1]))
        print(f"Focus on: {top_pattern[0]} ({len(top_pattern[1])} occurrences)")
        print(f"This is the most common failure pattern.")
        print()
        print("Example file to debug:")
        example = top_pattern[1][0]
        print(f"  File: {example[0]}")
        print(f"  Line: {example[1]}")
        print(f"  Error: {example[2]}")

if __name__ == '__main__':
    main()