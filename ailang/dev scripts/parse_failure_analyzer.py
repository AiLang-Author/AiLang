#!/usr/bin/env python3
"""
Parse Failure Analyzer
Extracts and categorizes the 27 failed programs from parser output
"""

import re
import sys
from collections import defaultdict

def analyze_parse_output(log_file):
    """Parse the log file and extract failure information"""
    
    with open(log_file, 'r') as f:
        content = f.read()
    
    # Extract failed program attempts
    failed_programs = []
    
    # Pattern: ✗ Failed to parse program at line XXXXX: error message
    failure_pattern = r'âœ— Failed to parse program at line (\d+): (.+?)(?=\n|$)'
    
    for match in re.finditer(failure_pattern, content):
        line_num = match.group(1)
        error_msg = match.group(2)
        failed_programs.append({
            'line': int(line_num),
            'error': error_msg
        })
    
    # Categorize errors
    categories = defaultdict(list)
    
    for failure in failed_programs:
        error = failure['error']
        
        if 'Expected PERIOD' in error:
            category = 'missing_period'
        elif 'Expected' in error and 'got' in error:
            category = 'unexpected_token'
        elif 'PICTURE' in error or 'PIC' in error:
            category = 'picture_clause'
        elif 'LEVEL' in error:
            category = 'level_number'
        elif 'DIVISION' in error:
            category = 'division_structure'
        elif 'SECTION' in error:
            category = 'section_structure'
        elif 'PERFORM' in error:
            category = 'perform_statement'
        elif 'IF' in error:
            category = 'if_statement'
        elif 'MOVE' in error:
            category = 'move_statement'
        elif 'EVALUATE' in error:
            category = 'evaluate_statement'
        else:
            category = 'other'
        
        categories[category].append(failure)
    
    return failed_programs, categories


def print_failure_report(failed_programs, categories):
    """Print formatted failure report"""
    
    print("="*80)
    print("PARSE FAILURE ANALYSIS")
    print("="*80)
    print(f"\nTotal Failed Programs: {len(failed_programs)}")
    print(f"Success Rate: {((473 - len(failed_programs)) / 473 * 100):.1f}%\n")
    
    print("="*80)
    print("FAILURE CATEGORIES")
    print("="*80)
    
    for category, failures in sorted(categories.items(), key=lambda x: -len(x[1])):
        print(f"\n{category.upper().replace('_', ' ')}: {len(failures)} failures")
        print("-"*80)
        
        for i, failure in enumerate(failures[:5], 1):  # Show first 5
            print(f"  {i}. Line {failure['line']}: {failure['error'][:70]}")
        
        if len(failures) > 5:
            print(f"  ... and {len(failures) - 5} more")
    
    print("\n" + "="*80)
    print("RECOMMENDATIONS")
    print("="*80)
    
    # Provide recommendations based on error categories
    if 'missing_period' in categories:
        print("\nâš  Missing Period Errors:")
        print("   These are often legitimate syntax errors in test files.")
        print("   Review if the COBOL source is valid or needs manual fixes.")
    
    if 'unexpected_token' in categories:
        print("\nâš  Unexpected Token Errors:")
        print("   May indicate parser needs to handle additional COBOL syntax.")
        print("   Check if these are obscure/deprecated COBOL features.")
    
    if 'picture_clause' in categories:
        print("\nâš  Picture Clause Errors:")
        print("   Complex PIC clauses may need additional parser support.")
        print("   Verify if syntax is valid COBOL-85.")
    
    print("\nâœ… Overall Assessment:")
    print(f"   With 94.3% success rate ({473-len(failed_programs)}/473 programs),")
    print("   the parser is performing well!")
    print("\n   Next steps:")
    print("   1. Review the 27 failures individually")
    print("   2. Determine if they're bad test syntax or parser bugs")
    print("   3. Fix legitimate parser issues")
    print("   4. Document/skip known bad test cases")


def extract_program_names_from_source(source_file, line_numbers):
    """Try to extract program names from source at given line numbers"""
    
    try:
        with open(source_file, 'r') as f:
            lines = f.readlines()
        
        programs = {}
        
        for line_num in line_numbers:
            # Search backwards for PROGRAM-ID
            for i in range(max(0, line_num - 50), line_num):
                if i < len(lines):
                    line = lines[i]
                    if 'PROGRAM-ID' in line:
                        # Extract program name
                        match = re.search(r'PROGRAM-ID\.\s+([A-Z0-9\-]+)', line, re.IGNORECASE)
                        if match:
                            programs[line_num] = match.group(1)
                            break
        
        return programs
    
    except Exception as e:
        print(f"Warning: Could not extract program names: {e}")
        return {}


def main():
    if len(sys.argv) < 2:
        print("Usage: python3 parse_failure_analyzer.py <log_file> [source_file]")
        print("\nExample:")
        print("  python3 parse_failure_analyzer.py NIST1.log cobol_frontend/tests/newcob.cbl")
        sys.exit(1)
    
    log_file = sys.argv[1]
    source_file = sys.argv[2] if len(sys.argv) > 2 else None
    
    print(f"Analyzing parse failures from: {log_file}\n")
    
    failed_programs, categories = analyze_parse_output(log_file)
    
    if not failed_programs:
        print("âœ… No parse failures found!")
        sys.exit(0)
    
    # Try to extract program names if source file provided
    if source_file:
        line_numbers = [f['line'] for f in failed_programs]
        program_names = extract_program_names_from_source(source_file, line_numbers)
        
        # Add program names to failures
        for failure in failed_programs:
            if failure['line'] in program_names:
                failure['program_name'] = program_names[failure['line']]
    
    print_failure_report(failed_programs, categories)
    
    # Save detailed report
    output_file = log_file.replace('.log', '_failures.txt')
    with open(output_file, 'w') as f:
        f.write("DETAILED PARSE FAILURE REPORT\n")
        f.write("="*80 + "\n\n")
        
        for i, failure in enumerate(failed_programs, 1):
            f.write(f"Failure #{i}\n")
            f.write(f"Line: {failure['line']}\n")
            if 'program_name' in failure:
                f.write(f"Program: {failure['program_name']}\n")
            f.write(f"Error: {failure['error']}\n")
            f.write("-"*80 + "\n\n")
    
    print(f"\nDetailed report saved to: {output_file}")


if __name__ == '__main__':
    main()