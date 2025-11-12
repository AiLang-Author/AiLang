#!/usr/bin/env python3
"""
Verify if lines in newval.cbl are wrapped in quotes
"""

def check_file_structure(filename, start_line, num_lines=10):
    """Check the raw structure of lines"""
    print(f"="*80)
    print(f"CHECKING RAW FILE STRUCTURE")
    print(f"="*80)
    
    with open(filename, 'r') as f:
        lines = f.readlines()
    
    print(f"\nExamining lines {start_line} to {start_line + num_lines - 1}")
    print("-"*80)
    
    for i in range(start_line - 1, min(len(lines), start_line - 1 + num_lines)):
        line_num = i + 1
        raw_line = lines[i]
        
        print(f"Line {line_num}:")
        print(f"  Raw bytes: {repr(raw_line[:80])}")
        print(f"  Length: {len(raw_line.rstrip())}")
        starts_quote = raw_line.startswith("'") or raw_line.startswith('"')
        ends_quote = raw_line.rstrip().endswith("'") or raw_line.rstrip().endswith('"')
        print(f"  Starts with quote: {starts_quote}")
        print(f"  Ends with quote: {ends_quote}")
        
        if raw_line.startswith("'") or raw_line.startswith('"'):
            unwrapped = raw_line[1:-2] if raw_line.rstrip()[-1] in ("'", '"') else raw_line[1:]
            print(f"  Unwrapped: {repr(unwrapped[:80])}")
            if len(unwrapped) >= 7:
                print(f"  Column 7 indicator: {repr(unwrapped[6])}")
        print()

def check_preprocessing():
    """Check if preprocessing handles quoted lines"""
    print("="*80)
    print("CHECKING PREPROCESSING")
    print("="*80)
    
    from cobol_frontend.cobol_lexer import preprocess_continuation_lines
    
    # Test a quoted comment line
    test_cases = [
        ("Raw comment line", "000400*COMMENT LINE"),
        ("Quoted comment line", "'000400*COMMENT LINE'"),
        ("Normal code line", "000400 MOVE X TO Y"),
        ("Quoted code line", "'000400 MOVE X TO Y'"),
    ]
    
    for name, test_line in test_cases:
        print(f"\n{name}:")
        print(f"  Input:  {repr(test_line)}")
        result = preprocess_continuation_lines(test_line)
        print(f"  Output: {repr(result)}")
        
        # Check if it was filtered
        if not result or result.isspace():
            print(f"  ✅ Filtered out (good for comments)")
        else:
            print(f"  ⚠️  Passed through")

if __name__ == '__main__':
    filename = 'cobol_frontend/tests/newval.cbl'
    
    # Check the structure around line 231149
    check_file_structure(filename, 231145, 10)
    check_preprocessing()