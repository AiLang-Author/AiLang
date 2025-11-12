#!/usr/bin/env python3
"""
Find the REAL line that's causing the tokenization failure
"""

import sys
sys.path.insert(0, '.')

from cobol_frontend.cobol_lexer import COBOLLexer

def test_full_file_until_error():
    """Tokenize the full file and catch exactly where it fails"""
    print("="*80)
    print("TOKENIZING FULL FILE UNTIL ERROR")
    print("="*80)
    
    filename = 'cobol_frontend/tests/newval.cbl'
    
    with open(filename, 'r') as f:
        source = f.read()
    
    print(f"\nFile loaded: {len(source)} bytes")
    print(f"Lines: {source.count(chr(10))}")
    
    # Read raw lines for reference
    lines = source.split('\n')
    
    try:
        lexer = COBOLLexer(source)
        tokens = lexer.tokenize()
        
        print(f"\n✅ SUCCESS: Tokenized entire file!")
        print(f"   Total tokens: {len(tokens)}")
        
    except Exception as e:
        error_msg = str(e)
        print(f"\n❌ FAILED with error:")
        print(f"   {error_msg}")
        
        # Extract line number from error
        if "line" in error_msg:
            import re
            match = re.search(r'line (\d+)', error_msg)
            if match:
                error_line = int(match.group(1))
                print(f"\n   Error occurred at processed line: {error_line}")
                
                # Show the processed source around that line
                processed_lines = lexer.source.split('\n')
                print(f"\n   Processed source has {len(processed_lines)} lines")
                
                if error_line <= len(processed_lines):
                    start = max(0, error_line - 5)
                    end = min(len(processed_lines), error_line + 5)
                    
                    print(f"\n   Showing processed lines {start} to {end}:")
                    for i in range(start, end):
                        marker = ">>>" if i == error_line - 1 else "   "
                        line_content = processed_lines[i] if i < len(processed_lines) else ""
                        print(f"   {marker} {i+1:6d}: {repr(line_content[:100])}")
                
                # Now find what raw line this corresponds to
                print(f"\n   Mapping back to raw file...")
                
                # Count non-empty lines in processed source up to error line
                non_empty_count = 0
                for i in range(min(error_line, len(processed_lines))):
                    if processed_lines[i].strip():
                        non_empty_count += 1
                
                # Find corresponding raw line (skip comment lines)
                raw_line_num = 0
                counted = 0
                for i, raw_line in enumerate(lines):
                    if i >= len(lines):
                        break
                    # Check if this would be processed (not a comment)
                    if len(raw_line) >= 7 and raw_line[6] in ('*', '/', 'D'):
                        continue  # Comment line, skipped
                    counted += 1
                    if counted == non_empty_count:
                        raw_line_num = i + 1
                        break
                
                print(f"   Likely corresponds to raw file line: ~{raw_line_num}")
                
                # Show raw lines around that area
                if raw_line_num > 0:
                    start = max(0, raw_line_num - 10)
                    end = min(len(lines), raw_line_num + 10)
                    print(f"\n   Raw file lines {start+1} to {end}:")
                    for i in range(start, end):
                        marker = ">>>" if i == raw_line_num - 1 else "   "
                        col7 = lines[i][6] if len(lines[i]) > 6 else ' '
                        print(f"   {marker} {i+1:6d} [{col7}] {repr(lines[i][:80])}")

if __name__ == '__main__':
    test_full_file_until_error()