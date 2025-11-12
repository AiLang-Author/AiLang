#!/usr/bin/env python3
"""
Trace exactly what happens when tokenizing the problematic line
"""

import sys
sys.path.insert(0, '.')

from cobol_frontend.cobol_lexer import COBOLLexer, preprocess_continuation_lines

def trace_preprocessing():
    """Trace what preprocessing does"""
    print("="*80)
    print("TRACING PREPROCESSING")
    print("="*80)
    
    # Simulate the exact line from newval.cbl
    raw_line = '000400****************************************************************  RL1134.2'
    
    print(f"\nInput line:")
    print(f"  Raw: {repr(raw_line)}")
    print(f"  Length: {len(raw_line)}")
    print(f"  Column 7: {repr(raw_line[6]) if len(raw_line) > 6 else 'N/A'}")
    
    # Preprocess it
    result = preprocess_continuation_lines(raw_line)
    
    print(f"\nAfter preprocessing:")
    print(f"  Result: {repr(result)}")
    print(f"  Length: {len(result)}")
    print(f"  Is empty: {not result.strip()}")

def trace_tokenization():
    """Trace what happens during tokenization"""
    print("\n" + "="*80)
    print("TRACING TOKENIZATION")
    print("="*80)
    
    # Test both a comment line and the next normal line
    test_source = """000400****************************************************************  RL1134.2
000500 IDENTIFICATION DIVISION."""
    
    print(f"\nInput source:")
    print(repr(test_source))
    
    # Add instrumentation to COBOLLexer
    original_tokenize_line = COBOLLexer._tokenize_line
    
    def instrumented_tokenize_line(self, line, line_num):
        print(f"\n  _tokenize_line called:")
        print(f"    line_num: {line_num}")
        print(f"    line: {repr(line)}")
        print(f"    line.strip(): {repr(line.strip())}")
        return original_tokenize_line(self, line, line_num)
    
    COBOLLexer._tokenize_line = instrumented_tokenize_line
    
    try:
        lexer = COBOLLexer(test_source)
        tokens = lexer.tokenize()
        
        print(f"\n  Tokenization complete!")
        print(f"  Total tokens: {len(tokens)}")
        for i, token in enumerate(tokens):
            print(f"    {i}: {token.type.name} = {repr(token.value)}")
            
    except Exception as e:
        print(f"\n  ❌ Tokenization failed: {e}")
    finally:
        COBOLLexer._tokenize_line = original_tokenize_line

def main():
    trace_preprocessing()
    trace_tokenization()

if __name__ == '__main__':
    main()