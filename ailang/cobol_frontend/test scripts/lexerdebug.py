#!/usr/bin/env python3
"""
COBOL Lexer Debugger
Deep inspection of lexer behavior around problematic lines
"""

import sys
sys.path.insert(0, 'cobol_frontend')

from cobol_lexer import COBOLLexer, preprocess_continuation_lines

def show_line_details(source: str, target_line: int, context: int = 5):
    """Show detailed information about a specific line"""
    lines = source.split('\n')
    
    print("=" * 80)
    print(f"DETAILED LINE ANALYSIS - Line {target_line}")
    print("=" * 80)
    
    # Show lines with context
    start = max(0, target_line - context - 1)
    end = min(len(lines), target_line + context)
    
    for i in range(start, end):
        line = lines[i]
        line_num = i + 1
        marker = " >>> " if line_num == target_line else "     "
        
        print(f"\n{marker}Line {line_num:4d}:")
        
        # Show raw line with character positions
        print(f"  Raw ({len(line)} chars): {repr(line)}")
        
        # Show character-by-character breakdown
        if line_num == target_line or (len(line) > 6 and line[6] == '-'):
            print("  Char breakdown:")
            for j, ch in enumerate(line[:80]):  # First 80 chars
                if j == 6:
                    print(f"    Col {j+1:2d}: {repr(ch)} ← INDICATOR COLUMN")
                elif j < 6:
                    print(f"    Col {j+1:2d}: {repr(ch)} (sequence)")
                elif j >= 72:
                    print(f"    Col {j+1:2d}: {repr(ch)} (program ID)")
                else:
                    print(f"    Col {j+1:2d}: {repr(ch)}")
        
        # Check for continuation
        if len(line) > 6 and line[6] == '-':
            print("  ⚠️  CONTINUATION LINE DETECTED")


def test_preprocessing(filename: str, target_line: int):
    """Test the preprocessing step"""
    print("\n" + "=" * 80)
    print("STEP 1: LOADING SOURCE")
    print("=" * 80)
    
    with open(filename, 'r') as f:
        original_source = f.read()
    
    lines = original_source.split('\n')
    print(f"Total lines: {len(lines)}")
    
    # Show original around target line
    show_line_details(original_source, target_line)
    
    print("\n" + "=" * 80)
    print("STEP 2: PREPROCESSING (Continuation line handling)")
    print("=" * 80)
    
    try:
        preprocessed = preprocess_continuation_lines(original_source)
        print("✓ Preprocessing succeeded")
        
        # Show preprocessed result
        show_line_details(preprocessed, target_line)
        
    except Exception as e:
        print(f"✗ Preprocessing failed: {e}")
        import traceback
        traceback.print_exc()
        return None
    
    return original_source # ✅ FIX: Return the original source for the tokenizer test


def test_tokenization(source: str, target_line: int):
    """Test tokenization and show where it fails"""
    print("\n" + "=" * 80)
    print("STEP 3: TOKENIZATION")
    print("   (Instantiating COBOLLexer with ORIGINAL source)")
    print("=" * 80)
    
    try:
        # ✅ FIX: The COBOLLexer class now handles its own preprocessing
        # during __init__. We must pass it the original, unprocessed source
        # to avoid the double-preprocessing bug.
        lexer = COBOLLexer(source)
        tokens = lexer.tokenize()
        
        print(f"✓ Tokenization succeeded - {len(tokens)} tokens")
        
        # Show tokens around target line
        print(f"\nTokens near line {target_line}:")
        for i, token in enumerate(tokens):
            if abs(token.line - target_line) <= 3:
                print(f"  {i:4d}. Line {token.line:4d}: {token.type.name:20s} = {repr(token.value)}")
        
    except Exception as e:
        print(f"\n✗ Tokenization failed: {e}")
        print(f"\nLexer state at failure:")
        print(f"  Position: {lexer.position}")
        print(f"  Line: {lexer.line}")
        print(f"  Column: {lexer.column}")
        
        # Show what the lexer was looking at
        lines = source.split('\n')
        if lexer.line - 1 < len(lines):
            problem_line = lines[lexer.line - 1]
            print(f"\n  Problem line {lexer.line}:")
            print(f"    {repr(problem_line)}")
            
            # Show character at position
            if lexer.column <= len(problem_line):
                print(f"\n  Character at column {lexer.column}:")
                print(f"    {repr(problem_line[lexer.column-1] if lexer.column > 0 else '')}")
        
        import traceback
        traceback.print_exc()
        return None
    
    return tokens


def find_string_literals(source: str, target_line: int):
    """Find all string literals near target line"""
    print("\n" + "=" * 80)
    print("STEP 4: STRING LITERAL ANALYSIS")
    print("=" * 80)
    
    lines = source.split('\n')
    start = max(0, target_line - 10)
    end = min(len(lines), target_line + 10)
    
    for i in range(start, end):
        line = lines[i]
        line_num = i + 1
        
        # Find all quotes
        quote_positions = []
        for j, ch in enumerate(line):
            if ch in ['"', "'"]:
                quote_positions.append((j, ch))
        
        if quote_positions:
            marker = " >>> " if line_num == target_line else "     "
            print(f"{marker}Line {line_num}: {len(quote_positions)} quotes at positions: {quote_positions}")
            print(f"      {repr(line[:80])}")
            
            # Check if balanced
            if len(quote_positions) % 2 != 0:
                print("      ⚠️  ODD NUMBER OF QUOTES - POSSIBLE UNTERMINATED STRING")


def main():
    # ✅ FIX: Make line_number argument mandatory to avoid false positives
    # from a hardcoded default.
    if len(sys.argv) < 3:
        print("Usage: python3 cobol_lexer_debugger.py <file.cbl> [line_number]")
        sys.exit(1)
    
    filename = sys.argv[1]
    target_line = int(sys.argv[2])
    
    print(f"""
╔══════════════════════════════════════════════════════════════════════════╗
║                     COBOL LEXER DEBUGGER                                  ║
║                                                                           ║
║  File: {filename:63s} ║
║  Target Line: {target_line:4d}                                                     ║
╚══════════════════════════════════════════════════════════════════════════╝
""")
    
    # Step 1: Load and analyze original source
    with open(filename, 'r') as f:
        original = f.read()
    
    show_line_details(original, target_line, context=3)
    
    # Step 2: Test preprocessing
    source_for_tokenizer = test_preprocessing(filename, target_line)
    if source_for_tokenizer is None:
        return
    
    # Step 3: Analyze string literals
    find_string_literals(source_for_tokenizer, target_line)
    
    # Step 4: Test tokenization
    tokens = test_tokenization(source_for_tokenizer, target_line)
    
    print("\n" + "=" * 80)
    print("SUMMARY")
    print("=" * 80)
    if tokens:
        print("✓ All steps completed successfully")
    else:
        print("✗ Tokenization failed - see details above")


if __name__ == "__main__":
    main()