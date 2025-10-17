#!/usr/bin/env python3
"""
Quick script to generate Ailang code from COBOL and save it
"""

import sys
import os

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

from cobol_frontend.cobol_lexer import COBOLLexer
from cobol_frontend.parser import COBOLMultiProgramParser
from cobol_frontend.converter import COBOLToAilangMultiProgramConverter, AILangASTSerializer

def generate_ailang(cobol_file, output_file=None, sample_programs=5):
    """Generate Ailang code from COBOL"""
    
    print(f"Reading: {cobol_file}")
    with open(cobol_file, 'r') as f:
        source = f.read()
    
    print("Tokenizing...")
    lexer = COBOLLexer(source)
    tokens = lexer.tokenize()
    print(f"  ✓ {len(tokens):,} tokens")
    
    print("Parsing...")
    parser = COBOLMultiProgramParser(tokens)
    ast = parser.parse_all_programs()
    print(f"  ✓ {len(ast.programs)} programs")
    
    print("Converting to Ailang AST...")
    converter = COBOLToAilangMultiProgramConverter(debug=False)
    ailang_ast = converter.convert(ast)
    print(f"  ✓ Conversion complete")
    
    print("Serializing to Ailang source...")
    serializer = AILangASTSerializer()
    
    # Check if converter has metadata header
    if hasattr(converter, '_metadata_header') and converter._metadata_header:
        ailang_source = serializer.serialize_with_header(ailang_ast, converter._metadata_header)
    else:
        ailang_source = serializer.serialize(ailang_ast)
    
    lines = ailang_source.split('\n')
    print(f"  ✓ Generated {len(lines):,} lines of Ailang")
    
    # Save to file if specified
    if output_file:
        with open(output_file, 'w') as f:
            f.write(ailang_source)
        print(f"\n✓ Saved to: {output_file}")
    
    # Show samples from first few programs
    print(f"\n{'='*80}")
    print(f"SAMPLE OUTPUT (first {sample_programs} programs)")
    print(f"{'='*80}\n")
    
    # Find SubRoutine boundaries
    program_starts = []
    for i, line in enumerate(lines):
        if line.strip().startswith('SubRoutine.'):
            program_starts.append(i)
    
    # Show samples
    for idx in range(min(sample_programs, len(program_starts))):
        start = program_starts[idx]
        # Find end (next SubRoutine or end of file)
        if idx + 1 < len(program_starts):
            end = program_starts[idx + 1]
        else:
            end = len(lines)
        
        # Show first 50 lines of this program
        program_lines = lines[start:min(start + 50, end)]
        
        print(f"--- Program {idx + 1} (lines {start + 1}-{min(start + 50, end)}) ---")
        print('\n'.join(program_lines))
        
        if end - start > 50:
            print(f"\n... ({end - start - 50} more lines) ...")
        
        print()
    
    print(f"{'='*80}")
    print(f"Total lines: {len(lines):,}")
    print(f"Total programs: {len(program_starts)}")
    print(f"{'='*80}")
    
    return ailang_source


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: python3 test_generate_ailang.py <cobol_file> [output_file]")
        print("\nExample:")
        print("  python3 test_generate_ailang.py cobol_frontend/tests/newcob.cbl output.ailang")
        sys.exit(1)
    
    cobol_file = sys.argv[1]
    output_file = sys.argv[2] if len(sys.argv) > 2 else None
    
    generate_ailang(cobol_file, output_file)