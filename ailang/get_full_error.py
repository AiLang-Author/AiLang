#!/usr/bin/env python3
"""
Get full traceback of the unpacking error
"""

import sys
import traceback

sys.path.insert(0, '.')

# Run a simple test that will trigger the error
try:
    from cobol_frontend.cobol_lexer import COBOLLexer
    from cobol_frontend.parser import COBOLMultiProgramParser
    from cobol_frontend.converter import COBOLToAilangMultiProgramConverter
    
    # Find a line with PERFORM...THRU
    test_cobol = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM START-PARA THRU END-PARA.
           STOP RUN.
       START-PARA.
           DISPLAY "START".
       END-PARA.
           DISPLAY "END".
    """
    
    print("Lexing...")
    lexer = COBOLLexer(test_cobol)
    tokens = lexer.tokenize()
    print(f"  Got {len(tokens)} tokens")
    
    print("\nParsing...")
    parser = COBOLMultiProgramParser(tokens)
    ast = parser.parse_all_programs()
    print(f"  Got AST")
    
    print("\nConverting...")
    # Check converter signature
    import inspect
    sig = inspect.signature(COBOLToAilangMultiProgramConverter.__init__)
    print(f"  Converter signature: {sig}")
    
    # Try without io_backend
    try:
        converter = COBOLToAilangMultiProgramConverter()
        ailang_ast = converter.convert(ast)
        print(f"  Success!")
    except Exception as conv_err:
        print(f"  Conversion failed: {conv_err}")
        raise
    
except Exception as e:
    print(f"\n❌ ERROR: {e}")
    print("\nFull traceback:")
    traceback.print_exc()
    
    # Print locals at error point if possible
    import sys
    tb = sys.exc_info()[2]
    print("\n" + "="*80)
    print("DETAILED ERROR LOCATION")
    print("="*80)
    
    while tb.tb_next:
        tb = tb.tb_next
    
    frame = tb.tb_frame
    print(f"File: {frame.f_code.co_filename}")
    print(f"Function: {frame.f_code.co_name}")
    print(f"Line: {tb.tb_lineno}")
    print(f"\nLocal variables:")
    for key, value in sorted(frame.f_locals.items()):
        if not key.startswith('__'):
            print(f"  {key} = {repr(value)[:100]}")