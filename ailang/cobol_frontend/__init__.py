#!/usr/bin/env python3
"""
COBOL Frontend Package
A complete COBOL to Ailang transpiler.

This package provides:
- COBOLLexer: Tokenizes COBOL source code
- COBOLParser: Builds COBOL Abstract Syntax Tree
- COBOLToAilangConverter: Converts COBOL AST to Ailang AST

Usage:
    from cobol_frontend import COBOLToAilangConverter
    from cobol_frontend.parser import COBOLParser
    from cobol_frontend.cobol_lexer import COBOLLexer
    
Or use the command-line interface:
    python3 cobol_frontend/cobol_integration.py program.cbl -o program
"""

__version__ = "1.0.0"
__author__ = "Ailang Project"
__all__ = [
    'COBOLLexer',
    'COBOLParser',
    'COBOLToAilangConverter',
    'AILangASTSerializer',
]

from .cobol_lexer import COBOLLexer, Token, COBOLTokenType, LexerError
# Import from the refactored parser structure
from .parser.parser_core import COBOLMultiProgramParser as COBOLParser, ParserError
# Import from the refactored converter structure
from .converter import COBOLToAilangMultiProgramConverter as COBOLToAilangConverter, AILangASTSerializer

# Note: COBOLIntegratedCompiler is available via:
#   from cobol_frontend.cobol_integration import COBOLIntegratedCompiler
# It's not included in __all__ to avoid circular import issues