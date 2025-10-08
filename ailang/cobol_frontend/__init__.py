#!/usr/bin/env python3
"""
COBOL Frontend Package
A complete COBOL to Ailang transpiler.

This package provides:
- COBOLLexer: Tokenizes COBOL source code
- COBOLParser: Builds COBOL Abstract Syntax Tree
- COBOLToAilangConverter: Converts COBOL AST to Ailang AST
- COBOLIntegratedCompiler: Full compilation pipeline

Usage:
    from cobol_frontend import COBOLIntegratedCompiler
    
    compiler = COBOLIntegratedCompiler(debug=False)
    compiler.compile_file('program.cbl', 'program')

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
    'COBOLIntegratedCompiler',
]

from .cobol_lexer import COBOLLexer, Token, COBOLTokenType, LexerError
from .cobol_parser import COBOLParser, ParserError
from .cobol_ast_converter import COBOLToAilangConverter, AILangASTSerializer
from .cobol_integration import COBOLIntegratedCompiler