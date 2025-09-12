# lexer.py - Main lexer interface
from .lexer_modules import TokenType, Token, LexerError, Lexer

# Re-export all necessary components for backward compatibility
__all__ = [
    'TokenType',
    'Token', 
    'LexerError',
    'Lexer'
]