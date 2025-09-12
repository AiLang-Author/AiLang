#parser.py
"""Main parser module - provides backward compatibility"""

from .parser_modules.parse_error import ParseError
from .parser_modules.parser_class import Parser

__all__ = ['Parser', 'ParseError']