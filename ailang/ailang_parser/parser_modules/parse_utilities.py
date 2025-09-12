#parse_utilities.py
"""Parser utility methods"""

from typing import List
from ..lexer import TokenType
from ..ailang_ast import *

class ParserUtilitiesMixin:
    """Mixin for utility parsing methods"""
    
    def parse_dotted_name(self) -> str:
        parts = [self.consume(TokenType.IDENTIFIER).value]
        while self.match(TokenType.DOT) and self.peek() and self.peek().type == TokenType.IDENTIFIER:
            self.consume(TokenType.DOT)
            parts.append(self.consume(TokenType.IDENTIFIER).value)
        return '.'.join(parts)

    def parse_type(self):
        """Parse type annotations"""
        # Simple implementation - just consume the type name
        if self.current_token.type == TokenType.IDENTIFIER:
            type_name = self.current_token.value
            self.advance()
            return type_name
        return "Any"  # Default type

    def parse_string_array(self) -> List[str]:
        """Parse an array of strings"""
        strings = []
        self.consume(TokenType.LBRACKET)
        while not self.match(TokenType.RBRACKET):
            if self.match(TokenType.STRING):
                strings.append(self.consume(TokenType.STRING).value)
            if self.match(TokenType.COMMA):
                self.consume(TokenType.COMMA)
                self.skip_newlines()
        self.consume(TokenType.RBRACKET)
        return strings