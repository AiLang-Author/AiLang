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

    def parse_qualified_name(self) -> str:
        """
        Parses a name that can start with a keyword or identifier, 
        e.g., FixedPool.Name or MyVar.field
        """
        allowed_first_tokens = [
            TokenType.IDENTIFIER, TokenType.FIXEDPOOL, TokenType.DYNAMICPOOL, 
            TokenType.TEMPORALPOOL, TokenType.NEURALPOOL, TokenType.KERNELPOOL, 
            TokenType.ACTORPOOL, TokenType.SECURITYPOOL, TokenType.CONSTRAINEDPOOL, 
            TokenType.FILEPOOL
        ]

        if not self.match(*allowed_first_tokens):
            self.error("Expected an identifier or pool type to start a qualified name.")

        parts = [self.current_token.value]
        self.advance()

        while self.match(TokenType.DOT) and self.peek() and self.peek().type == TokenType.IDENTIFIER:
            self.consume(TokenType.DOT)
            parts.append(self.consume(TokenType.IDENTIFIER).value)
        return '.'.join(parts)

    def parse_type(self):
        """Parse type annotations"""
        # A type can be a named keyword (Integer, Address, etc.) or a custom identifier
        type_tokens = [
            TokenType.IDENTIFIER, TokenType.INTEGER, TokenType.FLOATINGPOINT,
            TokenType.TEXT, TokenType.BOOLEAN, TokenType.ADDRESS, TokenType.ARRAY,
            TokenType.MAP, TokenType.TUPLE, TokenType.RECORD, TokenType.OPTIONALTYPE,
            TokenType.CONSTRAINEDTYPE, TokenType.ANY, TokenType.VOID,
            # Low-level types
            TokenType.BYTE, TokenType.WORD, TokenType.DWORD, TokenType.QWORD,
            TokenType.UINT8, TokenType.UINT16, TokenType.UINT32, TokenType.UINT64,
            TokenType.INT8, TokenType.INT16, TokenType.INT32, TokenType.INT64,
            TokenType.POINTER
        ]
        if self.match(*type_tokens):
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