# lexer_class.py - Main lexer class and supporting classes
from dataclasses import dataclass
from typing import List, Optional, Union, Any, Tuple
import re
from .token_type import TokenType
from .keywords import get_keyword_mapping, get_allowed_short_identifiers

@dataclass
class Token:
    type: TokenType
    value: Any
    line: int
    column: int
    length: int = 1

class LexerError(Exception):
    def __init__(self, message: str, line: int, column: int):
        self.message = message
        self.line = line
        self.column = column
        super().__init__(f"Lexer error at line {line}, column {column}: {message}")

class Lexer:
    def __init__(self, source: str, strict_mode: bool = True):
        self.source = source
        self.position = 0
        self.line = 1
        self.column = 1
        self.tokens: List[Token] = []
        self.strict_mode = strict_mode
        self.keywords = get_keyword_mapping()
        self.allowed_short_identifiers = get_allowed_short_identifiers()

    def error(self, message: str):
        raise LexerError(message, self.line, self.column)

    def warning(self, message: str):
        print(f"Warning at line {self.line}, column {self.column}: {message}")

    def current_char(self) -> Optional[str]:
        if self.position >= len(self.source):
            return None
        return self.source[self.position]

    def peek_char(self, offset: int = 1) -> Optional[str]:
        pos = self.position + offset
        if pos >= len(self.source):
            return None
        return self.source[pos]

    def advance(self):
        if self.position < len(self.source):
            if self.source[self.position] == '\n':
                self.line += 1
                self.column = 1
            else:
                self.column += 1
            self.position += 1

    def skip_whitespace(self):
        while self.current_char() and self.current_char() in ' \t\r':
            self.advance()

    def read_string(self) -> str:
        value = ''
        self.advance()
        while self.current_char() and self.current_char() != '"':
            if self.current_char() == '\\':
                self.advance()
                if self.current_char() is None:
                    self.error("Unterminated string escape")
                escape_char = self.current_char()
                if escape_char == 'n':
                    value += '\n'
                elif escape_char == 't':
                    value += '\t'
                elif escape_char == 'r':
                    value += '\r'
                elif escape_char == '"':
                    value += '"'
                elif escape_char == '\\':
                    value += '\\'
                elif escape_char == '0':
                    value += '\0'
                else:
                    self.warning(f"Unknown escape sequence '\\{escape_char}'")
                    value += escape_char
            else:
                value += self.current_char()
            self.advance()
        if self.current_char() == '"':
            self.advance()
        else:
            self.error("Unterminated string literal")
        return value

    def read_number(self) -> Token:
        """Read a numeric literal and return a NUMBER token with string value."""
        start_pos = self.position
        line_start = self.line
        col_start = self.column
        value = ''

        # Handle hexadecimal literals (e.g., 0xFF)
        if self.current_char() == '0' and self.peek_char() in 'xX':
            value += self.current_char()
            self.advance()
            value += self.current_char()
            self.advance()
            while self.current_char() and self.current_char() in '0123456789ABCDEFabcdef_':
                if self.current_char() != '_':
                    value += self.current_char()
                self.advance()
            if len(value) <= 2:  # Only "0x" or "0X"
                self.error("Invalid hexadecimal literal")
        else:
            # Handle decimal numbers (integers or floats)
            has_dot = False
            while self.current_char() and (self.current_char().isdigit() or self.current_char() in '._'):
                if self.current_char() == '.':
                    if has_dot:
                        break
                    has_dot = True
                    value += '.'
                elif self.current_char() != '_':
                    value += self.current_char()
                self.advance()

            # Handle scientific notation (e.g., 1e-10)
            if self.current_char() and self.current_char() in 'eE':
                value += self.current_char()
                self.advance()
                if self.current_char() and self.current_char() in '+-':
                    value += self.current_char()
                    self.advance()
                while self.current_char() and self.current_char().isdigit():
                    value += self.current_char()
                    self.advance()

            # Validate the number
            if not value or value == '.':
                self.error(f"Invalid number literal: {value}")
            if value.count('.') > 1:
                self.error(f"Invalid number literal: multiple decimal points in {value}")

        return Token(TokenType.NUMBER, value, line_start, col_start, len(value))

    def read_identifier(self) -> str:
        value = ''
        if self.current_char().isalpha() or self.current_char() == '_':
            value += self.current_char()
            self.advance()
        while self.current_char() and (self.current_char().isalnum() or self.current_char() == '_'):
            value += self.current_char()
            self.advance()
        if not value:
            self.error(f"Invalid identifier at line {self.line}, column {self.column}")
        return value


    def read_dotted_identifier(self) -> str:
        parts = [self.read_identifier()]
        while self.current_char() == '.':
            if self.peek_char() and self.peek_char().isalpha():
                self.advance()
                parts.append(self.read_identifier())
            else:
                break
        return '.'.join(parts)

    def read_comment(self) -> Tuple[TokenType, str]:
        if self.peek_char() != '/':
            self.error("Invalid comment start")
        self.advance()
        self.advance()
        comment_type = TokenType.COMMENT
        prefix = ''
        if self.current_char() and self.current_char().isalpha():
            while self.current_char() and self.current_char().isalpha():
                prefix += self.current_char()
                self.advance()
            if prefix == 'DOC':
                comment_type = TokenType.DOC_COMMENT
            elif prefix == 'COM':
                comment_type = TokenType.COM_COMMENT
            elif prefix == 'TAG':
                comment_type = TokenType.TAG_COMMENT
            else:
                self.position -= len(prefix)
                self.column -= len(prefix)
                prefix = ''
        if prefix and self.current_char() == ':':
            self.advance()
        while self.current_char() and self.current_char() in ' \t':
            self.advance()
        value = ''
        if comment_type in (TokenType.DOC_COMMENT, TokenType.COM_COMMENT):
            while self.position + 1 < len(self.source):
                if self.source[self.position:self.position+2] == '//':
                    self.advance()
                    self.advance()
                    break
                value += self.current_char()
                self.advance()
        else:
            while self.current_char() and self.current_char() != '\n':
                value += self.current_char()
                self.advance()
        return comment_type, value.strip()

    def tokenize(self) -> List[Token]:
        self.tokens = []
        while self.position < len(self.source):
            self.skip_whitespace()
            if not self.current_char():
                break
            line_start = self.line
            col_start = self.column
            if self.current_char() == '\n':
                self.tokens.append(Token(TokenType.NEWLINE, '\n', line_start, col_start))
                self.advance()
                continue
            if self.current_char() == '/' and self.peek_char() == '/':
                comment_type, value = self.read_comment()
                self.tokens.append(Token(comment_type, value, line_start, col_start))
                continue
            if self.current_char() == '"':
                value = self.read_string()
                self.tokens.append(Token(TokenType.STRING, value, line_start, col_start))
                continue
            if self.current_char().isdigit() or (self.current_char() == '-' and self.peek_char() and self.peek_char().isdigit()):
                value = ''
                if self.current_char() == '-':
                    value = self.current_char()
                    self.advance()
                value += self.read_number().value
                self.tokens.append(Token(TokenType.NUMBER, value, line_start, col_start, len(value)))
                continue
            two_char = self.source[self.position:self.position+2]
            if two_char == '->':
                self.advance()
                self.advance()
                self.tokens.append(Token(TokenType.ARROW, '->', line_start, col_start))
                continue
            single_char_tokens = {
                '=': TokenType.EQUALS,
                '{': TokenType.LBRACE,
                '}': TokenType.RBRACE,
                '(': TokenType.LPAREN,
                ')': TokenType.RPAREN,
                '[': TokenType.LBRACKET,
                ']': TokenType.RBRACKET,
                ',': TokenType.COMMA,
                ':': TokenType.COLON,
                ';': TokenType.SEMICOLON,
                '.': TokenType.DOT,
                '-': TokenType.DASH,
            }
            if self.current_char() in single_char_tokens:
                if self.current_char() == '.' and self.peek_char() and self.peek_char().isalpha():
                    pass
                else:
                    token_type = single_char_tokens[self.current_char()]
                    value = self.current_char()
                    self.advance()
                    self.tokens.append(Token(token_type, value, line_start, col_start, 1))
                    continue
            if self.current_char().isalpha() or self.current_char() == '_':
                value = self.read_identifier()
                token_type = self.keywords.get(value, TokenType.IDENTIFIER)
                if token_type != TokenType.IDENTIFIER:
                    self.tokens.append(Token(token_type, value, line_start, col_start, len(value)))
                    continue
                if self.current_char() == '.' and self.peek_char() and self.peek_char().isalpha():
                    parts = [value]
                    while self.current_char() == '.' and self.peek_char() and self.peek_char().isalpha():
                        self.advance()  # consume the dot
                        parts.append(self.read_identifier())
                    value = '.'.join(parts)
                self.tokens.append(Token(TokenType.IDENTIFIER, value, line_start, col_start, len(value)))
                continue
            if self.current_char() == '.':
                self.advance()
                self.tokens.append(Token(TokenType.DOT, '.', line_start, col_start, 1))
                continue
            self.error(f"Unknown character '{self.current_char()}'")
        self.tokens.append(Token(TokenType.EOF, None, self.line, self.column))
        return self.tokens