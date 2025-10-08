# Copyright (c) 2025 Sean Collins, 2 Paws Machine and Engineering. All rights reserved.
#
# Licensed under the Sean Collins Software License (SCSL). See the LICENSE file in the root directory of this project
# for the full terms and conditions, including restrictions on forking, corporate use, and permissions for private/teaching purposes.

# aimacro_frontend/lexer.py
"""
AIMacro Lexer - Tokenizes Python-like syntax with explicit end blocks
"""

import re
from typing import List, Optional, Union, Any
from enum import Enum, auto
from dataclasses import dataclass

class TokenType(Enum):
    # Literals
    NUMBER = auto()
    STRING = auto()
    IDENTIFIER = auto()
    
    # Keywords
    DEF = auto()
    END = auto()
    IF = auto()
    ELIF = auto()
    ELSE = auto()
    WHILE = auto()
    FOR = auto()
    IN = auto()
    RETURN = auto()
    TRY = auto()
    EXCEPT = auto()
    FINALLY = auto()
    AS = auto()
    WITH = auto()
    CLASS = auto()
    IMPORT = auto()
    FROM = auto()
    
    # Boolean/None
    TRUE = auto()
    FALSE = auto()
    NONE = auto()
    
    # Control flow
    PASS = auto()
    BREAK = auto()
    CONTINUE = auto()
    
    # Logical operators (keywords)
    AND = auto()
    OR = auto()
    NOT = auto()
    IS = auto()
    
    # Arithmetic operators
    PLUS = auto()
    MINUS = auto()
    MULTIPLY = auto()
    DIVIDE = auto()
    FLOOR_DIVIDE = auto()
    MODULO = auto()
    POWER = auto()
    
    # Assignment
    ASSIGN = auto()
    PLUS_ASSIGN = auto()
    MINUS_ASSIGN = auto()
    MULTIPLY_ASSIGN = auto()
    DIVIDE_ASSIGN = auto()
    
    # Comparison operators
    EQUAL = auto()
    NOT_EQUAL = auto()
    LESS = auto()
    LESS_EQUAL = auto()
    GREATER = auto()
    GREATER_EQUAL = auto()
    
    # Bitwise operators
    BITWISE_AND = auto()
    BITWISE_OR = auto()
    BITWISE_XOR = auto()
    BITWISE_NOT = auto()
    LEFT_SHIFT = auto()
    RIGHT_SHIFT = auto()
    
    # Delimiters
    LPAREN = auto()
    RPAREN = auto()
    LBRACKET = auto()
    RBRACKET = auto()
    LBRACE = auto()
    RBRACE = auto()
    COMMA = auto()
    COLON = auto()
    SEMICOLON = auto()
    DOT = auto()
    ARROW = auto()
    
    # Special
    NEWLINE = auto()
    INDENT = auto()
    DEDENT = auto()
    EOF = auto()
    
    # Comments
    COMMENT = auto()

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

class AIMacroLexer:
    """Lexical analyzer for AIMacro - handles Python-like syntax with explicit blocks"""
    
    def __init__(self, source: str):
        self.source = source
        self.position = 0
        self.line = 1
        self.column = 1
        self.tokens: List[Token] = []
        self.indent_stack = [0]  # Track indentation levels
        self.at_line_start = True
        
        # Python keywords mapping
        self.keywords = {
            # Function and control keywords
            'def': TokenType.DEF,
            'end': TokenType.END,
            'if': TokenType.IF,
            'elif': TokenType.ELIF,
            'else': TokenType.ELSE,
            'while': TokenType.WHILE,
            'for': TokenType.FOR,
            'in': TokenType.IN,
            'return': TokenType.RETURN,
            'try': TokenType.TRY,
            'except': TokenType.EXCEPT,
            'finally': TokenType.FINALLY,
            'as': TokenType.AS,
            'with': TokenType.WITH,
            'class': TokenType.CLASS,
            'import': TokenType.IMPORT,
            'from': TokenType.FROM,
            
            # Boolean and None
            'True': TokenType.TRUE,
            'False': TokenType.FALSE,
            'None': TokenType.NONE,
            
            # Control flow
            'pass': TokenType.PASS,
            'break': TokenType.BREAK,
            'continue': TokenType.CONTINUE,
            
            # Logical operators
            'and': TokenType.AND,
            'or': TokenType.OR,
            'not': TokenType.NOT,
            'is': TokenType.IS,
        }
    
    def error(self, message: str):
        raise LexerError(message, self.line, self.column)
    
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
                self.at_line_start = True
            else:
                self.column += 1
                if not self.source[self.position].isspace():
                    self.at_line_start = False
            self.position += 1
    
    def skip_whitespace(self):
        """Skip spaces and tabs, but not newlines"""
        while self.current_char() and self.current_char() in ' \t':
            self.advance()
    
    def read_string(self) -> str:
        """Read string literal with escape sequence support"""
        quote_char = self.current_char()
        value = ''
        self.advance()  # Skip opening quote
        
        while self.current_char() and self.current_char() != quote_char:
            if self.current_char() == '\\':
                self.advance()
                if self.current_char():
                    # Handle escape sequences
                    escape_chars = {
                        'n': '\n',
                        't': '\t', 
                        'r': '\r',
                        '\\': '\\',
                        '"': '"',
                        "'": "'",
                        '0': '\0'
                    }
                    escaped = escape_chars.get(self.current_char(), self.current_char())
                    value += escaped
                    self.advance()
                else:
                    self.error("Unterminated escape sequence")
            else:
                value += self.current_char()
                self.advance()
        
        if not self.current_char():
            self.error("Unterminated string literal")
        
        self.advance()  # Skip closing quote
        return value
    
    def read_number(self) -> Union[int, float]:
        """Read numeric literal (int or float)"""
        value = ''
        has_dot = False
        
        while self.current_char() and (self.current_char().isdigit() or 
                                      (self.current_char() == '.' and not has_dot)):
            if self.current_char() == '.':
                # Check if this is a decimal point or attribute access
                if self.peek_char() and self.peek_char().isdigit():
                    has_dot = True
                    value += self.current_char()
                    self.advance()
                else:
                    # This is attribute access, stop here
                    break
            else:
                value += self.current_char()
                self.advance()
        
        # Handle scientific notation (e.g., 1e10, 2.5e-3)
        if self.current_char() and self.current_char().lower() == 'e':
            has_dot = True  # Force float
            value += self.current_char()
            self.advance()
            
            if self.current_char() and self.current_char() in '+-':
                value += self.current_char()
                self.advance()
            
            while self.current_char() and self.current_char().isdigit():
                value += self.current_char()
                self.advance()
        
        try:
            return float(value) if has_dot else int(value)
        except ValueError:
            self.error(f"Invalid number format: {value}")
    
    def read_identifier(self) -> str:
        """Read identifier or keyword"""
        value = ''
        while (self.current_char() and 
               (self.current_char().isalnum() or self.current_char() == '_')):
            value += self.current_char()
            self.advance()
        return value
    
    def add_token(self, token_type: TokenType, value: Any = None):
        """Add token to the token list"""
        self.tokens.append(Token(token_type, value, self.line, self.column))
    
    def handle_indentation(self):
        """Handle Python-style indentation (optional in AIMacro due to end; blocks)"""
        if not self.at_line_start:
            return
            
        indent_level = 0
        saved_pos = self.position
        saved_col = self.column
        
        # Count indentation
        while self.current_char() and self.current_char() in ' \t':
            if self.current_char() == '\t':
                indent_level += 8  # Tab = 8 spaces
            else:
                indent_level += 1
            self.advance()
        
        # If line is empty or comment, ignore indentation
        if not self.current_char() or self.current_char() == '#' or self.current_char() == '\n':
            self.position = saved_pos
            self.column = saved_col
            return
        
        current_indent = self.indent_stack[-1]
        
        if indent_level > current_indent:
            self.indent_stack.append(indent_level)
            self.add_token(TokenType.INDENT, indent_level)
        elif indent_level < current_indent:
            while self.indent_stack and self.indent_stack[-1] > indent_level:
                self.indent_stack.pop()
                self.add_token(TokenType.DEDENT)
            
            if not self.indent_stack or self.indent_stack[-1] != indent_level:
                self.error("Indentation error")
        
        self.at_line_start = False
    
    def tokenize(self) -> List[Token]:
        """Main tokenization method"""
        while self.position < len(self.source):
            char = self.current_char()
            
            if char is None:
                break
            
            # Skip indentation - AIMacro uses end; blocks, not indentation
            if self.at_line_start and char in ' \t':
                self.skip_whitespace()
                continue
            
            # Handle newlines
            if char == '\n':
                self.add_token(TokenType.NEWLINE)
                self.advance()
                continue
            
            # Skip whitespace (spaces and tabs)
            if char in ' \t':
                self.skip_whitespace()
                continue
            
            # Handle comments
            if char == '#':
                comment_value = ''
                while self.current_char() and self.current_char() != '\n':
                    comment_value += self.current_char()
                    self.advance()
                self.add_token(TokenType.COMMENT, comment_value[1:].strip())  # Remove # and whitespace
                continue
            
            # Check for triple-quoted strings first
            if char in '"\'' and self.peek_char() == char and self.peek_char(2) == char:
                start_line = self.line
                start_col = self.column
                quote_char = char
                self.advance()
                self.advance()
                self.advance() # Consume opening triple quotes
                
                value = ''
                while self.current_char():
                    if (self.current_char() == quote_char and
                        self.peek_char() == quote_char and
                        self.peek_char(2) == quote_char):
                        self.advance()
                        self.advance()
                        self.advance() # Consume closing triple quotes
                        self.add_token(TokenType.STRING, value)
                        break # Exit the inner while loop
                    else:
                        value += self.current_char()
                        self.advance()
                else: # This 'else' belongs to the 'while', executed if 'break' is not hit
                    self.line = start_line
                    self.column = start_col
                    self.error("Unterminated multi-line string literal")
                continue

            # Single-quoted string literals
            if char in '"\'':
                value = self.read_string()
                self.add_token(TokenType.STRING, value)
                continue
            
            # Numbers
            if char.isdigit():
                value = self.read_number()
                self.add_token(TokenType.NUMBER, value)
                continue
            
            # Identifiers and keywords
            if char.isalpha() or char == '_':
                value = self.read_identifier()
                token_type = self.keywords.get(value, TokenType.IDENTIFIER)
                self.add_token(token_type, value)
                continue
            
            # Multi-character operators
            two_char = char + (self.peek_char() or '')
            
            two_char_ops = {
                '==': TokenType.EQUAL,
                '!=': TokenType.NOT_EQUAL,
                '<=': TokenType.LESS_EQUAL,
                '>=': TokenType.GREATER_EQUAL,
                '<<': TokenType.LEFT_SHIFT,
                '>>': TokenType.RIGHT_SHIFT,
                '**': TokenType.POWER,
                '//': TokenType.FLOOR_DIVIDE,
                '+=': TokenType.PLUS_ASSIGN,
                '-=': TokenType.MINUS_ASSIGN,
                '*=': TokenType.MULTIPLY_ASSIGN,
                '/=': TokenType.DIVIDE_ASSIGN,
                '->': TokenType.ARROW,
            }
            
            if two_char in two_char_ops:
                self.advance()
                self.advance()
                self.add_token(two_char_ops[two_char])
                continue
            
            # Single-character operators and delimiters
            single_char_ops = {
                '+': TokenType.PLUS,
                '-': TokenType.MINUS,
                '*': TokenType.MULTIPLY,
                '/': TokenType.DIVIDE,
                '%': TokenType.MODULO,
                '=': TokenType.ASSIGN,
                '<': TokenType.LESS,
                '>': TokenType.GREATER,
                '&': TokenType.BITWISE_AND,
                '|': TokenType.BITWISE_OR,
                '^': TokenType.BITWISE_XOR,
                '~': TokenType.BITWISE_NOT,
                '(': TokenType.LPAREN,
                ')': TokenType.RPAREN,
                '[': TokenType.LBRACKET,
                ']': TokenType.RBRACKET,
                '{': TokenType.LBRACE,
                '}': TokenType.RBRACE,
                ',': TokenType.COMMA,
                ':': TokenType.COLON,
                ';': TokenType.SEMICOLON,
                '.': TokenType.DOT,
            }
            
            if char in single_char_ops:
                self.add_token(single_char_ops[char])
                self.advance()
                continue
            
            # Unknown character
            self.error(f"Unexpected character: '{char}'")
        
        # Add final DEDENT tokens for any remaining indentation
        while len(self.indent_stack) > 1:
            self.indent_stack.pop()
            self.add_token(TokenType.DEDENT)
        
        # Add EOF token
        self.add_token(TokenType.EOF)
        return self.tokens

# Testing utility
def test_lexer():
    """Simple test function for the lexer"""
    test_code = '''
def fibonacci(n):
    if n <= 1:
        return n;
    end;
    return fibonacci(n-1) + fibonacci(n-2);
end;
'''
    
    lexer = AIMacroLexer(test_code)
    tokens = lexer.tokenize()
    
    for token in tokens:
        if token.type != TokenType.NEWLINE:  # Skip newlines for cleaner output
            print(f"{token.type.name}: {token.value}")

if __name__ == "__main__":
    test_lexer()