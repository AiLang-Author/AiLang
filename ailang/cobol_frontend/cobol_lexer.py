#!/usr/bin/env python3
"""
COBOL Lexer
Tokenizes COBOL source code (both fixed and free format) for the COBOL to Ailang transpiler.
"""

from enum import Enum, auto
from dataclasses import dataclass
from typing import List, Optional

class COBOLTokenType(Enum):
    # Divisions and Sections
    IDENTIFICATION = auto()
    ENVIRONMENT = auto()
    DIVISION = auto()
    PROGRAM_ID = auto()
    DATA = auto()
    WORKING_STORAGE = auto()
    SECTION = auto()
    LINKAGE = auto()
    FILE_SECTION = auto()
    PROCEDURE = auto()
    
    # Data declarations
    LEVEL_NUMBER = auto()  # 01, 02, 77, 88, etc.
    PIC = auto()
    PICTURE = auto()
    VALUE = auto()
    OCCURS = auto()
    TIMES = auto()
    REDEFINES = auto()
    COMP = auto()
    USAGE = auto()
    COMP_1 = auto()
    COMP_2 = auto()
    COMP_3 = auto()
    COMPUTATIONAL = auto()
    COMPUTATIONAL_3 = auto()
    BINARY = auto()
    PACKED_DECIMAL = auto()
    
     # String operation keywords
    STRING = auto()
    UNSTRING = auto()
    INSPECT = auto()
    DELIMITED = auto()
    SIZE = auto()
    REPLACING = auto()
    TALLYING = auto()
    FOR = auto()
    WITH = auto()
    POINTER = auto()
    IN = auto()  # Add this - used in TALLYING IN clause
    ALL = auto()  # Add this - used in INSPECT ALL and TALLYING FOR ALL
    
     
    
    
    # Statements
    DISPLAY = auto()
    ACCEPT = auto()
    MOVE = auto()
    TO = auto()
    FROM = auto()
    INTO = auto()
    COMPUTE = auto()
    ADD = auto()
    SUBTRACT = auto()
    MULTIPLY = auto()
    DIVIDE = auto()
    BY = auto()
    GIVING = auto()
    REMAINDER = auto()
    
    # Control flow
    IF = auto()
    THEN = auto()
    ELSE = auto()
    END_IF = auto()
    PERFORM = auto()
    UNTIL = auto()
    VARYING = auto()
    END_PERFORM = auto()
    CALL = auto()
    USING = auto()
    END_PROGRAM = auto()
    END = auto()  # ADD THIS LINE - standalone END keyword
    PROGRAM = auto()  # ADD THIS LINE - standalone PROGRAM keyword
    EVALUATE = auto()
    WHEN = auto()
    OTHER = auto()
    END_EVALUATE = auto()
    GO = auto()
    GOTO = auto()
    THROUGH = auto()
    THRU = auto()
    
    # Intrinsic Functions
    FUNCTION = auto()
    UPPER_CASE = auto()
    LOWER_CASE = auto()
    
    # Comparison operators
    EQUAL = auto()
    EQUALS = auto()
    GREATER = auto()
    LESS = auto()
    NOT = auto()
    AND = auto()
    OR = auto()
    THAN = auto()
    
    # Special statements
    STOP = auto()
    RUN = auto()
    GOBACK = auto()
    EXIT = auto()
    
    # Literals and identifiers
    STRING_LITERAL = auto()
    NUMBER_LITERAL = auto()
    IDENTIFIER = auto()
    
    # Punctuation
    PERIOD = auto()
    COMMA = auto()
    LPAREN = auto()
    RPAREN = auto()
    COLON = auto()
    SEMICOLON = auto()
    PLUS = auto()
    MINUS = auto()
    ASTERISK = auto()
    SLASH = auto()
    EQUALS_SIGN = auto()
    GT_SIGN = auto()
    LT_SIGN = auto()
    GTE_SIGN = auto()  
    LTE_SIGN = auto() 
    
       
    # Display-edited format characters (used in PIC clauses)
    DOLLAR_SIGN = auto()
    
    # Meta
    NEWLINE = auto()
    COMMENT = auto()
    EOF = auto()

@dataclass
class Token:
    type: COBOLTokenType
    value: str
    line: int
    column: int

class LexerError(Exception):
    def __init__(self, message: str, line: int, column: int):
        self.message = message
        self.line = line
        self.column = column
        super().__init__(f"Lexer error at line {line}, column {column}: {message}")

class COBOLLexer:
    """
    Lexical analyzer for COBOL source code.
    Supports both fixed-format and free-format COBOL.
    """
    
    # COBOL reserved words (case-insensitive)
    KEYWORDS = {
        'IDENTIFICATION': COBOLTokenType.IDENTIFICATION,
        'ENVIRONMENT': COBOLTokenType.ENVIRONMENT,
        'DIVISION': COBOLTokenType.DIVISION,
        'PROGRAM-ID': COBOLTokenType.PROGRAM_ID,
        'DATA': COBOLTokenType.DATA,
        'WORKING-STORAGE': COBOLTokenType.WORKING_STORAGE,
        'FILE': COBOLTokenType.FILE_SECTION,
        'LINKAGE': COBOLTokenType.LINKAGE,
        'SECTION': COBOLTokenType.SECTION,
        'PROCEDURE': COBOLTokenType.PROCEDURE,
        'PIC': COBOLTokenType.PIC,
        'PICTURE': COBOLTokenType.PICTURE,
        'VALUE': COBOLTokenType.VALUE,
        'OCCURS': COBOLTokenType.OCCURS,
        'TIMES': COBOLTokenType.TIMES,
        'REDEFINES': COBOLTokenType.REDEFINES,
        'COMP': COBOLTokenType.COMP,
        'USAGE': COBOLTokenType.USAGE,
        'COMP-1': COBOLTokenType.COMP_1,
        'COMP-2': COBOLTokenType.COMP_2,
        'COMP-3': COBOLTokenType.COMP_3,
        'COMPUTATIONAL': COBOLTokenType.COMPUTATIONAL,
        'COMPUTATIONAL-3': COBOLTokenType.COMPUTATIONAL_3,
        'BINARY': COBOLTokenType.BINARY,
        'PACKED-DECIMAL': COBOLTokenType.PACKED_DECIMAL,
        'DISPLAY': COBOLTokenType.DISPLAY,
        'ACCEPT': COBOLTokenType.ACCEPT,
        'MOVE': COBOLTokenType.MOVE,
        'TO': COBOLTokenType.TO,
        'FROM': COBOLTokenType.FROM,
        'INTO': COBOLTokenType.INTO,
        'COMPUTE': COBOLTokenType.COMPUTE,
        'ADD': COBOLTokenType.ADD,
        'SUBTRACT': COBOLTokenType.SUBTRACT,
        'MULTIPLY': COBOLTokenType.MULTIPLY,
        'DIVIDE': COBOLTokenType.DIVIDE,
        'BY': COBOLTokenType.BY,
        'GIVING': COBOLTokenType.GIVING,
        'REMAINDER': COBOLTokenType.REMAINDER,
        'IF': COBOLTokenType.IF,
        'THEN': COBOLTokenType.THEN,
        'ELSE': COBOLTokenType.ELSE,
        'END-IF': COBOLTokenType.END_IF,
        'PERFORM': COBOLTokenType.PERFORM,
        'UNTIL': COBOLTokenType.UNTIL,
        'VARYING': COBOLTokenType.VARYING,
        'END-PERFORM': COBOLTokenType.END_PERFORM,
        'CALL': COBOLTokenType.CALL,
        'USING': COBOLTokenType.USING,
        'END-PROGRAM': COBOLTokenType.END_PROGRAM,
        'PROGRAM': COBOLTokenType.PROGRAM,
        'END': COBOLTokenType.END,  # ADD THIS LINE
        'EVALUATE': COBOLTokenType.EVALUATE,
        'WHEN': COBOLTokenType.WHEN,
        'OTHER': COBOLTokenType.OTHER,
        'END-EVALUATE': COBOLTokenType.END_EVALUATE,
        'GO': COBOLTokenType.GO,
        'GOTO': COBOLTokenType.GOTO,
        'THROUGH': COBOLTokenType.THROUGH,
        'THRU': COBOLTokenType.THRU,
        'EQUAL': COBOLTokenType.EQUAL,
        'EQUALS': COBOLTokenType.EQUALS,
        'GREATER': COBOLTokenType.GREATER,
        'LESS': COBOLTokenType.LESS,
        'NOT': COBOLTokenType.NOT,
        'AND': COBOLTokenType.AND,
        'OR': COBOLTokenType.OR,
        'THAN': COBOLTokenType.THAN,
        'STOP': COBOLTokenType.STOP,
        'RUN': COBOLTokenType.RUN,
        'GOBACK': COBOLTokenType.GOBACK,
        'EXIT': COBOLTokenType.EXIT,
        'FUNCTION': COBOLTokenType.FUNCTION,
        'UPPER-CASE': COBOLTokenType.UPPER_CASE,
        'LOWER-CASE': COBOLTokenType.LOWER_CASE,
        'STRING': COBOLTokenType.STRING,
        'UNSTRING': COBOLTokenType.UNSTRING,
        'INSPECT': COBOLTokenType.INSPECT,
        'DELIMITED': COBOLTokenType.DELIMITED,
        'SIZE': COBOLTokenType.SIZE,
        'REPLACING': COBOLTokenType.REPLACING,
        'TALLYING': COBOLTokenType.TALLYING,
        'FOR': COBOLTokenType.FOR,
        'WITH': COBOLTokenType.WITH,
        'POINTER': COBOLTokenType.POINTER,
        'IN': COBOLTokenType.IN,
        'ALL': COBOLTokenType.ALL,
    }
    
    def __init__(self, source: str):
        self.source = source
        self.position = 0
        self.line = 1
        self.column = 1
        self.tokens: List[Token] = []
    
    def error(self, message: str):
        raise LexerError(message, self.line, self.column)
    
    def peek(self, offset: int = 0) -> Optional[str]:
        """Peek at character at current position + offset"""
        pos = self.position + offset
        if pos < len(self.source):
            return self.source[pos]
        return None
    
    def advance(self) -> str:
        """Move to next character and return current"""
        if self.position >= len(self.source):
            return None
        
        char = self.source[self.position]
        self.position += 1
        
        if char == '\n':
            self.line += 1
            self.column = 1
        else:
            self.column += 1
        
        return char
    
    def skip_whitespace(self):
        """Skip spaces and tabs (but not newlines)"""
        while self.peek() and self.peek() in ' \t\r':
            self.advance()
    
    def skip_line_comment(self):
        """Skip comment lines (start with * in column 7 or anywhere with *)"""
        while self.peek() and self.peek() != '\n':
            self.advance()
        if self.peek() == '\n':
            self.advance()
    
    def read_string_literal(self, quote: str) -> str:
        """Read a string literal"""
        self.advance()  # consume opening quote
        value = ""
        
        while self.peek() and self.peek() != quote:
            if self.peek() == '\\':
                self.advance()
                if self.peek():
                    value += self.advance()
            else:
                value += self.advance()
        
        if not self.peek():
            self.error("Unterminated string literal")
        
        self.advance()  # consume closing quote
        return value
    
    def read_number(self) -> str:
        """Read a number literal"""
        value = ""
        
        while self.peek() and self.peek().isdigit():
            value += self.advance()
        
        # Check for decimal point
        if self.peek() == '.' and self.peek(1) and self.peek(1).isdigit():
            value += self.advance()  # consume the '.'
            
            # Read fractional part
            while self.peek() and self.peek().isdigit():
                value += self.advance()
        
        return value
    
    def read_identifier(self) -> str:
        """Read an identifier or keyword"""
        value = ""
        
        # COBOL identifiers can contain letters, digits, and hyphens
        # Must start with a letter
        while self.peek() and (self.peek().isalnum() or self.peek() == '-'):
            value += self.advance()
        
        return value.upper()  # COBOL is case-insensitive
    
    def is_level_number(self, text: str) -> bool:
        """Check if text is a valid COBOL level number (01-49, 66, 77, 88)"""
        if not text.isdigit():
            return False
        num = int(text)
        return (1 <= num <= 49) or num in [66, 77, 88]
    
    def tokenize(self) -> List[Token]:
        """Main tokenization method - line-based for comment handling"""
        lines = self.source.split('\n')

        for line_num, line in enumerate(lines, 1):
            # Skip empty lines
            if not line.strip():
                continue

            # ✅ FIX: Skip COBOL comment lines BEFORE any tokenization
            # Fixed-format: asterisk in column 7 (index 6)
            if len(line) > 6 and line[6] == '*':
                continue

            # Free-format: line starts with *> or just *
            stripped = line.lstrip()
            if stripped.startswith('*>'):
                continue
            # ADD THIS LINE:
            if stripped.startswith('*'):
                continue

            # Process the line character by character
            self.source = line + '\n' # Process one line at a time
            self.position = 0
            self.line = line_num
            self.column = 1
            
            while self.position < len(self.source) and self.peek() != '\n':
                self.skip_whitespace()
                if self.peek() is None or self.peek() == '\n':
                    break
            
                # String literals
                if self.peek() in ['"', "'"]:
                    quote = self.peek()
                    value = self.read_string_literal(quote)
                    self.tokens.append(Token(COBOLTokenType.STRING_LITERAL, value, self.line, self.column))
                    continue
                
                # Numbers
                if self.peek().isdigit():
                    num_str = self.read_number()
                    
                    is_statement_start = (len(self.tokens) == 0 or 
                                        self.tokens[-1].type == COBOLTokenType.PERIOD)
                    
                    if is_statement_start and self.is_level_number(num_str) and '.' not in num_str:
                        self.tokens.append(Token(COBOLTokenType.LEVEL_NUMBER, num_str, self.line, self.column))
                    else:
                        self.tokens.append(Token(COBOLTokenType.NUMBER_LITERAL, num_str, self.line, self.column))
                    continue
                
                # Punctuation
                if self.peek() == '.':
                    self.tokens.append(Token(COBOLTokenType.PERIOD, '.', self.line, self.column))
                    self.advance()
                    continue
                
                if self.peek() == ',':
                    self.tokens.append(Token(COBOLTokenType.COMMA, ',', self.line, self.column))
                    self.advance()
                    continue
                
                if self.peek() == '(':
                    self.tokens.append(Token(COBOLTokenType.LPAREN, '(', self.line, self.column))
                    self.advance()
                    continue
                
                if self.peek() == ')':
                    self.tokens.append(Token(COBOLTokenType.RPAREN, ')', self.line, self.column))
                    self.advance()
                    continue
                
                if self.peek() == ':':
                    self.tokens.append(Token(COBOLTokenType.COLON, ':', self.line, self.column))
                    self.advance()
                    continue
                
                if self.peek() == ';':
                    self.tokens.append(Token(COBOLTokenType.SEMICOLON, ';', self.line, self.column))
                    self.advance()
                    continue
                
                # Dollar sign (used in display-edited PIC clauses)
                if self.peek() == '$':
                    self.tokens.append(Token(COBOLTokenType.DOLLAR_SIGN, '$', self.line, self.column))
                    self.advance()
                    continue
                
                if self.peek() == '+':
                    self.tokens.append(Token(COBOLTokenType.PLUS, '+', self.line, self.column))
                    self.advance()
                    continue
                
                if self.peek() == '-':
                    self.tokens.append(Token(COBOLTokenType.MINUS, '-', self.line, self.column))
                    self.advance()
                    continue
                
                if self.peek() == '*':
                    self.tokens.append(Token(COBOLTokenType.ASTERISK, '*', self.line, self.column))
                    self.advance()
                    continue
                
                if self.peek() == '/':
                    self.tokens.append(Token(COBOLTokenType.SLASH, '/', self.line, self.column))
                    self.advance()
                    continue
                
                # NEW: >= operator
                if self.peek() == '>' and self.peek(1) == '=':
                    self.tokens.append(Token(COBOLTokenType.GTE_SIGN, '>=', self.line, self.column))
                    self.advance()
                    self.advance()
                    continue
                
                # Existing > operator
                if self.peek() == '>':
                    self.tokens.append(Token(COBOLTokenType.GT_SIGN, '>', self.line, self.column))
                    self.advance()
                    continue
                
                # NEW: <= operator
                if self.peek() == '<' and self.peek(1) == '=':
                    self.tokens.append(Token(COBOLTokenType.LTE_SIGN, '<=', self.line, self.column))
                    self.advance()
                    self.advance()
                    continue
                
                # Existing < operator
                if self.peek() == '<':
                    self.tokens.append(Token(COBOLTokenType.LT_SIGN, '<', self.line, self.column))
                    self.advance()
                    continue
                
                if self.peek() == '=':
                    self.tokens.append(Token(COBOLTokenType.EQUALS_SIGN, '=', self.line, self.column))
                    self.advance()
                    continue
                
                # Identifiers and keywords
                if self.peek().isalpha():
                    ident = self.read_identifier()
                    token_type = self.KEYWORDS.get(ident, COBOLTokenType.IDENTIFIER)
                    self.tokens.append(Token(token_type, ident, self.line, self.column))
                    continue
                
                # Unknown character
                self.error(f"Unexpected character: '{self.peek()}'")
        
        # Add EOF token
        self.tokens.append(Token(COBOLTokenType.EOF, '', self.line + 1, 1))
        return self.tokens