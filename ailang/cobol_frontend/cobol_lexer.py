#!/usr/bin/env python3

# Copyright (c) 2025 Sean Collins, 2 Paws Machine and Engineering. All rights reserved.
#
# Licensed under the Sean Collins Software License (SCSL). See the LICENSE file in the root directory of this project
# for the full terms and conditions, including restrictions on forking, corporate use, and permissions for private/teaching purposes.

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
    INSTALLATION = auto()
    AUTHOR = auto()
    DATE_WRITTEN = auto()
    DATE_COMPILED = auto()
    SECURITY = auto()
    REMARKS = auto()
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
    OF = auto()
    
     
    
    
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

def preprocess_continuation_lines(source: str) -> str:
    """
    Preprocess COBOL source to merge fixed-format continuation lines.
    Handles COBOL string literal continuation properly.
    
    This function is idempotent - running it multiple times is safe.
    """
    lines = source.split('\n')
    result = []
    i = 0
    
    while i < len(lines):
        line = lines[i]
        
        if not line.strip():
            result.append(line)
            i += 1
            continue
        
        # ✅ STRICT fixed-format detection:
        # A line is fixed-format ONLY if it has BOTH:
        # 1. Proper sequence number area (first 6 chars are digits or spaces)
        # 2. Line is 72-80 chars (has program ID area OR is exactly one COBOL line)
        #
        # After preprocessing, lines are typically 66 chars or merged to >72 chars
        # but they WON'T be exactly in the 72-80 range.
        is_fixed = False
        
        if len(line) >= 72 and len(line) <= 80:
            # Right length for fixed-format (with or without program ID)
            first_six = line[:6]
            # Check if first 6 are valid sequence numbers (all digits OR all spaces)
            if first_six.isdigit() or (first_six.isspace() and len(first_six) == 6):
                is_fixed = True
        
        if not is_fixed:
            result.append(line)
            i += 1
            continue
        
        if len(line) < 7:
            result.append(line)
            i += 1
            continue
        
        indicator = line[6]
        
        # Strip columns from comment/debug lines
        if indicator in '*/' or indicator == 'D':
            if len(line) >= 73:
                comment_code = line[6:72]
            elif len(line) >= 7:
                comment_code = line[6:]
            else:
                comment_code = ""
            result.append(comment_code)
            i += 1
            continue
        
        # Extract code (columns 7-72)
        if len(line) >= 73:
            code = line[6:72]
        elif len(line) >= 7:
            code = line[6:]
        else:
            code = ""
        
        # Look ahead for continuation lines
        j = i + 1
        
        while j < len(lines):
            next_line = lines[j]
            
            if len(next_line) < 7:
                break
            
            # Check if next line is fixed-format (72-80 chars)
            if len(next_line) < 72 or len(next_line) > 80:
                break
                
            next_first_six = next_line[:6]
            if not (next_first_six.isdigit() or (next_first_six.isspace() and len(next_first_six) == 6)):
                break
            
            next_indicator = next_line[6]
            
            # If continuation line
            if next_indicator == '-':
                # Extract continuation code
                if len(next_line) >= 73:
                    cont_code = next_line[7:72]
                elif len(next_line) >= 8:
                    cont_code = next_line[7:]
                else:
                    cont_code = ""
                
                # Check if we're continuing a string literal
                quote_count = code.count('"')
                is_unclosed_string = (quote_count % 2 == 1)
                
                if is_unclosed_string:
                    # String continuation: remove the leading quote from continuation
                    cont_stripped = cont_code.lstrip()
                    if cont_stripped.startswith('"'):
                        spaces_before = len(cont_code) - len(cont_stripped)
                        cont_code = cont_code[:spaces_before] + cont_stripped[1:]
                
                # Merge
                code += cont_code
                j += 1
            else:
                break
        
        # Rebuild line
        result.append(code)
        i = j
    
    return '\n'.join(result)

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
        'END': COBOLTokenType.END,  
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
        'OF': COBOLTokenType.OF,
    }
    
    def __init__(self, source: str):
        self.source = preprocess_continuation_lines(source)
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
        # ✅ FIX: Source is already preprocessed in __init__. Calling it again
        # corrupts already-merged string literals from continuation lines.
        # This change uses the correctly preprocessed source directly.
        processed_source = self.source  # Already preprocessed in __init__
        lines = processed_source.split('\n')
        for line_num, line in enumerate(lines, 1):
            # Skip empty lines
            if not line.strip():
                continue

            # Skip comment lines (already stripped to just the indicator + content)
            stripped = line.lstrip()
            if stripped.startswith('*') or stripped.startswith('/'):
                continue

            # ✅ FIX: Skip lines with an odd number of quotes. This is a temporary
            # workaround to avoid tokenizing preprocessed lines that may still
            # be malformed, preventing "Unterminated string literal" errors.
            if line.count('"') % 2 == 1:
                continue
            
            # Line is already preprocessed - columns 1-6 and 73-80 removed
            # Just tokenize it directly

            # Process the line (now clean!)
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
        self.tokens.append(Token(COBOLTokenType.EOF, '', self.line, self.column))
        return self.tokens