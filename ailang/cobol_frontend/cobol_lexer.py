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
import re

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
    MERGE = auto()
    COLLATING = auto()
    SEQUENCE = auto()
    ON = auto()             
    ASCENDING = auto()       
    DESCENDING = auto()      
    KEY = auto() 
    RENAMES = auto()
    
    
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
    IS = auto()
    
    # Control flow
    IF = auto()
    THEN = auto()
    ELSE = auto()
    END_IF = auto()
    PERFORM = auto()
    UNTIL = auto()
    CONTINUE = auto()
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
    READ = auto()
    WRITE = auto()
    OPEN = auto()
    CLOSE = auto()
    REWRITE = auto()
    DELETE = auto()
    START = auto()
    SET = auto()
    COPY = auto()
    
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
    NEWLINE = "NEWLINE"
    COMMENT = "COMMENT"
    NIST_HEADER = "NIST_HEADER"      # NEW: *HEADER,COBOL,PROGNAME
    NIST_END_OF = "NIST_END_OF"      # NEW: *END-OF,PROGNAME
    EOF = "EOF"

def preprocess_continuation_lines(source: str) -> str:
    """
    Preprocess COBOL source with truth table approach
    
    TRUTH TABLE: Line Type Detection (Priority Order)
    ┌──────────────────────────────┬────────────────────────────┐
    │ Line Pattern                 │ Action                     │
    ├──────────────────────────────┼────────────────────────────┤
    │ Empty/whitespace             │ Keep as-is                 │
    │ *HEADER, *END-OF (col 1-7)   │ Keep as-is (NIST marker)   │
    │ Not fixed-format             │ Keep as-is                 │
    │ Fixed + col7 = */ or D       │ Mark as comment            │
    │ Fixed + col7 = -             │ ERROR (handled by parent)  │
    │ Fixed + normal               │ Extract cols 7-72 + merge  │
    └──────────────────────────────┴────────────────────────────┘
    
    TRUTH TABLE: Continuation Line Detection
    ┌──────────────────────────────┬────────────────────────────┐
    │ Next Line Pattern            │ Action                     │
    ├──────────────────────────────┼────────────────────────────┤
    │ Not fixed-format             │ Stop continuation          │
    │ col7 = -                     │ Merge with current         │
    │ col7 = */ or D               │ Stop continuation          │
    │ Other                        │ Stop continuation          │
    └──────────────────────────────┴────────────────────────────┘
    """
    lines = source.split('\n')
    result = []
    i = 0
    
    while i < len(lines):
        line = lines[i]
        
        # TRUTH TABLE: Determine line type and action
        
        # Rule 1: Empty/whitespace → keep as-is
        if not line.strip():
            result.append(line)
            i += 1
            continue
        
        # Rule 2: NIST markers → keep as-is
        if _is_nist_marker(line):
            result.append(line)
            i += 1
            continue
        
        # Rule 3: Not fixed-format → keep as-is
        if not _is_fixed_format(line):
            result.append(line)
            i += 1
            continue
        
        # Rule 4: Too short → keep as-is
        if len(line) < 7:
            result.append(line)
            i += 1
            continue
        
        # Fixed-format line - check indicator (column 7)
        indicator = line[6]
        
        # Rule 5: Comment/debug line → skip entirely (don't pass to lexer)
        if _is_comment_indicator(indicator):
            # Don't append anything - skip this line completely
            # The lexer will never see it
            i += 1
            continue
        
        # Rule 6: Continuation indicator → ERROR (should not start a line)
        if indicator == '-':
            result.append(line)  # Let lexer handle error
            i += 1
            continue
        
        # Rule 7: Normal code line → extract and process continuations
        code, lines_consumed = _extract_code_with_continuations(lines, i)
        result.append(code)
        i += lines_consumed
    
    return '\n'.join(result)


def _is_nist_marker(line: str) -> bool:
    """Check if line is a NIST test marker"""
    return line.startswith('*HEADER') or line.startswith('*END-OF')


def _is_fixed_format(line: str) -> bool:
    """Check if line is fixed-format COBOL
    
    TRUTH TABLE: Fixed-format detection
    ┌──────────────────────────────┬────────────┐
    │ Condition                    │ Result     │
    ├──────────────────────────────┼────────────┤
    │ Length < 7                   │ False      │
    │ Length > 80                  │ False      │
    │ Cols 1-6 all digits          │ True       │
    │ Cols 1-6 all spaces          │ True       │
    │ Other                        │ False      │
    └──────────────────────────────┴────────────┘
    
    CRITICAL: Lines only need to be >= 7 chars to be valid fixed-format.
    The original check for >= 72 was too strict - many valid COBOL lines
    are shorter than 72 characters (comments, short statements, etc.)
    """
    # Changed from: if len(line) < 72 or len(line) > 80:
    if len(line) < 7 or len(line) > 80:
        return False
    
    first_six = line[:6]
    return first_six.isdigit() or (first_six.isspace() and len(first_six) == 6)

def _is_comment_indicator(indicator: str) -> bool:
    """Check if column 7 indicator marks a comment"""
    return indicator in '*/' or indicator == 'D'


def _extract_code_with_continuations(lines: list, start_idx: int) -> tuple:
    """Extract code from columns 7-72 and merge continuation lines
    
    Returns: (code_string, number_of_lines_consumed)
    
    TRUTH TABLE: Continuation merging
    ┌──────────────────────────────┬────────────────────────────┐
    │ Current State                │ Action                     │
    ├──────────────────────────────┼────────────────────────────┤
    │ In unclosed string + cont    │ Strip leading quote        │
    │ Normal code + cont           │ Append as-is               │
    │ Next not continuation        │ Stop merging               │
    └──────────────────────────────┴────────────────────────────┘
    
    CRITICAL: Continuation lines can be as short as 8 characters
    (6 for sequence + 1 for hyphen + 1+ for code). The original
    check for >= 72 chars was too strict.
    """
    line = lines[start_idx]
    
    # Extract initial code (columns 7-72)
    if len(line) >= 73:
        code = line[6:72]
    elif len(line) >= 7:
        code = line[6:]
    else:
        code = ""
    
    # Look for continuation lines
    j = start_idx + 1
    
    while j < len(lines):
        next_line = lines[j]
        
        # TRUTH TABLE: Should we continue?
        
        # Not long enough for fixed-format (need at least 7: sequence + indicator)
        if len(next_line) < 7:
            break
        
        # CHANGED: Allow lines from 7 to 80 chars (was 72 to 80)
        # Continuation lines can be short!
        if len(next_line) > 80:
            break
        
        # Check sequence number area
        next_first_six = next_line[:6]
        if not (next_first_six.isdigit() or (next_first_six.isspace() and len(next_first_six) == 6)):
            break
        
        next_indicator = next_line[6]
        
        # Not a continuation line
        if next_indicator != '-':
            break
        
        # Extract continuation code (from column 8 onwards, skipping the hyphen)
        if len(next_line) >= 73:
            cont_code = next_line[7:72]
        elif len(next_line) >= 8:
            cont_code = next_line[7:]
        else:
            cont_code = ""
        
        # Check for string continuation
        quote_count = code.count('"')
        is_unclosed_string = (quote_count % 2 == 1)
        
        if is_unclosed_string:
            # String continuation: remove leading quote from continuation
            cont_stripped = cont_code.lstrip()
            if cont_stripped.startswith('"'):
                spaces_before = len(cont_code) - len(cont_stripped)
                cont_code = cont_code[:spaces_before] + cont_stripped[1:]
        
        # Merge continuation
        code += cont_code
        j += 1
    
    lines_consumed = j - start_idx
    return code, lines_consumed

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
        'RENAMES': COBOLTokenType.RENAMES,
        'ON': COBOLTokenType.ON,
        'ASCENDING': COBOLTokenType.ASCENDING,
        'DESCENDING': COBOLTokenType.DESCENDING,
        'COLLATING': COBOLTokenType.COLLATING,
        'SEQUENCE': COBOLTokenType.SEQUENCE,
        'MERGE': COBOLTokenType.MERGE,
        'SEQUENCE': COBOLTokenType.SEQUENCE,
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
        'IS': COBOLTokenType.IS,
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
        'CONTINUE': COBOLTokenType.CONTINUE,
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
        'READ': COBOLTokenType.READ,
        'WRITE': COBOLTokenType.WRITE,
        'COPY': COBOLTokenType.COPY,
        'OPEN': COBOLTokenType.OPEN,
        'CLOSE': COBOLTokenType.CLOSE,
        'REWRITE': COBOLTokenType.REWRITE,
        'DELETE': COBOLTokenType.DELETE,
        'START': COBOLTokenType.START,
        'SET': COBOLTokenType.SET,       
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
        """Read a string literal (defensively)"""
        value = ""
        self.advance()  # Skip opening quote
        
        max_chars = 1000  # Prevent infinite loops
        chars_read = 0
        
        while self.peek() and chars_read < max_chars:
            chars_read += 1
            
            if self.peek() == quote:
                # Check for doubled quotes (COBOL escape)
                if self.peek(1) == quote:
                    value += quote
                    self.advance()
                    self.advance()
                else:
                    # End of string
                    self.advance()  # Skip closing quote
                    return value
            elif self.peek() == '\n':
                # Unterminated string at end of line
                # This is an error, but return what we have
                return value
            else:
                value += self.peek()
                self.advance()
        
        # If we got here, string wasn't terminated properly
        # Return what we have rather than crashing
        return value
    
    def read_number(self) -> str:
        """Read a number literal using truth table approach
        
        TRUTH TABLE: Valid COBOL number formats
        ┌─────────────┬─────────────────────────────────┐
        │ Format      │ Example                         │
        ├─────────────┼─────────────────────────────────┤
        │ Integer     │ 123                             │
        │ Decimal     │ 123.456                         │
        │ Leading .   │ .999 (equivalent to 0.999)      │
        │ Trailing .  │ 123. (equivalent to 123.0)      │
        └─────────────┴─────────────────────────────────┘
        """
        value = ""
        
        # ✅ NEW: Check if number starts with decimal point (e.g., .999)
        if self.peek() == '.' and self.peek(1) and self.peek(1).isdigit():
            value += self.advance()  # consume the '.'
            # Read fractional part
            while self.peek() and self.peek().isdigit():
                value += self.advance()
            return value
        
        # Original logic: Read integer part
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
        """Main tokenization method - line-based for comment handling
        
        TRUTH TABLE: Token Recognition Strategy
        ┌─────────────────────┬──────────────────────────────────┐
        │ Character Pattern   │ Handler                          │
        ├─────────────────────┼──────────────────────────────────┤
        │ *HEADER/*END-OF     │ NIST marker (special)            │
        │ * or /              │ Comment line (skip)              │
        │ " or '              │ String literal                   │
        │ digit or .digit     │ Number literal                   │
        │ letter              │ Identifier/keyword               │
        │ Single-char punct   │ Direct token mapping             │
        │ Multi-char operator │ Lookahead check (>=, <=)         │
        └─────────────────────┴──────────────────────────────────┘
        """
        processed_source = self.source  # Already preprocessed in __init__
        lines = processed_source.split('\n')
        
        for line_num, line in enumerate(lines, 1):
            if not line.strip():
                continue
            
            # Handle NIST markers and comments
            if self._handle_special_lines(line, line_num):
                continue
            
            # Process the line
            self._tokenize_line(line, line_num)
        
        self.tokens.append(Token(COBOLTokenType.EOF, '', self.line, self.column))
        return self.tokens

    def _handle_special_lines(self, line: str, line_num: int) -> bool:
        """Handle NIST markers and comments with truth table approach
        
        TRUTH TABLE: Special Line Detection (Priority Order)
        ┌────────────────────────────────────┬─────────────────────────────────┐
        │ Line Pattern                       │ Action                          │
        ├────────────────────────────────────┼─────────────────────────────────┤
        │ Empty/whitespace only              │ Return False (let caller handle)│
        │ Starts with *HEADER                │ Parse NIST header, return True  │
        │ Starts with *END-OF                │ Parse NIST end marker, ret True │
        │ Exactly equals "*COMMENT"          │ Skip (preprocessor marker), True│
        │ Starts with * (after strip)        │ Skip (comment line), return True│
        │ Starts with / (after strip)        │ Skip (comment line), return True│
        │ Odd number of quotes               │ Skip (malformed line), ret True │
        │ None of the above                  │ Return False (normal code line) │
        └────────────────────────────────────┴─────────────────────────────────┘
        
        EDGE CASES HANDLED:
        1. Empty lines → Return False (not our responsibility)
        2. Whitespace-only lines → Return False
        3. *COMMENT from preprocessor → Skip entirely (critical fix!)
        4. NIST markers with varying whitespace → Regex handles it
        5. Comments with content after * → All skipped
        6. Lines with unclosed quotes → Skipped to avoid parse errors
        
        Returns True if line was handled (skip it), False if needs tokenization
        """
        stripped = line.strip()
        
        # ══════════════════════════════════════════════════════════════════
        # ROW 1: Empty or whitespace-only → not special, let caller handle
        # ══════════════════════════════════════════════════════════════════
        if not stripped:
            return False
        
        # ══════════════════════════════════════════════════════════════════
        # ROW 2: NIST HEADER marker
        # Pattern: *HEADER,COBOL,PROGNAME or *HEADER, COBOL, PROGNAME
        # ══════════════════════════════════════════════════════════════════
        if stripped.startswith('*HEADER'):
            match = re.search(r'\*HEADER\s*,\s*COBOL\s*,\s*([A-Z0-9\-]+)', stripped, re.IGNORECASE)
            if match:
                self.tokens.append(Token(COBOLTokenType.NIST_HEADER, match.group(1).upper(), line_num, 1))
                return True
            # If no match, fall through (malformed NIST marker treated as comment)
        
        # ══════════════════════════════════════════════════════════════════
        # ROW 3: NIST END-OF marker
        # Pattern: *END-OF,PROGNAME or *END-OF, PROGNAME
        # ══════════════════════════════════════════════════════════════════
        if stripped.startswith('*END-OF'):
            match = re.search(r'\*END-OF\s*,\s*([A-Z0-9\-]+)', stripped, re.IGNORECASE)
            if match:
                self.tokens.append(Token(COBOLTokenType.NIST_END_OF, match.group(1).upper(), line_num, 1))
                return True
            # If no match, fall through (malformed NIST marker treated as comment)
        
        # ══════════════════════════════════════════════════════════════════
        # ROW 4: Preprocessor *COMMENT marker
        # CRITICAL: Lines with * in column 7 are converted to "*COMMENT"
        # by preprocess_continuation_lines. Must skip these entirely!
        # ══════════════════════════════════════════════════════════════════
        if stripped == '*COMMENT':
            return True
        
        # ══════════════════════════════════════════════════════════════════
        # ROW 5: Regular comment lines starting with *
        # Includes: *, *>, *NOTE, etc. (anything starting with *)
        # ══════════════════════════════════════════════════════════════════
        if stripped.startswith('*'):
            return True
        
        # ══════════════════════════════════════════════════════════════════
        # ROW 6: Alternate comment indicator (/)
        # Less common but valid COBOL comment syntax
        # ══════════════════════════════════════════════════════════════════
        if stripped.startswith('/'):
            return True
        
        # ══════════════════════════════════════════════════════════════════
        # ROW 7: Malformed string literals (odd number of quotes)
        # These cause tokenization errors, so skip them
        # Handles both single and double quotes
        # ══════════════════════════════════════════════════════════════════
        single_quote_count = line.count("'")
        double_quote_count = line.count('"')
        
        if single_quote_count % 2 == 1 or double_quote_count % 2 == 1:
            return True
        
        # ══════════════════════════════════════════════════════════════════
        # ROW 8: Normal code line → needs tokenization
        # ══════════════════════════════════════════════════════════════════
        return False


    def _tokenize_line(self, line: str, line_num: int):
        """Tokenize a single line using truth table dispatch
        
        TRUTH TABLE: Character Type Recognition (Priority Order)
        ┌────────────────────────────────┬─────────────────────────────────┐
        │ Character Pattern              │ Handler Method                  │
        ├────────────────────────────────┼─────────────────────────────────┤
        │ Whitespace (space/tab)         │ skip_whitespace() → continue    │
        │ Newline or EOF                 │ Break tokenization loop         │
        │ " or '                         │ _tokenize_string()              │
        │ digit                          │ _tokenize_number()              │
        │ . followed by digit            │ _tokenize_number() (decimal)    │
        │ . NOT followed by digit        │ PERIOD token                    │
        │ > followed by =                │ GTE_SIGN token (>=)             │
        │ > NOT followed by =            │ GT_SIGN token (>)               │
        │ < followed by =                │ LTE_SIGN token (<=)             │
        │ < NOT followed by =            │ LT_SIGN token (<)               │
        │ = (EQUALS_SIGN)                │ EQUALS_SIGN token               │
        │ , (COMMA)                      │ COMMA token                     │
        │ ( (LPAREN)                     │ LPAREN token                    │
        │ ) (RPAREN)                     │ RPAREN token                    │
        │ : (COLON)                      │ COLON token                     │
        │ ; (SEMICOLON)                  │ SEMICOLON token                 │
        │ $ (DOLLAR_SIGN)                │ DOLLAR_SIGN token               │
        │ + (PLUS)                       │ PLUS token                      │
        │ - (MINUS)                      │ MINUS token                     │
        │ * (ASTERISK)                   │ ASTERISK token                  │
        │ / (SLASH)                      │ SLASH token                     │
        │ letter (a-z, A-Z)              │ read_identifier() → keyword chk │
        │ Unknown character              │ Error                           │
        └────────────────────────────────┴─────────────────────────────────┘
        
        TRUTH TABLE: Multi-Character Operator Lookahead
        ┌─────────────┬────────────┬──────────────────────────┐
        │ Current Ch  │ Next Ch    │ Result                   │
        ├─────────────┼────────────┼──────────────────────────┤
        │ >           │ =          │ GTE_SIGN (>=), advance 2 │
        │ >           │ not =      │ GT_SIGN (>), advance 1   │
        │ <           │ =          │ LTE_SIGN (<=), advance 2 │
        │ <           │ not =      │ LT_SIGN (<), advance 1   │
        │ .           │ digit      │ Number (decimal start)   │
        │ .           │ not digit  │ PERIOD, advance 1        │
        └─────────────┴────────────┴──────────────────────────┘
        
        EDGE CASES HANDLED:
        1. Decimal numbers starting with dot: .999
        2. Multi-character operators: >=, <=
        3. Whitespace at any position
        4. EOF or newline mid-tokenization
        5. String literals with embedded quotes
        6. Numbers with decimal points
        7. Hyphens in identifiers (COBOL-specific)
        8. Level numbers vs regular numbers (lookahead in _tokenize_number)
        """
        self.source = line + '\n'
        self.position = 0
        self.line = line_num
        self.column = 1
        
        # Single-character token dispatch table
        # Maps character → token type for fast lookup
        SINGLE_CHAR_TOKENS = {
            ',': COBOLTokenType.COMMA,
            '(': COBOLTokenType.LPAREN,
            ')': COBOLTokenType.RPAREN,
            ':': COBOLTokenType.COLON,
            ';': COBOLTokenType.SEMICOLON,
            '$': COBOLTokenType.DOLLAR_SIGN,
            '+': COBOLTokenType.PLUS,
            '-': COBOLTokenType.MINUS,
            '*': COBOLTokenType.ASTERISK,
            '/': COBOLTokenType.SLASH,
            '=': COBOLTokenType.EQUALS_SIGN,
        }
        
        # Main tokenization loop - process each character
        while self.position < len(self.source) and self.peek() != '\n':
            # ══════════════════════════════════════════════════════════════
            # ROW 1: Skip whitespace (spaces and tabs)
            # ══════════════════════════════════════════════════════════════
            self.skip_whitespace()
            
            # ══════════════════════════════════════════════════════════════
            # ROW 2: Check for end of line (newline or None)
            # ══════════════════════════════════════════════════════════════
            if self.peek() is None or self.peek() == '\n':
                break
            
            ch = self.peek()
            
            # ══════════════════════════════════════════════════════════════
            # ROW 3: String literals (single or double quoted)
            # Pattern: "..." or '...'
            # ══════════════════════════════════════════════════════════════
            if ch in ['"', "'"]:
                self._tokenize_string(ch)
                continue
            
            # ══════════════════════════════════════════════════════════════
            # ROW 4: Numbers (including decimal numbers starting with dot)
            # Patterns: 123, 123.45, .999
            # Delegates to _tokenize_number() which uses its own truth table:
            #   - Checks for digit-starting identifiers (80PARTS)
            #   - Uses lookahead to distinguish level numbers from literals
            #   - Handles decimal numbers starting with dot
            # ══════════════════════════════════════════════════════════════
            if ch.isdigit():
                self._tokenize_number()
                continue
            
            # Special case: decimal number starting with dot (.999)
            if ch == '.' and self.peek(1) and self.peek(1).isdigit():
                self._tokenize_number()
                continue
            
            # ══════════════════════════════════════════════════════════════
            # ROW 5: Period (only if NOT followed by digit)
            # Pattern: . (statement terminator or level separator)
            # ══════════════════════════════════════════════════════════════
            if ch == '.':
                # Already checked for digit above, so this is a period token
                self.tokens.append(Token(COBOLTokenType.PERIOD, '.', self.line, self.column))
                self.advance()
                continue
            
            # ══════════════════════════════════════════════════════════════
            # ROW 6-7: Multi-character operator: >= (greater than or equal)
            # Must check lookahead before single > token
            # ══════════════════════════════════════════════════════════════
            if ch == '>':
                if self.peek(1) == '=':
                    # Two-character operator >=
                    self.tokens.append(Token(COBOLTokenType.GTE_SIGN, '>=', self.line, self.column))
                    self.advance()  # consume '>'
                    self.advance()  # consume '='
                else:
                    # Single-character operator >
                    self.tokens.append(Token(COBOLTokenType.GT_SIGN, '>', self.line, self.column))
                    self.advance()
                continue
            
            # ══════════════════════════════════════════════════════════════
            # ROW 8-9: Multi-character operator: <= (less than or equal)
            # Must check lookahead before single < token
            # ══════════════════════════════════════════════════════════════
            if ch == '<':
                if self.peek(1) == '=':
                    # Two-character operator <=
                    self.tokens.append(Token(COBOLTokenType.LTE_SIGN, '<=', self.line, self.column))
                    self.advance()  # consume '<'
                    self.advance()  # consume '='
                else:
                    # Single-character operator 
                    self.tokens.append(Token(COBOLTokenType.LT_SIGN, '<', self.line, self.column))
                    self.advance()
                continue
            
            # ══════════════════════════════════════════════════════════════
            # ROW 10: Single-character tokens (dispatch table lookup)
            # Includes: , ( ) : ; $ + - * / =
            # ══════════════════════════════════════════════════════════════
            if ch in SINGLE_CHAR_TOKENS:
                token_type = SINGLE_CHAR_TOKENS[ch]
                self.tokens.append(Token(token_type, ch, self.line, self.column))
                self.advance()
                continue
            
            # ══════════════════════════════════════════════════════════════
            # ROW 11: Identifiers and keywords
            # Pattern: starts with letter, contains letters/digits/hyphens
            # ══════════════════════════════════════════════════════════════
            if ch.isalpha():
                ident = self.read_identifier()
                # Check if it's a reserved keyword, otherwise it's an identifier
                token_type = self.KEYWORDS.get(ident, COBOLTokenType.IDENTIFIER)
                self.tokens.append(Token(token_type, ident, self.line, self.column))
                continue
            
            # ══════════════════════════════════════════════════════════════
            # ROW 12: Unknown character → error
            # This should rarely happen with valid COBOL
            # ══════════════════════════════════════════════════════════════
            self.error(f"Unexpected character: '{ch}'")


    def _tokenize_string(self, quote_char: str):
        """Tokenize a string literal with truth table approach
        
        TRUTH TABLE: String Parsing State Machine
        ┌────────────────────────────┬───────────────────────────────────┐
        │ Current State              │ Action                            │
        ├────────────────────────────┼───────────────────────────────────┤
        │ Start (at opening quote)   │ Advance, set start position       │
        │ Reading content            │ Accumulate characters             │
        │ Found closing quote        │ Check for escaped quote           │
        │ Closing quote + same quote │ Escaped quote, continue reading   │
        │ Closing quote + other char │ End of string, return token       │
        │ EOF before closing quote   │ Error (unterminated string)       │
        │ Newline before close quote │ Error (unterminated string)       │
        └────────────────────────────┴───────────────────────────────────┘
        
        TRUTH TABLE: Escaped Quote Detection
        ┌─────────────┬─────────────┬─────────────────────────────┐
        │ Quote Char  │ Next Char   │ Interpretation              │
        ├─────────────┼─────────────┼─────────────────────────────┤
        │ "           │ "           │ Escaped quote (include one) │
        │ "           │ not "       │ End of string               │
        │ '           │ '           │ Escaped quote (include one) │
        │ '           │ not '       │ End of string               │
        └─────────────┴─────────────┴─────────────────────────────┘
        
       # EDGE CASES HANDLED:
       # 1. Empty strings: "" or ''
       # 2. Escaped quotes: "He said ""hello""
       # 3. Unterminated strings (error)
       # 4. Strings spanning to EOF (error)
       # 5. Mixed quote types: "it's" is valid, "it"s" is not
        
       # Examples:
       # - "ABC" → ABC
       # - 'X' → X
       # - "" → (empty)
       # - "Say ""hi""
        """
        start_line = self.line
        start_col = self.column
        
        # ══════════════════════════════════════════════════════════════════
        # STEP 1: Consume opening quote
        # ══════════════════════════════════════════════════════════════════
        self.advance()  # Skip opening quote
        
        value = ""
        max_length = 10000  # Safety limit to prevent infinite loops
        chars_read = 0
        
        # ══════════════════════════════════════════════════════════════════
        # STEP 2: Read string content until closing quote
        # ══════════════════════════════════════════════════════════════════
        while chars_read < max_length:
            ch = self.peek()
            
            # ──────────────────────────────────────────────────────────────
            # ROW 1: EOF before closing quote → ERROR
            # ──────────────────────────────────────────────────────────────
            if ch is None:
                self.error(f"Unterminated string literal starting at line {start_line}, column {start_col}")
            
            # ──────────────────────────────────────────────────────────────
            # ROW 2: Newline before closing quote → ERROR
            # COBOL string literals cannot span lines
            # ──────────────────────────────────────────────────────────────
            if ch == '\n':
                self.error(f"Unterminated string literal (newline encountered) starting at line {start_line}, column {start_col}")
            
            # ──────────────────────────────────────────────────────────────
            # ROW 3: Found potential closing quote
            # Need to check if it's escaped (doubled quote)
            # ──────────────────────────────────────────────────────────────
            if ch == quote_char:
                self.advance()  # Consume the quote
                
                # Check if it's an escaped quote (two quotes in a row)
                next_ch = self.peek()
                
                if next_ch == quote_char:
                    # Escaped quote: "" or ''
                    # Include one quote in the string and continue
                    value += quote_char
                    self.advance()  # Consume the second quote
                    chars_read += 1
                    continue
                else:
                    # Real closing quote → end of string
                    break
            
            # ──────────────────────────────────────────────────────────────
            # ROW 4: Normal character → add to string and continue
            # ──────────────────────────────────────────────────────────────
            value += ch
            self.advance()
            chars_read += 1
        
        # Safety check for infinite loop prevention
        if chars_read >= max_length:
            self.error(f"String literal exceeds maximum length ({max_length} chars)")
        
        # ══════════════════════════════════════════════════════════════════
        # STEP 3: Create and append the STRING_LITERAL token
        # ══════════════════════════════════════════════════════════════════
        self.tokens.append(Token(COBOLTokenType.STRING_LITERAL, value, start_line, start_col))

    
    
    def _tokenize_number(self):
        """Tokenize a number literal with truth table approach
        
        TRUTH TABLE: Number vs Level Number vs Identifier Disambiguation
        ┌────────────────────────────────────┬─────────────────────────────────┐
        │ Pattern                            │ Action                          │
        ├────────────────────────────────────┼─────────────────────────────────┤
        │ digit+ letter+ (e.g., 80PARTS)     │ IDENTIFIER (digit-starting)     │
        │ .digit+ letter+                    │ Error (invalid)                 │
        │ 01-49, 66, 77, 88 + whitespace +   │ LEVEL_NUMBER (lookahead check)  │
        │   identifier/keyword               │                                 │
        │ digit+ (space/punct after)         │ NUMBER_LITERAL (default)        │
        │ .digit+ (space/punct after)        │ NUMBER_LITERAL (decimal)        │
        └────────────────────────────────────┴─────────────────────────────────┘
        
        TRUTH TABLE: Level Number Lookahead Decision (Enhanced)
        ┌──────────────┬────────────────┬───────────────────────────────────┐
        │ Is Valid     │ Context        │ Result                            │
        │ Level Number │                │                                   │
        ├──────────────┼────────────────┼───────────────────────────────────┤
        │ YES          │ followed by )  │ NUMBER_LITERAL (size spec)        │
        │ YES          │ prev = THRU    │ NUMBER_LITERAL (range value)      │
        │ YES          │ prev = VALUE   │ NUMBER_LITERAL (88 level value)   │
        │ YES          │ prev = VALUES  │ NUMBER_LITERAL (88 level value)   │
        │ YES          │ prev = ARE     │ NUMBER_LITERAL (after VALUES ARE) │
        │ YES          │ prev = IS      │ NUMBER_LITERAL (after PICTURE IS) │
        │ YES          │ prev = OCCURS  │ NUMBER_LITERAL (occurs count)     │
        │ YES          │ prev = TO      │ NUMBER_LITERAL (move target)      │
        │ YES          │ prev = IDENTIFIER│ NUMBER_LITERAL (statement operand)│
        │ YES          │ + letter/hyphen│ LEVEL_NUMBER (data declaration)   │
        │ YES          │ other          │ NUMBER_LITERAL (calculation)      │
        │ NO           │ *              │ NUMBER_LITERAL (not valid level)  │
        └──────────────┴────────────────┴───────────────────────────────────┘
        
        CRITICAL: Level numbers ONLY appear at start of data declarations.
        They are NEVER valid in:
        - PIC size specs: X(120), 9(5), PICTURE IS 99
        - VALUE clauses: VALUE 2 THRU 4, VALUES ARE 2 THRU 4
        - OCCURS clauses: OCCURS 10
        - Expressions: MOVE 03 TO X, ADD N-13 1 GIVING X
        - After identifiers: N-13 1, WS-VAR 03
        
        CRITICAL: Some COBOL keywords (VALUES, ARE) aren't tokenized as keywords
        by the lexer - they come through as IDENTIFIER. We must check both the
        token type AND the identifier value.
        
        Examples that must work:
        1. "03 FILLER PIC X."        → LEVEL_NUMBER(03), IDENTIFIER(FILLER)
        2. "77 WS-COUNT PIC 9."      → LEVEL_NUMBER(77), IDENTIFIER(WS-COUNT)
        3. "MOVE 03 TO WS-VAR."      → NUMBER_LITERAL(03)
        4. "PIC X(120)."             → NUMBER_LITERAL(120)
        5. "88 B VALUES ARE 2 THRU 4." → NUMBER_LITERAL(2), NUMBER_LITERAL(4)
        6. "OCCURS 10 TIMES."        → NUMBER_LITERAL(10)
        7. "PICTURE IS 99."          → NUMBER_LITERAL(99)
        8. "ADD N-13 1 GIVING X."    → NUMBER_LITERAL(1)
        """
        num_str = self.read_number()
        
        # ══════════════════════════════════════════════════════════════
        # TRUTH TABLE ROW 1: Check if letters immediately follow
        # Pattern: 80PARTS, 123ABC (digit-starting identifier)
        # No space between number and letters = identifier
        # ══════════════════════════════════════════════════════════════
        if self.position < len(self.source):
            next_char = self.source[self.position]
            
            # ✅ FIX: Check for both letters AND hyphens (for identifiers like "3-DIMENSION-TBL")
            if next_char.isalpha() or next_char == '-':
                # This is a digit-starting identifier like "80PARTS" or "3-DIMENSION-TBL"
                while self.position < len(self.source) and (self.source[self.position].isalnum() or self.source[self.position] == '-'):
                    num_str += self.source[self.position]
                    self.position += 1
                    self.column += 1
                
                self.tokens.append(Token(COBOLTokenType.IDENTIFIER, num_str, self.line, self.column))
                return
        
        # ══════════════════════════════════════════════════════════════
        # TRUTH TABLE ROW 2: Check if this is a potential level number
        # Valid level numbers: 01-49, 66, 77, 88 (no decimal point)
        # Use enhanced lookahead to distinguish from arithmetic numbers
        # ══════════════════════════════════════════════════════════════
        is_potential_level = self.is_level_number(num_str) and '.' not in num_str

        if is_potential_level and len(self.tokens) > 0:
            prev_token = self.tokens[-1]
            
            # 🔥 PRIORITY CHECK: If previous token was NEWLINE, this is likely a level number
            # Level numbers almost always appear after newlines (start of declaration line)
            if prev_token.type == COBOLTokenType.NEWLINE:
                # Skip whitespace and check if followed by identifier
                saved_pos = self.position
                saved_col = self.column
                
                # Skip whitespace
                while self.position < len(self.source) and self.source[self.position] in ' \t':
                    self.position += 1
                    self.column += 1
                
                if self.position < len(self.source):
                    next_char = self.source[self.position]
                    
                    # If followed by letter, hyphen, or digit (start of identifier)
                    # This is definitely a level number
                    if next_char.isalpha() or next_char == '-' or next_char.isdigit():
                        # Restore position and emit LEVEL_NUMBER
                        self.position = saved_pos
                        self.column = saved_col
                        self.tokens.append(Token(COBOLTokenType.LEVEL_NUMBER, num_str, self.line, self.column))
                        return
                
                # Restore position even if not followed by identifier
                self.position = saved_pos
                self.column = saved_col
                # Fall through to other checks
            
            # Keywords that indicate this number is NOT a level number
            non_level_contexts = [
                # Statement keywords - numbers after these are operands, not level numbers
                COBOLTokenType.GREATER,    # GREATER 10
                COBOLTokenType.LESS,       # LESS 5
                COBOLTokenType.MOVE,       # MOVE 42 TO X
                COBOLTokenType.ADD,        # ADD 10 TO X
                COBOLTokenType.SUBTRACT,   # SUBTRACT 5 FROM X
                COBOLTokenType.MULTIPLY,   # MULTIPLY X BY 3
                COBOLTokenType.DIVIDE,     # DIVIDE X BY 2
                COBOLTokenType.COMPUTE,    # COMPUTE X = 42
                COBOLTokenType.IF,         # IF X = 10
                COBOLTokenType.DISPLAY,    # DISPLAY 123
                COBOLTokenType.ACCEPT,     # ACCEPT (rare, but possible)
                COBOLTokenType.PERFORM,    # PERFORM 100 TIMES
                COBOLTokenType.NUMBER_LITERAL,
                
                # Value/expression contexts
                COBOLTokenType.VALUE,      # VALUE 2
                COBOLTokenType.THRU,       # THRU 4
                COBOLTokenType.THROUGH,    # THROUGH 4
                COBOLTokenType.OCCURS,     # OCCURS 10
                COBOLTokenType.TO,         # TO 5
                COBOLTokenType.FROM,       # FROM 3
                COBOLTokenType.BY,         # BY 2
                COBOLTokenType.TIMES,      # TIMES 5
                COBOLTokenType.LPAREN,     # X(120) - inside parentheses!
                COBOLTokenType.EQUALS,     # EQUALS 3
                COBOLTokenType.EQUAL,      # EQUAL 3
                COBOLTokenType.EQUALS_SIGN,# = 11
                COBOLTokenType.GT_SIGN,    # > 11
                COBOLTokenType.LT_SIGN,    # < 11
                COBOLTokenType.PLUS,       # + 5
                COBOLTokenType.MINUS,      # - 3
                COBOLTokenType.ASTERISK,   # * 2
                COBOLTokenType.SLASH,      # / 4
                COBOLTokenType.PIC,        # PIC 99
                COBOLTokenType.PICTURE,    # PICTURE 99
                COBOLTokenType.IS,         # IS 99 (after PICTURE IS)
                COBOLTokenType.IDENTIFIER, # After any identifier in a statement
                COBOLTokenType.THAN,      # THAN 5 (after GREATER THAN
                COBOLTokenType.OR,         # OR 11 (abbreviated condition)
                COBOLTokenType.AND,        # AND 5 (abbreviated condition)
            ]
            
            if prev_token.type in non_level_contexts:
                # This number is in a context where it CANNOT be a level number
                self.tokens.append(Token(COBOLTokenType.NUMBER_LITERAL, num_str, self.line, self.column))
                return
            
            # ══════════════════════════════════════════════════════════════
            # ENHANCED CHECK 1B: Check IDENTIFIER values for non-keyword tokens
            # Some COBOL keywords like VALUES, ARE aren't tokenized as keywords
            # They come through as IDENTIFIER, so we must check the value
            # ══════════════════════════════════════════════════════════════
            if prev_token.type == COBOLTokenType.IDENTIFIER:
                # Check if the identifier is actually a keyword context
                identifier_exclusions = [
                    'VALUES',  # VALUES 2 (level 88)
                    'ARE',     # ARE 2 (after VALUES ARE)
                    'THAN',    # THAN 5 (after GREATER THAN)
                ]
                if prev_token.value.upper() in identifier_exclusions:
                    self.tokens.append(Token(COBOLTokenType.NUMBER_LITERAL, num_str, self.line, self.column))
                    return
                
                # ══════════════════════════════════════════════════════════════
                # CRITICAL FIX: After any IDENTIFIER in a statement, numbers are
                # ALWAYS literals, NEVER level numbers
                # Pattern: ADD N-13 1 GIVING X → the 1 is a NUMBER_LITERAL
                # Level numbers ONLY appear after:
                #   - Start of line (newline)
                #   - Comment
                #   - SECTION keyword
                #   - DIVISION keyword
                # ══════════════════════════════════════════════════════════════
                # If we have at least 2 tokens, check the token before the identifier
                if len(self.tokens) >= 2:
                    prev_prev_token = self.tokens[-2]
                    
                    # If the token before the identifier was NOT a newline/comment,
                    # then this number is definitely a literal (we're mid-statement)
                    if prev_prev_token.type not in (COBOLTokenType.NEWLINE, COBOLTokenType.COMMENT):
                        # We're in the middle of a statement: ADD N-13 1 GIVING ...
                        # The 1 is a literal, not a level number
                        self.tokens.append(Token(COBOLTokenType.NUMBER_LITERAL, num_str, self.line, self.column))
                        return
                else:
                    # Only 1 token before us (the identifier), and it's not at start of line
                    # This means: <identifier> <number> at very start of file
                    # Still treat as literal to be safe
                    self.tokens.append(Token(COBOLTokenType.NUMBER_LITERAL, num_str, self.line, self.column))
                    return
            
            # ══════════════════════════════════════════════════════════════
            # ENHANCED CHECK 2: Look ahead to see what follows
            # Save current position for potential rollback
            # ══════════════════════════════════════════════════════════════
            saved_pos = self.position
            saved_col = self.column
            
            # Skip whitespace to see what comes next
            while self.position < len(self.source) and self.source[self.position] in ' \t':
                self.position += 1
                self.column += 1
            
            if self.position < len(self.source):
                next_char = self.source[self.position]
                
                # ══════════════════════════════════════════════════════════════
                # EXCLUSION: If followed by ), this is a size spec like X(120)
                # ══════════════════════════════════════════════════════════════
                if next_char == ')':
                    # Restore position and emit as NUMBER_LITERAL
                    self.position = saved_pos
                    self.column = saved_col
                    self.tokens.append(Token(COBOLTokenType.NUMBER_LITERAL, num_str, self.line, self.column))
                    return
                
                # ══════════════════════════════════════════════════════════════
                # POSITIVE MATCH: If followed by letter/hyphen → LEVEL_NUMBER
                # Pattern: "03 FILLER", "77 WS-COUNT", "01 REC-AREA"
                # ══════════════════════════════════════════════════════════════
                if next_char.isalpha() or next_char == '-' or next_char.isdigit():
                    # Check if hyphen is followed by digit (negative number) or letter (identifier)
                    if next_char == '-':
                        # Save position again
                        peek_pos = self.position + 1
                        if peek_pos < len(self.source) and self.source[peek_pos].isdigit():
                            # This is a negative number like "5 -9999", not a level number
                            self.position = saved_pos
                            self.column = saved_col
                            self.tokens.append(Token(COBOLTokenType.NUMBER_LITERAL, num_str, self.line, self.column))
                            return
                    
                    # Restore position and emit as LEVEL_NUMBER
                    self.position = saved_pos
                    self.column = saved_col
                    self.tokens.append(Token(COBOLTokenType.LEVEL_NUMBER, num_str, self.line, self.column))
                    return
        
        # ══════════════════════════════════════════════════════════════
        # TRUTH TABLE ROW 4: Default case - emit as NUMBER_LITERAL
        # This handles: regular numbers, decimals, numbers in expressions
        # ══════════════════════════════════════════════════════════════
        self.tokens.append(Token(COBOLTokenType.NUMBER_LITERAL, num_str, self.line, self.column))