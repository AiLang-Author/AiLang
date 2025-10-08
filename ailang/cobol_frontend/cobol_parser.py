#!/usr/bin/env python3

# Copyright (c) 2025 Sean Collins, 2 Paws Machine and Engineering. All rights reserved.
#
# Licensed under the Sean Collins Software License (SCSL). See the LICENSE file in the root directory of this project
# for the full terms and conditions, including restrictions on forking, corporate use, and permissions for private/teaching purposes.

"""
COBOL Parser - Complete Version with ACCEPT
"""

from typing import List, Optional, Union
from dataclasses import dataclass, field 
from enum import Enum
import re
from cobol_lexer import Token, COBOLTokenType

class ParseContext(Enum):
    TOP_LEVEL = 1
    INSIDE_IF = 2
    INSIDE_LOOP = 3
    INSIDE_PARAGRAPH = 4

class ParserError(Exception):
    def __init__(self, message: str, line: int, column: int):
        self.message = message
        self.line = line
        self.column = column
        super().__init__(f"Parser error at line {line}, column {column}: {message}")




@dataclass
class COBOLASTNode:
    line: int
    column: int

@dataclass
class COBOLCompilationUnit:
    """
    Represents a complete COBOL source file.
    May contain multiple independent PROGRAM-IDs.
    
    This is the new TOP-LEVEL AST node.
    """
    programs: List['COBOLProgram']
    line: int = 0
    column: int = 0


# MODIFY the existing COBOLProgram class to add this field:
@dataclass
class COBOLProgram(COBOLASTNode):
    """Represents ONE COBOL program (one PROGRAM-ID)"""
    program_id: str
    data_division: Optional['COBOLDataDivision'] = None
    procedure_division: Optional['COBOLProcedureDivision'] = None
    contained_programs: List['COBOLProgram'] = field(default_factory=list)
    is_nested: bool = False

@dataclass
class COBOLDataDivision(COBOLASTNode):
    working_storage: List['COBOLVariableDecl']
    linkage_section: Optional[List['COBOLVariableDecl']] = None

@dataclass
class COBOLLinkageSection(COBOLASTNode):
    """LINKAGE SECTION - parameters passed to programs"""
    variables: List['COBOLVariableDecl']

@dataclass
class COBOLVariableDecl(COBOLASTNode):
    level: int
    name: str
    pic_clause: Optional[str]
    value: Optional[str]
    occurs_count: Optional[int] = None
    decimal_places: Optional[int] = None
    usage_type: Optional[str] = None  # NEW: COMP, COMP-3, DISPLAY, etc.
    is_signed: bool = False           # NEW: True if PIC starts with S

@dataclass
class COBOLProcedureDivision(COBOLASTNode):
    paragraphs: List['COBOLParagraph']
    using_params: Optional[List[str]] = None

@dataclass
class COBOLParagraph(COBOLASTNode):
    name: Optional[str]
    statements: List[COBOLASTNode]

@dataclass
class COBOLDisplay(COBOLASTNode):
    expressions: List[COBOLASTNode]

@dataclass
class COBOLAccept(COBOLASTNode):
    variable: str

@dataclass
class COBOLMove(COBOLASTNode):
    source: COBOLASTNode
    target: COBOLASTNode

@dataclass
class COBOLCompute(COBOLASTNode):
    target: COBOLASTNode
    expression: COBOLASTNode

@dataclass
class COBOLArithmetic(COBOLASTNode):
    operation: str
    operands: List[COBOLASTNode]
    target: COBOLASTNode
    giving: Optional[COBOLASTNode] = None

@dataclass
class COBOLIf(COBOLASTNode):
    condition: COBOLASTNode
    then_statements: List[COBOLASTNode]
    else_statements: Optional[List[COBOLASTNode]] = None

@dataclass
class COBOLPerformUntil(COBOLASTNode):
    """PERFORM UNTIL condition or PERFORM paragraph UNTIL condition"""
    condition: COBOLASTNode
    statements: List[COBOLASTNode]
    paragraph_name: Optional[str] = None

@dataclass
class COBOLPerformParagraph(COBOLASTNode):
    paragraph_name: str
    until_condition: Optional[COBOLASTNode] = None

@dataclass
class COBOLPerformTimes(COBOLASTNode):
    """PERFORM paragraph-name N TIMES or PERFORM N TIMES ... END-PERFORM"""
    times_expr: COBOLASTNode  # Number of iterations
    paragraph_name: Optional[str] = None  # If calling paragraph
    statements: Optional[List[COBOLASTNode]] = None  # If inline

@dataclass
class COBOLPerformVarying(COBOLASTNode):
    variable: str
    from_expr: COBOLASTNode
    by_expr: COBOLASTNode
    until_condition: COBOLASTNode
    statements: List[COBOLASTNode]

@dataclass
class COBOLCall(COBOLASTNode):
    program_name: str  # "NESTED-CHILD" or identifier
    parameters: List[COBOLASTNode] = field(default_factory=list)
    using_params: Optional[List[str]] = None  # NEW: Parameter names for USING clause


@dataclass
class COBOLWhenClause(COBOLASTNode):
    value: Optional[COBOLASTNode]
    statements: List[COBOLASTNode]

@dataclass
class COBOLEvaluate(COBOLASTNode):
    subject: COBOLASTNode
    when_clauses: List[COBOLWhenClause]

@dataclass
class COBOLStopRun(COBOLASTNode):
    pass

@dataclass
class COBOLGoback(COBOLASTNode):
    pass

@dataclass
class COBOLExit(COBOLASTNode):
    pass

@dataclass
class COBOLBinaryOp(COBOLASTNode):
    operator: str
    left: COBOLASTNode
    right: COBOLASTNode

@dataclass
class COBOLUnaryOp(COBOLASTNode):
    operator: str
    operand: COBOLASTNode

@dataclass
class COBOLIdentifier(COBOLASTNode):
    name: str

@dataclass
class COBOLArraySubscript(COBOLASTNode):
    """Array element access: ARRAY-NAME(index)"""
    array_name: str
    index: COBOLASTNode  # Can be literal or variable

@dataclass
class COBOLNumberLiteral(COBOLASTNode):
    value: str

@dataclass
class COBOLStringLiteral(COBOLASTNode):
    value: str

@dataclass
class COBOLFunctionCall(COBOLASTNode):
    function_name: str
    arguments: List[COBOLASTNode]

@dataclass
class COBOLStringConcat(COBOLASTNode):
    """STRING statement for concatenation
    STRING field1 field2 DELIMITED BY SIZE INTO target
    """
    source_fields: List[COBOLASTNode]
    delimiters: List[Optional[COBOLASTNode]]  # None means SIZE
    target: str
    pointer: Optional[str] = None  # WITH POINTER clause

@dataclass
class COBOLUnstring(COBOLASTNode):
    """UNSTRING statement for splitting
    UNSTRING source DELIMITED BY delimiter INTO field1 field2
    """
    source: COBOLASTNode
    delimiter: COBOLASTNode
    targets: List[str]
    count: Optional[str] = None  # TALLYING IN clause

@dataclass
class COBOLInspect(COBOLASTNode):
    """INSPECT statement for string manipulation
    INSPECT field REPLACING ALL "x" BY "y"
    INSPECT field TALLYING counter FOR ALL "x"
    """
    target: str
    operation: str  # 'REPLACING' or 'TALLYING'
    pattern: COBOLASTNode
    replacement: Optional[COBOLASTNode] = None  # For REPLACING
    counter: Optional[str] = None  # For TALLYING
    scope: Optional[str] = None  # 'ALL', 'FIRST', 'LEADING'


class COBOLMultiProgramParser:
    def __init__(self, tokens: List[Token]):
        self.tokens = tokens
        self.pos = 0
        self.context_stack = [ParseContext.TOP_LEVEL]
    
    def push_context(self, context: ParseContext):
        self.context_stack.append(context)
    
    def pop_context(self):
        if len(self.context_stack) > 1:
            self.context_stack.pop()
    
    def current_context(self) -> ParseContext:
        return self.context_stack[-1]
    
    def requires_period(self) -> bool:
        ctx = self.current_context()
        return ctx in [ParseContext.TOP_LEVEL, ParseContext.INSIDE_PARAGRAPH]
    
    def current_token(self) -> Token:
        if self.pos < len(self.tokens):
            return self.tokens[self.pos]
        return Token(COBOLTokenType.EOF, "", self.tokens[-1].line if self.tokens else 1, 0)
    
    def peek_token(self, offset: int = 1) -> Optional[Token]:
        pos = self.pos + offset
        if pos < len(self.tokens):
            return self.tokens[pos]
        return None
    
    def advance(self):
        self.pos += 1
    
    def match(self, *token_types: COBOLTokenType) -> bool:
        return self.current_token().type in token_types
    
    def consume(self, token_type: COBOLTokenType) -> Token:
        token = self.current_token()
        if token.type != token_type:
            self.error(f"Expected {token_type.name}, got {token.type.name}")
        self.advance()
        return token
    
    def error(self, message: str):
        token = self.current_token()
        raise ParserError(message, token.line, token.column)
    
    def consume_optional_period(self):
        """Consumes a PERIOD token if it exists, otherwise does nothing."""
        if self.match(COBOLTokenType.PERIOD):
            self.advance()

    def parse_all_programs(self) -> COBOLCompilationUnit:
        """Parse ALL programs from the token stream"""
        programs = []
        
        while not self.match(COBOLTokenType.EOF):
            # Skip comments and whitespace
            if self.match(COBOLTokenType.COMMENT, COBOLTokenType.NEWLINE):
                self.advance()
                continue
            
            # Look for IDENTIFICATION DIVISION (start of a program)
            if self.match(COBOLTokenType.IDENTIFICATION):
                program = self.parse_single_program()
                programs.append(program)
            else:
                # Unexpected token - might be legacy format
                self.advance()
        
        if not programs:
            raise ParserError("No COBOL programs found in source file", 1, 1)
        
        return COBOLCompilationUnit(programs=programs, line=1, column=1)

    def parse_single_program(self) -> COBOLProgram:
        """Parse ONE COBOL program (from IDENTIFICATION to STOP RUN or END PROGRAM)"""
        token = self.current_token()
        # Parse IDENTIFICATION DIVISION
        program_id = self.parse_identification_division()

        # Parse ENVIRONMENT DIVISION (optional, skip it for now)
        environment_division = None
        if self.current_token and self.current_token().type == COBOLTokenType.ENVIRONMENT:
            environment_division = self.parse_environment_division()

        # Parse DATA DIVISION (optional)
        data_division = None
        if self.current_token and self.match(COBOLTokenType.DATA):
            data_division = self.parse_data_division()
        # Parse PROCEDURE DIVISION
        procedure_division = self.parse_procedure_division()
        # Parse nested programs ONLY if we haven't hit STOP/GOBACK yet
        # If the program ended with STOP RUN, next IDENTIFICATION is a sibling, not nested
        contained_programs = []

        # Check if the last statement was STOP RUN or GOBACK
        program_ended = False
        if procedure_division and procedure_division.paragraphs:
            for para in procedure_division.paragraphs:
                if para.statements:
                    last_stmt = para.statements[-1]
                    if isinstance(last_stmt, (COBOLStopRun, COBOLGoback)):
                        program_ended = True
                        break

        # Only parse nested if program hasn't ended naturally
        if not program_ended:
            while self.match(COBOLTokenType.IDENTIFICATION):
                print(f"  DEBUG: Found nested IDENTIFICATION at position {self.pos}")
                nested = self.parse_single_program()
                nested.is_nested = True
                contained_programs.append(nested)
                print(f"  DEBUG: Finished parsing nested program {nested.program_id}")
                
                # Continue parsing parent's statements after nested program
                while not self.match(COBOLTokenType.EOF, COBOLTokenType.IDENTIFICATION, 
                                    COBOLTokenType.END_PROGRAM, COBOLTokenType.END):
                    if self.match(COBOLTokenType.END):
                        next_tok = self.peek_token(1)
                        if next_tok and next_tok.type == COBOLTokenType.PROGRAM:
                            break
                    
                    stmt = self.parse_statement()
                    if stmt:
                        for para in procedure_division.paragraphs:
                            if not para.name:
                                para.statements.append(stmt)
                                break
        # Parse END PROGRAM if present
        self.parse_end_program(program_id)
        
        return COBOLProgram(
            program_id=program_id,
            data_division=data_division,
            procedure_division=procedure_division,
            contained_programs=contained_programs,
            is_nested=False,  # Will be set to True by parent if nested
            line=token.line,
            column=token.column
        )
    
    def parse_identification_division(self) -> str:
        self.consume(COBOLTokenType.IDENTIFICATION)
        self.consume(COBOLTokenType.DIVISION)
        self.consume(COBOLTokenType.PERIOD)
        self.consume(COBOLTokenType.PROGRAM_ID)
        self.consume(COBOLTokenType.PERIOD)
        
        program_id = self.consume(COBOLTokenType.IDENTIFIER).value
        self.consume(COBOLTokenType.PERIOD)
        
        return program_id
    
    def parse_environment_division(self):
        """Parse and skip ENVIRONMENT DIVISION - not needed for transpilation"""
        self.consume(COBOLTokenType.ENVIRONMENT)
        self.consume(COBOLTokenType.DIVISION)
        self.consume(COBOLTokenType.PERIOD)
        
        # Skip everything until we hit DATA or PROCEDURE
        while self.current_token and \
              self.current_token().type not in [COBOLTokenType.DATA, COBOLTokenType.PROCEDURE]:
            self.advance()
        
        return None  # We don't need this for transpilation
    
    def parse_data_division(self) -> COBOLDataDivision:
        token = self.current_token()
        self.consume(COBOLTokenType.DATA)
        self.consume(COBOLTokenType.DIVISION)
        if self.requires_period():
            self.consume(COBOLTokenType.PERIOD)
        
        working_storage_vars = []
        if self.match(COBOLTokenType.WORKING_STORAGE):
            ws_section = self.parse_working_storage()
            working_storage_vars = ws_section if isinstance(ws_section, list) else []
        
        linkage_section_vars = []
        if self.match(COBOLTokenType.LINKAGE):
            ls_section = self.parse_linkage_section()
            linkage_section_vars = ls_section.variables if hasattr(ls_section, 'variables') else []
        
        return COBOLDataDivision(
            working_storage=working_storage_vars,
            linkage_section=linkage_section_vars if linkage_section_vars else None,
            line=token.line,
            column=token.column
        )
    
    def parse_working_storage(self) -> List[COBOLVariableDecl]:
        self.consume(COBOLTokenType.WORKING_STORAGE)
        self.consume(COBOLTokenType.SECTION)
        self.consume(COBOLTokenType.PERIOD)
        
        variables = []
        while self.match(COBOLTokenType.LEVEL_NUMBER):
            variables.append(self.parse_variable_decl())
        
        return variables
    
    def parse_linkage_section(self) -> COBOLLinkageSection:
        """Parse LINKAGE SECTION (same format as WORKING-STORAGE)"""
        token = self.current_token()
        self.consume(COBOLTokenType.LINKAGE)
        self.consume(COBOLTokenType.SECTION)
        if self.requires_period():
            self.consume(COBOLTokenType.PERIOD)
        
        variables = []
        while self.match(COBOLTokenType.LEVEL_NUMBER):
            variables.append(self.parse_variable_decl())
        
        return COBOLLinkageSection(variables=variables, line=token.line, column=token.column)
    
    def parse_variable_decl(self) -> COBOLVariableDecl:
        token = self.current_token()
        level_token = self.consume(COBOLTokenType.LEVEL_NUMBER)
        level = int(level_token.value)

        # Parse variable name (keywords can be variable names in COBOL!)
        name_token = self.current_token()
        
        if self.match(COBOLTokenType.IDENTIFIER):
            name = self.consume(COBOLTokenType.IDENTIFIER).value
        
        # COBOL allows keywords as variable names (e.g., "01 USAGE PIC X(10).")
        elif self.match(
            COBOLTokenType.USAGE,
            COBOLTokenType.COMP,
            COBOLTokenType.COMP_1,
            COBOLTokenType.COMP_2,
            COBOLTokenType.COMP_3,
            COBOLTokenType.COMPUTATIONAL,
            COBOLTokenType.COMPUTATIONAL_3,
            COBOLTokenType.BINARY,
            COBOLTokenType.PACKED_DECIMAL,
            COBOLTokenType.DISPLAY
        ):
            name = name_token.value
            self.advance()
        else:
            self.error("Expected variable name (identifier or keyword)")

        # NEW: Parse OCCURS clause (comes before PIC)
        occurs_count = None
        if self.match(COBOLTokenType.OCCURS):
            self.advance()  # consume OCCURS
            count_token = self.consume(COBOLTokenType.NUMBER_LITERAL)
            occurs_count = int(count_token.value)
            # Optional TIMES keyword
            if self.match(COBOLTokenType.TIMES):
                self.advance()

        # Parse PIC clause
        pic_clause = None
        if self.match(COBOLTokenType.PIC, COBOLTokenType.PICTURE):
            self.advance()
            pic_clause = self.parse_pic_clause()

        # NEW: Parse decimal places from PIC clause
        decimal_places = None
        if pic_clause:
            # Check for PIC 9(n)V9(n) format (e.g., PIC 9(5)V9(2))
            decimal_match = re.search(r'9\((\d+)\)V9\((\d+)\)', pic_clause, re.IGNORECASE)
            if decimal_match:
                # Extract decimal places (the digits after V)
                decimal_places = int(decimal_match.group(2))

        # NEW: Extract sign from PIC clause
        is_signed = False
        if pic_clause:
            pic_upper = pic_clause.upper()
            if pic_upper.startswith('S') or 'S9' in pic_upper:
                is_signed = True

        # NEW: Parse USAGE clause (comes after PIC, before VALUE)
        usage_type = None
        if self.match(COBOLTokenType.USAGE):
            self.advance()  # consume USAGE
            usage_type = self.parse_usage_type()
        # Storage types can appear without USAGE keyword
        elif self.match(
            COBOLTokenType.COMP,
            COBOLTokenType.COMP_1,
            COBOLTokenType.COMP_2,
            COBOLTokenType.COMP_3,
            COBOLTokenType.COMPUTATIONAL,      # NEW
            COBOLTokenType.COMPUTATIONAL_3,    # NEW
            COBOLTokenType.BINARY,             # NEW
            COBOLTokenType.PACKED_DECIMAL,     # NEW
            COBOLTokenType.DISPLAY             # NEW - FIXES THE BUG!
        ):
            token = self.current_token()
            
            # Map token type to usage string
            if token.type == COBOLTokenType.COMP:
                usage_type = 'COMP'
            elif token.type == COBOLTokenType.COMP_1:
                usage_type = 'COMP-1'
            elif token.type == COBOLTokenType.COMP_2:
                usage_type = 'COMP-2'
            elif token.type == COBOLTokenType.COMP_3:
                usage_type = 'COMP-3'
            elif token.type == COBOLTokenType.COMPUTATIONAL:    # NEW
                usage_type = 'COMP'
            elif token.type == COBOLTokenType.COMPUTATIONAL_3:  # NEW
                usage_type = 'COMP-3'
            elif token.type == COBOLTokenType.BINARY:           # NEW
                usage_type = 'BINARY'
            elif token.type == COBOLTokenType.PACKED_DECIMAL:   # NEW
                usage_type = 'PACKED-DECIMAL'
            elif token.type == COBOLTokenType.DISPLAY:          # NEW - THE FIX!
                usage_type = 'DISPLAY'
            
            self.advance()

        # Parse VALUE clause
        value = None
        if self.match(COBOLTokenType.VALUE):
            self.advance()
            
            # String literals
            if self.match(COBOLTokenType.STRING_LITERAL):
                value = self.current_token().value
                self.advance()
            
            # Signed numbers: VALUE -123.45 or VALUE +123.45
            elif self.match(COBOLTokenType.PLUS, COBOLTokenType.MINUS):
                sign = self.current_token().value
                self.advance()
                if self.match(COBOLTokenType.NUMBER_LITERAL):
                    value = sign + self.current_token().value
                    self.advance()
                else:
                    self.error("Expected number after sign in VALUE clause")
            
            # Unsigned numbers: VALUE 123.45
            elif self.match(COBOLTokenType.NUMBER_LITERAL):
                value = self.current_token().value
                self.advance()
            
            # Special constants: SPACES, ZEROS, etc.
            elif self.match(COBOLTokenType.IDENTIFIER):
                id_value = self.current_token().value.upper()
                if id_value in ['SPACES', 'SPACE', 'ZEROS', 'ZEROES', 'ZERO']:
                    value = id_value
                    self.advance()

        self.consume(COBOLTokenType.PERIOD)

        return COBOLVariableDecl(
            level=level,
            name=name,
            pic_clause=pic_clause,
            value=value,
            occurs_count=occurs_count,
            decimal_places=decimal_places,
            usage_type=usage_type,      # NEW
            is_signed=is_signed,        # NEW
            line=token.line,
            column=token.column
        )

    def parse_pic_clause(self) -> str:
        """
        Parse a PIC clause, collecting all tokens until we hit a terminator.
        
        Handles:
        - Basic formats: 9(4), X(20), S9(5)V99
        - Display-edited: $$$$,$$9.99, ZZZ9, ***9.99, +999.99-, etc.
        - All punctuation and special characters in PIC context
        
        Returns the complete PIC string (e.g., "$$$$,$$9.99")
        """
        pic_str = ""
        
        # Terminators: things that end a PIC clause
        terminators = [
            COBOLTokenType.PERIOD,
            COBOLTokenType.VALUE,
            COBOLTokenType.OCCURS,
            COBOLTokenType.REDEFINES,
            COBOLTokenType.USAGE,
            COBOLTokenType.COMP,
            COBOLTokenType.COMP_1,
            COBOLTokenType.COMP_2,
            COBOLTokenType.COMP_3,
            COBOLTokenType.LEVEL_NUMBER,
            COBOLTokenType.PROCEDURE
        ]
        
        while not self.match(*terminators):
            token = self.current_token()
            
            # Collect various token types that can appear in PIC
            if self.match(
                COBOLTokenType.IDENTIFIER,       # S, V, X, A, Z, CR, DB, etc.
                COBOLTokenType.NUMBER_LITERAL,   # 9, 99, etc.
                COBOLTokenType.LPAREN,           # (
                COBOLTokenType.RPAREN,           # )
                COBOLTokenType.COMMA,            # , (insertion)
                COBOLTokenType.PLUS,             # + (sign)
                COBOLTokenType.MINUS,            # - (sign)
                COBOLTokenType.ASTERISK,         # * (fill)
                COBOLTokenType.SLASH,            # / (insertion)
                COBOLTokenType.DOLLAR_SIGN       # $ (currency)
            ):
                pic_str += token.value
                self.advance()
            else:
                # Unknown token type - stop parsing
                break
        
        return pic_str.strip()
    
    def parse_usage_type(self) -> str:
        """
        Parse USAGE clause value.
        
        Examples:
          USAGE COMP
          USAGE COMP-3
          USAGE DISPLAY
          USAGE BINARY
        
        Returns the usage type as a string.
        """
        if self.match(COBOLTokenType.COMP):
            self.advance()
            return 'COMP'
        elif self.match(COBOLTokenType.COMP_1):
            self.advance()
            return 'COMP-1'
        elif self.match(COBOLTokenType.COMP_2):
            self.advance()
            return 'COMP-2'
        elif self.match(COBOLTokenType.COMP_3):
            self.advance()
            return 'COMP-3'
        elif self.match(COBOLTokenType.COMPUTATIONAL):
            self.advance()
            return 'COMP'
        elif self.match(COBOLTokenType.COMPUTATIONAL_3):
            self.advance()
            return 'COMP-3'
        elif self.match(COBOLTokenType.BINARY):
            self.advance()
            return 'BINARY'
        elif self.match(COBOLTokenType.PACKED_DECIMAL):
            self.advance()
            return 'PACKED-DECIMAL'
        elif self.match(COBOLTokenType.DISPLAY):
            self.advance()
            return 'DISPLAY'
        else:
            # Default to DISPLAY if nothing matches
            return 'DISPLAY'
    
    def parse_procedure_division(self) -> COBOLProcedureDivision:
        token = self.current_token()
        self.consume(COBOLTokenType.PROCEDURE)
        self.consume(COBOLTokenType.DIVISION)
        
        # NEW: Check for USING clause BEFORE period
        using_params = []
        if self.match(COBOLTokenType.USING):
            self.consume(COBOLTokenType.USING)
            # Parse parameter list
            while self.match(COBOLTokenType.IDENTIFIER):
                param_name = self.consume(COBOLTokenType.IDENTIFIER).value
                using_params.append(param_name)
                # Stop if we hit a period
                if self.match(COBOLTokenType.PERIOD):
                    break
        
        # Consume period (now happens after checking for USING)
        self.consume(COBOLTokenType.PERIOD)
        
        # Parse paragraphs and statements
        paragraphs = []
        
        while not self.match(COBOLTokenType.EOF):
            # Stop if we hit IDENTIFICATION DIVISION (sibling or nested)
            if self.match(COBOLTokenType.IDENTIFICATION):
                break
            # Stop if we hit END PROGRAM
            if self.match(COBOLTokenType.END_PROGRAM):
                break
            if self.match(COBOLTokenType.END):
                next_tok = self.peek_token(1)
                if next_tok and next_tok.type == COBOLTokenType.PROGRAM:
                    break
            # Skip comments
            if self.match(COBOLTokenType.COMMENT, COBOLTokenType.NEWLINE):
                self.advance()
                continue
            
            if self.match(COBOLTokenType.IDENTIFIER):
                next_token = self.peek_token()
                if next_token and next_token.type == COBOLTokenType.PERIOD:
                    identifier_value = self.current_token().value
                    if identifier_value.upper() not in ['STOP', 'GOBACK', 'EXIT', 'DISPLAY', 
                                                    'MOVE', 'ADD', 'SUBTRACT', 'MULTIPLY',
                                                    'DIVIDE', 'COMPUTE', 'ACCEPT', 'PERFORM',
                                                    'IF', 'ELSE', 'EVALUATE', 'CALL']:
                        para = self.parse_paragraph()
                        if para:
                            paragraphs.append(para)
                        continue
            
            stmt = self.parse_statement()
            if stmt:
                # ✅ FIX: Use paragraphs[-1] not paragraphs[0]
                # Ensure top-level statements are added to an unnamed paragraph
                if not paragraphs or paragraphs[-1].name is not None:
                    paragraphs.append(COBOLParagraph(name=None, statements=[], line=stmt.line, column=stmt.column))
                paragraphs[-1].statements.append(stmt)  # ✅ FIXED: was paragraphs[0]
        
        return COBOLProcedureDivision(
            paragraphs=paragraphs,
            using_params=using_params if using_params else None,
            line=token.line,
            column=token.column
        )
    
    def parse_end_program(self, expected_program_id: str) -> None:
        """
        Parse END PROGRAM program-id.
        Can be END-PROGRAM or END PROGRAM (two tokens)
        """
        if self.match(COBOLTokenType.END_PROGRAM):
            self.consume(COBOLTokenType.END_PROGRAM)
        elif self.match(COBOLTokenType.END):
            self.consume(COBOLTokenType.END)
            self.consume(COBOLTokenType.PROGRAM)
        else:
            return  # END PROGRAM is optional for top-level programs
        
        # Optional: verify program-id matches
        if self.match(COBOLTokenType.IDENTIFIER):
            end_program_id = self.consume(COBOLTokenType.IDENTIFIER).value
            if end_program_id != expected_program_id:
                self.error(f"END PROGRAM {end_program_id} doesn't match PROGRAM-ID {expected_program_id}")
        
        if self.requires_period():
            self.consume(COBOLTokenType.PERIOD)

    def parse_paragraph(self) -> COBOLParagraph:
        token = self.current_token()
        name_token = self.consume(COBOLTokenType.IDENTIFIER)
        self.consume(COBOLTokenType.PERIOD)
        
        self.push_context(ParseContext.INSIDE_PARAGRAPH)
        
        statements = []
        while not self.match(COBOLTokenType.EOF):
            # ✅ ADD THIS CRITICAL CHECK:
            # Stop if we hit another IDENTIFICATION DIVISION
            if self.match(COBOLTokenType.IDENTIFICATION):
                break
            
            if self.match(COBOLTokenType.IDENTIFIER):
                next_token = self.peek_token()
                if next_token and next_token.type == COBOLTokenType.PERIOD:
                    identifier_value = self.current_token().value
                    if identifier_value.upper() not in ['STOP', 'GOBACK', 'EXIT', 'DISPLAY', 
                                                        'MOVE', 'ADD', 'SUBTRACT', 'COMPUTE',
                                                        'MULTIPLY', 'DIVIDE', 'IF', 'PERFORM', 
                                                        'EVALUATE', 'ACCEPT']:
                        break
            
            stmt = self.parse_statement()
            if stmt:
                statements.append(stmt)
        
        self.pop_context()
        
        return COBOLParagraph(
            name=name_token.value,
            statements=statements,
            line=token.line,
            column=token.column
        )
    
    def parse_statement(self) -> Optional[COBOLASTNode]:
        token = self.current_token()
        
        print(f"DEBUG parse_statement: token={token.type.name} value={token.value} context={self.current_context()}")
        
        # ✅ ADD THIS: Skip comment tokens
        while self.match(COBOLTokenType.COMMENT):
            self.advance()
            if self.match(COBOLTokenType.EOF):
                return None
            token = self.current_token()

        
        if self.match(COBOLTokenType.DISPLAY):
            return self.parse_display(token)
        elif self.match(COBOLTokenType.ACCEPT):
            return self.parse_accept(token)
        elif self.match(COBOLTokenType.MOVE):
            return self.parse_move(token)
        elif self.match(COBOLTokenType.COMPUTE):
            return self.parse_compute(token)
        elif self.match(COBOLTokenType.ADD):
            return self.parse_add(token)
        elif self.match(COBOLTokenType.SUBTRACT):
            return self.parse_subtract(token)
        elif self.match(COBOLTokenType.MULTIPLY):
            return self.parse_multiply(token)
        elif self.match(COBOLTokenType.DIVIDE):
            return self.parse_divide(token)
        elif self.match(COBOLTokenType.IF):
            return self.parse_if(token)  # ✅ Pass the token
        elif self.match(COBOLTokenType.PERFORM):
            return self.parse_perform(token)
        elif self.match(COBOLTokenType.CALL):
            return self.parse_call(token)
        elif self.match(COBOLTokenType.EVALUATE):
            return self.parse_evaluate(token)
        elif self.match(COBOLTokenType.STRING): # MODIFIED
            return self.parse_string_statement(token)
        elif self.match(COBOLTokenType.UNSTRING):
            return self.parse_unstring_statement(token)
        elif self.match(COBOLTokenType.INSPECT):
            return self.parse_inspect_statement(token)
        elif self.match(COBOLTokenType.STOP):
            return self.parse_stop_run(token)
        elif self.match(COBOLTokenType.GOBACK):
            self.advance()
            if self.requires_period():
                self.consume(COBOLTokenType.PERIOD)
            return COBOLGoback(line=token.line, column=token.column)
        elif self.match(COBOLTokenType.EXIT):
            self.advance()
            if self.requires_period():
                self.consume(COBOLTokenType.PERIOD)
            return COBOLExit(line=token.line, column=token.column)
        elif self.match(COBOLTokenType.EOF):
            return None
        else:
            self.advance()
            return None
    
    def parse_display(self, token: Token) -> COBOLDisplay:
        self.consume(COBOLTokenType.DISPLAY)
        
        expressions = []
        while not self.match(COBOLTokenType.PERIOD, COBOLTokenType.ELSE, 
                            COBOLTokenType.END_IF, COBOLTokenType.END_PERFORM,
                            COBOLTokenType.DISPLAY, COBOLTokenType.MOVE,
                            COBOLTokenType.ADD, COBOLTokenType.SUBTRACT,
                            COBOLTokenType.IF, COBOLTokenType.PERFORM,
                            COBOLTokenType.STOP, COBOLTokenType.COMPUTE,
                            COBOLTokenType.EVALUATE, COBOLTokenType.WHEN,
                            COBOLTokenType.END_EVALUATE, COBOLTokenType.ACCEPT):
            expressions.append(self.parse_expression())
        
        if self.requires_period() and self.match(COBOLTokenType.PERIOD):
            self.consume(COBOLTokenType.PERIOD)
        
        return COBOLDisplay(expressions=expressions, line=token.line, column=token.column)
    
    def parse_accept(self, token: Token) -> COBOLAccept:
        self.consume(COBOLTokenType.ACCEPT)
        
        variable = self.consume(COBOLTokenType.IDENTIFIER).value
        
        if self.requires_period():
            self.consume(COBOLTokenType.PERIOD)
        
        return COBOLAccept(
            variable=variable,
            line=token.line,
            column=token.column
        )
    
    def parse_move(self, token: Token) -> COBOLMove:
        self.consume(COBOLTokenType.MOVE)
        
        source = self.parse_expression()
        self.consume(COBOLTokenType.TO)
        target = self.parse_expression()
        
        if self.requires_period():
            self.consume(COBOLTokenType.PERIOD)
        
        return COBOLMove(source=source, target=target, line=token.line, column=token.column)
    
    def parse_compute(self, token: Token) -> COBOLCompute:
        self.consume(COBOLTokenType.COMPUTE)
        
        target = self.parse_expression()
        self.consume(COBOLTokenType.EQUALS_SIGN)
        expression = self.parse_arithmetic_expression()
        
        if self.requires_period():
            self.consume(COBOLTokenType.PERIOD)
        
        return COBOLCompute(target=target, expression=expression, line=token.line, column=token.column)
    
    def parse_add(self, token: Token) -> COBOLArithmetic:
        self.consume(COBOLTokenType.ADD)
        
        operands = [self.parse_expression()]  # NUM1
        
        while not self.match(COBOLTokenType.TO, COBOLTokenType.GIVING):
            operands.append(self.parse_expression())
        
        # TO clause is required for ADD (unless direct GIVING)
        if self.match(COBOLTokenType.GIVING):
            # ADD NUM1 GIVING RESULT (rare, direct form)
            self.advance()
            giving = self.parse_expression()
            target = None
        else:
            self.consume(COBOLTokenType.TO)
            to_value = self.parse_expression()  # NUM2
            
            # Check for GIVING after TO
            giving = None
            target = None
            if self.match(COBOLTokenType.GIVING):
                self.advance()
                giving = self.parse_expression()  # RESULT
                # For GIVING: put TO value as first operand
                # ADD NUM1 TO NUM2 GIVING RESULT → operands = [NUM1, NUM2]
                operands.append(to_value)
            else:
                # No GIVING: modify in place
                target = to_value
        
        self.consume_optional_period()
        
        return COBOLArithmetic(
            operation="ADD",
            operands=operands,
            target=target,
            giving=giving,
            line=token.line,
            column=token.column
        )

    def parse_subtract(self, token: Token) -> COBOLArithmetic:
        self.consume(COBOLTokenType.SUBTRACT)
        
        operands = [self.parse_expression()]  # NUM1 (subtrahend)
        
        while not self.match(COBOLTokenType.FROM):
            operands.append(self.parse_expression())
        
        # FROM clause is required for SUBTRACT
        self.consume(COBOLTokenType.FROM)
        from_value = self.parse_expression()  # NUM2 (minuend)
        
        # Check for optional GIVING
        giving = None
        target = None
        if self.match(COBOLTokenType.GIVING):
            self.advance()
            giving = self.parse_expression()  # RESULT
            # For GIVING: put FROM value as first operand
            # operands becomes [NUM2, NUM1] for NUM2 - NUM1
            operands.insert(0, from_value)
        else:
            # No GIVING: modify in place
            target = from_value
        
        self.consume_optional_period()
        
        return COBOLArithmetic(
            operation="SUBTRACT",
            operands=operands,
            target=target,
            giving=giving,
            line=token.line,
            column=token.column
        )
    
    def parse_multiply(self, token: Token) -> COBOLArithmetic:
        self.consume(COBOLTokenType.MULTIPLY)
        
        operand1 = self.parse_expression()
        self.consume(COBOLTokenType.BY)
        operand2 = self.parse_expression()
        
        giving = None
        if self.match(COBOLTokenType.GIVING):
            self.advance()
            giving = self.parse_expression()
        
        self.consume_optional_period()
        
        return COBOLArithmetic(
            operation="MULTIPLY",
            operands=[operand1, operand2],
            target=None,
            giving=giving,
            line=token.line,
            column=token.column
        )
    
    def parse_divide(self, token: Token) -> COBOLArithmetic:
        self.consume(COBOLTokenType.DIVIDE)
        
        operand1 = self.parse_expression()
        
        if self.match(COBOLTokenType.BY):
            self.advance()
            operand2 = self.parse_expression()
        elif self.match(COBOLTokenType.INTO):
            self.advance()
            operand2 = self.parse_expression()
        else:
            self.error("Expected BY or INTO after DIVIDE")
        
        giving = None
        target = None
        if self.match(COBOLTokenType.GIVING):
            self.advance()
            giving = self.parse_expression()
        else:
            target = operand2
        
        self.consume_optional_period()
        
        return COBOLArithmetic(
            operation="DIVIDE",
            operands=[operand1, operand2],
            target=target,
            giving=giving,
            line=token.line,
            column=token.column
        )

    def parse_string_statement(self, token: Token) -> COBOLStringConcat:
        """
        Parse STRING statement
        Syntax: STRING field1 DELIMITED BY SIZE field2 DELIMITED BY delim INTO target
        """
        self.consume(COBOLTokenType.STRING)
        
        source_fields = []
        delimiters = []
        
        # Parse source fields and their delimiters
        while not self.match(COBOLTokenType.INTO):
            # Parse a source field (string literal or identifier)
            if self.match(COBOLTokenType.STRING_LITERAL, COBOLTokenType.IDENTIFIER):
                source_fields.append(self.parse_expression())
            else:
                self.error("Expected source field in STRING statement")
            
            # Parse DELIMITED BY clause
            if self.match(COBOLTokenType.DELIMITED):
                self.consume(COBOLTokenType.DELIMITED)
                self.consume(COBOLTokenType.BY)
                
                if self.match(COBOLTokenType.SIZE):
                    self.consume(COBOLTokenType.SIZE)
                    delimiters.append(None)  # None means SIZE (use entire field)
                else:
                    # Parse delimiter expression (string literal or identifier)
                    delimiters.append(self.parse_expression())
            else:
                # No DELIMITED clause for this field - use SIZE by default
                delimiters.append(None)
        
        # Parse INTO target
        self.consume(COBOLTokenType.INTO)
        if not self.match(COBOLTokenType.IDENTIFIER):
            self.error("Expected target identifier after INTO")
        target = self.consume(COBOLTokenType.IDENTIFIER).value
        
        # Optional WITH POINTER clause
        pointer = None
        if self.match(COBOLTokenType.WITH):
            self.consume(COBOLTokenType.WITH)
            self.consume(COBOLTokenType.POINTER)
            if self.match(COBOLTokenType.IDENTIFIER):
                pointer = self.consume(COBOLTokenType.IDENTIFIER).value
        
        self.consume_optional_period()
        
        return COBOLStringConcat(
            source_fields=source_fields,
            delimiters=delimiters,
            target=target,
            pointer=pointer,
            line=token.line,
            column=token.column
        )
    
    def parse_unstring_statement(self, token: Token) -> COBOLUnstring:
        """
        Parse UNSTRING statement
        Syntax: UNSTRING source DELIMITED BY delimiter INTO field1 field2 [TALLYING IN counter]
        """
        self.consume(COBOLTokenType.UNSTRING)
        
        # Parse source field
        if not self.match(COBOLTokenType.IDENTIFIER, COBOLTokenType.STRING_LITERAL):
            self.error("Expected source field in UNSTRING statement")
        source = self.parse_expression()
        
        # Parse DELIMITED BY clause
        delimiter = None
        if self.match(COBOLTokenType.DELIMITED):
            self.consume(COBOLTokenType.DELIMITED)
            self.consume(COBOLTokenType.BY)
            delimiter = self.parse_expression()
        
        # Parse INTO clause
        self.consume(COBOLTokenType.INTO)
        targets = []
        while self.match(COBOLTokenType.IDENTIFIER):
            targets.append(self.consume(COBOLTokenType.IDENTIFIER).value)
            # Stop if we hit TALLYING or period
            if self.match(COBOLTokenType.TALLYING, COBOLTokenType.PERIOD):
                break
        
        if not targets:
            self.error("Expected at least one target field in UNSTRING INTO clause")
        
        # Optional TALLYING IN clause
        counter = None
        if self.match(COBOLTokenType.TALLYING):
            self.consume(COBOLTokenType.TALLYING)
            self.consume(COBOLTokenType.IN)
            if self.match(COBOLTokenType.IDENTIFIER):
                counter = self.consume(COBOLTokenType.IDENTIFIER).value
        
        self.consume_optional_period()
        
        return COBOLUnstring(
            source=source,
            delimiter=delimiter,
            targets=targets,
            count=counter,
            line=token.line,
            column=token.column
        )

    def parse_inspect(self, token: Token) -> COBOLInspect:
        """
        Parse INSPECT statement
        INSPECT field REPLACING ALL "x" BY "y"
        INSPECT field REPLACING FIRST "x" BY "y"
        INSPECT field TALLYING counter FOR ALL "x"
        """
        # INSPECT
        target = self.consume(COBOLTokenType.IDENTIFIER).value
        
        # REPLACING or TALLYING
        operation_token = self.current_token()
        if operation_token.type == COBOLTokenType.REPLACING:
            self.consume(COBOLTokenType.REPLACING)
            
            # Check for ALL, FIRST, LEADING as identifiers (not keywords!)
            scope = 'ALL'  # Default
            if self.match(COBOLTokenType.IDENTIFIER):
                scope_value = self.current_token().value.upper()
                if scope_value in ['ALL', 'FIRST', 'LEADING']:
                    self.advance()
                    scope = scope_value
                # else: it's the pattern expression, don't consume
            elif self.match(COBOLTokenType.ALL):
                # Handle case where ALL is a keyword (for backward compatibility)
                self.consume(COBOLTokenType.ALL)
                scope = 'ALL'
            
            # Pattern
            pattern = self.parse_expression()
            
            # BY
            self.consume(COBOLTokenType.BY)
            
            # Replacement
            replacement = self.parse_expression()
            
            return COBOLInspect(
                token.line, token.column,
                target=target,
                operation='REPLACING',
                pattern=pattern,
                replacement=replacement,
                scope=scope
            )
        
        elif operation_token.type == COBOLTokenType.TALLYING:
            self.consume(COBOLTokenType.TALLYING)
            counter = self.consume(COBOLTokenType.IDENTIFIER).value
            
            # FOR
            self.consume(COBOLTokenType.FOR)
            
            # Check for ALL as identifier
            scope = 'ALL'
            if self.match(COBOLTokenType.IDENTIFIER):
                scope_value = self.current_token().value.upper()
                if scope_value == 'ALL':
                    self.advance()
            elif self.match(COBOLTokenType.ALL):
                    # Handle case where ALL is a keyword
                    self.consume(COBOLTokenType.ALL)
            
            # Pattern
            pattern = self.parse_expression()
            
            return COBOLInspect(
                token.line, token.column,
                target=target,
                operation='TALLYING',
                pattern=pattern,
                counter=counter,
                scope=scope
            )
        else:
            self.error(f"Expected REPLACING or TALLYING in INSPECT, got {operation_token.type.name}")

    def parse_inspect_statement(self, token: Token) -> COBOLInspect:
        """Wrapper to keep existing call structure"""
        self.consume(COBOLTokenType.INSPECT)
        result = self.parse_inspect(token)
        self.consume_optional_period()
        return result

    def parse_if(self, token: Token) -> COBOLIf:
        self.consume(COBOLTokenType.IF)
        condition = self.parse_condition()
        
        self.push_context(ParseContext.INSIDE_IF)
        
        # Parse THEN block
        then_statements = []
        # ✅ ADD EOF CHECK HERE:
        while not self.match(COBOLTokenType.ELSE, COBOLTokenType.END_IF, COBOLTokenType.EOF):
            stmt = self.parse_statement()
            if stmt:
                then_statements.append(stmt)
        
        # Parse ELSE block (if present)
        else_statements = None
        if self.match(COBOLTokenType.ELSE):
            self.advance()
            else_statements = []
            # ✅ ADD EOF CHECK HERE TOO:
            while not self.match(COBOLTokenType.END_IF, COBOLTokenType.EOF):
                stmt = self.parse_statement()
                if stmt:
                    else_statements.append(stmt)
        
        # ✅ ADD EOF CHECK BEFORE CONSUMING END-IF:
        if self.match(COBOLTokenType.END_IF):
            self.consume(COBOLTokenType.END_IF)
        elif self.match(COBOLTokenType.EOF):
            # Graceful error: IF without END-IF
            self.pop_context()
            raise ParserError("IF statement missing END-IF before end of file", 
                             token.line, token.column)
        
        self.pop_context()
        
        if self.requires_period() and self.match(COBOLTokenType.PERIOD):
            self.consume(COBOLTokenType.PERIOD)
        
        return COBOLIf(
            condition=condition,
            then_statements=then_statements,
            else_statements=else_statements,
            line=token.line,
            column=token.column
        )
    
    def parse_perform(self, token: Token) -> COBOLASTNode:
        self.consume(COBOLTokenType.PERFORM)
        
        # NEW: Check for PERFORM UNTIL or PERFORM paragraph UNTIL
        if self.match(COBOLTokenType.UNTIL):
            return self.parse_perform_until(token, None)
        
        # Check if paragraph name followed by UNTIL
        if self.match(COBOLTokenType.IDENTIFIER):
            peek_next = self.peek_token(1)
            if peek_next and peek_next.type == COBOLTokenType.UNTIL:
                para_name = self.consume(COBOLTokenType.IDENTIFIER).value
                return self.parse_perform_until(token, para_name)
        
        if self.match(COBOLTokenType.VARYING):
            return self.parse_perform_varying(token)
        
        elif self.match(COBOLTokenType.NUMBER_LITERAL, COBOLTokenType.IDENTIFIER) and self.peek_token(1).type == COBOLTokenType.TIMES:
            # Inline PERFORM N TIMES ... END-PERFORM
            times_expr = self.parse_expression()
            self.consume(COBOLTokenType.TIMES)
            
            self.push_context(ParseContext.INSIDE_LOOP)
            statements = []
            while not self.match(COBOLTokenType.END_PERFORM, COBOLTokenType.EOF):
                stmt = self.parse_statement()
                if stmt:
                    statements.append(stmt)
            
            if self.match(COBOLTokenType.END_PERFORM):
                self.consume(COBOLTokenType.END_PERFORM)
            elif self.match(COBOLTokenType.EOF):
                self.pop_context()
                raise ParserError("PERFORM TIMES missing END-PERFORM", 
                                token.line, token.column)
            
            self.pop_context()
            if self.requires_period():
                self.consume(COBOLTokenType.PERIOD)
            
            return COBOLPerformTimes(
                times_expr=times_expr,
                paragraph_name=None,
                statements=statements,
                line=token.line,
                column=token.column
            )

        elif self.match(COBOLTokenType.IDENTIFIER):
            para_name = self.consume(COBOLTokenType.IDENTIFIER).value
            print(f"DEBUG parse_perform: Consumed paragraph name '{para_name}'")
            print(f"DEBUG parse_perform: Current token = {self.current_token()}")
            print(f"DEBUG parse_perform: Peek(1) = {self.peek_token(1)}")
            
            # Check for N TIMES
            if self.match(COBOLTokenType.NUMBER_LITERAL, COBOLTokenType.IDENTIFIER):
                peek_next = self.peek_token(1)
                print(f"DEBUG parse_perform: Matched NUMBER/ID, peek(1) = {peek_next}")
                if peek_next and peek_next.type == COBOLTokenType.TIMES:
                    print(f"DEBUG parse_perform: Creating COBOLPerformTimes with paragraph")
                    times_expr = self.parse_expression()
                    self.consume(COBOLTokenType.TIMES)
                    if self.requires_period():
                        self.consume(COBOLTokenType.PERIOD)
                    return COBOLPerformTimes(
                        times_expr=times_expr,
                        paragraph_name=para_name,
                        statements=None,
                        line=token.line,
                        column=token.column
                    )
            until_condition = None
            if self.match(COBOLTokenType.UNTIL):
                self.advance()
                until_condition = self.parse_condition()
            if self.requires_period(): self.consume(COBOLTokenType.PERIOD)
            return COBOLPerformParagraph(
                paragraph_name=para_name,
                until_condition=until_condition,
                line=token.line,
                column=token.column
            )
        else:
            self.error("Invalid PERFORM statement syntax")
    
    def parse_perform_until(self, token: Token, paragraph_name: Optional[str]) -> COBOLPerformUntil:
        """Parse PERFORM [paragraph] UNTIL condition [statements END-PERFORM]"""
        self.consume(COBOLTokenType.UNTIL)
        condition = self.parse_condition()
        
        statements = []
        if paragraph_name is None:
            # Inline statements
            self.push_context(ParseContext.INSIDE_LOOP)
            while not self.match(COBOLTokenType.END_PERFORM, COBOLTokenType.EOF):
                stmt = self.parse_statement()
                if stmt:
                    statements.append(stmt)
            
            if self.match(COBOLTokenType.END_PERFORM):
                self.consume(COBOLTokenType.END_PERFORM)
            elif self.match(COBOLTokenType.EOF):
                self.pop_context()
                raise ParserError("PERFORM UNTIL missing END-PERFORM", 
                                token.line, token.column)
            self.pop_context()
        
        if self.requires_period() and self.match(COBOLTokenType.PERIOD):
            self.consume(COBOLTokenType.PERIOD)
        
        return COBOLPerformUntil(
            condition=condition,
            statements=statements,
            paragraph_name=paragraph_name,
            line=token.line,
            column=token.column
        )
    def parse_perform_varying(self, token: Token) -> COBOLPerformVarying:
        self.consume(COBOLTokenType.VARYING)
        
        variable = self.consume(COBOLTokenType.IDENTIFIER).value
        
        self.consume(COBOLTokenType.FROM)
        from_expr = self.parse_expression()
        
        self.consume(COBOLTokenType.BY)
        by_expr = self.parse_expression()
        
        self.consume(COBOLTokenType.UNTIL)
        until_condition = self.parse_condition()
        
        self.push_context(ParseContext.INSIDE_LOOP)
        
        statements = []
        # ✅ FIX: Add EOF check here
        while not self.match(COBOLTokenType.END_PERFORM, COBOLTokenType.EOF):
            stmt = self.parse_statement()
            if stmt:
                statements.append(stmt)
        
        # ✅ FIX: Check for EOF before consuming
        if self.match(COBOLTokenType.END_PERFORM):
            self.consume(COBOLTokenType.END_PERFORM)
        elif self.match(COBOLTokenType.EOF):
            self.pop_context()
            raise ParserError("PERFORM VARYING missing END-PERFORM before end of file",
                             token.line, token.column)
        
        self.pop_context()
        
        if self.requires_period() and self.match(COBOLTokenType.PERIOD):
            self.consume(COBOLTokenType.PERIOD)
        
        return COBOLPerformVarying(
            variable=variable,
            from_expr=from_expr,
            by_expr=by_expr,
            until_condition=until_condition,
            statements=statements,
            line=token.line,
            column=token.column
        )
    
    def parse_call(self, token: Token) -> COBOLCall:
        self.consume(COBOLTokenType.CALL)
        
        # String literal or identifier
        if self.match(COBOLTokenType.STRING_LITERAL):
            prog_name = self.consume(COBOLTokenType.STRING_LITERAL).value.strip('"')
        else:
            prog_name = self.consume(COBOLTokenType.IDENTIFIER).value
        
        # Optional USING clause
        parameters = []
        using_params = []
        if self.match(COBOLTokenType.USING):
            self.consume(COBOLTokenType.USING)
            # Parse parameter list
            while self.match(COBOLTokenType.IDENTIFIER):
                param_name = self.consume(COBOLTokenType.IDENTIFIER).value
                using_params.append(param_name)
                # Check for comma-separated list
                if not self.match(COBOLTokenType.IDENTIFIER):
                    break
        
        if self.requires_period():
            self.consume(COBOLTokenType.PERIOD)
        
        return COBOLCall(program_name=prog_name, parameters=parameters,
                         using_params=using_params if using_params else None,
                         line=token.line, column=token.column)

    def parse_evaluate(self, token: Token) -> COBOLEvaluate:
        self.consume(COBOLTokenType.EVALUATE)
        
        subject = self.parse_expression()
        
        self.push_context(ParseContext.INSIDE_IF)
        
        when_clauses = []
        while self.match(COBOLTokenType.WHEN):
            when_clauses.append(self.parse_when_clause())
        
        if not when_clauses:
            self.error("EVALUATE must have at least one WHEN clause")
        
        self.consume(COBOLTokenType.END_EVALUATE)
        
        self.pop_context()
        
        if self.requires_period() and self.match(COBOLTokenType.PERIOD):
            self.consume(COBOLTokenType.PERIOD)
        
        return COBOLEvaluate(
            subject=subject,
            when_clauses=when_clauses,
            line=token.line,
            column=token.column
        )
    
    def parse_when_clause(self) -> COBOLWhenClause:
        token = self.current_token()
        self.consume(COBOLTokenType.WHEN)
        
        value = None
        if self.match(COBOLTokenType.OTHER):
            self.advance()
            value = None
        else:
            value = self.parse_expression()
        
        statements = []
        while not self.match(COBOLTokenType.WHEN, COBOLTokenType.END_EVALUATE):
            stmt = self.parse_statement()
            if stmt:
                statements.append(stmt)
        
        return COBOLWhenClause(
            value=value,
            statements=statements,
            line=token.line,
            column=token.column
        )
    
    def parse_stop_run(self, token: Token) -> COBOLStopRun:
        self.consume(COBOLTokenType.STOP)
        self.consume(COBOLTokenType.RUN)
        
        if self.requires_period():
            self.consume(COBOLTokenType.PERIOD)
        
        return COBOLStopRun(line=token.line, column=token.column)
    
    def parse_condition(self) -> COBOLASTNode:
        return self.parse_or_expression()
    
    def parse_or_expression(self) -> COBOLASTNode:
        left = self.parse_and_expression()
        
        while self.match(COBOLTokenType.OR):
            op_token = self.current_token()
            self.advance()
            right = self.parse_and_expression()
            left = COBOLBinaryOp(
                operator='OR',
                left=left,
                right=right,
                line=op_token.line,
                column=op_token.column
            )
        
        return left
    
    def parse_and_expression(self) -> COBOLASTNode:
        left = self.parse_not_expression()
        
        while self.match(COBOLTokenType.AND):
            op_token = self.current_token()
            self.advance()
            right = self.parse_not_expression()
            left = COBOLBinaryOp(
                operator='AND',
                left=left,
                right=right,
                line=op_token.line,
                column=op_token.column
            )
        
        return left
    
    def parse_not_expression(self) -> COBOLASTNode:
        if self.match(COBOLTokenType.NOT):
            op_token = self.current_token()
            self.advance()
            operand = self.parse_comparison()
            return COBOLUnaryOp(
                operator='NOT',
                operand=operand,
                line=op_token.line,
                column=op_token.column
            )
        
        return self.parse_comparison()
    
    def parse_comparison(self) -> COBOLASTNode:
        left = self.parse_arithmetic_expression()
        
        if self.match(COBOLTokenType.GTE_SIGN, COBOLTokenType.GREATER):
            op_token = self.current_token()
            self.advance()
            if self.match(COBOLTokenType.EQUAL, COBOLTokenType.EQUALS, COBOLTokenType.THAN): # Handles "GREATER THAN OR EQUAL TO"
                self.advance()
                if self.match(COBOLTokenType.OR): self.advance()
                if self.match(COBOLTokenType.EQUAL): self.advance()
                if self.match(COBOLTokenType.TO): self.advance()

            right = self.parse_arithmetic_expression()
            return COBOLBinaryOp(
                operator='>=',
                left=left,
                right=right,
                line=op_token.line,
                column=op_token.column
            )
        elif self.match(COBOLTokenType.LTE_SIGN, COBOLTokenType.LESS):
            op_token = self.current_token()
            self.advance()
            if self.match(COBOLTokenType.EQUAL, COBOLTokenType.EQUALS, COBOLTokenType.THAN): # Handles "LESS THAN OR EQUAL TO"
                self.advance()
                if self.match(COBOLTokenType.OR): self.advance()
                if self.match(COBOLTokenType.EQUAL): self.advance()
                if self.match(COBOLTokenType.TO): self.advance()

            right = self.parse_arithmetic_expression()
            return COBOLBinaryOp(
                operator='<=',
                left=left,
                right=right,
                line=op_token.line,
                column=op_token.column
            )
        elif self.match(COBOLTokenType.GT_SIGN):
            op_token = self.current_token()
            self.advance()
            right = self.parse_arithmetic_expression()
            return COBOLBinaryOp(
                operator='>',
                left=left,
                right=right,
                line=op_token.line,
                column=op_token.column
            )
        elif self.match(COBOLTokenType.LT_SIGN):
            op_token = self.current_token()
            self.advance()
            right = self.parse_arithmetic_expression()
            return COBOLBinaryOp(
                operator='<',
                left=left,
                right=right,
                line=op_token.line,
                column=op_token.column
            )
        elif self.match(COBOLTokenType.EQUALS_SIGN, COBOLTokenType.EQUAL, COBOLTokenType.EQUALS):
            op_token = self.current_token()
            self.advance()
            right = self.parse_arithmetic_expression()
            return COBOLBinaryOp(
                operator='=',
                left=left,
                right=right,
                line=op_token.line,
                column=op_token.column
            )
        elif self.match(COBOLTokenType.NOT):
            self.advance()
            if self.match(COBOLTokenType.EQUALS_SIGN, COBOLTokenType.EQUAL, COBOLTokenType.EQUALS):
                op_token = self.current_token()
                self.advance()
                right = self.parse_arithmetic_expression()
                return COBOLBinaryOp(
                    operator='!=',
                    left=left,
                    right=right,
                    line=op_token.line,
                    column=op_token.column
                )
        
        return left
    
    def parse_arithmetic_expression(self) -> COBOLASTNode:
        return self.parse_additive()
    
    def parse_additive(self) -> COBOLASTNode:
        left = self.parse_multiplicative()
        
        while self.match(COBOLTokenType.PLUS, COBOLTokenType.MINUS):
            op = '+' if self.current_token().type == COBOLTokenType.PLUS else '-'
            token = self.current_token()
            self.advance()
            right = self.parse_multiplicative()
            left = COBOLBinaryOp(operator=op, left=left, right=right, line=token.line, column=token.column)
        
        return left
    
    def parse_multiplicative(self) -> COBOLASTNode:
        left = self.parse_primary()
        
        while self.match(COBOLTokenType.ASTERISK, COBOLTokenType.SLASH):
            op = '*' if self.current_token().type == COBOLTokenType.ASTERISK else '/'
            token = self.current_token()
            self.advance()
            right = self.parse_primary()
            left = COBOLBinaryOp(operator=op, left=left, right=right, line=token.line, column=token.column)
        
        return left
    
    def parse_primary(self) -> COBOLASTNode:
        token = self.current_token()
        
        print(f"DEBUG parse_primary: token={token.type.name} value={token.value}")
        
        if self.match(COBOLTokenType.NUMBER_LITERAL):
            value = token.value
            self.advance()
            return COBOLNumberLiteral(value=value, line=token.line, column=token.column)
        
        elif self.match(COBOLTokenType.STRING_LITERAL):
            value = token.value
            self.advance()
            return COBOLStringLiteral(value=value, line=token.line, column=token.column)
        
        elif self.match(COBOLTokenType.FUNCTION):
            return self.parse_function_call()

        elif self.match(COBOLTokenType.IDENTIFIER):
            name = token.value
            self.advance()
            
            # NEW: Check for array subscript
            if self.match(COBOLTokenType.LPAREN):
                self.advance() # consume (
                index_expr = self.parse_expression()
                self.consume(COBOLTokenType.RPAREN)
                return COBOLArraySubscript(
                    array_name=name,
                    index=index_expr,
                    line=token.line,
                    column=token.column
                )
            
            # Regular identifier
            return COBOLIdentifier(name=name, line=token.line, column=token.column)
        
        # Accept USAGE keywords as identifiers (variable names)
        elif self.match(
            COBOLTokenType.USAGE,
            COBOLTokenType.COMP, COBOLTokenType.COMP_1, COBOLTokenType.COMP_2, COBOLTokenType.COMP_3,
            COBOLTokenType.COMPUTATIONAL, COBOLTokenType.COMPUTATIONAL_3,
            COBOLTokenType.BINARY, COBOLTokenType.PACKED_DECIMAL, COBOLTokenType.DISPLAY
        ):
            name = token.value
            self.advance()
            return COBOLIdentifier(name=name, line=token.line, column=token.column)
        
        elif self.match(COBOLTokenType.LPAREN):
            self.advance()
            expr = self.parse_arithmetic_expression()
            self.consume(COBOLTokenType.RPAREN)
            return expr
        
        elif self.match(COBOLTokenType.MINUS):
            self.advance()
            operand = self.parse_primary()
            return COBOLUnaryOp(operator='-', operand=operand, line=token.line, column=token.column)
        
        else:
            self.error(f"Unexpected token in expression: {token.type.name}")
    
    def parse_function_call(self) -> COBOLFunctionCall:
        token = self.current_token()
        self.advance() # consume FUNCTION
        func_name_token = self.current_token()
        func_name = func_name_token.value
        self.advance()
        self.consume(COBOLTokenType.LPAREN)
        arg = self.parse_expression()
        self.consume(COBOLTokenType.RPAREN)
        return COBOLFunctionCall(
            function_name=func_name,
            arguments=[arg],
            line=token.line,
            column=token.column
        )
    def parse_expression(self) -> COBOLASTNode:
        return self.parse_primary()
    
    
    def parse_string(self) -> COBOLStringConcat:
        """Parse STRING statement
        STRING field1 field2 ... DELIMITED BY SIZE/literal INTO target
        """
        self.consume(COBOLTokenType.STRING)
        
        source_fields = []
        delimiters = []
        
        # Parse source fields and delimiters
        while not self.match(COBOLTokenType.INTO):
            # Parse source field
            field = self.parse_expression()
            source_fields.append(field)
            
            # Check for DELIMITED BY
            if self.match(COBOLTokenType.DELIMITED):
                self.advance()  # DELIMITED
                self.consume(COBOLTokenType.BY)
                
                if self.match(COBOLTokenType.SIZE):
                    self.advance()
                    delimiters.append(None)  # SIZE means no delimiter
                else:
                    delimiter = self.parse_expression()
                    delimiters.append(delimiter)
            else:
                delimiters.append(None)
        
        # Parse INTO clause
        self.consume(COBOLTokenType.INTO)
        target = self.consume(COBOLTokenType.IDENTIFIER).value
        
        # Optional WITH POINTER
        pointer = None
        if self.match(COBOLTokenType.WITH):
            self.advance()
            self.consume(COBOLTokenType.POINTER)
            pointer = self.consume(COBOLTokenType.IDENTIFIER).value
        
        return COBOLStringConcat(
            source_fields=source_fields,
            delimiters=delimiters,
            target=target,
            pointer=pointer,
            line=source_fields[0].line,
            column=source_fields[0].column
        )