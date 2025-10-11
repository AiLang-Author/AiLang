#!/usr/bin/env python3

# Copyright (c) 2025 Sean Collins, 2 Paws Machine and Engineering. All rights reserved.
#
# Licensed under the Sean Collins Software License (SCSL). See the LICENSE file in the root directory of this project
# for the full terms and conditions, including restrictions on forking, corporate use, and permissions for private/teaching purposes.

"""
COBOL Parser - Complete Version with ACCEPT
"""

from typing import List, Optional, Dict
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
    metadata_lines: List[str] = field(default_factory=list)  # ✅ NEW: Just store as strings


@dataclass
class COBOLDataDivision(COBOLASTNode):
    working_storage: List['COBOLVariableDecl']
    file_section: Optional[List['COBOLFileDescriptor']] = None      # ✅ NEW
    select_statements: Optional[List['COBOLSelectStatement']] = None # ✅ NEW
    linkage_section: Optional[List['COBOLVariableDecl']] = None

@dataclass
class COBOLSelectStatement(COBOLASTNode):
    """SELECT statement from FILE-CONTROL - captures ALL clauses"""
    file_name: str
    assign_to: str
    is_optional: bool = False
    organization: Optional[str] = None      # SEQUENTIAL, INDEXED, RELATIVE, LINE SEQUENTIAL
    access_mode: Optional[str] = None       # SEQUENTIAL, RANDOM, DYNAMIC
    record_key: Optional[str] = None        # For INDEXED files
    alternate_keys: List[str] = field(default_factory=list)
    file_status: Optional[str] = None       # Status variable
    reserve_areas: Optional[int] = None     # RESERVE n AREAS
    padding_character: Optional[str] = None # PADDING CHARACTER


@dataclass
class COBOLFileDescriptor(COBOLASTNode):
    """FD entry - Complete file descriptor with ALL clauses"""
    file_name: str
    
    # Record description
    records: List['COBOLVariableDecl'] = field(default_factory=list)
    
    # FD clauses (all optional)
    block_contains: Optional[int] = None
    block_contains_to: Optional[int] = None  # BLOCK CONTAINS min TO max
    record_contains: Optional[int] = None
    record_contains_to: Optional[int] = None # RECORD CONTAINS min TO max
    record_varying: Optional[dict] = None    # RECORD IS VARYING details
    label_records: Optional[str] = None      # STANDARD, OMITTED
    value_of: dict[str, str] = field(default_factory=dict)  # VALUE OF clauses
    data_records: List[str] = field(default_factory=list)
    linage: Optional[dict] = None            # LINAGE clause for printed reports
    code_set: Optional[str] = None           # CODE-SET clause

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
    children: List['COBOLVariableDecl'] = field(default_factory=list)  # NEW!
    redefines_target: Optional[str] = None  # NEW: Name of variable being redefined
    # NEW: Variable-length table fields
    occurs_min: Optional[int] = None
    occurs_max: Optional[int] = None
    depending_on: Optional[str] = None
    is_external: bool = False  # NEW: EXTERNAL clause for shared data
    is_global: bool = False    # NEW: GLOBAL clause for nested programs

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
class COBOLRead(COBOLASTNode):
    """READ statement"""
    filename: str
    into_variable: Optional[str] = None
    at_end_statements: Optional[List[COBOLASTNode]] = None
    not_at_end_statements: Optional[List[COBOLASTNode]] = None

@dataclass
class COBOLMove(COBOLASTNode):
    source: COBOLASTNode
    targets: List[COBOLASTNode]
    is_corresponding: bool = False

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
    paragraph_name: Optional[str] = None
    statements: Optional[List[COBOLASTNode]] = None

@dataclass
class COBOLCall(COBOLASTNode):
    program_name: str  # "NESTED-CHILD" or identifier
    parameters: List[str] = field(default_factory=list) # For USING clause
    overflow_statements: Optional[List[COBOLASTNode]] = None

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
    is_program: bool = False  # True for EXIT PROGRAM, False for EXIT

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
    indices: List[COBOLASTNode]  # Can be literal or variable

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
    UNSTRING source DELIMITED BY [ALL] delimiter [OR [ALL] delimiter...] 
             INTO field1 field2 [TALLYING IN counter]
    """
    source: COBOLASTNode
    delimiters: List[COBOLASTNode]  # ✅ CHANGED: Single → List
    delimiter_all_flags: List[bool]  # ✅ NEW: ALL flag for each delimiter
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
    def __init__(self, tokens: List[Token], debug: bool = False):
        self.tokens = tokens
        self.pos = 0
        self.debug = debug
        self._current_metadata = []
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

    def skip_insignificant_tokens(self):
        """Skip newlines and comments"""
        while self.match(COBOLTokenType.NEWLINE, COBOLTokenType.COMMENT):
            self.advance()

    def parse_all_programs(self) -> COBOLCompilationUnit:
        """Parse ALL programs from the token stream - handle EOF gracefully"""
        programs = []
        
        while not self.match(COBOLTokenType.EOF):
            # Skip comments and whitespace
            if self.match(COBOLTokenType.COMMENT, COBOLTokenType.NEWLINE):
                self.advance()
                continue
            
            # Look for IDENTIFICATION DIVISION (start of a program)
            if self.match(COBOLTokenType.IDENTIFICATION):
                try:
                    program = self.parse_single_program()
                    programs.append(program)
                except Exception as e:
                    # If parsing fails partway through, that's OK for truncated files
                    print(f"Warning: Failed to complete parsing program: {e}")
                    break
            else:
                # Unexpected token - might be legacy format or truncated
                # Try to skip to next IDENTIFICATION or EOF
                if not self.match(COBOLTokenType.EOF):
                    self.advance()
        
        if not programs:
            raise ParserError("No COBOL programs found in source file", 1, 1)
        
        return COBOLCompilationUnit(programs=programs, line=1, column=1)
    
    def is_data_only_program(program: COBOLProgram) -> bool:
        """Check if this is a data-only program (copybook)"""
        return (program.data_division is not None and 
                program.procedure_division is None)


    def validate_program(program: COBOLProgram) -> list[str]:
        """Validate program structure and return warnings"""
        warnings = []
        
        if is_data_only_program(program):
            warnings.append(f"Program {program.program_id} has no PROCEDURE DIVISION (copybook/data definition)")
        
        if program.data_division is None and program.procedure_division is None:
            warnings.append(f"Program {program.program_id} has no DATA or PROCEDURE divisions")
        
        return warnings    

    def parse_single_program(self) -> COBOLProgram:
        """Parse ONE COBOL program - all divisions now optional except IDENTIFICATION"""
        token = self.current_token()
        
        # Clear metadata for the new program
        self._current_metadata = []

        # Parse IDENTIFICATION DIVISION
        program_id = self.parse_identification_division()
        
        # Parse ENVIRONMENT DIVISION (optional)
        select_statements = []
        if self.current_token() and self.match(COBOLTokenType.ENVIRONMENT):
            select_statements = self.parse_environment_division()
        
        # Parse DATA DIVISION (optional)
        data_division = None
        if self.current_token() and self.match(COBOLTokenType.DATA):
            data_division = self.parse_data_division()
            # Attach SELECT statements to data division
            if data_division and select_statements:
                data_division.select_statements = select_statements
    
        # ✅ Parse PROCEDURE DIVISION (NOW OPTIONAL)
        procedure_division = None
        if self.current_token() and self.match(COBOLTokenType.PROCEDURE):
            procedure_division = self.parse_procedure_division()
        elif self.match(COBOLTokenType.EOF):
            # EOF reached before PROCEDURE - that's OK for copybooks/truncated files
            procedure_division = None
    
        # Parse nested programs (only if we haven't hit EOF)
        contained_programs = []
# DISABLED:         while not self.match(COBOLTokenType.EOF) and self.match(COBOLTokenType.IDENTIFICATION):
# DISABLED:             nested = self.parse_single_program()
# DISABLED:             nested.is_nested = True
# DISABLED:             contained_programs.append(nested)
# DISABLED:     
        # ✅ Parse END PROGRAM if present (also optional now)
        if not self.match(COBOLTokenType.EOF):
            self.parse_end_program(program_id)
    
        return COBOLProgram(
            program_id=program_id,
            data_division=data_division,
            procedure_division=procedure_division,  # Can be None now
            contained_programs=contained_programs,
            is_nested=False,
            metadata_lines=self._current_metadata,  # ✅ Attach captured metadata
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
        
        metadata = []
        metadata_paragraphs = {
            'AUTHOR': COBOLTokenType.AUTHOR,
            'INSTALLATION': COBOLTokenType.INSTALLATION,
            'DATE-WRITTEN': COBOLTokenType.DATE_WRITTEN,
            'DATE-COMPILED': COBOLTokenType.DATE_COMPILED,
            'SECURITY': COBOLTokenType.SECURITY,
            'REMARKS': COBOLTokenType.REMARKS
        }
        
        while self.match(*metadata_paragraphs.values()):
            token = self.current_token()
            self.advance()  # consume the paragraph keyword
            if self.match(COBOLTokenType.PERIOD):
                self.advance()
            
            metadata_line = []
            while True:
                if self.match(COBOLTokenType.ENVIRONMENT, COBOLTokenType.DATA, 
                            COBOLTokenType.PROCEDURE, COBOLTokenType.IDENTIFICATION, 
                            COBOLTokenType.EOF):
                    break
                if self.match(*metadata_paragraphs.values()):
                    break
                metadata_line.append(self.current_token().value)
                self.advance()
            
            if metadata_line:
                metadata.append(" ".join(metadata_line))
        
        self._current_metadata = metadata
        return program_id
    
    def parse_environment_division(self):
        """Parse ENVIRONMENT DIVISION - don't skip, capture SELECT statements"""
        self.consume(COBOLTokenType.ENVIRONMENT)
        self.consume(COBOLTokenType.DIVISION)
        self.consume(COBOLTokenType.PERIOD)
        
        select_statements = []
        
        # Skip to INPUT-OUTPUT SECTION
        while not self.match(COBOLTokenType.EOF):
            # CONFIGURATION SECTION - skip for now but recognize it
            if self.current_token() and self.current_token().value == 'CONFIGURATION':
                self.advance()
                if self.match(COBOLTokenType.SECTION):
                    self.advance()
                if self.match(COBOLTokenType.PERIOD):
                    self.advance()
                # Skip to next section
                while not self.match(COBOLTokenType.DATA, COBOLTokenType.PROCEDURE):
                    if self.current_token() and self.current_token().value in ['INPUT-OUTPUT', 'FILE-CONTROL']:
                        break
                    self.advance()
            
            # INPUT-OUTPUT SECTION
            if self.current_token() and self.current_token().value == 'INPUT-OUTPUT':
                self.advance()
                if self.match(COBOLTokenType.SECTION):
                    self.advance()
                if self.match(COBOLTokenType.PERIOD):
                    self.advance()
            
            # FILE-CONTROL paragraph
            if self.current_token() and self.current_token().value == 'FILE-CONTROL':
                self.advance()
                if self.match(COBOLTokenType.PERIOD):
                    self.advance()
                
                # Parse SELECT statements
                while self.current_token() and self.current_token().value == 'SELECT':
                    select = self.parse_select_statement()
                    if select:
                        select_statements.append(select)
            
            # Stop if we hit DATA or PROCEDURE
            if self.match(COBOLTokenType.DATA, COBOLTokenType.PROCEDURE):
                break
            
            if self.match(COBOLTokenType.EOF):
                break
            
            self.advance()
        
        return select_statements


    def parse_select_statement(self) -> COBOLSelectStatement:
        """Parse SELECT statement with ALL clauses"""
        token = self.current_token()
        self.advance()  # SELECT
        
        # Check for OPTIONAL
        is_optional = False
        if self.current_token() and self.current_token().value == 'OPTIONAL':
            is_optional = True
            self.advance()
        
        # File name
        file_name = self.consume(COBOLTokenType.IDENTIFIER).value
        
        # ASSIGN TO clause
        assign_to = None
        if self.current_token() and self.current_token().value == 'ASSIGN':
            self.advance()
            if self.match(COBOLTokenType.TO):
                self.advance()
            if self.match(COBOLTokenType.IDENTIFIER):
                assign_to = self.current_token().value
                self.advance()
        
        # Optional clauses
        organization = None
        access_mode = None
        record_key = None
        file_status = None
        
        # Parse until we hit another SELECT or end of section
        while not self.match(COBOLTokenType.PERIOD):
            if self.match(COBOLTokenType.EOF):
                break
            
            token_val = self.current_token().value if self.current_token() else None
            
            # ORGANIZATION clause
            if token_val == 'ORGANIZATION':
                self.advance()
                if self.current_token() and self.current_token().value == 'IS':
                    self.advance()
                if self.match(COBOLTokenType.IDENTIFIER):
                    organization = self.current_token().value
                    self.advance()
            
            # ACCESS MODE clause
            elif token_val == 'ACCESS':
                self.advance()
                if self.current_token() and self.current_token().value == 'MODE':
                    self.advance()
                if self.current_token() and self.current_token().value == 'IS':
                    self.advance()
                if self.match(COBOLTokenType.IDENTIFIER):
                    access_mode = self.current_token().value
                    self.advance()
            
            # RECORD KEY clause
            elif token_val == 'RECORD':
                self.advance()
                if self.current_token() and self.current_token().value == 'KEY':
                    self.advance()
                if self.current_token() and self.current_token().value == 'IS':
                    self.advance()
                if self.match(COBOLTokenType.IDENTIFIER):
                    record_key = self.current_token().value
                    self.advance()
            
            # FILE STATUS clause
            elif token_val == 'FILE':
                self.advance()
                if self.current_token() and self.current_token().value == 'STATUS':
                    self.advance()
                if self.current_token() and self.current_token().value == 'IS':
                    self.advance()
                if self.match(COBOLTokenType.IDENTIFIER):
                    file_status = self.current_token().value
                    self.advance()
            
            else:
                self.advance()
        
        # Consume the period
        if self.match(COBOLTokenType.PERIOD):
            self.advance()
        
        return COBOLSelectStatement(
            file_name=file_name,
            assign_to=assign_to,
            is_optional=is_optional,
            organization=organization,
            access_mode=access_mode,
            record_key=record_key,
            file_status=file_status,
            line=token.line,
            column=token.column
        )


    def parse_file_section(self) -> List[COBOLFileDescriptor]:
        """Parse FILE SECTION completely - capture ALL FD clauses"""
        self.consume(COBOLTokenType.FILE_SECTION)
        self.consume(COBOLTokenType.SECTION)
        self.consume(COBOLTokenType.PERIOD)
        
        file_descriptors = []
        
        while not self.match(COBOLTokenType.WORKING_STORAGE, 
                            COBOLTokenType.LINKAGE,
                            COBOLTokenType.PROCEDURE,
                            COBOLTokenType.EOF):
            
            # Look for FD keyword
            if self.current_token() and self.current_token().value == 'FD':
                fd = self.parse_fd_entry()
                if fd:
                    file_descriptors.append(fd)
            else:
                self.advance()
        
        return file_descriptors
    
    def parse_data_division(self) -> COBOLDataDivision:
        token = self.current_token()
        self.consume(COBOLTokenType.DATA)
        self.consume(COBOLTokenType.DIVISION)
        if self.requires_period():
            self.consume(COBOLTokenType.PERIOD)
        
        file_section_descriptors = []
        if self.match(COBOLTokenType.FILE_SECTION):
            file_section_descriptors = self.parse_file_section()

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
            file_section=file_section_descriptors,
            select_statements=None, # Will be populated from ENVIRONMENT DIVISION
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
    
    def parse_fd_entry(self) -> COBOLFileDescriptor:
        """Parse FD entry with ALL clauses - production quality"""
        token = self.current_token()
        self.advance()  # FD
        
        # File name
        file_name = self.consume(COBOLTokenType.IDENTIFIER).value
        
        # Optional period after file name
        if self.match(COBOLTokenType.PERIOD):
            self.advance()
        
        # Initialize FD clause storage
        fd_data = {
            'block_contains': None,
            'block_contains_to': None,
            'record_contains': None,
            'record_contains_to': None,
            'label_records': None,
            'data_records': [],
        }
        
        # Parse FD clauses until we hit a level number (record description)
        while not self.match(COBOLTokenType.LEVEL_NUMBER):
            if self.match(COBOLTokenType.EOF):
                break
            
            token_val = self.current_token().value if self.current_token() else None
            
            # BLOCK CONTAINS clause
            if token_val == 'BLOCK':
                self.advance()
                if self.current_token() and self.current_token().value == 'CONTAINS':
                    self.advance()
                
                # Get first number
                if self.match(COBOLTokenType.NUMBER_LITERAL):
                    fd_data['block_contains'] = int(self.current_token().value)
                    self.advance()
                
                # Check for TO (range)
                if self.current_token() and self.current_token().value == 'TO':
                    self.advance()
                    if self.match(COBOLTokenType.NUMBER_LITERAL):
                        fd_data['block_contains_to'] = int(self.current_token().value)
                        self.advance()
                
                # Skip RECORDS/CHARACTERS keyword
                while not self.match(COBOLTokenType.PERIOD):
                    if self.match(COBOLTokenType.LEVEL_NUMBER):
                        break
                    self.advance()
            
            # RECORD CONTAINS clause
            elif token_val == 'RECORD':
                self.advance()
                if self.current_token() and self.current_token().value == 'CONTAINS':
                    self.advance()
                
                # Get number
                if self.match(COBOLTokenType.NUMBER_LITERAL):
                    fd_data['record_contains'] = int(self.current_token().value)
                    self.advance()
                
                # Check for TO (range)
                if self.current_token() and self.current_token().value == 'TO':
                    self.advance()
                    if self.match(COBOLTokenType.NUMBER_LITERAL):
                        fd_data['record_contains_to'] = int(self.current_token().value)
                        self.advance()
                
                # Skip rest
                while not self.match(COBOLTokenType.PERIOD):
                    if self.match(COBOLTokenType.LEVEL_NUMBER):
                        break
                    self.advance()
            
            else:
                self.advance()
            
            # Consume period
            if self.match(COBOLTokenType.PERIOD):
                self.advance()
        
        # Parse record descriptions (01-level entries)
        records = []
        while self.match(COBOLTokenType.LEVEL_NUMBER):
            level_token = self.current_token()
            if level_token.value == '01':
                record = self.parse_variable_decl()
                if record:
                    records.append(record)
            else:
                # Sub-level under previous 01 - shouldn't happen at this point
                break
        
        return COBOLFileDescriptor(
            file_name=file_name,
            records=records,
            block_contains=fd_data['block_contains'],
            block_contains_to=fd_data['block_contains_to'],
            record_contains=fd_data['record_contains'],
            record_contains_to=fd_data['record_contains_to'],
            label_records=fd_data['label_records'],
            data_records=fd_data['data_records'],
            line=token.line,
            column=token.column
        )

    def parse_variable_decl(self) -> COBOLVariableDecl:
        """
        Parse COBOL variable declaration - FINAL DEBUGGED VERSION
        """
        token = self.current_token()
        
        if self.debug:
            print(f"[PARSE_VAR] Starting at line {token.line}, token: {token.type.name} = '{token.value}'")
        
        # Parse level
        level_token = self.consume(COBOLTokenType.LEVEL_NUMBER)
        level = int(level_token.value)
        
        # Parse name
        name_token = self.current_token()
        if self.match(COBOLTokenType.IDENTIFIER):
            name = self.consume(COBOLTokenType.IDENTIFIER).value
        elif self.match(
            COBOLTokenType.USAGE, COBOLTokenType.COMP, COBOLTokenType.COMP_1,
            COBOLTokenType.COMP_2, COBOLTokenType.COMP_3, COBOLTokenType.COMPUTATIONAL,
            COBOLTokenType.COMPUTATIONAL_3, COBOLTokenType.BINARY,
            COBOLTokenType.PACKED_DECIMAL, COBOLTokenType.DISPLAY
        ):
            name = name_token.value
            self.advance()
        else:
            self.error("Expected variable name")
        
        # 🔧 NEW: Handle level 88 condition names
        # Level 88 is special: no PIC, just VALUE clause
        # Example: 88  CN1 VALUE 'A'.
        if level == 88:
            if self.debug:
                print(f"[PARSE_VAR] Level 88 condition name: {name}")
            
            # Skip whitespace
            while self.match(COBOLTokenType.NEWLINE, COBOLTokenType.COMMENT):
                self.advance()
            
            # Expect VALUE clause
            value = None
            if self.match(COBOLTokenType.VALUE):
                self.advance()
                # Optional IS keyword
                if self.current_token() and self.current_token().value and \
                   self.current_token().value.upper() == 'IS':
                    self.advance()
                
                # Get the value (could be string, number, or identifier)
                if self.match(COBOLTokenType.STRING_LITERAL, COBOLTokenType.NUMBER_LITERAL):
                    value = self.current_token().value
                    self.advance()
            
            self.consume(COBOLTokenType.PERIOD)
            
            return COBOLVariableDecl(
                level=level, name=name, pic_clause=None, value=value,
                line=token.line, column=token.column
            )
        
        if self.debug:
            print(f"[PARSE_VAR] Level {level}, name '{name}', next token: {self.current_token().type.name}")
        
        # Skip whitespace
        while self.match(COBOLTokenType.NEWLINE, COBOLTokenType.COMMENT):
            self.advance()
        
        # REDEFINES
        redefines_target = None
        if self.match(COBOLTokenType.REDEFINES):
            self.advance()
            while self.match(COBOLTokenType.NEWLINE, COBOLTokenType.COMMENT):
                self.advance()
            redefines_target = self.consume(COBOLTokenType.IDENTIFIER).value
            while self.match(COBOLTokenType.NEWLINE, COBOLTokenType.COMMENT):
                self.advance()
        
        # 🔧 NEW: Handle EXTERNAL and GLOBAL clauses
        # These can appear on group items (01 level) before children
        # Example: 01  DATA-NAME IS EXTERNAL.
        #          01  DATA-NAME IS GLOBAL.
        is_external = False
        is_global = False
        
        if self.match(COBOLTokenType.IDENTIFIER):
            peek_val = self.current_token().value.upper()
            
            # Check for IS EXTERNAL or just EXTERNAL
            if peek_val == 'IS':
                self.advance()  # consume IS
                while self.match(COBOLTokenType.NEWLINE, COBOLTokenType.COMMENT):
                    self.advance()
                
                if self.match(COBOLTokenType.IDENTIFIER):
                    next_val = self.current_token().value.upper()
                    if next_val == 'EXTERNAL':
                        is_external = True
                        self.advance()
                    elif next_val == 'GLOBAL':
                        is_global = True
                        self.advance()
            
            # Check for EXTERNAL or GLOBAL without IS
            elif peek_val == 'EXTERNAL':
                is_external = True
                self.advance()
            elif peek_val == 'GLOBAL':
                is_global = True
                self.advance()
        
        # Skip whitespace after EXTERNAL/GLOBAL
        while self.match(COBOLTokenType.NEWLINE, COBOLTokenType.COMMENT):
            self.advance()
        
        if self.debug and (is_external or is_global):
            print(f"[PARSE_VAR] {name} is {'EXTERNAL' if is_external else ''}{'GLOBAL' if is_global else ''}")
        
        # USAGE IS INDEX special case
        if self.match(COBOLTokenType.USAGE):
            self.advance()
            if self.current_token() and self.current_token().value and \
            self.current_token().value.upper() in ['IS', 'INDEX']:
                if self.current_token().value.upper() == 'IS':
                    self.advance()
                if self.current_token() and self.current_token().value and \
                self.current_token().value.upper() == 'INDEX':
                    self.advance()
                    self.consume(COBOLTokenType.PERIOD)
                    return COBOLVariableDecl(
                        level=level, name=name, pic_clause=None, value=None,
                        occurs_count=None, decimal_places=None, usage_type='INDEX',
                        is_signed=False, children=[], redefines_target=redefines_target, 
                        is_external=is_external,
                        is_global=is_global,
                        line=token.line, column=token.column
                    )
        
        # Skip whitespace
        while self.match(COBOLTokenType.NEWLINE, COBOLTokenType.COMMENT):
            self.advance()
        
        # OCCURS - Handle both fixed and variable-length tables
        occurs_count = None
        occurs_min = None
        occurs_max = None
        depending_on = None
        
        if self.match(COBOLTokenType.OCCURS):
            self.advance()
            
            # 🔍 DEBUG: Show current state
            print(f"DEBUG: After consuming OCCURS, pos={self.pos}")
            print(f"DEBUG: Current token = {self.current_token().type.name} '{self.current_token().value}'")
            
            # Get first number (could be fixed size or minimum)
            print(f"DEBUG: About to consume NUMBER_LITERAL")
            first_num = int(self.consume(COBOLTokenType.NUMBER_LITERAL).value)
            print(f"DEBUG: Consumed number {first_num}, pos now={self.pos}")
            print(f"DEBUG: Current token = {self.current_token().type.name} '{self.current_token().value}'")
            
            # 🔧 NEW: Check for TO keyword (variable-length table)
            # Example: OCCURS 1 TO 15 TIMES DEPENDING ON DN3
            print(f"DEBUG: Checking if current token is TO...")
            print(f"DEBUG: self.match(COBOLTokenType.TO) = {self.match(COBOLTokenType.TO)}")
            
            if self.match(COBOLTokenType.TO):
                print(f"DEBUG: YES! Found TO keyword")
                self.advance()
                # This is OCCURS min TO max
                occurs_min = first_num
                occurs_max = int(self.consume(COBOLTokenType.NUMBER_LITERAL).value)
                occurs_count = occurs_max  # Use max for allocation
                print(f"DEBUG: Variable-length table: {occurs_min} TO {occurs_max}")
            else:
                print(f"DEBUG: NO TO found, treating as fixed-size table")
                # This is OCCURS count (fixed size)
                # Example: OCCURS 15 TIMES
                occurs_count = first_num
            
            # Skip whitespace/comments
            while self.match(COBOLTokenType.NEWLINE, COBOLTokenType.COMMENT):
                self.advance()
            
            # Optional TIMES keyword
            if self.match(COBOLTokenType.TIMES):
                self.advance()
            
            # Skip whitespace/comments after TIMES
            while self.match(COBOLTokenType.NEWLINE, COBOLTokenType.COMMENT):
                self.advance()
            
            # 🔧 NEW: Check for DEPENDING ON clause (on new line or same line)
            # Example: DEPENDING ON DN3
            # This can appear on a continuation line, so check as IDENTIFIER
            if self.match(COBOLTokenType.IDENTIFIER):
                peek_val = self.current_token().value.upper()
                if peek_val == 'DEPENDING':
                    self.advance()  # consume DEPENDING
                    
                    # Skip whitespace/comments
                    while self.match(COBOLTokenType.NEWLINE, COBOLTokenType.COMMENT):
                        self.advance()
                    
                    # Optional ON keyword
                    if self.match(COBOLTokenType.IDENTIFIER):
                        if self.current_token().value.upper() == 'ON':
                            self.advance()
                            # Skip whitespace/comments
                            while self.match(COBOLTokenType.NEWLINE, COBOLTokenType.COMMENT):
                                self.advance()
                    
                    # Get the variable name
                    if self.match(COBOLTokenType.IDENTIFIER):
                        depending_on = self.current_token().value
                        self.advance()
            
            # Skip more whitespace/comments
            while self.match(COBOLTokenType.NEWLINE, COBOLTokenType.COMMENT):
                self.advance()
            
            # 🔧 Check for INDEXED BY clause (can be on new line)
            # Example: INDEXED BY IN1
            if self.match(COBOLTokenType.IDENTIFIER):
                peek_val = self.current_token().value.upper()
                if peek_val == 'INDEXED':
                    self.advance()  # consume INDEXED
                    
                    # Skip whitespace/comments
                    while self.match(COBOLTokenType.NEWLINE, COBOLTokenType.COMMENT):
                        self.advance()
                    
                    # Optional BY keyword
                    if self.match(COBOLTokenType.BY):
                        self.advance()
                        # Skip whitespace/comments
                        while self.match(COBOLTokenType.NEWLINE, COBOLTokenType.COMMENT):
                            self.advance()
                    
                    # Skip all index names (we don't use them in transpilation yet)
                    while self.match(COBOLTokenType.IDENTIFIER):
                        self.advance()
                        # Skip whitespace/comments between index names
                        while self.match(COBOLTokenType.NEWLINE, COBOLTokenType.COMMENT):
                            self.advance()
        
        # Skip any remaining whitespace/comments before checking for PIC or PERIOD
        while self.match(COBOLTokenType.NEWLINE, COBOLTokenType.COMMENT):
            self.advance()
        
        # *** CRITICAL DECISION POINT ***
        # Check what comes next: PIC or PERIOD?
        next_is_pic = self.match(COBOLTokenType.PIC, COBOLTokenType.PICTURE)
        next_is_period = self.match(COBOLTokenType.PERIOD)
        
        if self.debug:
            print(f"[PARSE_VAR] After name/OCCURS, next_is_pic={next_is_pic}, next_is_period={next_is_period}")
        
        # GROUP ITEM: No PIC, just PERIOD
        if not next_is_pic and next_is_period:
            if self.debug:
                print(f"[PARSE_VAR] Detected GROUP ITEM")
            self.consume(COBOLTokenType.PERIOD)
            
            children = []
            while self.match(COBOLTokenType.LEVEL_NUMBER):
                child_level = int(self.current_token().value)
                if child_level <= level:
                    break
                child = self.parse_variable_decl()
                children.append(child)
            
            return COBOLVariableDecl(
                level=level, name=name, pic_clause=None, value=None,
                occurs_count=occurs_count, decimal_places=None, usage_type=None,
                is_signed=False, children=children, redefines_target=redefines_target, 
                occurs_min=occurs_min,
                occurs_max=occurs_max,
                depending_on=depending_on,
                is_external=is_external,
                is_global=is_global,
                line=token.line, column=token.column
            )
        
        # ELEMENTARY ITEM: Must have PIC
        if not next_is_pic:
            self.error(f"Expected PIC clause or PERIOD for '{name}' at level {level}")
        
        if self.debug:
            print(f"[PARSE_VAR] Detected ELEMENTARY ITEM, parsing PIC")
        
        # Parse PIC
        self.advance()  # consume PIC/PICTURE
        if self.current_token() and self.current_token().value and \
        self.current_token().value.upper() == 'IS':
            self.advance()
        pic_clause = self.parse_pic_clause()
        
        if self.debug:
            print(f"[PARSE_VAR] PIC = '{pic_clause}'")
        
        # Extract decimal places
        decimal_places = None
        if pic_clause:
            import re
            match = re.search(r'9\((\d+)\)[V\.]9\((\d+)\)', pic_clause, re.IGNORECASE)
            if match:
                decimal_places = int(match.group(2))
            else:
                match = re.search(r'[V\.]9+', pic_clause, re.IGNORECASE)
                if match:
                    decimal_places = len([c for c in match.group(0)[1:] if c == '9'])
        
        # Check signed
        is_signed = False
        if pic_clause:
            pic_upper = pic_clause.upper()
            if pic_upper.startswith('S') or 'S9' in pic_upper:
                is_signed = True
        
        # USAGE
        usage_type = None
        if self.match(COBOLTokenType.USAGE):
            self.advance()
            usage_type = self.parse_usage_type()
            
            # 🔧 FIX: Skip optional comma after USAGE type
            # Allows: USAGE COMPUTATIONAL, VALUE ZERO
            if self.match(COBOLTokenType.COMMA):
                self.advance()
                
        elif self.match(
            COBOLTokenType.COMP, COBOLTokenType.COMP_1, COBOLTokenType.COMP_2,
            COBOLTokenType.COMP_3, COBOLTokenType.COMPUTATIONAL,
            COBOLTokenType.COMPUTATIONAL_3, COBOLTokenType.BINARY,
            COBOLTokenType.PACKED_DECIMAL, COBOLTokenType.DISPLAY
        ):
            usage_map = {
                COBOLTokenType.COMP: 'COMP', COBOLTokenType.COMP_1: 'COMP-1',
                COBOLTokenType.COMP_2: 'COMP-2', COBOLTokenType.COMP_3: 'COMP-3',
                COBOLTokenType.COMPUTATIONAL: 'COMP', COBOLTokenType.COMPUTATIONAL_3: 'COMP-3',
                COBOLTokenType.BINARY: 'BINARY', COBOLTokenType.PACKED_DECIMAL: 'PACKED-DECIMAL',
                COBOLTokenType.DISPLAY: 'DISPLAY'
            }
            usage_type = usage_map.get(self.current_token().type, 'COMP')
            self.advance()
            
            # 🔧 FIX: Skip optional comma after direct USAGE keyword
            if self.match(COBOLTokenType.COMMA):
                self.advance()
                
        elif self.match(COBOLTokenType.IDENTIFIER):
            val = self.current_token().value.upper()
            if 'BINARY' in val or 'PACKED' in val:
                usage_type = val
                self.advance()
                
                # 🔧 FIX: Skip optional comma here too
                if self.match(COBOLTokenType.COMMA):
                    self.advance()
        
        # OCCURS after PIC (alternate syntax)
        if occurs_count is None and self.match(COBOLTokenType.OCCURS):
            self.advance()
            
            # Get first number (could be fixed size or minimum)
            first_num = int(self.consume(COBOLTokenType.NUMBER_LITERAL).value)
            
            # 🔧 CRITICAL: Check for TO keyword (variable-length table)
            # Example: PIC X OCCURS 1 TO 15 TIMES
            if self.match(COBOLTokenType.TO):
                self.advance()
                # This is OCCURS min TO max
                occurs_min = first_num
                occurs_max = int(self.consume(COBOLTokenType.NUMBER_LITERAL).value)
                occurs_count = occurs_max  # Use max for allocation
            else:
                # This is OCCURS count (fixed size)
                occurs_count = first_num
            
            # Optional TIMES keyword
            if self.match(COBOLTokenType.TIMES):
                self.advance()
            
            # Skip whitespace/comments
            while self.match(COBOLTokenType.NEWLINE, COBOLTokenType.COMMENT):
                self.advance()
            
            # Check for DEPENDING ON clause
            if self.match(COBOLTokenType.IDENTIFIER):
                if self.current_token().value.upper() == 'DEPENDING':
                    self.advance()
                    # Optional ON keyword
                    if self.match(COBOLTokenType.IDENTIFIER):
                        if self.current_token().value.upper() == 'ON':
                            self.advance()
                    # Get the variable name
                    if self.match(COBOLTokenType.IDENTIFIER):
                        depending_on = self.current_token().value
                        self.advance()
            
            # Skip whitespace/comments
            while self.match(COBOLTokenType.NEWLINE, COBOLTokenType.COMMENT):
                self.advance()
            
            # Check for INDEXED BY clause
            if self.current_token() and self.current_token().value and \
               self.current_token().value.upper() == 'INDEXED':
                self.advance()
                if self.match(COBOLTokenType.BY):
                    self.advance()
                # Skip all index names
                while self.match(COBOLTokenType.IDENTIFIER):
                    self.advance()
        
        # VALUE
        value = None
        if self.match(COBOLTokenType.VALUE):
            self.advance()
            if self.current_token() and self.current_token().value and \
            self.current_token().value.upper() == 'IS':
                self.advance()
            
            if self.match(COBOLTokenType.STRING_LITERAL):
                value = self.current_token().value
                self.advance()
            elif self.match(COBOLTokenType.PLUS, COBOLTokenType.MINUS):
                sign = self.current_token().value
                self.advance()
                if self.match(COBOLTokenType.NUMBER_LITERAL):
                    value = sign + self.current_token().value
                    self.advance()
            elif self.match(COBOLTokenType.NUMBER_LITERAL):
                value = self.current_token().value
                self.advance()
            elif self.match(COBOLTokenType.IDENTIFIER):
                val = self.current_token().value.upper()
                if val in ['SPACES', 'SPACE', 'ZEROS', 'ZEROES', 'ZERO']:
                    value = val
                    self.advance()
        
        # Terminating PERIOD
        self.consume(COBOLTokenType.PERIOD)
        
        if self.debug:
            print(f"[PARSE_VAR] Complete: {name}, PIC={pic_clause}, VALUE={value}")
        
        return COBOLVariableDecl(
            level=level, name=name, pic_clause=pic_clause, value=value,
            occurs_count=occurs_count, decimal_places=decimal_places,
            usage_type=usage_type, is_signed=is_signed, children=[], 
            occurs_min=occurs_min,
            occurs_max=occurs_max,
            depending_on=depending_on,
            is_external=is_external,
            is_global=is_global,
            redefines_target=redefines_target, line=token.line, column=token.column
        )

    def parse_pic_clause(self) -> str:
        """
        Parse a PIC clause, collecting all tokens until we hit a terminator.
        
        Handles:
        - Basic formats: 9(4), X(20), S9(5)V99
        - Decimal formats: -9(9).9(9), -.9(18) (period as decimal point)
        - Display-edited: $$$$,$$9.99, ZZZ9, ***9.99, +999.99-, etc.
        - All punctuation and special characters in PIC context
        
        CRITICAL FIX: The PERIOD character can be:
        1. A decimal point inside the PIC: PIC 9.99
        2. The terminating period: PIC 9(4).
        
        We look ahead to determine which case we're in!
        
        CRITICAL FIX #2: LEVEL_NUMBER on a new line is NEVER part of PIC!
        PIC X(5).
        07 NEXT-VAR  <- This 07 is NOT part of the PIC!
        
        Returns the complete PIC string (e.g., "-9(9).9(9)")
        """
        pic_str = ""
        
        # Remember what line we started on
        start_line = self.current_token().line if self.current_token() else 0
        
        # Terminators: things that end a PIC clause
        # NOTE: PERIOD is NOT a terminator because it can be the decimal point!
        terminators = [
            COBOLTokenType.VALUE,
            COBOLTokenType.OCCURS,
            COBOLTokenType.REDEFINES,
            COBOLTokenType.USAGE,
            COBOLTokenType.COMP,
            COBOLTokenType.COMP_1,
            COBOLTokenType.COMP_2,
            COBOLTokenType.COMP_3,
            COBOLTokenType.COMPUTATIONAL,
            COBOLTokenType.COMPUTATIONAL_3,
            COBOLTokenType.BINARY,
            COBOLTokenType.PACKED_DECIMAL,
            COBOLTokenType.DISPLAY,
            COBOLTokenType.PROCEDURE,
            COBOLTokenType.EOF
        ]
        
        seen_content = False
        last_was_closing_paren = False
        
        while not self.match(*terminators):
            token = self.current_token()
            
            if token is None:
                break
            
            # Check for real LEVEL_NUMBER (columns 7-12 in fixed format COBOL)
            # These mark the start of a new variable declaration
            if self.match(COBOLTokenType.LEVEL_NUMBER):
                # CRITICAL FIX: Level numbers on a NEW LINE are NEVER part of PIC
                if token.line != start_line:
                    # This is a new variable declaration on the next line
                    break
                
                # Real level numbers are in area A (columns 7-12)
                # PIC digits that got tokenized as LEVEL_NUMBER are in area B (after column 12)
                if token.column <= 12:
                    # This is a real level number - stop parsing PIC
                    break
                # Otherwise, treat it as a digit in the PIC clause (fall through to handling below)
            
            # Special handling for PERIOD - could be decimal point or terminator
            if self.match(COBOLTokenType.PERIOD):
                next_pos = self.pos + 1
                if next_pos < len(self.tokens):
                    next_token = self.tokens[next_pos]
                    # Check if followed by digit (as NUMBER_LITERAL or LEVEL_NUMBER in area B)
                    is_decimal = False
                    
                    if next_token.type == COBOLTokenType.NUMBER_LITERAL:
                        is_decimal = True
                    elif next_token.type == COBOLTokenType.LPAREN:
                        is_decimal = True
                    elif next_token.type == COBOLTokenType.LEVEL_NUMBER:
                        # CRITICAL FIX: Only treat as decimal if:
                        # 1. On the SAME line (not a new variable declaration)
                        # 2. AND in area B (after column 12)
                        if next_token.line == token.line and next_token.column > 12:
                            # LEVEL_NUMBER in area B on same line - part of PIC
                            is_decimal = True
                        else:
                            # LEVEL_NUMBER on new line or in area A - NOT part of PIC
                            is_decimal = False
                    elif next_token.type == COBOLTokenType.IDENTIFIER:
                        # Check if identifier looks like a digit pattern
                        next_val = next_token.value.upper()
                        if next_val in ['9', 'X', 'A', 'Z']:
                            is_decimal = True
                    
                    if is_decimal:
                        pic_str += token.value
                        self.advance()
                        seen_content = True
                        last_was_closing_paren = False
                        continue
                    else:
                        # Terminating period
                        break
                else:
                    break
            
            # Handle LEVEL_NUMBER in area B ON SAME LINE as part of PIC
            if self.match(COBOLTokenType.LEVEL_NUMBER):
                if token.line == start_line and token.column > 12:
                    # Digit in area B on same line - part of PIC clause
                    pic_str += token.value
                    seen_content = True
                    last_was_closing_paren = False
                    self.advance()
                    continue
                else:
                    # Level number on new line or in area A - stop
                    break
            
            # Collect other PIC tokens
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
                seen_content = True
                last_was_closing_paren = (token.type == COBOLTokenType.RPAREN)
                self.advance()
            else:
                # Unknown token type - stop parsing
                break
        
        return pic_str.strip()


    
    def parse_usage_type(self) -> str:
        """
        Parse USAGE clause value.
        
        Examples:
        USAGE COMP          -> "COMP"
        USAGE COMP-3        -> "COMP-3" 
        USAGE DISPLAY       -> "DISPLAY"
        USAGE BINARY        -> "BINARY"
        USAGE IS COMP       -> "COMP" (with optional IS)
        
        Returns the usage type as a string.
        """
        # Handle optional IS keyword: USAGE IS COMP
        if self.current_token() and self.current_token().value and \
        self.current_token().value.upper() == 'IS':
            self.advance()
        
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
        elif self.match(COBOLTokenType.IDENTIFIER):
            # Handle compound type names: BINARY-LONG, PACKED-DECIMAL, etc.
            ident_value = self.current_token().value.upper()
            self.advance()
            return ident_value
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
                
                # 🔧 FIX: Skip comma between parameters
                if self.match(COBOLTokenType.COMMA):
                    self.advance()
                
                # Stop if we hit a period or something that isn't an identifier
                if self.match(COBOLTokenType.PERIOD):
                    break
        
        # Consume period (now happens after checking for USING)
        self.consume(COBOLTokenType.PERIOD)
        
        # Parse paragraphs and statements
        paragraphs = []
        
        while not self.match(COBOLTokenType.EOF):
            
             # 🔧 FIX: Check for division markers FIRST
            if self.match(COBOLTokenType.PROCEDURE, COBOLTokenType.DATA,
                        COBOLTokenType.ENVIRONMENT, COBOLTokenType.DIVISION):
                break            
            
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
        Parse END PROGRAM program-id. (optional)
        Some programs don't have END PROGRAM, especially copybooks.
        """
        # Only try to parse if we see END token
        if not self.match(COBOLTokenType.END):
            return  # No END PROGRAM, that's OK
        
        self.consume(COBOLTokenType.END)
        
        # Check for PROGRAM keyword
        if not self.match(COBOLTokenType.PROGRAM):
            # Might be END. for something else, back out
            return
        
        self.consume(COBOLTokenType.PROGRAM)
        
        # Optional program-id check
        if self.match(COBOLTokenType.IDENTIFIER):
            actual_program_id = self.consume(COBOLTokenType.IDENTIFIER).value
            if actual_program_id != expected_program_id:
                # Warn but don't fail
                print(f"Warning: END PROGRAM {actual_program_id} doesn't match PROGRAM-ID {expected_program_id}")
        
        # Optional period
        if self.match(COBOLTokenType.PERIOD):
            self.consume(COBOLTokenType.PERIOD)

    def parse_go_to(self, token: Token) -> COBOLASTNode:
        """
        Parse GO TO statement
        Syntax: GO TO paragraph-name
            GO TO paragraph-name DEPENDING ON variable
        """
        # Consume GO (and optional TO)
        self.advance()  # GO
        if self.match(COBOLTokenType.TO):
            self.advance()  # TO (optional in some COBOL dialects)
        
        # Parse target paragraph name
        if not self.match(COBOLTokenType.IDENTIFIER):
            self.error("Expected paragraph name after GO TO")
        
        paragraph_name = self.consume(COBOLTokenType.IDENTIFIER).value
        
        # Optional DEPENDING ON clause
        depending_on = None
        if self.match(COBOLTokenType.IDENTIFIER):
            if self.current_token().value.upper() == 'DEPENDING':
                self.advance()
                if self.match(COBOLTokenType.IDENTIFIER) and self.current_token().value.upper() == 'ON':
                    self.advance()
                    depending_on = self.consume(COBOLTokenType.IDENTIFIER).value
        
        self.consume_optional_period()
        
        # For now, treat GO TO as a PERFORM (jump to paragraph)
        # In a real compiler, this would be a proper goto
        return COBOLPerformParagraph(
            paragraph_name=paragraph_name,
            line=token.line,
            column=token.column
        )


    def parse_paragraph(self) -> COBOLParagraph:
        token = self.current_token()
        name_token = self.consume(COBOLTokenType.IDENTIFIER)
        self.consume(COBOLTokenType.PERIOD)
        
        self.push_context(ParseContext.INSIDE_PARAGRAPH)
        
        statements = []
        while not self.match(COBOLTokenType.EOF):
            # Skip comments and newlines first
            while self.match(COBOLTokenType.COMMENT, COBOLTokenType.NEWLINE):
                self.advance()
                if self.match(COBOLTokenType.EOF):
                    break
            
            # 🔧 FIX: Check for division markers (PROCEDURE, DATA, etc.)
            # These mark the start of a new division/program
            if self.match(COBOLTokenType.PROCEDURE, COBOLTokenType.DATA,
                         COBOLTokenType.ENVIRONMENT, COBOLTokenType.DIVISION):
                break
            
            # Check for the start of the next program or the end of the current one
            if self.match(COBOLTokenType.IDENTIFICATION):
                break
            if self.match(COBOLTokenType.END) and self.peek_token() and self.peek_token().type == COBOLTokenType.PROGRAM:
                break

            # Check for SECTION keyword
            if self.match(COBOLTokenType.SECTION):
                break
            
            # Check for next paragraph (IDENTIFIER followed by PERIOD, not a statement keyword)
            if self.match(COBOLTokenType.IDENTIFIER):
                next_token = self.peek_token()
                if next_token and next_token.type == COBOLTokenType.PERIOD:
                    identifier_value = self.current_token().value
                    # List of statement keywords that can be followed by PERIOD
                    if identifier_value.upper() not in ['STOP', 'GOBACK', 'EXIT', 'DISPLAY', 
                                                        'MOVE', 'ADD', 'SUBTRACT', 'COMPUTE',
                                                        'MULTIPLY', 'DIVIDE', 'IF', 'PERFORM', 
                                                        'EVALUATE', 'ACCEPT']:
                        # This is a new paragraph name
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
        """Parse a single COBOL statement"""
        token = self.current_token()
        
        print(f"DEBUG parse_statement: token={token.type.name} value={token.value} context={self.current_context()}")
        
        # Skip comment tokens
        while self.match(COBOLTokenType.COMMENT):
            self.advance()
            if self.match(COBOLTokenType.EOF):
                return None
            token = self.current_token()

        # ✅ Don't parse division markers as statements - return without advancing
        if self.match(COBOLTokenType.IDENTIFICATION, COBOLTokenType.ENVIRONMENT, 
                    COBOLTokenType.DATA, COBOLTokenType.PROCEDURE, COBOLTokenType.DIVISION):
            return None  # Don't advance! Let the caller (IF/paragraph loop) see it and break
        
        # Handle READ statement (special case - not a token type)
        if self.current_token().value and self.current_token().value.upper() == 'READ':
            return self.parse_read_statement()

        # Main statement dispatch
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
            return self.parse_if(token)
        elif self.match(COBOLTokenType.PERFORM):
            return self.parse_perform(token)
        elif self.match(COBOLTokenType.GO, COBOLTokenType.GOTO):
            return self.parse_go_to(token)
        elif self.match(COBOLTokenType.CALL):
            return self.parse_call(token)
        elif self.match(COBOLTokenType.EVALUATE):
            return self.parse_evaluate(token)
        elif self.match(COBOLTokenType.STRING):
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
            # ✅ Handle both EXIT and EXIT PROGRAM
            exit_token = self.current_token()
            self.advance()  # consume EXIT
            
            # Check if PROGRAM follows
            is_program = False
            if self.match(COBOLTokenType.PROGRAM):
                is_program = True
                self.advance()  # consume PROGRAM
            
            # Now consume the period
            if self.requires_period():
                self.consume(COBOLTokenType.PERIOD)
            
            return COBOLExit(is_program=is_program, line=exit_token.line, column=exit_token.column)
        elif self.match(COBOLTokenType.EOF):
            return None
        else:
            # Unknown token - advance past it and return None
            self.advance()
            return None
    
    # ✅ ADD THIS NEW METHOD
    def parse_read_statement(self) -> 'COBOLRead':
        """
        Parse READ statement with AT END clause
        READ filename [INTO variable] [AT END statements...] [NOT AT END statements...]
        """
        token = self.current_token()
        self.advance()  # consume READ
        
        # File name
        filename = self.consume(COBOLTokenType.IDENTIFIER).value
        
        # Optional INTO clause
        into_variable = None
        if self.match(COBOLTokenType.INTO):
            self.advance()
            into_variable = self.consume(COBOLTokenType.IDENTIFIER).value
        
        # Optional AT END clause (can have multiple statements!)
        at_end_statements = []
        not_at_end_statements = []
        
        if self.current_token() and self.current_token().value and self.current_token().value.upper() == 'AT':
            self.advance()
            # Consume END keyword
            if self.current_token() and self.current_token().value and self.current_token().value.upper() == 'END':
                self.advance()
                
                # Parse multiple statements until NOT AT END, period, or major keyword
                while not self.match(COBOLTokenType.PERIOD, COBOLTokenType.EOF):
                    # Get current token value for checks
                    current_val = self.current_token().value.upper() if self.current_token().value else ""

                    # Check for NOT AT END
                    if current_val == 'NOT':
                        break

                    # Check for next major statement
                    # This is the critical fix: check if the current token starts a new statement
                    # before attempting to parse it.
                    STATEMENT_KEYWORDS = [
                        'MOVE', 'DISPLAY', 'PERFORM', 'IF', 'ELSE', 'STOP', 'ADD', 'SUBTRACT', 
                        'MULTIPLY', 'DIVIDE', 'COMPUTE', 'ACCEPT', 'READ', 'WRITE', 'OPEN', 'CLOSE'
                    ]
                    if current_val in STATEMENT_KEYWORDS and len(at_end_statements) > 0:
                        break
                    
                    stmt = self.parse_statement()
                    if stmt:
                        at_end_statements.append(stmt)
        
        # Optional NOT AT END clause
        if self.current_token() and self.current_token().value and self.current_token().value.upper() == 'NOT':
            self.advance()
            if self.current_token() and self.current_token().value and self.current_token().value.upper() == 'AT':
                self.advance()
            if self.current_token() and self.current_token().value and self.current_token().value.upper() == 'END':
                self.advance()
                
                while not self.match(COBOLTokenType.PERIOD, COBOLTokenType.EOF):
                    # Get current token value for checks
                    current_val = self.current_token().value.upper() if self.current_token().value else ""

                    # Check if we've hit the end of the block
                    STATEMENT_KEYWORDS = [
                        'MOVE', 'DISPLAY', 'PERFORM', 'IF', 'ELSE', 'STOP', 'ADD', 'SUBTRACT',
                        'MULTIPLY', 'DIVIDE', 'COMPUTE', 'ACCEPT', 'READ', 'WRITE', 'OPEN', 'CLOSE'
                    ]
                    if current_val in STATEMENT_KEYWORDS and len(not_at_end_statements) > 0:
                        break
                    
                    stmt = self.parse_statement()
                    if stmt:
                        not_at_end_statements.append(stmt)
        
        # Optional NOT AT END clause
        if self.match(COBOLTokenType.PERIOD):
            self.consume(COBOLTokenType.PERIOD)
        
        return COBOLRead(
            filename=filename,
            into_variable=into_variable,
            at_end_statements=at_end_statements if at_end_statements else None,
            not_at_end_statements=not_at_end_statements if not_at_end_statements else None,
            line=token.line,
            column=token.column
        )
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
            # Stop parsing expressions if we hit the UPON clause
            if self.current_token().value.upper() == 'UPON':
                break

            expressions.append(self.parse_expression())
        
        # After parsing the expression(s), check for UPON clause
        if self.current_token().value.upper() == 'UPON':
            self.advance()  # consume UPON
            if self.match(COBOLTokenType.IDENTIFIER):
                # This is the device name (CONSOLE, SYSERR, etc.)
                self.advance()  # consume the device name

        if self.requires_period() and self.match(COBOLTokenType.PERIOD):
            self.consume(COBOLTokenType.PERIOD)
        
        return COBOLDisplay(expressions=expressions, line=token.line, column=token.column)
    
    def parse_accept(self, token: Token) -> COBOLAccept:
        self.consume(COBOLTokenType.ACCEPT)
        
        variable = self.consume(COBOLTokenType.IDENTIFIER).value
        
        self.consume_optional_period()
        return COBOLAccept(
            variable=variable,
            line=token.line,
            column=token.column
        )
    
    def parse_move(self, token: Token) -> COBOLMove:
        """Parse MOVE statement
        MOVE source TO target
        MOVE CORRESPONDING source TO target
        """
        self.consume(COBOLTokenType.MOVE)
        
        # Check for CORRESPONDING/CORR
        is_corresponding = False
        if self.match(COBOLTokenType.IDENTIFIER):
            peek = self.current_token().value.upper()
            if peek in ['CORRESPONDING', 'CORR']:
                is_corresponding = True
                self.advance()
        
        # Parse source
        source = self.parse_expression()
        
        # Expect TO
        self.consume(COBOLTokenType.TO)
        
        # Parse one or more targets, stopping at the next statement keyword
        targets = []
        while not self.match(COBOLTokenType.PERIOD, COBOLTokenType.EOF, COBOLTokenType.END_IF):
            # 🔧 FIX: Check for statement keyword TOKEN TYPES first
            # GO, CALL, PERFORM, etc. are lexed as their own token types, not IDENTIFIER
            if self.match(COBOLTokenType.GO, COBOLTokenType.GOTO, COBOLTokenType.CALL,
                          COBOLTokenType.PERFORM, COBOLTokenType.IF, COBOLTokenType.STOP,
                          COBOLTokenType.ADD, COBOLTokenType.SUBTRACT, COBOLTokenType.MULTIPLY,
                          COBOLTokenType.DIVIDE, COBOLTokenType.COMPUTE, COBOLTokenType.ACCEPT,
                          COBOLTokenType.DISPLAY, COBOLTokenType.EVALUATE, COBOLTokenType.EXIT,
                          COBOLTokenType.GOBACK, COBOLTokenType.STRING, COBOLTokenType.UNSTRING,
                          COBOLTokenType.INSPECT):
                break  # Stop - this is a statement keyword!
            
            # Also check IDENTIFIER tokens for statement keywords
            if self.match(COBOLTokenType.IDENTIFIER):
                next_val = self.current_token().value.upper()
                STATEMENT_KEYWORDS = [
                    'MOVE', 'DISPLAY', 'PERFORM', 'IF', 'ELSE', 'STOP', 'ADD', 'SUBTRACT', 
                    'MULTIPLY', 'DIVIDE', 'COMPUTE', 'ACCEPT', 'READ', 'WRITE', 'OPEN', 'CLOSE',
                    'CALL', 'EVALUATE', 'GO', 'GOTO', 'EXIT', 'GOBACK', 'STRING', 'UNSTRING',
                    'INSPECT', 'SEARCH', 'SET', 'CONTINUE'
                ]
                if next_val in STATEMENT_KEYWORDS:
                    break  # Stop - this is a new statement
            
            # If it's not a keyword, parse it as a target expression
            targets.append(self.parse_expression())
            
            # 🔧 FIX: Check for keywords AFTER consuming comma
            if self.match(COBOLTokenType.COMMA):
                self.advance()
                
                # After comma, check if next token is a statement keyword TOKEN TYPE
                if self.match(COBOLTokenType.GO, COBOLTokenType.GOTO, COBOLTokenType.CALL,
                              COBOLTokenType.PERFORM, COBOLTokenType.IF, COBOLTokenType.STOP,
                              COBOLTokenType.ADD, COBOLTokenType.SUBTRACT, COBOLTokenType.MULTIPLY,
                              COBOLTokenType.DIVIDE, COBOLTokenType.COMPUTE, COBOLTokenType.ACCEPT,
                              COBOLTokenType.DISPLAY, COBOLTokenType.EVALUATE, COBOLTokenType.EXIT,
                              COBOLTokenType.GOBACK, COBOLTokenType.STRING, COBOLTokenType.UNSTRING,
                              COBOLTokenType.INSPECT):
                    break  # Comma separates statements!
                
                # Also check IDENTIFIER for statement keywords
                if self.match(COBOLTokenType.IDENTIFIER):
                    next_val = self.current_token().value.upper()
                    STATEMENT_KEYWORDS = [
                        'MOVE', 'DISPLAY', 'PERFORM', 'IF', 'ELSE', 'STOP', 'ADD', 'SUBTRACT', 
                        'MULTIPLY', 'DIVIDE', 'COMPUTE', 'ACCEPT', 'READ', 'WRITE', 'OPEN', 'CLOSE',
                        'CALL', 'EVALUATE', 'GO', 'GOTO', 'EXIT', 'GOBACK', 'STRING', 'UNSTRING',
                        'INSPECT', 'SEARCH', 'SET', 'CONTINUE'
                    ]
                    if next_val in STATEMENT_KEYWORDS:
                        break  # Comma separates statements!
                # Otherwise continue parsing more targets
            else:
                # No comma means list of targets is done
                break
        
        self.consume_optional_period()
        return COBOLMove(
            source=source, 
            targets=targets, 
            is_corresponding=is_corresponding,
            line=token.line, 
            column=token.column
        )
    
    def parse_compute(self, token: Token) -> COBOLCompute:
        self.consume(COBOLTokenType.COMPUTE)
        
        target = self.parse_expression()
        self.consume(COBOLTokenType.EQUALS_SIGN)
        expression = self.parse_arithmetic_expression()
        
        self.consume_optional_period()
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
        Parse UNSTRING statement with multiple delimiters
        
        Examples:
            UNSTRING source DELIMITED BY "," INTO f1 f2
            UNSTRING source DELIMITED BY ALL " " INTO f1 f2
            UNSTRING source DELIMITED BY "," OR " " INTO f1 f2
            UNSTRING source DELIMITED BY ALL "," OR " " OR ALL ";" INTO f1 f2
        """
        self.consume(COBOLTokenType.UNSTRING)
        
        # Parse source field
        if not self.match(COBOLTokenType.IDENTIFIER, COBOLTokenType.STRING_LITERAL):
            self.error("Expected source field in UNSTRING statement")
        source = self.parse_expression()
        
        # Parse DELIMITED BY clause with multiple delimiters
        delimiters = []
        delimiter_all_flags = []
        
        if self.match(COBOLTokenType.DELIMITED):
            self.consume(COBOLTokenType.DELIMITED)
            self.consume(COBOLTokenType.BY)
            
            # Parse first delimiter
            has_all = False
            if self.match(COBOLTokenType.ALL):
                self.advance()
                has_all = True
            
            delimiter = self.parse_expression()
            delimiters.append(delimiter)
            delimiter_all_flags.append(has_all)
            
            # ✅ NEW: Parse additional delimiters with OR
            while self.match(COBOLTokenType.OR):
                self.advance()  # consume OR
                
                # Check for optional ALL before next delimiter
                has_all = False
                if self.match(COBOLTokenType.ALL):
                    self.advance()
                    has_all = True
                
                delimiter = self.parse_expression()
                delimiters.append(delimiter)
                delimiter_all_flags.append(has_all)
        
        # ✅ FIX: Skip any junk/comments between DELIMITED BY and INTO
        # This handles line continuations with sequence numbers.
        while not self.match(COBOLTokenType.INTO, COBOLTokenType.EOF, COBOLTokenType.PERIOD):
            self.advance()

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
            delimiters=delimiters,  # ✅ List of delimiters
            delimiter_all_flags=delimiter_all_flags,  # ✅ ALL flag for each
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
        
        # Optional THEN keyword (can be omitted in COBOL)
        if self.match(COBOLTokenType.THEN):
            self.advance()
        
        self.push_context(ParseContext.INSIDE_IF)
        
        # Parse THEN block
        then_statements = []
        
        while not self.match(COBOLTokenType.ELSE, COBOLTokenType.END_IF, 
                            COBOLTokenType.PERIOD, COBOLTokenType.EOF):
            # 🔧 FIX: Check for structural keywords that end IF
            # SECTION, paragraph names followed by PERIOD, etc.
            if self.match(COBOLTokenType.IDENTIFIER):
                # Check if this is "SECTION" keyword
                if self.current_token().value.upper() == 'SECTION':
                    # This starts a new section, IF is implicitly ended
                    self.pop_context()
                    return COBOLIf(
                        condition=condition,
                        then_statements=then_statements,
                        else_statements=None,
                        line=token.line,
                        column=token.column
                    )
                
                # Check for paragraph name (IDENTIFIER followed by PERIOD)
                next_token = self.peek_token(1)
                if next_token and next_token.type == COBOLTokenType.PERIOD:
                    # This is a paragraph declaration, IF ended
                    self.pop_context()
                    return COBOLIf(
                        condition=condition,
                        then_statements=then_statements,
                        else_statements=None,
                        line=token.line,
                        column=token.column
                    )
            
            # Check for next program starting
            if self.match(COBOLTokenType.IDENTIFICATION):
                self.pop_context()
                return COBOLIf(
                    condition=condition,
                    then_statements=then_statements,
                    else_statements=None,
                    line=token.line,
                    column=token.column
                )
            
            stmt = self.parse_statement()
            if stmt:
                then_statements.append(stmt)
                
                # After each statement, check if PERIOD terminates the IF
                if self.match(COBOLTokenType.PERIOD):
                    # This is old-style IF - PERIOD terminates it
                    self.consume(COBOLTokenType.PERIOD)
                    self.pop_context()
                    
                    return COBOLIf(
                        condition=condition,
                        then_statements=then_statements,
                        else_statements=None,
                        line=token.line,
                        column=token.column
                    )
        
        # Parse ELSE block (if present)
        else_statements = None
        if self.match(COBOLTokenType.ELSE):
            self.advance()
            else_statements = []
            
            while not self.match(COBOLTokenType.END_IF, COBOLTokenType.PERIOD, COBOLTokenType.EOF):
                # 🔧 FIX: Apply same logic to ELSE block
                if self.match(COBOLTokenType.IDENTIFIER):
                    if self.current_token().value.upper() == 'SECTION':
                        self.pop_context()
                        return COBOLIf(
                            condition=condition,
                            then_statements=then_statements,
                            else_statements=else_statements,
                            line=token.line,
                            column=token.column
                        )
                    
                    next_token = self.peek_token(1)
                    if next_token and next_token.type == COBOLTokenType.PERIOD:
                        self.pop_context()
                        return COBOLIf(
                            condition=condition,
                            then_statements=then_statements,
                            else_statements=else_statements,
                            line=token.line,
                            column=token.column
                        )

                # Check for next program starting
                if self.match(COBOLTokenType.IDENTIFICATION):
                    self.pop_context()
                    return COBOLIf(
                        condition=condition,
                        then_statements=then_statements,
                        else_statements=else_statements,
                        line=token.line,
                        column=token.column
                    )
                
                stmt = self.parse_statement()
                if stmt:
                    else_statements.append(stmt)
                    
                    # PERIOD can also terminate ELSE block in old-style
                    if self.match(COBOLTokenType.PERIOD):
                        self.consume(COBOLTokenType.PERIOD)
                        self.pop_context()
                        
                        return COBOLIf(
                            condition=condition,
                            then_statements=then_statements,
                            else_statements=else_statements,
                            line=token.line,
                            column=token.column
                        )
        
        # Check for PERIOD before END-IF (mixed style)
        if self.match(COBOLTokenType.PERIOD):
            self.consume(COBOLTokenType.PERIOD)
            self.pop_context()
            
            return COBOLIf(
                condition=condition,
                then_statements=then_statements,
                else_statements=else_statements,
                line=token.line,
                column=token.column
            )
        
        # Modern style - consume END-IF
        if self.match(COBOLTokenType.END_IF):
            self.consume(COBOLTokenType.END_IF)
        elif self.match(COBOLTokenType.EOF):
            # Graceful error: IF without END-IF
            self.pop_context()
            raise ParserError("IF statement missing END-IF or PERIOD before end of file", 
                            token.line, token.column)
        
        self.pop_context()
        
        # Optional period after END-IF
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
            self.consume_optional_period()
            
            return COBOLPerformTimes(
                times_expr=times_expr,
                paragraph_name=None,
                statements=statements,
                line=token.line,
                column=token.column
            )

        elif self.match(COBOLTokenType.IDENTIFIER):
            para_name_token = self.current_token()
            para_name = para_name_token.value

            # Look ahead to decide what kind of PERFORM this is
            peek1 = self.peek_token(1)

            # Check for VARYING after paragraph name
            if peek1 and peek1.type == COBOLTokenType.VARYING:
                self.advance() # consume para_name
                return self.parse_perform_varying(token, para_name)

            # Check for UNTIL after paragraph name
            elif peek1 and peek1.type == COBOLTokenType.UNTIL:
                self.advance() # consume para_name
                return self.parse_perform_until(token, para_name)

            elif peek1 and peek1.type == COBOLTokenType.TIMES:
                 # This is PERFORM N TIMES, not a paragraph name
                 return self.parse_perform_times_inline(token)

            else:
                # Default case: PERFORM para.
                self.advance() # consume para_name
                self.consume_optional_period()
                return COBOLPerformParagraph(
                    paragraph_name=para_name,
                    line=token.line, column=token.column
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
        
    def parse_perform_varying(self, token: Token, paragraph_name: Optional[str] = None) -> COBOLPerformVarying:
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
        
        # Only parse inline statements if there's NO paragraph name
        if paragraph_name is None:
            # Inline PERFORM VARYING ... END-PERFORM
            while not self.match(COBOLTokenType.END_PERFORM, COBOLTokenType.EOF):
                stmt = self.parse_statement()
                if stmt:
                    statements.append(stmt)
            
            if self.match(COBOLTokenType.END_PERFORM):
                self.consume(COBOLTokenType.END_PERFORM)
            elif self.match(COBOLTokenType.EOF):
                self.pop_context()
                raise ParserError("PERFORM VARYING missing END-PERFORM before end of file",
                                token.line, token.column)
        # else: PERFORM paragraph VARYING has no inline statements
        
        self.pop_context()
        
        self.consume_optional_period()
        
        return COBOLPerformVarying(
            variable=variable,
            from_expr=from_expr,
            by_expr=by_expr,
            until_condition=until_condition,
            paragraph_name=paragraph_name,
            statements=statements if paragraph_name is None else None,
            line=token.line,
            column=token.column
        )
    
    def parse_call(self, token: Token) -> COBOLCall:
        print(f"[DEBUG] parse_call called at line {token.line}") 
        self.consume(COBOLTokenType.CALL)
        
        # String literal or identifier
        if self.match(COBOLTokenType.STRING_LITERAL):
            prog_name = self.consume(COBOLTokenType.STRING_LITERAL).value.strip('"')
        else:
            prog_name = self.consume(COBOLTokenType.IDENTIFIER).value
        
        # Optional USING clause
        parameters = []
        if self.match(COBOLTokenType.USING):
            self.consume(COBOLTokenType.USING)
            # Parse parameter list
            while self.match(COBOLTokenType.IDENTIFIER):
                param_name = self.consume(COBOLTokenType.IDENTIFIER).value
                parameters.append(param_name)
                # Check for comma-separated list
                if self.match(COBOLTokenType.COMMA):
                    self.advance()
        
        # 🔧 FIX: Skip optional semicolon after USING clause
        if self.match(COBOLTokenType.SEMICOLON):
            self.advance()
        
        # Optional ON OVERFLOW clause
        overflow_statements = []
        has_overflow = False
        
        if self.match(COBOLTokenType.IDENTIFIER):
            val = self.current_token().value.upper()
            if val == 'ON':
                self.advance()
                if self.match(COBOLTokenType.IDENTIFIER) and self.current_token().value.upper() == 'OVERFLOW':
                    self.advance()
                    has_overflow = True
            elif val == 'OVERFLOW':
                self.advance()
                has_overflow = True
        
        if has_overflow:
            while not self.match(COBOLTokenType.PERIOD, COBOLTokenType.EOF):
                if self.match(COBOLTokenType.COMMA, COBOLTokenType.SEMICOLON):
                    self.advance()
                    continue
                
                stmt = self.parse_statement()
                if stmt:
                    overflow_statements.append(stmt)
        
        self.consume_optional_period()
        
        return COBOLCall(
            program_name=prog_name,
            parameters=parameters if parameters else [],
            overflow_statements=overflow_statements if overflow_statements else None,
                         line=token.line, column=token.column)

    def parse_evaluate(self, token: Token) -> COBOLEvaluate:
        self.consume(COBOLTokenType.EVALUATE)
        
        subject = self.parse_expression()
        
        # ✅ ADD THESE 3 LINES:
        while not self.match(COBOLTokenType.WHEN, COBOLTokenType.END_EVALUATE, COBOLTokenType.EOF):
            self.advance()
        # ✅ END OF FIX
        
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
        
        # Handle GREATER THAN OR EQUAL TO / >=
        if self.match(COBOLTokenType.GTE_SIGN, COBOLTokenType.GREATER):
            op_token = self.current_token()
            self.advance()
            
            # Handle various forms: GREATER, GREATER THAN, GREATER THAN OR EQUAL, etc.
            if self.match(COBOLTokenType.THAN):
                self.advance()
            if self.match(COBOLTokenType.OR):
                self.advance()
            if self.match(COBOLTokenType.EQUAL, COBOLTokenType.EQUALS):
                self.advance()
            if self.match(COBOLTokenType.TO):
                self.advance()
            
            right = self.parse_arithmetic_expression()
            return COBOLBinaryOp(
                operator='>=',
                left=left,
                right=right,
                line=op_token.line,
                column=op_token.column
            )
        
        # Handle LESS THAN OR EQUAL TO / <=
        elif self.match(COBOLTokenType.LTE_SIGN, COBOLTokenType.LESS):
            op_token = self.current_token()
            self.advance()
            
            # Handle various forms: LESS, LESS THAN, LESS THAN OR EQUAL, etc.
            if self.match(COBOLTokenType.THAN):
                self.advance()
            if self.match(COBOLTokenType.OR):
                self.advance()
            if self.match(COBOLTokenType.EQUAL, COBOLTokenType.EQUALS):
                self.advance()
            if self.match(COBOLTokenType.TO):
                self.advance()
            
            right = self.parse_arithmetic_expression()
            return COBOLBinaryOp(
                operator='<=',
                left=left,
                right=right,
                line=op_token.line,
                column=op_token.column
            )
        
        # Handle > (greater than only, no equal)
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
        
        # Handle < (less than only, no equal)
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
        
        # Handle EQUAL / EQUAL TO / =
        elif self.match(COBOLTokenType.EQUALS_SIGN, COBOLTokenType.EQUAL, COBOLTokenType.EQUALS):
            op_token = self.current_token()
            self.advance()
            # Handle optional "TO" in "EQUAL TO"
            if self.match(COBOLTokenType.TO):
                self.advance()
            right = self.parse_arithmetic_expression()
            return COBOLBinaryOp(
                operator='=',
                left=left,
                right=right,
                line=op_token.line,
                column=op_token.column
            )
        
        # Handle NOT EQUAL / NOT EQUAL TO
        elif self.match(COBOLTokenType.NOT):
            op_token = self.current_token()
            self.advance()
            if self.match(COBOLTokenType.EQUALS_SIGN, COBOLTokenType.EQUAL, COBOLTokenType.EQUALS):
                self.advance()
                # ✅ Handle optional "TO" in "NOT EQUAL TO"
                if self.match(COBOLTokenType.TO):
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
            token = self.current_token()  # ✅ Fix #4: Capture FIRST
            op = '+' if token.type == COBOLTokenType.PLUS else '-'
            self.advance()
            right = self.parse_multiplicative()
            left = COBOLBinaryOp(operator=op, left=left, right=right, line=token.line, column=token.column)
        
        return left
    
    def parse_multiplicative(self) -> COBOLASTNode:
        left = self.parse_primary()
        
        while self.match(COBOLTokenType.ASTERISK, COBOLTokenType.SLASH):
            token = self.current_token()  # ✅ Fix #4: Capture FIRST
            op = '*' if token.type == COBOLTokenType.ASTERISK else '/'
            self.advance()
            right = self.parse_primary()
            left = COBOLBinaryOp(operator=op, left=left, right=right, line=token.line, column=token.column)
        
        return left
    
    def parse_primary(self) -> COBOLASTNode:
        token = self.current_token()
        
        if self.debug:  # ✅ Fix #7
            print(f"DEBUG parse_primary: token={token.type.name} value={token.value}")
        
        if self.match(COBOLTokenType.NUMBER_LITERAL):
            value = token.value
            self.advance()
            return COBOLNumberLiteral(value=value, line=token.line, column=token.column)
        
        elif self.match(COBOLTokenType.STRING_LITERAL):
            value = token.value
            self.advance()
            return COBOLStringLiteral(value=value, line=token.line, column=token.column)
        
        elif self.match(COBOLTokenType.ALL):
            self.advance()
            if self.match(COBOLTokenType.STRING_LITERAL):
                literal = self.current_token().value
                self.advance()
                return COBOLStringLiteral(value=f"ALL:{literal}", line=token.line, column=token.column)
            elif self.match(COBOLTokenType.IDENTIFIER):
                identifier = self.current_token().value
                self.advance()
                return COBOLIdentifier(name=f"ALL:{identifier}", line=token.line, column=token.column)
            else:
                if self.debug:  # ✅ Fix #3
                    print(f"WARNING: Standalone ALL keyword at line {token.line}, column {token.column}")
                return COBOLIdentifier(name="ALL", line=token.line, column=token.column)
        
        elif self.match(COBOLTokenType.FUNCTION):
            return self.parse_function_call()

        elif self.match(COBOLTokenType.IDENTIFIER):
            name = token.value
            self.advance()
            
            # NEW: Handle qualified names (IDENTIFIER OF IDENTIFIER)
            # Example: HOURS OF MSG-TIME, MINUTES OF MSG-TIME
            if self.match(COBOLTokenType.OF):
                self.advance()  # consume OF
                if not self.match(COBOLTokenType.IDENTIFIER):
                    self.error("Expected identifier after OF keyword")
                parent_name = self.current_token().value
                self.advance()
                # Create a qualified identifier: "HOURS.MSG_TIME" style
                # Note: The converter will need to normalize this to MSG_TIME_HOURS
                qualified_name = f"{name}.{parent_name}"
                name = qualified_name
            
            if self.match(COBOLTokenType.LPAREN):
                self.advance()
                
                # Special case: ARRAY(ALL)
                if self.match(COBOLTokenType.ALL):
                    self.advance()
                    self.consume(COBOLTokenType.RPAREN)
                    return COBOLArraySubscript(
                        array_name=name,
                        indices=[COBOLIdentifier(name="ALL", line=token.line, column=token.column)],  # ✅ Fix #1
                        line=token.line,
                        column=token.column
                    )
                
                # Parse subscripts
                subscripts = []
                subscripts.append(self.parse_expression())
                
                # ✅ Fix #2: Add infinite loop protection
                while not self.match(COBOLTokenType.RPAREN):
                    if self.match(COBOLTokenType.COMMA):
                        self.advance()
                    
                    if not self.match(COBOLTokenType.RPAREN):
                        before_pos = self.pos
                        subscripts.append(self.parse_expression())
                        
                        if self.pos == before_pos:
                            self.error(f"Parser stuck at position {self.pos} in array subscript")
                
                # ✅ Fix #5: Validate subscript count
                if len(subscripts) > 7:
                    self.error(f"Too many array subscripts: {len(subscripts)} (COBOL max is 7)")
                
                self.consume(COBOLTokenType.RPAREN)
                return COBOLArraySubscript(
                    array_name=name,
                    indices=subscripts,
                    line=token.line,
                    column=token.column
                )
            
            return COBOLIdentifier(name=name, line=token.line, column=token.column)
        
        elif self.match(
            COBOLTokenType.USAGE,
            COBOLTokenType.COMP, COBOLTokenType.COMP_1, COBOLTokenType.COMP_2, COBOLTokenType.COMP_3,
            COBOLTokenType.COMPUTATIONAL, COBOLTokenType.COMPUTATIONAL_3,
            COBOLTokenType.BINARY, COBOLTokenType.PACKED_DECIMAL, COBOLTokenType.DISPLAY
        ):
            name = token.value
            self.advance()
            if self.debug:  # ✅ Fix #6
                print(f"WARNING: Using USAGE keyword '{name}' as identifier at line {token.line}")
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
            """
    Parse COBOL intrinsic function call
    
    Syntax:
        FUNCTION function-name(arg1, arg2, ...)
        FUNCTION function-name(arg1 arg2 ...)  <-- IBM COBOL allows space-separated!
    
    Where arguments can be:
        - Simple identifiers: FIELD1
        - Literals: "text", 123
        - Arithmetic expressions: VALUE1 / 12
        - Nested function calls: FUNCTION OTHER(X)
        - Parenthesized expressions: (INTEREST / 12)
    """
            token = self.current_token()
            self.consume(COBOLTokenType.FUNCTION)
            
            # Get function name (UPPER-CASE, NUMVAL-C, ANNUITY, etc.)
            if not self.match(COBOLTokenType.IDENTIFIER, COBOLTokenType.UPPER_CASE, COBOLTokenType.LOWER_CASE):
                self.error("Expected function name after FUNCTION keyword")
            
            function_name = self.current_token().value
            self.advance()
            
            # Parse arguments in parentheses
            arguments = []
            
            if self.match(COBOLTokenType.LPAREN):
                self.consume(COBOLTokenType.LPAREN)
                
                # Parse arguments (comma-separated OR space-separated)
                while not self.match(COBOLTokenType.RPAREN):
                    # Parse full arithmetic expression as argument
                    arg = self.parse_arithmetic_expression()
                    arguments.append(arg)
                    
                    # ✅ FIX: Accept either COMMA or just continue if we see another argument
                    if self.match(COBOLTokenType.COMMA):
                        self.advance()  # Consume the comma and continue
                    elif not self.match(COBOLTokenType.RPAREN):
                        # No comma, but not at closing paren yet
                        # Check if the next token could start an argument (IBM COBOL space-separated args)
                        if self.match(
                            COBOLTokenType.IDENTIFIER,
                            COBOLTokenType.NUMBER_LITERAL,
                            COBOLTokenType.STRING_LITERAL,
                            COBOLTokenType.LPAREN,  # Parenthesized expression
                            COBOLTokenType.FUNCTION,  # Nested function call
                            COBOLTokenType.MINUS  # Negative number
                        ):
                            # This looks like another argument, continue parsing (space-separated)
                            continue
                        else:
                            # Unknown token - error
                            self.error(
                                f"Expected comma, closing parenthesis, or another argument in function call. "
                                f"Got {self.current_token().type.name} '{self.current_token().value}'"
                            )
                
                self.consume(COBOLTokenType.RPAREN)
            
            return COBOLFunctionCall(
                function_name=function_name,
                arguments=arguments,
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