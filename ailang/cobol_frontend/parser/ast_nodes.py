#!/usr/bin/env python3

# Copyright (c) 2025 Sean Collins, 2 Paws Machine and Engineering. All rights reserved.
#
# Licensed under the Sean Collins Software License (SCSL). See the LICENSE file in the root directory of this project
# for the full terms and conditions, including restrictions on forking, corporate use, and permissions for private/teaching purposes.

"""
COBOL AST Node Definitions
"""

from typing import List, Optional, Dict , Union
from dataclasses import dataclass, field
from enum import Enum


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
    failed_programs: List[tuple] = None  # List of (line, error_msg) tuples
    line: int = 0
    column: int = 0


@dataclass
class COBOLProgram(COBOLASTNode):
    """Represents ONE COBOL program (one PROGRAM-ID)"""
    program_id: str
    data_division: Optional['COBOLDataDivision'] = None
    procedure_division: Optional['COBOLProcedureDivision'] = None
    contained_programs: List['COBOLProgram'] = field(default_factory=list)
    is_nested: bool = False
    metadata_lines: List[str] = field(default_factory=list)


@dataclass
class COBOLDataDivision(COBOLASTNode):
    working_storage: List['COBOLVariableDecl']
    file_section: Optional[List['COBOLFileDescriptor']] = None
    select_statements: Optional[List['COBOLSelectStatement']] = None
    linkage_section: Optional[List['COBOLVariableDecl']] = None


@dataclass
class COBOLSelectStatement(COBOLASTNode):
    """SELECT statement from FILE-CONTROL - captures ALL clauses"""
    file_name: str
    assign_to: str
    is_optional: bool = False
    organization: Optional[str] = None
    access_mode: Optional[str] = None
    record_key: Optional[str] = None
    alternate_keys: List[str] = field(default_factory=list)
    file_status: Optional[str] = None
    reserve_areas: Optional[int] = None
    padding_character: Optional[str] = None


@dataclass
class COBOLFileDescriptor(COBOLASTNode):
    """FD entry - Complete file descriptor with ALL clauses"""
    file_name: str
    records: List['COBOLVariableDecl'] = field(default_factory=list)
    block_contains: Optional[int] = None
    block_contains_to: Optional[int] = None
    record_contains: Optional[int] = None
    record_contains_to: Optional[int] = None
    record_varying: Optional[dict] = None
    label_records: Optional[str] = None
    value_of: dict[str, str] = field(default_factory=dict)
    data_records: List[str] = field(default_factory=list)
    linage: Optional[dict] = None
    code_set: Optional[str] = None


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
    usage_type: Optional[str] = None
    is_signed: bool = False
    children: List['COBOLVariableDecl'] = field(default_factory=list)
    redefines_target: Optional[str] = None
    occurs_min: Optional[int] = None
    occurs_max: Optional[int] = None
    depending_on: Optional[str] = None
    is_external: bool = False
    is_global: bool = False
    index_names: List[str] = field(default_factory=list) # Names of associated indexes
    sign_info: Optional[dict] = None  # SIGN clause info (position, is_separate)

@dataclass
class COBOLReferenceModification(COBOLASTNode):
    """Reference modification: identifier(start:length)
    
    COBOL substring operation (1-based indexing):
    - identifier(start:length) - substring from start, length chars
    - identifier(start:)       - from start to end
    - identifier(:length)      - first length characters
    - array(i,j)(start:length) - CHAINED: get array element, then substring
    
    Examples:
    - WS-NAME(1:10)           → substring from position 1, length 10
    - WS-NAME(5:)             → from position 5 to end  
    - WS-NAME(:3)             → first 3 characters
    - TABLE-1(3, 2)(2:5)      → CHAINED: array[3,2] then substring(2,5)
    - TEST-DATA(WS-START: WS-LEN) → using variables
    """
    identifier: Union[str, COBOLASTNode]  # Can be string OR array access node
    start: Optional[COBOLASTNode]         # Starting position (1-based), None = position 1
    length: Optional[COBOLASTNode]        # Length in chars, None = "to end"
    line: int
    column: int


@dataclass
class COBOLProcedureDivision(COBOLASTNode):
    paragraphs: List['COBOLParagraph']  # Flat paragraphs
    sections: List['COBOLSection'] = field(default_factory=list)  # NEW!
    using_params: Optional[List[str]] = None


@dataclass
class COBOLSection(COBOLASTNode):
    name: str
    segment_number: Optional[int] = None  # ← ADD THIS
    paragraphs: List['COBOLParagraph'] = field(default_factory=list)

@dataclass
class COBOLPerformInline(COBOLASTNode):
    statements: List[COBOLASTNode]

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
    thru_paragraph: Optional[str] = None  
    until_condition: Optional[COBOLASTNode] = None


@dataclass
class COBOLPerformTimes(COBOLASTNode):
    """PERFORM paragraph-name N TIMES or PERFORM N TIMES ... END-PERFORM"""
    times_expr: COBOLASTNode
    paragraph_name: Optional[str] = None
    statements: Optional[List[COBOLASTNode]] = None


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
    program_name: str
    parameters: List[str] = field(default_factory=list)
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
    is_program: bool = False


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
    indices: List[COBOLASTNode]


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
    """STRING statement for concatenation"""
    source_fields: List[COBOLASTNode]
    delimiters: List[Optional[COBOLASTNode]]
    target: str
    pointer: Optional[str] = None


@dataclass
class COBOLUnstring(COBOLASTNode):
    """UNSTRING statement for splitting"""
    source: COBOLASTNode
    delimiters: List[COBOLASTNode]
    delimiter_all_flags: List[bool]
    targets: List[str]
    count: Optional[str] = None


@dataclass
class COBOLInspect(COBOLASTNode):
    """INSPECT statement for string manipulation"""
    target: str
    operation: str
    pattern: COBOLASTNode
    replacement: Optional[COBOLASTNode] = None
    counter: Optional[str] = None
    scope: Optional[str] = None
    target_subscript: Optional[COBOLASTNode] = None  
    after_before: Optional[str] = None  
    initial_string: Optional[COBOLASTNode] = None  
    
    
@dataclass
class COBOLMerge(COBOLASTNode):
    """AST node for MERGE statement"""
    target_file: str
    sort_keys: List[Dict]
    input_files: List[str]
    collating_sequence: Optional[str] = None
    output_procedure: Optional[str] = None
    output_procedure_thru: Optional[str] = None
    giving_file: Optional[str] = None