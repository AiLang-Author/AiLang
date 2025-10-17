print("[MODULE] division_parsers.py loading...")
#!/usr/bin/env python3

# Copyright (c) 2025 Sean Collins, 2 Paws Machine and Engineering. All rights reserved.
#
# Licensed under the Sean Collins Software License (SCSL). See the LICENSE file in the root directory of this project
# for the full terms and conditions, including restrictions on forking, corporate use, and permissions for private/teaching purposes.

"""
COBOL Division Parsers - Handles IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE divisions
"""

import re
from typing import List, Optional
from cobol_frontend.cobol_lexer import Token, COBOLTokenType 
from .ast_nodes import *
from .parser_core import COBOLMultiProgramParser, ParseContext, ParserError


def parse_single_program(self) -> COBOLProgram:
    print(f"[DEBUG] parse_single_program called at line {self.current_token().line}")
    """Parse ONE COBOL program - all divisions now optional except IDENTIFICATION"""
    token = self.current_token()
    self._current_metadata = []

    print(f"[DEBUG] About to call parse_identification_division, function is: {self.parse_identification_division}")
    program_id = self.parse_identification_division()
    
    select_statements = []
    if self.current_token() and self.current_token().type == COBOLTokenType.ENVIRONMENT:
        select_statements = self.parse_environment_division()
    
    data_division = None
    if self.current_token() and self.current_token().type == COBOLTokenType.DATA:
        data_division = self.parse_data_division()
        if data_division and select_statements:
            data_division.select_statements = select_statements

    procedure_division = None
    if self.current_token() and self.current_token().type == COBOLTokenType.PROCEDURE:
        procedure_division = self.parse_procedure_division()
    elif self.match(COBOLTokenType.EOF):
        procedure_division = None

    contained_programs = []
    
    # ✅ ENHANCED FIX: Detect program boundaries intelligently
    # Check for boundaries BEFORE trying to parse END PROGRAM
    # This handles old COBOL without END PROGRAM statements
    
    if self.match(COBOLTokenType.EOF):
        # At end of file - program is complete
        if self.debug:
            print(f"[BOUNDARY] Program {program_id} ended at EOF")
    
    elif self.match(COBOLTokenType.IDENTIFICATION):
        # ✅ NEW: Next program starting! Current program must be done.
        # This handles programs without END PROGRAM (common in old COBOL)
        # Don't consume the token - let parse_all_programs handle it
        if self.debug:
            print(f"[BOUNDARY] Program {program_id} ended: next IDENTIFICATION DIVISION detected")
    
    elif self.match(COBOLTokenType.NIST_END_OF, COBOLTokenType.NIST_HEADER):
        # NIST marker indicates program boundary
        if self.debug:
            print(f"[BOUNDARY] Program {program_id} ended at NIST marker")
    
    else:
        # Try to parse explicit END PROGRAM statement
        end_program_found = self.parse_end_program(program_id)
        
        if end_program_found:
            if self.debug:
                print(f"[BOUNDARY] Program {program_id} ended with END PROGRAM statement")
        else:
            # No END PROGRAM found - this is OK for some COBOL programs
            # but log a warning for debugging
            if self.debug:
                print(f"[WARNING] Program {program_id} has no clear END PROGRAM at line {self.current_token().line}")
    
    return COBOLProgram(
        program_id=program_id,
        data_division=data_division,
        procedure_division=procedure_division,
        contained_programs=contained_programs,
        is_nested=False,
        metadata_lines=self._current_metadata,
        line=token.line,
        column=token.column
    )


def parse_identification_division(self) -> str:
    """Parse IDENTIFICATION DIVISION and return PROGRAM-ID
    
    PROGRAM-ID syntax:
    - PROGRAM-ID. program-name.
    - PROGRAM-ID. program-name IS COMMON PROGRAM.
    - PROGRAM-ID. program-name IS INITIAL PROGRAM.
    
    TRUTH TABLE for IS keyword:
    | Token Type | Token Value | Action |
    |------------|-------------|--------|
    | IS         | -           | Skip, then skip attribute |
    | IDENTIFIER | "IS"        | Skip, then skip attribute |
    | IDENTIFIER | "COMMON"    | Skip (no IS present) |
    | IDENTIFIER | "INITIAL"   | Skip (no IS present) |
    | PERIOD     | -           | Done, consume it |
    """
    print(f"[DEBUG] parse_identification_division called at line {self.current_token().line}")
    
    self.consume(COBOLTokenType.IDENTIFICATION)
    self.consume(COBOLTokenType.DIVISION)
    self.consume(COBOLTokenType.PERIOD)
    
    print(f"[DEBUG] After IDENTIFICATION DIVISION., current token: {self.current_token().type.name} = '{self.current_token().value}'")
    
    # ✅ SPECIAL CASE: Check for COMMENT ENTRY paragraph
    if self.current_token() and self.current_token().type == COBOLTokenType.IDENTIFIER:
        print(f"[DEBUG] Current token is IDENTIFIER, checking for comment entry...")
        if self._is_comment_entry_start():
            print(f"[COMMENT_ENTRY] Detected at line {self.current_token().line} - skipping to next division")
            self._skip_comment_entry_paragraph()
            return "COMMENT_ENTRY_PLACEHOLDER"
        else:
            print(f"[DEBUG] Not a comment entry, proceeding normally")
    
    # Check if next token is ENVIRONMENT/DATA/PROCEDURE instead of PROGRAM-ID
    if self.current_token().type in (COBOLTokenType.ENVIRONMENT, COBOLTokenType.DATA, 
                                      COBOLTokenType.PROCEDURE):
        print(f"[COMMENT_ENTRY] Found {self.current_token().type.name} instead of PROGRAM-ID - this is a comment entry!")
        self._skip_comment_entry_paragraph()
        return "COMMENT_ENTRY_PLACEHOLDER"
    
    # Normal path: expect PROGRAM-ID
    print(f"[DEBUG] Expecting PROGRAM-ID...")
    self.consume(COBOLTokenType.PROGRAM_ID)
    self.consume(COBOLTokenType.PERIOD)
    
    program_id = self.consume(COBOLTokenType.IDENTIFIER).value
    print(f"[DEBUG] Got program-id: {program_id}")
    
    # TRUTH TABLE: Handle optional IS keyword and program attributes
    # Current token could be: IS (token type), IS (identifier), COMMON, INITIAL, RECURSIVE, or PERIOD
    
    # Rule 1: Check for IS as a token type (COBOLTokenType.IS)
    if self.match(COBOLTokenType.IS):
        print(f"[DEBUG] Found IS token (token type)")
        self.advance()  # Skip IS
        # Now expect attribute: COMMON, INITIAL, RECURSIVE, or PROGRAM
        if self.match(COBOLTokenType.IDENTIFIER):
            attr = self.current_token().value.upper()
            print(f"[DEBUG] Program attribute: {attr}")
            self.advance()
            # If attribute is COMMON/INITIAL, might be followed by PROGRAM keyword
            if attr in ['COMMON', 'INITIAL'] and self.match(COBOLTokenType.IDENTIFIER):
                if self.current_token().value.upper() == 'PROGRAM':
                    print(f"[DEBUG] Found PROGRAM keyword after {attr}")
                    self.advance()
    
    # Rule 2: Check for IS as an identifier (fallback if not lexed as IS token)
    elif self.match(COBOLTokenType.IDENTIFIER):
        ident_value = self.current_token().value.upper()
        
        if ident_value == 'IS':
            print(f"[DEBUG] Found IS identifier")
            self.advance()  # Skip IS
            # Now expect attribute
            if self.match(COBOLTokenType.IDENTIFIER):
                attr = self.current_token().value.upper()
                print(f"[DEBUG] Program attribute: {attr}")
                self.advance()
                # If attribute is COMMON/INITIAL, might be followed by PROGRAM keyword
                if attr in ['COMMON', 'INITIAL'] and self.match(COBOLTokenType.IDENTIFIER):
                    if self.current_token().value.upper() == 'PROGRAM':
                        print(f"[DEBUG] Found PROGRAM keyword after {attr}")
                        self.advance()
        
        elif ident_value in ['COMMON', 'INITIAL', 'RECURSIVE']:
            # Attribute without IS keyword (less common but valid)
            print(f"[DEBUG] Program attribute without IS: {ident_value}")
            self.advance()
            if ident_value in ['COMMON', 'INITIAL'] and self.match(COBOLTokenType.IDENTIFIER):
                if self.current_token().value.upper() == 'PROGRAM':
                    print(f"[DEBUG] Found PROGRAM keyword after {ident_value}")
                    self.advance()
    
    # Rule 3: Consume the terminating PERIOD
    if not self.match(COBOLTokenType.PERIOD):
        print(f"[ERROR] Expected PERIOD after PROGRAM-ID, got {self.current_token().type.name} = '{self.current_token().value}'")
    
    self.consume(COBOLTokenType.PERIOD)
    
    # ============================================================================
    # TRUTH TABLE: Parse optional metadata paragraphs (AUTHOR, DATE-WRITTEN, etc.)
    # ============================================================================
    # State Machine for Metadata Parsing:
    # 
    # Current Token Type | Next Token Type | Action
    # -------------------|-----------------|----------------------------------------
    # AUTHOR/etc.        | PERIOD          | Advance, advance, collect content
    # AUTHOR/etc.        | NEWLINE         | Advance, collect content
    # AUTHOR/etc.        | Other           | Advance, collect content
    # IDENTIFIER         | (in metadata)   | Collect as metadata content, advance
    # COMMENT            | -               | Advance (skip)
    # NEWLINE            | AUTHOR/etc.     | Advance, continue metadata loop
    # NEWLINE            | ENV/DATA/PROC   | STOP - next division starts
    # NEWLINE            | Other           | Advance
    # ENV/DATA/PROC/ID   | -               | STOP - next division starts
    # EOF                | -               | STOP - end of file
    # ============================================================================
    
    metadata = []
    metadata_paragraphs = {
        'AUTHOR': COBOLTokenType.AUTHOR,
        'INSTALLATION': COBOLTokenType.INSTALLATION,
        'DATE-WRITTEN': COBOLTokenType.DATE_WRITTEN,
        'DATE-COMPILED': COBOLTokenType.DATE_COMPILED,
        'SECURITY': COBOLTokenType.SECURITY,
        'REMARKS': COBOLTokenType.REMARKS
    }
    
    metadata_keyword_names = {'AUTHOR', 'INSTALLATION', 'DATE-WRITTEN', 'DATE-COMPILED', 
                              'SECURITY', 'REMARKS'}
    
    # Helper function to check if current token is a metadata paragraph
    def is_metadata_paragraph():
        if self.match(*metadata_paragraphs.values()):
            return True
        # ✅ CRITICAL FIX: Also check for IDENTIFIER tokens with metadata names
        if self.match(COBOLTokenType.IDENTIFIER):
            return self.current_token().value.upper() in metadata_keyword_names
        return False
    
    # Outer loop: Process each metadata paragraph
    while is_metadata_paragraph():
        token = self.current_token()
        metadata_type = token.type
        metadata_keyword = token.value.upper()
        print(f"[DEBUG] Found metadata paragraph: {metadata_keyword} at line {token.line}")
        self.advance()  # Consume the metadata keyword (AUTHOR, etc.)
        
        # Optional PERIOD after metadata keyword
        if self.match(COBOLTokenType.PERIOD):
            self.advance()
        
        # Inner loop: Collect metadata content line(s)
        metadata_line = []
        
        while True:
            current = self.current_token()
            
            # TRUTH TABLE ROW 1: Division boundary - STOP (must be followed by DIVISION keyword!)
            if self.match(COBOLTokenType.ENVIRONMENT, COBOLTokenType.DATA, 
                        COBOLTokenType.PROCEDURE, COBOLTokenType.IDENTIFICATION):
                # CRITICAL: Check if followed by DIVISION keyword
                peek = self.peek_token(1)
                if peek and peek.type == COBOLTokenType.DIVISION:
                    print(f"[DEBUG] Metadata parse stopped at division: {current.type.name}")
                    break
                else:
                    # Just the word "DATA" or "ENVIRONMENT" in text - treat as content
                    print(f"[DEBUG] Found '{current.value}' in metadata text (not division)")
                    metadata_line.append(current.value)
                    self.advance()
                    continue
            
            # TRUTH TABLE ROW 2: EOF - STOP
            if self.match(COBOLTokenType.EOF):
                print(f"[DEBUG] Metadata parse stopped at EOF")
                break
            
            # TRUTH TABLE ROW 3: Next metadata paragraph - STOP
            if is_metadata_paragraph():
                print(f"[DEBUG] Metadata parse stopped at next metadata: {current.type.name}")
                break
            
            # TRUTH TABLE ROW 4: COMMENT - Skip it
            if self.match(COBOLTokenType.COMMENT):
                self.advance()
                continue
            
            # TRUTH TABLE ROW 5: NEWLINE - Check what's after it
            if self.match(COBOLTokenType.NEWLINE):
                # Peek ahead to see if next division starts
                next_tok = self.peek_token(1)
                if next_tok and next_tok.type in (COBOLTokenType.ENVIRONMENT, 
                                                   COBOLTokenType.DATA, 
                                                   COBOLTokenType.PROCEDURE,
                                                   COBOLTokenType.IDENTIFICATION):
                    print(f"[DEBUG] Metadata parse: NEWLINE before {next_tok.type.name} - STOP")
                    self.advance()  # Consume the newline
                    break
                else:
                    # Just a newline in the metadata content - keep going
                    self.advance()
                    continue
            
            # TRUTH TABLE ROW 6: Regular token - Collect as metadata content
            metadata_line.append(current.value)
            self.advance()
        
        if metadata_line:
            metadata.append(" ".join(metadata_line))
    
    self._current_metadata = metadata
    
    print(f"[DEBUG] Finished parse_identification_division at position {self.pos}")
    print(f"[DEBUG] Current token after ID division: {self.current_token().type.name} at line {self.current_token().line}")
    
    return program_id



def parse_environment_division(self):
    """Parse ENVIRONMENT DIVISION - capture SELECT statements"""
    self.consume(COBOLTokenType.ENVIRONMENT)
    self.consume(COBOLTokenType.DIVISION)
    self.consume(COBOLTokenType.PERIOD)
    
    select_statements = []
    
    while not self.match(COBOLTokenType.EOF):
        if self.current_token() and self.current_token().value == 'CONFIGURATION':
            self.advance()
            if self.match(COBOLTokenType.SECTION):
                self.advance()
            if self.match(COBOLTokenType.PERIOD):
                self.advance()
            while not self.match(COBOLTokenType.DATA, COBOLTokenType.PROCEDURE):
                if self.current_token() and self.current_token().value in ['INPUT-OUTPUT', 'FILE-CONTROL']:
                    break
                self.advance()
        
        if self.current_token() and self.current_token().value == 'INPUT-OUTPUT':
            self.advance()
            if self.match(COBOLTokenType.SECTION):
                self.advance()
            if self.match(COBOLTokenType.PERIOD):
                self.advance()
        
        if self.current_token() and self.current_token().value == 'FILE-CONTROL':
            self.advance()
            if self.match(COBOLTokenType.PERIOD):
                self.advance()
            
            while self.current_token() and self.current_token().value == 'SELECT':
                select = self.parse_select_statement()
                if select:
                    select_statements.append(select)
        
        if self.match(COBOLTokenType.DATA, COBOLTokenType.PROCEDURE):
            break
        
        if self.match(COBOLTokenType.EOF):
            break
        
        self.advance()
    
    return select_statements


def parse_select_statement(self) -> COBOLSelectStatement:
    """Parse SELECT statement with ALL clauses"""
    token = self.current_token()
    self.advance()
    
    is_optional = False
    if self.current_token() and self.current_token().value == 'OPTIONAL':
        is_optional = True
        self.advance()
    
    file_name = self.consume(COBOLTokenType.IDENTIFIER).value
    
    assign_to = None
    if self.current_token() and self.current_token().value == 'ASSIGN':
        self.advance()
        if self.match(COBOLTokenType.TO):
            self.advance()
        if self.match(COBOLTokenType.IDENTIFIER):
            assign_to = self.current_token().value
            self.advance()
    
    organization = None
    access_mode = None
    record_key = None
    file_status = None
    
    while not self.match(COBOLTokenType.PERIOD):
        if self.match(COBOLTokenType.EOF):
            break
        
        token_val = self.current_token().value if self.current_token() else None
        
        if token_val == 'ORGANIZATION':
            self.advance()
            if self.current_token() and self.current_token().value == 'IS':
                self.advance()
            if self.match(COBOLTokenType.IDENTIFIER):
                organization = self.current_token().value
                self.advance()
        
        elif token_val == 'ACCESS':
            self.advance()
            if self.current_token() and self.current_token().value == 'MODE':
                self.advance()
            if self.current_token() and self.current_token().value == 'IS':
                self.advance()
            if self.match(COBOLTokenType.IDENTIFIER):
                access_mode = self.current_token().value
                self.advance()
        
        elif token_val == 'RECORD':
            self.advance()
            if self.current_token() and self.current_token().value == 'KEY':
                self.advance()
            if self.current_token() and self.current_token().value == 'IS':
                self.advance()
            if self.match(COBOLTokenType.IDENTIFIER):
                record_key = self.current_token().value
                self.advance()
        
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
        select_statements=None,
        linkage_section=linkage_section_vars if linkage_section_vars else None,
        line=token.line,
        column=token.column
    )


def parse_section(self) -> COBOLSection:
    """Parse a SECTION with all its paragraphs (COBOL-74/85 compatible)
    
    TRUTH TABLE: SECTION Declaration Patterns
    ┌─────────────────────────────────┬──────────────────────────────┐
    │ Pattern                         │ Validity (COBOL-74/85)       │
    ├─────────────────────────────────┼──────────────────────────────┤
    │ SECTION-NAME SECTION.           │ YES (both)                   │
    │ SECTION-NAME SECTION 00.        │ YES (COBOL-74 segmentation)  │
    │ 00 SECTION 00.                  │ YES (segment number as name) │
    │ IDENTIFIER SECTION NUMBER.      │ Parse all 3 tokens           │
    └─────────────────────────────────┴──────────────────────────────┘
    
    TRUTH TABLE: Token Sequence After SECTION Keyword
    ┌────────────────┬──────────────────────────────────────────────┐
    │ Next Token     │ Action                                       │
    ├────────────────┼──────────────────────────────────────────────┤
    │ PERIOD         │ Done - no segment number                     │
    │ NUMBER_LITERAL │ Consume segment number, then expect PERIOD   │
    │ Other          │ Error - unexpected token                     │
    └────────────────┴──────────────────────────────────────────────┘
    """
    token = self.current_token()
    
    # ═══════════════════════════════════════════════════════════════
    # STEP 1: Parse section name (can be identifier OR number)
    # ═══════════════════════════════════════════════════════════════
    if self.match(COBOLTokenType.IDENTIFIER):
        section_name_token = self.consume(COBOLTokenType.IDENTIFIER)
    elif self.match(COBOLTokenType.NUMBER_LITERAL):
        # COBOL-74: Section name can be just a number (00, 01, etc.)
        section_name_token = self.consume(COBOLTokenType.NUMBER_LITERAL)
    else:
        self.error(f"Expected section name (identifier or number)")
    
    section_name = section_name_token.value
    
    # ═══════════════════════════════════════════════════════════════
    # STEP 2: Consume SECTION keyword
    # ═══════════════════════════════════════════════════════════════
    self.consume(COBOLTokenType.SECTION)
    
    # ═══════════════════════════════════════════════════════════════
    # STEP 3: Check for optional segment number (COBOL-74 feature)
    # TRUTH TABLE: What comes after SECTION?
    # ═══════════════════════════════════════════════════════════════
    segment_number = None
    
    current = self.current_token()
    
    if current.type == COBOLTokenType.NUMBER_LITERAL:
        # COBOL-74 segmentation: SECTION-NAME SECTION 00.
        segment_number = int(self.consume(COBOLTokenType.NUMBER_LITERAL).value)
        if self.debug:
            print(f"[PARSE_SECTION] Found segment number: {segment_number}")
    elif current.type == COBOLTokenType.PERIOD:
        # Standard: SECTION-NAME SECTION.
        pass
    else:
        # Unexpected token after SECTION
        self.error(f"Expected PERIOD or segment number after SECTION, got {current.type.name}")
    
    # ═══════════════════════════════════════════════════════════════
    # STEP 4: Consume terminating PERIOD
    # ═══════════════════════════════════════════════════════════════
    self.consume(COBOLTokenType.PERIOD)
    
    # ═══════════════════════════════════════════════════════════════
    # STEP 5: Parse all paragraphs until next section/division/boundary
    # ═══════════════════════════════════════════════════════════════
    paragraphs = []
    
    while not self.match(COBOLTokenType.EOF):
        # ───────────────────────────────────────────────────────────
        # TRUTH TABLE: Token Type → Action
        # ───────────────────────────────────────────────────────────
        current = self.current_token()
        
        # ROW 1: NOISE tokens → Skip
        if self.match(COBOLTokenType.COMMENT, COBOLTokenType.NEWLINE):
            self.advance()
            continue
        
        # ROW 2: DIVISION boundaries → Stop parsing section
        if self.match(COBOLTokenType.IDENTIFICATION, COBOLTokenType.ENVIRONMENT,
                      COBOLTokenType.DATA, COBOLTokenType.PROCEDURE,
                      COBOLTokenType.DIVISION):
            break
        
        # ROW 3: PROGRAM boundaries → Stop parsing section
        if self.match(COBOLTokenType.END_PROGRAM):
            break
        
        if self.match(COBOLTokenType.END):
            next_tok = self.peek_token()
            if next_tok and next_tok.type == COBOLTokenType.PROGRAM:
                break
        
        # ROW 4: IDENTIFIER → Could be paragraph or next section
        if self.match(COBOLTokenType.IDENTIFIER):
            next_token = self.peek_token()
            
            # ROW 4a: IDENTIFIER SECTION → Next section, stop
            if next_token and next_token.type == COBOLTokenType.SECTION:
                break
            
            # ROW 4b: IDENTIFIER PERIOD → Paragraph declaration
            if next_token and next_token.type == COBOLTokenType.PERIOD:
                identifier_value = current.value.upper()
                
                # Skip statement keywords (they're not paragraphs)
                if identifier_value not in ['STOP', 'GOBACK', 'EXIT', 'DISPLAY',
                                            'MOVE', 'ADD', 'SUBTRACT', 'MULTIPLY',
                                            'DIVIDE', 'COMPUTE', 'ACCEPT', 'PERFORM',
                                            'IF', 'ELSE', 'EVALUATE', 'CALL']:
                    para = self.parse_paragraph()
                    if para:
                        paragraphs.append(para)
                    continue
        
        # ROW 5: NUMBER_LITERAL → Could be next numbered section
        if self.match(COBOLTokenType.NUMBER_LITERAL):
            next_token = self.peek_token()
            
            # NUMBER SECTION → Next numbered section, stop
            if next_token and next_token.type == COBOLTokenType.SECTION:
                break
        
        # ROW 6: Unknown token → Skip to prevent infinite loop
        if not self.match(COBOLTokenType.EOF):
            if self.debug:
                print(f"[PARSE_SECTION] Skipping unexpected token: {current.type.name} = {current.value}")
            self.advance()
    
    # ═══════════════════════════════════════════════════════════════
    # STEP 6: Return completed section
    # ═══════════════════════════════════════════════════════════════
    return COBOLSection(
        name=section_name,
        segment_number=segment_number,  # Store segment number for COBOL-74
        paragraphs=paragraphs,
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
        
        if self.current_token() and self.current_token().value == 'FD':
            fd = self.parse_fd_entry()
            if fd:
                file_descriptors.append(fd)
        else:
            self.advance()
    
    return file_descriptors


def parse_fd_entry(self) -> COBOLFileDescriptor:
    """Parse FD entry with ALL clauses"""
    token = self.current_token()
    self.advance()
    
    file_name = self.consume(COBOLTokenType.IDENTIFIER).value
    
    if self.match(COBOLTokenType.PERIOD):
        self.advance()
    
    fd_data = {
        'block_contains': None,
        'block_contains_to': None,
        'record_contains': None,
        'record_contains_to': None,
        'label_records': None,
        'data_records': [],
    }
    
    while not self.match(COBOLTokenType.LEVEL_NUMBER):
        if self.match(COBOLTokenType.EOF):
            break
        
        token_val = self.current_token().value if self.current_token() else None
        
        if token_val == 'BLOCK':
            self.advance()
            if self.current_token() and self.current_token().value == 'CONTAINS':
                self.advance()
            
            if self.match(COBOLTokenType.NUMBER_LITERAL):
                fd_data['block_contains'] = int(self.current_token().value)
                self.advance()
            
            if self.current_token() and self.current_token().value == 'TO':
                self.advance()
                if self.match(COBOLTokenType.NUMBER_LITERAL):
                    fd_data['block_contains_to'] = int(self.current_token().value)
                    self.advance()
            
            while not self.match(COBOLTokenType.PERIOD):
                if self.match(COBOLTokenType.LEVEL_NUMBER):
                    break
                self.advance()
        
        elif token_val == 'RECORD':
            self.advance()
            if self.current_token() and self.current_token().value == 'CONTAINS':
                self.advance()
            
            if self.match(COBOLTokenType.NUMBER_LITERAL):
                fd_data['record_contains'] = int(self.current_token().value)
                self.advance()
            
            if self.current_token() and self.current_token().value == 'TO':
                self.advance()
                if self.match(COBOLTokenType.NUMBER_LITERAL):
                    fd_data['record_contains_to'] = int(self.current_token().value)
                    self.advance()
            
            while not self.match(COBOLTokenType.PERIOD):
                if self.match(COBOLTokenType.LEVEL_NUMBER):
                    break
                self.advance()
        
        else:
            self.advance()
        
        if self.match(COBOLTokenType.PERIOD):
            self.advance()
    
    records = []
    while self.match(COBOLTokenType.LEVEL_NUMBER):
        level_token = self.current_token()
        if level_token.value == '01':
            record = self.parse_variable_decl()
            if record:
                records.append(record)
        else:
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


def parse_working_storage(self) -> List[COBOLVariableDecl]:
    """Parse WORKING-STORAGE SECTION with truth table-based division boundary detection
    
    TRUTH TABLE: Variable Declaration vs Division Boundary Detection
    ┌─────────────────┬──────────────────┬─────────────────┬────────────────────────────┐
    │ Token[0]        │ Token[1]         │ Token[2]        │ Action                     │
    ├─────────────────┼──────────────────┼─────────────────┼────────────────────────────┤
    │ LEVEL_NUMBER    │ IDENTIFIER       │ PIC/PERIOD/etc  │ Parse variable (normal)    │
    │ LEVEL_NUMBER    │ PROCEDURE        │ DIVISION        │ STOP (division boundary)   │
    │ LEVEL_NUMBER    │ DATA             │ DIVISION        │ STOP (division boundary)   │
    │ LEVEL_NUMBER    │ ENVIRONMENT      │ DIVISION        │ STOP (division boundary)   │
    │ LEVEL_NUMBER    │ IDENTIFICATION   │ DIVISION        │ STOP (division boundary)   │
    │ LEVEL_NUMBER    │ PROCEDURE        │ PIC             │ Parse (PROCEDURE = name)   │
    │ LEVEL_NUMBER    │ COPY             │ PIC             │ Parse (COPY = name)        │
    │ LEVEL_NUMBER    │ COPY             │ IDENTIFIER      │ Parse (COPY statement)     │
    │ EOF             │ -                │ -               │ STOP (end of file)         │
    │ Other           │ -                │ -               │ STOP (unexpected token)    │
    └─────────────────┴──────────────────┴─────────────────┴────────────────────────────┘
    
    CRITICAL PATTERNS:
    - "77 PROCEDURE DIVISION." → STOP (not "77 PROCEDURE PIC X")
    - "77 COPY K1W02." → Parse as COPY statement (keyword as name + copybook)
    - "77 COPY PIC X." → Parse as variable (keyword as name + PIC clause)
    
    Examples from NIST failures:
    - Line 169431: "77 PROCEDURE DIVISION." → Hit boundary, stop parsing
    - Line 256731: "77 COPY K1W02." → This is actually a COPY statement, not variable
    """
    self.consume(COBOLTokenType.WORKING_STORAGE)
    self.consume(COBOLTokenType.SECTION)
    self.consume(COBOLTokenType.PERIOD)
    
    variables = []
    max_iterations = 10000  # Safety limit for infinite loop protection
    iteration = 0
    
    while iteration < max_iterations:
        iteration += 1
        
        # ═══════════════════════════════════════════════════════════════════════
        # TRUTH TABLE ROW 0: EOF → STOP
        # ═══════════════════════════════════════════════════════════════════════
        if self.match(COBOLTokenType.EOF):
            break
        
        # ═══════════════════════════════════════════════════════════════════════
        # TRUTH TABLE ROW 1: No LEVEL_NUMBER → STOP (end of section)
        # ═══════════════════════════════════════════════════════════════════════
        if not self.match(COBOLTokenType.LEVEL_NUMBER):
            break
        
        # ═══════════════════════════════════════════════════════════════════════
        # TRUTH TABLE ROWS 2-5: Check for division boundary pattern
        # Pattern: LEVEL_NUMBER + DIVISION_KEYWORD + DIVISION
        # ═══════════════════════════════════════════════════════════════════════
        
        # Save position for lookahead
        saved_pos = self.pos
        
        # Token[0]: LEVEL_NUMBER (already matched)
        level_token = self.current_token()
        self.advance()
        
        # Token[1]: Check what follows the level number
        token1 = self.current_token()
        
        # Division keywords that can appear after level number
        division_keywords = {
            COBOLTokenType.PROCEDURE,
            COBOLTokenType.DATA,
            COBOLTokenType.ENVIRONMENT,
            COBOLTokenType.IDENTIFICATION
        }
        
        # TRUTH TABLE ROW 2-5: LEVEL_NUMBER + DIVISION_KEYWORD + ?
        if token1.type in division_keywords:
            # Look ahead one more token
            token2 = self.peek_token(1)
            
            # TRUTH TABLE: Division boundary detected
            if token2 and token2.type in (COBOLTokenType.DIVISION, COBOLTokenType.SECTION):
                if self.debug:
                    print(f"[PARSE_WS] Division boundary detected:")
                    print(f"  Level: {level_token.value}")
                    print(f"  Token1: {token1.type.name} = '{token1.value}'")
                    print(f"  Token2: {token2.type.name} = '{token2.value}'")
                    print(f"  → STOP parsing WORKING-STORAGE")
                
                # Restore position to level number and exit loop
                self.pos = saved_pos
                break
            
            # TRUTH TABLE ROW 6-7: Keyword followed by PIC or other valid clause
            # This means the keyword is being used as a variable name
            # Example: "77 PROCEDURE PIC X(10)."
            else:
                if self.debug:
                    print(f"[PARSE_WS] Keyword as variable name:")
                    print(f"  {level_token.value} {token1.value} {token2.type.name if token2 else 'EOF'}")
                
                # Restore and parse as normal variable
                self.pos = saved_pos
                # Continue to variable parsing below
        
        # TRUTH TABLE ROW 8: LEVEL_NUMBER + IDENTIFIER (standard case)
        else:
            # Restore position and parse normally
            self.pos = saved_pos
        
        # ═══════════════════════════════════════════════════════════════════════
        # PARSE VARIABLE: Try to parse, but catch division boundary errors
        # ═══════════════════════════════════════════════════════════════════════
        try:
            var = self.parse_variable_decl()
            if var:
                variables.append(var)
            else:
                # parse_variable_decl returned None - might be at boundary
                break
        
        except Exception as e:
            error_msg = str(e)
            
            # Check if error is due to hitting division boundary
            # Pattern: "Expected PIC clause for 'PROCEDURE' ... Current token: DIVISION"
            if "Expected PIC clause" in error_msg and "DIVISION" in error_msg:
                if self.debug:
                    print(f"[PARSE_WS] Caught division boundary error:")
                    print(f"  {error_msg[:100]}")
                    print(f"  → Stopping WORKING-STORAGE parse")
                
                # This is expected - we hit a division boundary
                # Exit the loop gracefully
                break
            
            # Check for other boundary indicators
            elif any(keyword in error_msg for keyword in ["PROCEDURE", "DATA", "ENVIRONMENT", "IDENTIFICATION"]):
                if "DIVISION" in error_msg or "SECTION" in error_msg:
                    if self.debug:
                        print(f"[PARSE_WS] Detected division/section keyword in error")
                    break
            
            # Not a division boundary error - re-raise
            else:
                raise
    
    if iteration >= max_iterations:
        print(f"[WARNING] parse_working_storage hit iteration limit")
    
    return variables



def parse_linkage_section(self) -> COBOLLinkageSection:
    """Parse LINKAGE SECTION (uses same truth table as WORKING-STORAGE)
    
    TRUTH TABLE: Same as parse_working_storage - see above
    """
    token = self.current_token()
    self.consume(COBOLTokenType.LINKAGE)
    self.consume(COBOLTokenType.SECTION)
    if self.requires_period():
        self.consume(COBOLTokenType.PERIOD)
    
    variables = []
    max_iterations = 10000
    iteration = 0
    
    while iteration < max_iterations:
        iteration += 1
        
        # ROW 0: EOF
        if self.match(COBOLTokenType.EOF):
            break
        
        # ROW 1: No LEVEL_NUMBER
        if not self.match(COBOLTokenType.LEVEL_NUMBER):
            break
        
        # ROWS 2-5: Division boundary check
        saved_pos = self.pos
        level_token = self.current_token()
        self.advance()
        token1 = self.current_token()
        
        division_keywords = {
            COBOLTokenType.PROCEDURE,
            COBOLTokenType.DATA,
            COBOLTokenType.ENVIRONMENT,
            COBOLTokenType.IDENTIFICATION
        }
        
        if token1.type in division_keywords:
            token2 = self.peek_token(1)
            if token2 and token2.type in (COBOLTokenType.DIVISION, COBOLTokenType.SECTION):
                if self.debug:
                    print(f"[PARSE_LINKAGE] Division boundary at {token1.type.name} {token2.type.name}")
                self.pos = saved_pos
                break
        
        self.pos = saved_pos
        
        # Parse variable with error handling
        try:
            var = self.parse_variable_decl()
            if var:
                variables.append(var)
            else:
                break
        except Exception as e:
            error_msg = str(e)
            if "Expected PIC clause" in error_msg and "DIVISION" in error_msg:
                if self.debug:
                    print(f"[PARSE_LINKAGE] Division boundary error caught")
                break
            elif any(kw in error_msg for kw in ["PROCEDURE", "DATA", "ENVIRONMENT"]):
                if "DIVISION" in error_msg or "SECTION" in error_msg:
                    break
            else:
                raise
    
    if iteration >= max_iterations:
        print(f"[WARNING] parse_linkage_section hit iteration limit")
    
    return COBOLLinkageSection(variables=variables, line=token.line, column=token.column)


def parse_procedure_division(self) -> COBOLProcedureDivision:
    token = self.current_token()
    self.consume(COBOLTokenType.PROCEDURE)
    self.consume(COBOLTokenType.DIVISION)
    
    using_params = []
    if self.match(COBOLTokenType.USING):
        self.consume(COBOLTokenType.USING)
        while self.match(COBOLTokenType.IDENTIFIER):
            param_name = self.consume(COBOLTokenType.IDENTIFIER).value
            using_params.append(param_name)
            
            if self.match(COBOLTokenType.COMMA):
                self.advance()
            
            if self.match(COBOLTokenType.PERIOD):
                break
    
    self.consume(COBOLTokenType.PERIOD)
    
    paragraphs = []
    sections = []  # 🔧 ADD THIS LINE
    
    while not self.match(COBOLTokenType.EOF):
        
        if self.match(COBOLTokenType.PROCEDURE, COBOLTokenType.DATA,
                    COBOLTokenType.ENVIRONMENT, COBOLTokenType.DIVISION):
            break            
        
        if self.match(COBOLTokenType.IDENTIFICATION):
            break
        if self.match(COBOLTokenType.END_PROGRAM):
            break
        if self.match(COBOLTokenType.END):
            next_tok = self.peek_token(1)
            if next_tok and next_tok.type == COBOLTokenType.PROGRAM:
                break
        if self.match(COBOLTokenType.COMMENT, COBOLTokenType.NEWLINE):
            self.advance()
            continue
        
        if self.match(COBOLTokenType.IDENTIFIER):
            next_token = self.peek_token()
            
            # 🔧 NEW: Check if this is "IDENTIFIER SECTION."
            if next_token and next_token.type == COBOLTokenType.SECTION:
                section = self.parse_section()
                sections.append(section)
                continue
            
            # Existing: Check if this is a paragraph
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
            if not paragraphs or paragraphs[-1].name is not None:
                paragraphs.append(COBOLParagraph(name=None, statements=[], line=stmt.line, column=stmt.column))
            paragraphs[-1].statements.append(stmt)
    
    return COBOLProcedureDivision(
        paragraphs=paragraphs,
        sections=sections,  # 🔧 ADD THIS
        using_params=using_params if using_params else None,
        line=token.line,
        column=token.column
    )


def parse_paragraph(self) -> COBOLParagraph:
    """Parse a paragraph with all its statements until we hit another paragraph or division."""
    token = self.current_token()
    name_token = self.consume(COBOLTokenType.IDENTIFIER)
    self.consume(COBOLTokenType.PERIOD)
    
    self.push_context(ParseContext.INSIDE_PARAGRAPH)
    
    statements = []
    max_statements = 10000
    statement_count = 0
    
    while not self.match(COBOLTokenType.EOF) and statement_count < max_statements:
        statement_count += 1
        
        # Step 1: Skip whitespace, comments, and standalone periods
        while self.match(COBOLTokenType.COMMENT, COBOLTokenType.NEWLINE, COBOLTokenType.PERIOD):
            self.advance()
            if self.match(COBOLTokenType.EOF):
                break
        
        # Step 2: Check for hard boundaries
        if self.match(COBOLTokenType.EOF) or self._is_division_boundary() or self._is_program_boundary() or self.match(COBOLTokenType.SECTION):
            break
        
         # Step 3: Check for new paragraph OR section boundary
        if self.match(COBOLTokenType.IDENTIFIER):
            next_tok = self.peek_token()
            
            # ✅ TRUTH TABLE: What comes after IDENTIFIER inside paragraph?
            # | Next Token | Action |
            # |------------|--------|
            # | SECTION    | Break - section boundary |
            # | PERIOD     | Check if new paragraph or statement |
            # | Other      | Continue to parse_statement |
            
            # Check for section boundary (IDENTIFIER SECTION.)
            if next_tok and next_tok.type == COBOLTokenType.SECTION:
                break  # Section starts, exit current paragraph
            
            # Check for paragraph (IDENTIFIER PERIOD)
            if next_tok and next_tok.type == COBOLTokenType.PERIOD:
                if self._is_new_paragraph_declaration():
                    break  # New paragraph starts, exit current one
                else:
                    # It's a statement (e.g., identifier in MOVE X TO Y)
                    stmt = self.parse_statement()
                    if stmt:
                        statements.append(stmt)
                    continue
        
        # Step 4: Parse statement (for statement keywords or other valid starts)
        stmt = self.parse_statement()
        if stmt:
            statements.append(stmt)
        else:
            # Unexpected token, advance to prevent infinite loop
            if not (self.match(COBOLTokenType.EOF) or self._is_division_boundary() or self._is_program_boundary()):
                self.advance()
    
    if statement_count >= max_statements:
        print(f"[WARNING] parse_paragraph statement limit reached at line {token.line}")
    
    self.pop_context()
    
    return COBOLParagraph(
        name=name_token.value,
        statements=statements,
        line=token.line,
        column=token.column
    )


def _is_division_boundary(self) -> bool:
    """Check if current token is a division boundary"""
    if self.match(COBOLTokenType.IDENTIFICATION, COBOLTokenType.ENVIRONMENT,
                  COBOLTokenType.DATA, COBOLTokenType.PROCEDURE,
                  COBOLTokenType.DIVISION):
        print(f"[BOUNDARY] Hit division at line {self.current_token().line}")
        return True
    
    if self.match(COBOLTokenType.NIST_HEADER, COBOLTokenType.NIST_END_OF):
        print(f"[BOUNDARY] Hit NIST marker at line {self.current_token().line}")
        return True
    
    return False


def _is_program_boundary(self) -> bool:
    """Check if current token marks end of program"""
    if self.match(COBOLTokenType.END):
        next_tok = self.peek_token()
        if next_tok and next_tok.type == COBOLTokenType.PROGRAM:
            return True
    
    if self.match(COBOLTokenType.END_PROGRAM):
        return True
    
    return False


def _is_new_paragraph_declaration(self) -> bool:
    """
    Determine if IDENTIFIER PERIOD is a new paragraph using truth table approach.
    
    TRUTH TABLE:
    Current=IDENTIFIER, Next=PERIOD, Next+1=?
    
    | Next+1 Token Type       | Is Paragraph? | Reason                    |
    |-------------------------|---------------|---------------------------|
    | Statement keyword       | YES           | Statement follows         |
    | IDENTIFIER + PERIOD     | YES           | Another paragraph         |
    | Division/Section        | YES           | Boundary marker           |
    | NIST marker            | YES           | End marker                |
    | EOF                    | YES           | End of file               |
    | TO/FROM/INTO/BY        | NO            | Part of MOVE statement    |
    | COMMA                  | NO            | Part of list              |
    | Anything else          | NO            | Variable reference        |
    """
    identifier_value = self.current_token().value.upper()
    
    print(f"[DEBUG _is_new_paragraph_declaration] Checking: {identifier_value} at line {self.current_token().line}")
    
    # STATE 1: Check if it's a known statement keyword (definitely NOT a paragraph)
    statement_keywords = {
        'STOP', 'GOBACK', 'EXIT', 'DISPLAY', 'MOVE', 'ADD', 
        'SUBTRACT', 'COMPUTE', 'MULTIPLY', 'DIVIDE', 'IF', 
        'PERFORM', 'EVALUATE', 'ACCEPT', 'CALL', 'STRING',
        'UNSTRING', 'INSPECT', 'INITIALIZE', 'CONTINUE',
        'READ', 'WRITE', 'OPEN', 'CLOSE', 'REWRITE', 'DELETE',
        'START', 'RETURN', 'RELEASE', 'SORT', 'MERGE',
        'SEARCH', 'SET', 'CANCEL', 'ENTRY', 'ALTER', 'GO'
    }
    
    if identifier_value in statement_keywords:
        print(f"[DEBUG] {identifier_value} is statement keyword - FALSE")
        return False
    
    # STATE 2: Look ahead to see what follows IDENTIFIER PERIOD
    # Skip past the period (we're currently at IDENTIFIER, next is PERIOD)
    peek_idx = 2  # Look past IDENTIFIER and PERIOD
    peek_token = self.peek_token(peek_idx)
    
    # Skip whitespace/comments
    while peek_token and peek_token.type in (COBOLTokenType.NEWLINE, COBOLTokenType.COMMENT):
        peek_idx += 1
        peek_token = self.peek_token(peek_idx)
    
    print(f"[DEBUG] After '{identifier_value}.', next token: {peek_token.type.name if peek_token else 'EOF'} = {repr(peek_token.value) if peek_token else 'EOF'}")
    
    # STATE 3: Apply truth table rules
    
    # Rule 1: EOF or NIST markers → YES (paragraph at end)
    if not peek_token or peek_token.type in (COBOLTokenType.EOF, COBOLTokenType.NIST_HEADER, COBOLTokenType.NIST_END_OF):
        print(f"[PARAGRAPH] {identifier_value} at boundary/EOF - TRUE")
        return True
    
    # Rule 2: Division/Section keywords → YES (paragraph before boundary)
    if peek_token.type in (COBOLTokenType.IDENTIFICATION, COBOLTokenType.ENVIRONMENT,
                           COBOLTokenType.DATA, COBOLTokenType.PROCEDURE,
                           COBOLTokenType.DIVISION, COBOLTokenType.SECTION):
        print(f"[PARAGRAPH] {identifier_value} before {peek_token.type.name} - TRUE")
        return True
    
    # Rule 3: Statement keyword token types → YES
    statement_token_types = {
        COBOLTokenType.MOVE, COBOLTokenType.ADD, COBOLTokenType.SUBTRACT,
        COBOLTokenType.MULTIPLY, COBOLTokenType.DIVIDE, COBOLTokenType.COMPUTE,
        COBOLTokenType.IF, COBOLTokenType.PERFORM, COBOLTokenType.DISPLAY,
        COBOLTokenType.ACCEPT, COBOLTokenType.CALL, COBOLTokenType.EVALUATE,
        COBOLTokenType.STOP, COBOLTokenType.GOBACK, COBOLTokenType.EXIT,
        COBOLTokenType.GO, COBOLTokenType.STRING, COBOLTokenType.UNSTRING
    }
    
    if peek_token.type in statement_token_types:
        print(f"[PARAGRAPH] {identifier_value} followed by {peek_token.type.name} - TRUE")
        return True
    
    # Rule 4: Another IDENTIFIER followed by PERIOD → YES (another paragraph)
    if peek_token.type == COBOLTokenType.IDENTIFIER:
        peek_next = self.peek_token(peek_idx + 1)
        if peek_next and peek_next.type == COBOLTokenType.PERIOD:
            print(f"[PARAGRAPH] {identifier_value} followed by '{peek_token.value}.' - TRUE")
            return True
    
    # Rule 5: Statement preposition keywords → NO (part of statement)
    if peek_token.type in (COBOLTokenType.TO, COBOLTokenType.FROM, COBOLTokenType.INTO, 
                           COBOLTokenType.BY, COBOLTokenType.GIVING):
        print(f"[VARIABLE] {identifier_value} part of statement (followed by {peek_token.type.name}) - FALSE")
        return False
    
    # Rule 6: COMMA → NO (part of list)
    if peek_token.type == COBOLTokenType.COMMA:
        print(f"[VARIABLE] {identifier_value} part of list - FALSE")
        return False
    
    # Rule 7: Default - check if identifier looks like variable name
    # Very short names (1-3 chars) without hyphens are probably variables
    if len(identifier_value) <= 3 and '-' not in identifier_value:
        print(f"[VARIABLE] {identifier_value} is short without hyphen - FALSE")
        return False
    
    # Rule 8: If it has hyphens or is longer, probably a paragraph
    if '-' in identifier_value or len(identifier_value) > 8:
        print(f"[PARAGRAPH] {identifier_value} has hyphen or is long - TRUE")
        return True
    
    # Rule 9: Conservative default - assume variable to be safe
    print(f"[VARIABLE] {identifier_value} default conservative - FALSE")
    return False



    
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#DUMB BULLSHIT PEOPLE ALLOWED TO GET OUT OF CONTROL WITHOUT BREAKING ANYTHING
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




def _is_comment_entry_start(self) -> bool:
    """Check if we're at the start of a COMMENT ENTRY paragraph"""
    print(f"[DEBUG] _is_comment_entry_start called, current pos: {self.pos}")
    
    saved_pos = self.pos
    found_program_id = False
    found_division = False
    
    # Look ahead up to 10 tokens
    for i in range(10):
        if self.pos + i >= len(self.tokens):
            break
        
        token = self.tokens[self.pos + i]
        print(f"[DEBUG]   lookahead[{i}]: {token.type.name} = '{token.value}'")
        
        if token.type == COBOLTokenType.PROGRAM_ID:
            found_program_id = True
            print(f"[DEBUG]   Found PROGRAM-ID at offset {i}")
            break
        
        if token.type in (COBOLTokenType.ENVIRONMENT, COBOLTokenType.DATA, 
                         COBOLTokenType.PROCEDURE):
            found_division = True
            print(f"[DEBUG]   Found division {token.type.name} at offset {i}")
            break
    
    # Restore position
    self.pos = saved_pos
    
    result = found_division and not found_program_id
    print(f"[DEBUG] _is_comment_entry_start returning: {result}")
    return result


def _skip_comment_entry_paragraph(self):
    """Skip everything until we hit a real division"""
    start_line = self.current_token().line
    tokens_skipped = 0
    max_skip = 2000
    
    print(f"[COMMENT_ENTRY] Skipping from line {start_line}...")
    
    # First, consume any immediate division keywords (these are fake)
    if self.match(COBOLTokenType.ENVIRONMENT, COBOLTokenType.DATA, COBOLTokenType.PROCEDURE):
        print(f"[COMMENT_ENTRY] Consuming fake {self.current_token().type.name} at line {self.current_token().line}")
        self.advance()
        tokens_skipped += 1
    
    # Now skip everything until we hit the REAL program structure
    # The real structure will have ENVIRONMENT followed by CONFIGURATION or INPUT-OUTPUT
    while not self.match(COBOLTokenType.EOF) and tokens_skipped < max_skip:
        # Look for the real ENVIRONMENT DIVISION by checking context
        if self.match(COBOLTokenType.ENVIRONMENT):
            # Peek ahead - if next is DIVISION, this might be real
            next_tok = self.peek_token()
            if next_tok and next_tok.type == COBOLTokenType.DIVISION:
                print(f"[COMMENT_ENTRY] Skipped {tokens_skipped} tokens from line {start_line} to {self.current_token().line}")
                return
        
        self.advance()
        tokens_skipped += 1
    
    print(f"[WARNING] Comment entry skip limit reached")


def parse_all_programs(self) -> COBOLCompilationUnit:
    """Parse ALL programs with proper state reset between programs
    
    TRUTH TABLE: Token Type Handling
    ┌──────────────────────┬─────────────────────────────────────┐
    │ Token Type           │ Action                              │
    ├──────────────────────┼─────────────────────────────────────┤
    │ EOF                  │ Stop parsing                        │
    │ COMMENT              │ Skip                                │
    │ NEWLINE              │ Skip                                │
    │ NIST_END_OF          │ Mark program end, reset state       │
    │ NIST_HEADER          │ Mark program start                  │
    │ IDENTIFICATION       │ Parse program, reset state after    │
    │ Other                │ Skip (noise between programs)       │
    └──────────────────────┴─────────────────────────────────────┘
    
    CRITICAL: Parser state must be reset between programs to prevent
    corruption bleeding from one program to the next!
    
    State to reset:
    - Parse context stack
    - Any cached lookahead
    - Error recovery flags
    """
    print(f"[DEBUG] parse_all_programs called, pos={self.pos}")
    
    programs = []
    failed_programs = []
    current_program_name = None
    
    while not self.match(COBOLTokenType.EOF):
        # TRUTH TABLE ROW 2-3: Skip whitespace/comments
        if self.match(COBOLTokenType.COMMENT, COBOLTokenType.NEWLINE):
            self.advance()
            continue
        
        # TRUTH TABLE ROW 4: NIST END-OF marker
        if self.match(COBOLTokenType.NIST_END_OF):
            end_prog_name = self.current_token().value
            print(f"[NIST] Found *END-OF,{end_prog_name} at line {self.current_token().line}")
            self.advance()
            current_program_name = None
            # ✅ CRITICAL: Reset parser state after program ends
            self._reset_parser_state()
            continue
        
        # TRUTH TABLE ROW 5: NIST HEADER marker
        if self.match(COBOLTokenType.NIST_HEADER):
            header_prog_name = self.current_token().value
            print(f"[NIST] Found *HEADER,COBOL,{header_prog_name} at line {self.current_token().line}")
            current_program_name = header_prog_name
            self.advance()
            # ✅ CRITICAL: Reset parser state before new program starts
            self._reset_parser_state()
            continue
        
        # TRUTH TABLE ROW 6: IDENTIFICATION DIVISION - parse program
        if self.match(COBOLTokenType.IDENTIFICATION):
            start_line = self.current_token().line
            start_pos = self.pos
            
            # ✅ CRITICAL: Reset state before parsing each program
            self._reset_parser_state()
            
            try:
                program = self.parse_single_program()
                
                # ✅ SPECIAL CASE: Skip placeholder comment entry programs
                if program.program_id == "COMMENT_ENTRY_PLACEHOLDER":
                    print(f"[COMMENT_ENTRY] Skipped fake program (comment entry) at line {start_line}")
                    continue
                
                programs.append(program)
                print(f"✓ Successfully parsed program #{len(programs)}: {program.program_id}")
                
                # ✅ CRITICAL: Reset state after successfully parsing program
                self._reset_parser_state()
                
            except Exception as e:
                error_line = self.current_token().line if self.pos < len(self.tokens) else "EOF"
                error_msg = str(e)
                print(f"✗ Failed at line {error_line}: {error_msg}")
                failed_programs.append((start_line, error_msg))
                
                # ✅ CRITICAL: Reset state before attempting recovery
                self._reset_parser_state()
                
                # Recovery: Skip to next program boundary
                print(f"  → Attempting recovery...")
                recovery_start_pos = self.pos
                max_tokens_to_skip = 10000
                tokens_skipped = 0
                
                while not self.match(COBOLTokenType.EOF) and tokens_skipped < max_tokens_to_skip:
                    # Found a boundary marker - stop here
                    if self.match(COBOLTokenType.NIST_HEADER, COBOLTokenType.NIST_END_OF):
                        print(f"  → Recovered: Found NIST marker at line {self.current_token().line}")
                        break
                    
                    # Found next program start - stop here
                    if self.match(COBOLTokenType.IDENTIFICATION):
                        if self.pos > recovery_start_pos + 10:
                            print(f"  → Recovered: Found next IDENTIFICATION at line {self.current_token().line}")
                            break
                    
                    self.advance()
                    tokens_skipped += 1
                
                if tokens_skipped >= max_tokens_to_skip:
                    print(f"  → Recovery failed: Hit token skip limit")
                
                # ✅ CRITICAL: Reset state after recovery
                self._reset_parser_state()
                continue
        
        # TRUTH TABLE ROW 7: Other tokens - skip (noise between programs)
        else:
            if not self.match(COBOLTokenType.EOF):
                self.advance()
    
    # Validate we parsed at least one program
    if not programs:
        raise ParserError("No COBOL programs found in source file", 1, 1)
    
    # Report summary
    print(f"\n{'='*80}")
    print(f"PARSE SUMMARY:")
    print(f"  ✓ Successfully parsed: {len(programs)} programs")
    if failed_programs:
        print(f"  ✗ Failed to parse: {len(failed_programs)} programs")
    print(f"{'='*80}\n")
    
    return COBOLCompilationUnit(programs=programs, failed_programs=failed_programs, line=1, column=1)


def _reset_parser_state(self):
    """Reset parser state between programs to prevent corruption
    
    CRITICAL: This prevents state from one program bleeding into the next.
    
    State that must be reset:
    - Parse context stack (clear any lingering IF/PERFORM/PARAGRAPH contexts)
    - Any cached lookahead or buffered tokens
    - Error recovery flags
    - Any temporary parsing state
    """
    # Reset parse context stack to default TOP_LEVEL
    # CRITICAL: current_context() is a METHOD, not a property!
    # We must reset the context_stack directly, which current_context() reads from
    self.context_stack = [ParseContext.TOP_LEVEL]
    
    # Clear any error recovery flags
    if hasattr(self, 'in_error_recovery'):
        self.in_error_recovery = False
    
    # Note: We do NOT reset self.pos or self.tokens - those track file position
    # We only reset parsing STATE, not position



# ============================================================================
# MONKEY-PATCH: Attach all parser methods to COBOLMultiProgramParser
# ============================================================================
# Standard division parsers (also patched in parser/__init__.py)
COBOLMultiProgramParser.parse_single_program = parse_single_program
COBOLMultiProgramParser.parse_identification_division = parse_identification_division
COBOLMultiProgramParser.parse_environment_division = parse_environment_division
COBOLMultiProgramParser.parse_select_statement = parse_select_statement
COBOLMultiProgramParser.parse_data_division = parse_data_division
COBOLMultiProgramParser.parse_file_section = parse_file_section
COBOLMultiProgramParser.parse_fd_entry = parse_fd_entry
COBOLMultiProgramParser.parse_working_storage = parse_working_storage
COBOLMultiProgramParser.parse_linkage_section = parse_linkage_section
COBOLMultiProgramParser.parse_procedure_division = parse_procedure_division
COBOLMultiProgramParser.parse_section = parse_section
COBOLMultiProgramParser.parse_paragraph = parse_paragraph
COBOLMultiProgramParser._reset_parser_state = _reset_parser_state 









#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# I can't belive I am enabling this behavoir but whats done is done 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Special handlers for NIST COBOL-74 COMMENT ENTRY paragraphs
# (Obsolete feature required for NIST-85 test suite compliance)
COBOLMultiProgramParser._is_comment_entry_start = _is_comment_entry_start
COBOLMultiProgramParser._skip_comment_entry_paragraph = _skip_comment_entry_paragraph


