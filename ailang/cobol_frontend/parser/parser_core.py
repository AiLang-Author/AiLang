#!/usr/bin/env python3

# Copyright (c) 2025 Sean Collins, 2 Paws Machine and Engineering. All rights reserved.
#
# Licensed under the Sean Collins Software License (SCSL). See the LICENSE file in the root directory of this project
# for the full terms and conditions, including restrictions on forking, corporate use, and permissions for private/teaching purposes.

"""
COBOL Parser Core - Main parser class and utilities
"""

from typing import List, Optional
from enum import Enum
from cobol_frontend.cobol_lexer import Token, COBOLTokenType  
from .ast_nodes import *


class ParseContext(Enum):
    TOP_LEVEL = 1        
    INSIDE_IF = 2        
    INSIDE_LOOP = 3
    INSIDE_PARAGRAPH = 4
    INSIDE_EVALUATE = 5


class ParserError(Exception):
    def __init__(self, message: str, line: int, column: int):
        self.message = message
        self.line = line
        self.column = column
        super().__init__(f"Parser error at line {line}, column {column}: {message}")


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

    # ============================================================================
    # LEGACY VERSION - Kept for reference
    # This version works but doesn't handle COMMENT ENTRY paragraphs.
    # The active version with COMMENT_ENTRY_PLACEHOLDER handling is monkey-patched
    # from division_parsers.py via parser/__init__.py
    # ============================================================================
    # def parse_all_programs(self) -> COBOLCompilationUnit:
    #     """Parse ALL programs - use NIST markers as boundaries, keep going on errors"""
    #     programs = []
    #     failed_programs = []
    #     current_program_name = None
    #     
    #     while not self.match(COBOLTokenType.EOF):
    #         # Skip comments/newlines
    #         if self.match(COBOLTokenType.COMMENT, COBOLTokenType.NEWLINE):
    #             self.advance()
    #             continue
    #         
    #         # ✅ Recognize NIST END-OF marker
    #         if self.match(COBOLTokenType.NIST_END_OF):
    #             end_prog_name = self.current_token().value
    #             print(f"[NIST] Found *END-OF,{end_prog_name} at line {self.current_token().line}")
    #             self.advance()
    #             current_program_name = None
    #             continue
    #         
    #         # ✅ Recognize NIST HEADER marker  
    #         if self.match(COBOLTokenType.NIST_HEADER):
    #             header_prog_name = self.current_token().value
    #             print(f"[NIST] Found *HEADER,COBOL,{header_prog_name} at line {self.current_token().line}")
    #             current_program_name = header_prog_name
    #             self.advance()
    #             continue
    #         
    #         # Parse IDENTIFICATION DIVISION
    #         if self.match(COBOLTokenType.IDENTIFICATION):
    #             start_line = self.current_token().line
    #             try:
    #                 program = self.parse_single_program()
    #                 programs.append(program)
    #                 print(f"✓ Successfully parsed program #{len(programs)}: {program.program_id}")
    #             except Exception as e:
    #                 error_line = self.current_token().line if self.pos < len(self.tokens) else "EOF"
    #                 print(f"✗ Failed at line {error_line}: {e}")
    #                 failed_programs.append((start_line, str(e)))
    #                 
    #                 # IMPROVED RECOVERY: Skip to next program boundary
    #                 print(f"  → Attempting recovery...")
    #                 
    #                 recovery_start_pos = self.pos
    #                 max_tokens_to_skip = 10000
    #                 tokens_skipped = 0
    #                 
    #                 while not self.match(COBOLTokenType.EOF) and tokens_skipped < max_tokens_to_skip:
    #                     # Found a clear program boundary - stop recovery WITHOUT consuming it
    #                     if self.match(COBOLTokenType.NIST_HEADER, COBOLTokenType.NIST_END_OF):
    #                         print(f"  → Recovered: Found NIST marker at line {self.current_token().line} (skipped {tokens_skipped} tokens)")
    #                         break  # DON'T advance - let outer loop handle the marker
    #                     
    #                     # Found next IDENTIFICATION DIVISION - stop recovery WITHOUT consuming it
    #                     if self.match(COBOLTokenType.IDENTIFICATION):
    #                         if self.pos > recovery_start_pos + 10:
    #                             print(f"  → Recovered: Found next IDENTIFICATION at line {self.current_token().line} (skipped {tokens_skipped} tokens)")
    #                             break  # DON'T advance - let outer loop handle it
    #                     
    #                     self.advance()
    #                     tokens_skipped += 1
    #                 
    #                 if tokens_skipped >= max_tokens_to_skip:
    #                     print(f"  ⚠️  Recovery limit reached, but continuing to parse...")
    #                 
    #                 continue
    #         else:
    #             # Not at a program start, just skip this token
    #             if not self.match(COBOLTokenType.EOF):
    #                 self.advance()
    #     
    #     if not programs:
    #         raise ParserError("No COBOL programs found in source file", 1, 1)
    #     
    #     # Report summary
    #     print(f"\n{'='*80}")
    #     print(f"PARSE SUMMARY:")
    #     print(f"  ✓ Successfully parsed: {len(programs)} programs")
    #     if failed_programs:
    #         print(f"  ✗ Failed to parse: {len(failed_programs)} programs")
    #         print(f"\nFailed programs:")
    #         for idx, (line, error) in enumerate(failed_programs[:10], 1):
    #             short_error = error[:80] + "..." if len(error) > 80 else error
    #             print(f"    {idx}. Line {line}: {short_error}")
    #         if len(failed_programs) > 10:
    #             print(f"    ... and {len(failed_programs) - 10} more")
    #     print(f"{'='*80}\n")
    #     
    #     return COBOLCompilationUnit(programs=programs, failed_programs=failed_programs, line=1, column=1)
    
    def is_data_only_program(self, program: COBOLProgram) -> bool:
        """Check if this is a data-only program (copybook)"""
        return (program.data_division is not None and 
                program.procedure_division is None)

    def validate_program(self, program: COBOLProgram) -> list[str]:
        """Validate program structure and return warnings"""
        warnings = []
        
        if self.is_data_only_program(program):
            warnings.append(f"Program {program.program_id} has no PROCEDURE DIVISION (copybook/data definition)")
        
        if program.data_division is None and program.procedure_division is None:
            warnings.append(f"Program {program.program_id} has no DATA or PROCEDURE divisions")
        
        return warnings
    
    def parse_end_program(self, expected_program_id: str) -> bool:
        """
        Parse END PROGRAM statement (optional in older COBOL)
        
        Returns:
            True if END PROGRAM was found and parsed
            False if not found (not an error - some programs don't have it)
        
        Handles both:
            END PROGRAM program-id.     (two tokens)
            END-PROGRAM program-id.     (hyphenated)
        """
        if not self.match(COBOLTokenType.END, COBOLTokenType.END_PROGRAM):
            return False  # ✅ FIX: Return False explicitly
        
        # Handle "END PROGRAM" (two tokens)
        if self.match(COBOLTokenType.END):
            self.consume(COBOLTokenType.END)
            
            if not self.match(COBOLTokenType.PROGRAM):
                # Just "END" without "PROGRAM" - not an END PROGRAM statement
                self.pos -= 1  # Back up
                return False  # ✅ FIX: Return False
            
            self.consume(COBOLTokenType.PROGRAM)
        
        # Handle "END-PROGRAM" (single hyphenated token)
        elif self.match(COBOLTokenType.END_PROGRAM):
            self.consume(COBOLTokenType.END_PROGRAM)
        
        # Optional program-id
        if self.match(COBOLTokenType.IDENTIFIER):
            actual_program_id = self.consume(COBOLTokenType.IDENTIFIER).value
            if actual_program_id != expected_program_id:
                print(f"Warning: END PROGRAM {actual_program_id} doesn't match PROGRAM-ID {expected_program_id}")
        
        # Mandatory period
        if self.match(COBOLTokenType.PERIOD):
            self.consume(COBOLTokenType.PERIOD)
        
        return True  # ✅ FIX: Return True on success