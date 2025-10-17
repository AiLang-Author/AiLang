#!/usr/bin/env python3

# Copyright (c) 2025 Sean Collins, 2 Paws Machine and Engineering. All rights reserved.
#
# Licensed under the Sean Collins Software License (SCSL). See the LICENSE file in the root directory of this project
# for the full terms and conditions, including restrictions on forking, corporate use, and permissions for private/teaching purposes.

"""
COBOL Statement Parsers - Handles all statement types
"""

import re
from typing import List, Optional
from .parser_core import COBOLMultiProgramParser, ParseContext
from cobol_frontend.cobol_lexer import Token, COBOLTokenType  # ✅ FIXED
from .ast_nodes import *
from .parser_core import COBOLMultiProgramParser



def parse_statement(self) -> Optional[COBOLASTNode]:
    """Parse a single COBOL statement using state machine approach"""
    token = self.current_token()
    
    print(f"DEBUG parse_statement: token={token.type.name} value={token.value} context={self.current_context()}")
    
    # TRUTH TABLE: What to do with each token type
    # Format: (token_type, action, advance_before_action, return_value)
    
    token_type = token.type
    
    # 1. BOUNDARIES - Stop and return None
    if token_type in (COBOLTokenType.IDENTIFICATION, COBOLTokenType.ENVIRONMENT,
                      COBOLTokenType.DATA, COBOLTokenType.PROCEDURE, 
                      COBOLTokenType.DIVISION, COBOLTokenType.NIST_HEADER, 
                      COBOLTokenType.NIST_END_OF, COBOLTokenType.EOF):
        print(f"[BOUNDARY] Hit {token_type.name} at line {token.line}")
        self.advance()
        return None
    
    # 2. NOISE TOKENS - Skip them
    if token_type in (COBOLTokenType.COMMENT, COBOLTokenType.NEWLINE, COBOLTokenType.PERIOD):
        self.advance()
        return None
    
    # 3. STATEMENT KEYWORDS - Dispatch to specific parsers
    statement_dispatch = {
        COBOLTokenType.DISPLAY: self.parse_display,
        COBOLTokenType.ACCEPT: self.parse_accept,
        COBOLTokenType.MOVE: self.parse_move,
        COBOLTokenType.COMPUTE: self.parse_compute,
        COBOLTokenType.ADD: self.parse_add,
        COBOLTokenType.SUBTRACT: self.parse_subtract,
        COBOLTokenType.MULTIPLY: self.parse_multiply,
        COBOLTokenType.DIVIDE: self.parse_divide,
        COBOLTokenType.IF: self.parse_if,
        COBOLTokenType.PERFORM: self.parse_perform,
        COBOLTokenType.GO: self.parse_go_to,
        COBOLTokenType.GOTO: self.parse_go_to,
        COBOLTokenType.CALL: self.parse_call,
        COBOLTokenType.EVALUATE: self.parse_evaluate,
        COBOLTokenType.STRING: self.parse_string_statement,
        COBOLTokenType.UNSTRING: self.parse_unstring_statement,
        COBOLTokenType.INSPECT: self.parse_inspect_statement,
        COBOLTokenType.STOP: self.parse_stop_run,
        COBOLTokenType.MERGE: self.parse_merge,
        COBOLTokenType.GOBACK: self.parse_goback,
        COBOLTokenType.EXIT: self.parse_exit,
        COBOLTokenType.COPY: self.parse_copy_statement,
    }
    
    if token_type in statement_dispatch:
        parser_func = statement_dispatch[token_type]
        return parser_func(token)
    
    # 4. IDENTIFIER - Could be paragraph or variable reference
    if token_type == COBOLTokenType.IDENTIFIER:
        # Check if it's "READ" (special case - not a token type)
        if token.value and token.value.upper() == 'READ':
            return self.parse_read_statement()
        
        # Check if it's a paragraph (IDENTIFIER followed by PERIOD)
        next_token = self.peek_token()
        if next_token and next_token.type == COBOLTokenType.PERIOD:
            # Check if it's actually a new paragraph declaration
            if self._is_new_paragraph_declaration():
                print(f"[PARAGRAPH HINT] Found '{token.value}.' at line {token.line} - returning to paragraph parser")
                return None
            # Otherwise, it's just an identifier with period (like end of PERFORM statement)
            # Fall through to handle as variable/unknown
        
        # It's a variable reference or unknown - advance past it
        print(f"[SKIP] Unknown identifier '{token.value}' at line {token.line}")
        self.advance()
        return None
    
    # 5. ANYTHING ELSE - Unknown token, skip it
    print(f"[SKIP] Unknown token type {token_type.name} = '{token.value}' at line {token.line}")
    self.advance()
    return None
        
        


def parse_display(self, token: Token) -> COBOLDisplay:
    """Parse DISPLAY statement with truth table for expression boundaries
    
    COBOL DISPLAY can have multiple expressions separated by spaces or commas:
    - DISPLAY X Y Z.
    - DISPLAY X, Y, Z.
    - DISPLAY "Hello" NAME.
    
    TRUTH TABLE: When to stop parsing DISPLAY expressions
    | Token Type | Stop? | Reason |
    |------------|-------|--------|
    | PERIOD     | YES   | Statement end |
    | Statement keyword | MAYBE | Only if not a valid expression token |
    | END_IF, END_PERFORM, etc | YES | Block end |
    | UPON       | YES   | DISPLAY...UPON clause |
    | COMMA      | NO    | Just a separator, continue |
    | STRING_LITERAL, NUMBER_LITERAL, IDENTIFIER | NO | Valid expression tokens |
    """
    self.consume(COBOLTokenType.DISPLAY)
    
    expressions = []
    max_expressions = 100  # Safety limit
    expr_count = 0
    
    print(f"[PARSE_DISPLAY] Starting parse at line {token.line}")
    
    while expr_count < max_expressions:
        current = self.current_token()
        print(f"[PARSE_DISPLAY] Iteration {expr_count+1}: token={current.type.name} value='{current.value}' line={current.line}")
        
        # ═══════════════════════════════════════════════════════════════
        # STEP 1: Check for explicit terminators FIRST
        # ═══════════════════════════════════════════════════════════════
        if self.match(COBOLTokenType.PERIOD):
            print(f"[PARSE_DISPLAY] Hit PERIOD, breaking")
            break
        
        # ═══════════════════════════════════════════════════════════════
        # STEP 2: Check for block enders
        # ═══════════════════════════════════════════════════════════════
        if self.match(COBOLTokenType.END_IF, COBOLTokenType.END_PERFORM,
                     COBOLTokenType.END_EVALUATE, COBOLTokenType.ELSE,
                     COBOLTokenType.WHEN):
            print(f"[PARSE_DISPLAY] Hit block ender {current.type.name}, breaking")
            break
        
        # ═══════════════════════════════════════════════════════════════
        # STEP 3: Check for UPON keyword (DISPLAY...UPON device-name)
        # ═══════════════════════════════════════════════════════════════
        if self.match(COBOLTokenType.IDENTIFIER):
            if self.current_token().value.upper() == 'UPON':
                print(f"[PARSE_DISPLAY] Hit UPON, breaking")
                break
        
        # ═══════════════════════════════════════════════════════════════
        # STEP 4: Check for statement boundaries (but be smart about it)
        # ═══════════════════════════════════════════════════════════════
        is_boundary = self._is_statement_boundary()
        print(f"[PARSE_DISPLAY] _is_statement_boundary() = {is_boundary}")
        
        if is_boundary:
            # ✅ FIX: Don't break if this token can start a valid expression!
            # Even if it's technically a "statement keyword", STRING_LITERAL,
            # NUMBER_LITERAL, IDENTIFIER, etc. are valid DISPLAY operands
            can_start_expr = self.match(COBOLTokenType.STRING_LITERAL, 
                                       COBOLTokenType.NUMBER_LITERAL,
                                       COBOLTokenType.IDENTIFIER, 
                                       COBOLTokenType.FUNCTION,
                                       COBOLTokenType.ALL, 
                                       COBOLTokenType.LPAREN,
                                       COBOLTokenType.PLUS,
                                       COBOLTokenType.MINUS)
            print(f"[PARSE_DISPLAY] Is boundary but can_start_expr = {can_start_expr}")
            
            if not can_start_expr:
                print(f"[PARSE_DISPLAY] Breaking due to statement boundary (token cannot start expression)")
                break
            else:
                print(f"[PARSE_DISPLAY] Statement boundary but token CAN start expression, continuing...")
        
        # ═══════════════════════════════════════════════════════════════
        # STEP 5: Skip commas (they're just separators)
        # ═══════════════════════════════════════════════════════════════
        if self.match(COBOLTokenType.COMMA):
            print(f"[PARSE_DISPLAY] Skipping COMMA")
            self.advance()
            continue
        
        # ═══════════════════════════════════════════════════════════════
        # STEP 6: Parse the expression
        # ═══════════════════════════════════════════════════════════════
        print(f"[PARSE_DISPLAY] About to call parse_expression() on token {current.type.name}")
        expr = self.parse_expression()
        
        if expr:
            print(f"[PARSE_DISPLAY] ✓ Parsed expression: {type(expr).__name__}, added to list (total={expr_count+1})")
            expressions.append(expr)
            expr_count += 1
        else:
            print(f"[PARSE_DISPLAY] ✗ parse_expression() returned None, breaking loop")
            break
    
    # ═══════════════════════════════════════════════════════════════
    # STEP 7: Loop finished - report results
    # ═══════════════════════════════════════════════════════════════
    if expr_count >= max_expressions:
        print(f"[WARNING] PARSE_DISPLAY expression limit reached at line {token.line}")
    
    print(f"[PARSE_DISPLAY] Finished loop with {len(expressions)} expressions")
    
    # ═══════════════════════════════════════════════════════════════
    # STEP 8: Handle optional UPON clause
    # ═══════════════════════════════════════════════════════════════
    if self.match(COBOLTokenType.IDENTIFIER):
        if self.current_token().value.upper() == 'UPON':
            print(f"[PARSE_DISPLAY] Found UPON clause after expressions")
            self.advance()
            if self.match(COBOLTokenType.IDENTIFIER):
                self.advance()  # Consume device name

    # ═══════════════════════════════════════════════════════════════
    # STEP 9: Consume optional period
    # ═══════════════════════════════════════════════════════════════
    if self.requires_period() and self.match(COBOLTokenType.PERIOD):
        print(f"[PARSE_DISPLAY] Consuming final PERIOD")
        self.consume(COBOLTokenType.PERIOD)
    
    print(f"[PARSE_DISPLAY] Returning COBOLDisplay with {len(expressions)} expressions")
    return COBOLDisplay(expressions=expressions, line=token.line, column=token.column)



def parse_accept(self, token: Token) -> COBOLAccept:
    """Parse ACCEPT statement with truth table for variants
    
    TRUTH TABLE: ACCEPT Statement Patterns
    ┌──────────────────────────────┬────────────────────────┐
    │ Pattern                      │ Meaning                │
    ├──────────────────────────────┼────────────────────────┤
    │ ACCEPT var                   │ From stdin             │
    │ ACCEPT var FROM source       │ From device/special    │
    │ ACCEPT var FROM DATE         │ Get system date        │
    │ ACCEPT var (format)          │ Formatted input (old)  │
    │ ACCEPT var FOR format        │ Formatted input (new)  │
    └──────────────────────────────┴────────────────────────┘
    
    Examples:
    - ACCEPT WS-DATE FROM DATE.
    - ACCEPT USER-INPUT.
    - ACCEPT SCREEN-FIELD (0325).        ← Handle this!
    - ACCEPT SCREEN-FIELD FOR 0325.
    """
    self.consume(COBOLTokenType.ACCEPT)
    
    variable = self.consume(COBOLTokenType.IDENTIFIER).value
    
    # Check what follows the variable
    next_token = self.current_token()
    
    # Pattern 1: ACCEPT var FROM source
    if next_token.type == COBOLTokenType.FROM:
        self.advance()
        # Consume source (DATE, TIME, DAY, etc.)
        if self.match(COBOLTokenType.IDENTIFIER):
            self.advance()
    
    # Pattern 2: ACCEPT var FOR format (standard)
    elif next_token.type == COBOLTokenType.FOR:
        self.advance()
        # Consume format specification
        if self.match(COBOLTokenType.NUMBER_LITERAL, COBOLTokenType.IDENTIFIER):
            self.advance()
    
    # Pattern 3: ACCEPT var (format) - LPAREN variant
    elif next_token.type == COBOLTokenType.LPAREN:
        self.advance()  # Consume LPAREN
        # Parse format inside parentheses
        if self.match(COBOLTokenType.NUMBER_LITERAL, COBOLTokenType.IDENTIFIER):
            self.advance()
        self.consume(COBOLTokenType.RPAREN)
    
    # Pattern 4: Just ACCEPT var (no source/format)
    # Fall through - nothing more to consume
    
    self.consume_optional_period()
    return COBOLAccept(
        variable=variable,
        line=token.line,
        column=token.column
    )



def parse_perform_times_inline(self, token: Token) -> COBOLPerformTimes:
    """Parse PERFORM paragraph-name N TIMES
    
    Syntax: PERFORM paragraph-name number TIMES.
    Example: PERFORM WRITE-LINE 2 TIMES.
    """
    # Current token is the paragraph name (IDENTIFIER)
    para_name = self.consume(COBOLTokenType.IDENTIFIER).value
    
    # Next should be the number (NUMBER_LITERAL or IDENTIFIER for variable)
    if not self.match(COBOLTokenType.NUMBER_LITERAL, COBOLTokenType.IDENTIFIER):
        self.error(f"Expected number or variable after paragraph name '{para_name}' in PERFORM TIMES")
    
    # Parse the times expression (just a simple number or variable reference)
    times_token = self.current_token()
    if times_token.type == COBOLTokenType.NUMBER_LITERAL:
        times_expr = COBOLNumberLiteral(
            value=times_token.value,
            line=times_token.line,
            column=times_token.column
        )
        self.advance()
    else:  # IDENTIFIER (variable)
        times_expr = COBOLIdentifier(
            name=times_token.value,
            line=times_token.line,
            column=times_token.column
        )
        self.advance()
    
    # Consume TIMES keyword
    if not self.match(COBOLTokenType.TIMES):
        self.error(f"Expected TIMES keyword after number in PERFORM statement")
    self.consume(COBOLTokenType.TIMES)
    
    # Consume optional period
    self.consume_optional_period()
    
    # Return PERFORM TIMES node with paragraph name and no inline statements
    return COBOLPerformTimes(
        times_expr=times_expr,
        paragraph_name=para_name,  # ✅ Important: paragraph name, not inline statements
        statements=None,  # No inline statements - it's calling a paragraph
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





def parse_subtract(self, token: Token) -> COBOLArithmetic:
    self.consume(COBOLTokenType.SUBTRACT)
    
    operands = [self.parse_expression()]
    
    while not self.match(COBOLTokenType.FROM):
        operands.append(self.parse_expression())
    
    self.consume(COBOLTokenType.FROM)
    from_value = self.parse_expression()
    
    giving = None
    target = None
    if self.match(COBOLTokenType.GIVING):
        self.advance()
        giving = self.parse_expression()
        operands.insert(0, from_value)
    else:
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


def parse_read_statement(self) -> 'COBOLRead':
    """Parse READ statement with AT END clause"""
    token = self.current_token()
    self.advance()
    
    filename = self.consume(COBOLTokenType.IDENTIFIER).value
    
    into_variable = None
    if self.match(COBOLTokenType.INTO):
        self.advance()
        into_variable = self.consume(COBOLTokenType.IDENTIFIER).value
    
    at_end_statements = []
    not_at_end_statements = []
    
    if self.current_token() and self.current_token().value and self.current_token().value.upper() == 'AT':
        self.advance()
        if self.current_token() and self.current_token().value and self.current_token().value.upper() == 'END':
            self.advance()
            
            while not self.match(COBOLTokenType.PERIOD, COBOLTokenType.EOF):
                current_val = self.current_token().value.upper() if self.current_token().value else ""

                if current_val == 'NOT':
                    break

                STATEMENT_KEYWORDS = [
                    'MOVE', 'DISPLAY', 'PERFORM', 'IF', 'ELSE', 'STOP', 'ADD', 'SUBTRACT', 
                    'MULTIPLY', 'DIVIDE', 'COMPUTE', 'ACCEPT', 'READ', 'WRITE', 'OPEN', 'CLOSE'
                ]
                if current_val in STATEMENT_KEYWORDS and len(at_end_statements) > 0:
                    break
                
                stmt = self.parse_statement()
                if stmt:
                    at_end_statements.append(stmt)
    
    if self.current_token() and self.current_token().value and self.current_token().value.upper() == 'NOT':
        self.advance()
        if self.current_token() and self.current_token().value and self.current_token().value.upper() == 'AT':
            self.advance()
        if self.current_token() and self.current_token().value and self.current_token().value.upper() == 'END':
            self.advance()
            
            while not self.match(COBOLTokenType.PERIOD, COBOLTokenType.EOF):
                current_val = self.current_token().value.upper() if self.current_token().value else ""

                STATEMENT_KEYWORDS = [
                    'MOVE', 'DISPLAY', 'PERFORM', 'IF', 'ELSE', 'STOP', 'ADD', 'SUBTRACT',
                    'MULTIPLY', 'DIVIDE', 'COMPUTE', 'ACCEPT', 'READ', 'WRITE', 'OPEN', 'CLOSE'
                ]
                if current_val in STATEMENT_KEYWORDS and len(not_at_end_statements) > 0:
                    break
                
                stmt = self.parse_statement()
                if stmt:
                    not_at_end_statements.append(stmt)
    
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


def parse_merge(self, token: Token) -> COBOLASTNode:
    """Parse MERGE statement with truth table approach
    
    TRUTH TABLE: MERGE Statement Structure
    ┌──────────────────────────────────────┬─────────────────────────────┐
    │ Current Token State                  │ Action                      │
    ├──────────────────────────────────────┼─────────────────────────────┤
    │ After MERGE                          │ Parse file-name             │
    │ After file-name                      │ Expect ON clause            │
    │ ON + ASC/DESC + KEY                  │ Parse sort keys             │
    │ After keys, ON again                 │ Parse more keys (loop)      │
    │ After keys, COLLATING                │ Parse collating sequence    │
    │ After keys/collating, USING          │ Parse input files           │
    │ After USING, OUTPUT                  │ Parse OUTPUT PROCEDURE      │
    │ After USING, GIVING                  │ Parse GIVING file           │
    │ After output clause, PERIOD/EOF      │ Complete statement          │
    └──────────────────────────────────────┴─────────────────────────────┘
    
    TRUTH TABLE: Sort Key Parsing
    ┌──────────────────────┬─────────────────────────────────────────┐
    │ Token Type           │ Action                                  │
    ├──────────────────────┼─────────────────────────────────────────┤
    │ IDENTIFIER (keyword) │ Check if keyword ends key list          │
    │ IDENTIFIER (data)    │ Add to current key list                 │
    │ COMMA                │ Continue to next key                    │
    │ PERIOD               │ End of statement                        │
    └──────────────────────┴─────────────────────────────────────────┘
    
    Example:
        MERGE SORT-FILE
            ON ASCENDING KEY MERGE-KEY1 MERGE-KEY2
            ON DESCENDING KEY SECONDARY-KEY
            COLLATING SEQUENCE IS MYSEQ
            USING INPUT-FILE-1 INPUT-FILE-2
            GIVING OUTPUT-FILE.
    """
    from cobol_frontend.parser.ast_nodes import COBOLMerge
    
    self.consume(COBOLTokenType.MERGE)
    
    # ═══════════════════════════════════════════════════════════════
    # PHASE 1: Parse target file name
    # ═══════════════════════════════════════════════════════════════
    if not self.match(COBOLTokenType.IDENTIFIER):
        self.error("Expected file name after MERGE")
    target_file = self.consume(COBOLTokenType.IDENTIFIER).value
    
    # ═══════════════════════════════════════════════════════════════
    # PHASE 2: Parse ON ASCENDING/DESCENDING KEY clauses (loop)
    # ═══════════════════════════════════════════════════════════════
    sort_keys = []
    max_key_clauses = 100
    key_clause_count = 0
    
    while key_clause_count < max_key_clauses:
        # Truth table: What token are we at?
        # ┌─────────────────┬──────────────────────────┐
        # │ Token           │ Action                   │
        # ├─────────────────┼──────────────────────────┤
        # │ ON              │ Parse another key clause │
        # │ COLLATING       │ Break to phase 3         │
        # │ USING           │ Break to phase 4         │
        # │ IDENTIFIER      │ Check keyword or error   │
        # │ Other           │ Error                    │
        # └─────────────────┴──────────────────────────┘
        
        current = self.current_token()
        
        # Rule 1: ON token → parse key clause
        if self.match(COBOLTokenType.ON):
            self.advance()  # Consume ON
            
            # Expect ASCENDING or DESCENDING
            if not self.match(COBOLTokenType.ASCENDING, COBOLTokenType.DESCENDING):
                self.error(f"Expected ASCENDING or DESCENDING after ON at line {current.line}")
            
            direction = self.current_token().value.upper()
            self.advance()
            
            # Expect KEY keyword (could be token or identifier)
            if self.match(COBOLTokenType.IDENTIFIER):
                if self.current_token().value.upper() != 'KEY':
                    self.error(f"Expected KEY after {direction}, got {self.current_token().value}")
                self.advance()
            elif hasattr(COBOLTokenType, 'KEY') and self.match(COBOLTokenType.KEY):
                self.advance()
            else:
                self.error(f"Expected KEY after {direction}")
            
            # Parse key field names (loop until keyword)
            keys_for_direction = []
            max_keys = 100
            key_count = 0
            
            while key_count < max_keys:
                # Truth table: Is this a key field or boundary?
                # ┌─────────────────┬─────────────────────────┐
                # │ Token + Value   │ Action                  │
                # ├─────────────────┼─────────────────────────┤
                # │ IDENTIFIER +    │                         │
                # │  not keyword    │ Add to keys             │
                # │ IDENTIFIER +    │                         │
                # │  is keyword     │ Break (boundary)        │
                # │ COMMA           │ Consume, continue       │
                # │ Other           │ Break                   │
                # └─────────────────┴─────────────────────────┘
                
                if self.match(COBOLTokenType.IDENTIFIER):
                    value = self.current_token().value.upper()
                    
                    # Keywords that end the key list
                    boundary_keywords = {'ON', 'COLLATING', 'USING', 'OUTPUT', 'GIVING'}
                    
                    if value in boundary_keywords:
                        # Boundary hit - stop parsing keys
                        break
                    
                    # It's a key field
                    keys_for_direction.append(self.current_token().value)
                    self.advance()
                    key_count += 1
                    
                    # Optional comma separator
                    if self.match(COBOLTokenType.COMMA):
                        self.advance()
                        continue
                    
                elif self.match(COBOLTokenType.COMMA):
                    # Consume comma, continue
                    self.advance()
                    continue
                    
                else:
                    # Not an identifier - stop
                    break
            
            if not keys_for_direction:
                self.error(f"Expected at least one key field name after KEY")
            
            sort_keys.append({
                'direction': direction,
                'keys': keys_for_direction
            })
            
            key_clause_count += 1
            continue
        
        # Rule 2: COLLATING keyword → break to next phase
        elif self.match(COBOLTokenType.COLLATING):
            break
        
        # Rule 3: USING token → break to next phase
        elif self.match(COBOLTokenType.USING):
            break
        
        # Rule 4: IDENTIFIER that might be COLLATING/USING
        elif self.match(COBOLTokenType.IDENTIFIER):
            value = self.current_token().value.upper()
            if value in {'COLLATING', 'USING', 'OUTPUT', 'GIVING'}:
                break
            else:
                self.error(f"Expected ON, COLLATING, or USING, got {value}")
        
        # Rule 5: Other token → break or error
        else:
            break
    
    if not sort_keys:
        self.error("MERGE requires at least one ON ASCENDING/DESCENDING KEY clause")
    
    # ═══════════════════════════════════════════════════════════════
    # PHASE 3: Parse optional COLLATING SEQUENCE
    # ═══════════════════════════════════════════════════════════════
    collating_sequence = None
    
    # Truth table: COLLATING clause parsing
    # ┌─────────────────┬────────────────────────────┐
    # │ Current Token   │ Action                     │
    # ├─────────────────┼────────────────────────────┤
    # │ COLLATING       │ Parse sequence             │
    # │ IDENTIFIER +    │                            │
    # │  "COLLATING"    │ Parse sequence             │
    # │ Other           │ Skip to phase 4            │
    # └─────────────────┴────────────────────────────┘
    
    if self.match(COBOLTokenType.COLLATING):
        self.advance()
        self.consume(COBOLTokenType.SEQUENCE)
        
        # Optional IS
        if self.match(COBOLTokenType.IS):
            self.advance()
        
        if self.match(COBOLTokenType.IDENTIFIER):
            collating_sequence = self.current_token().value
            self.advance()
    
    elif self.match(COBOLTokenType.IDENTIFIER):
        if self.current_token().value.upper() == 'COLLATING':
            self.advance()
            
            # Expect SEQUENCE (as identifier)
            if self.match(COBOLTokenType.IDENTIFIER):
                if self.current_token().value.upper() == 'SEQUENCE':
                    self.advance()
            
            # Optional IS
            if self.match(COBOLTokenType.IS):
                self.advance()
            
            if self.match(COBOLTokenType.IDENTIFIER):
                collating_sequence = self.current_token().value
                self.advance()
    
    # ═══════════════════════════════════════════════════════════════
    # PHASE 4: Parse USING clause (required)
    # ═══════════════════════════════════════════════════════════════
    
    # Truth table: Find USING keyword
    # ┌──────────────────────────┬────────────────────────────┐
    # │ Current Token            │ Action                     │
    # ├──────────────────────────┼────────────────────────────┤
    # │ COBOLTokenType.USING     │ Consume and parse files    │
    # │ IDENTIFIER + "USING"     │ Consume and parse files    │
    # │ Other                    │ Error                      │
    # └──────────────────────────┴────────────────────────────┘
    
    # Rule 1: Check for USING token
    if self.match(COBOLTokenType.USING):
        self.advance()  # Consume USING token
    
    # Rule 2: Check for USING as identifier
    elif self.match(COBOLTokenType.IDENTIFIER) and \
         self.current_token().value.upper() == 'USING':
        self.advance()  # Consume USING identifier
    
    # Rule 3: Not USING - error
    else:
        current = self.current_token()
        self.error(f"Expected USING clause, got {current.type.name}='{current.value}' at line {token.line}")
    
    # Parse input file list
    input_files = []
    max_files = 100
    file_count = 0
    
    while file_count < max_files:
        # Truth table: Is this a file name or boundary?
        # ┌─────────────────┬─────────────────────────┐
        # │ Token + Value   │ Action                  │
        # ├─────────────────┼─────────────────────────┤
        # │ IDENTIFIER +    │                         │
        # │  not keyword    │ Add to files            │
        # │ IDENTIFIER +    │                         │
        # │  OUTPUT/GIVING  │ Break (boundary)        │
        # │ COMMA           │ Consume, continue       │
        # │ Other           │ Break                   │
        # └─────────────────┴─────────────────────────┘
        
        if self.match(COBOLTokenType.IDENTIFIER):
            value = self.current_token().value.upper()
            
            if value in {'OUTPUT', 'GIVING'}:
                # Boundary - stop parsing files
                break
            
            input_files.append(self.current_token().value)
            self.advance()
            file_count += 1
            
            # Optional comma
            if self.match(COBOLTokenType.COMMA):
                self.advance()
                continue
        
        elif self.match(COBOLTokenType.COMMA):
            self.advance()
            continue
        
        else:
            break
    
    if len(input_files) < 2:
        self.error(f"MERGE requires at least 2 input files, got {len(input_files)}")
    
    # ═══════════════════════════════════════════════════════════════
    # PHASE 5: Parse OUTPUT PROCEDURE or GIVING (required, exclusive)
    # ═══════════════════════════════════════════════════════════════
    
    # Truth table: Determine output clause type
    # ┌─────────────────────────┬────────────────────────────┐
    # │ Current Token Value     │ Action                     │
    # ├─────────────────────────┼────────────────────────────┤
    # │ IDENTIFIER + "OUTPUT"   │ Parse OUTPUT PROCEDURE     │
    # │ IDENTIFIER + "GIVING"   │ Parse GIVING file          │
    # │ GIVING token            │ Parse GIVING file          │
    # │ Other                   │ Error                      │
    # └─────────────────────────┴────────────────────────────┘
    
    output_procedure = None
    output_procedure_thru = None
    giving_file = None
    
    # Rule 1: Check for GIVING token
    if self.match(COBOLTokenType.GIVING):
        self.advance()  # GIVING
        
        if not self.match(COBOLTokenType.IDENTIFIER):
            self.error("Expected file name after GIVING")
        giving_file = self.current_token().value
        self.advance()
    
    # Rule 2: Check for IDENTIFIER (OUTPUT or GIVING)
    elif self.match(COBOLTokenType.IDENTIFIER):
        keyword = self.current_token().value.upper()
        
        if keyword == 'OUTPUT':
            # Parse OUTPUT PROCEDURE
            self.advance()  # OUTPUT
            
            # Check for PROCEDURE (as token or identifier)
            if self.match(COBOLTokenType.PROCEDURE):
                self.advance()
            elif self.match(COBOLTokenType.IDENTIFIER) and \
                 self.current_token().value.upper() == 'PROCEDURE':
                self.advance()
            else:
                self.error("Expected PROCEDURE after OUTPUT")
            
            # PROCEDURE consumed above
            
            # Optional IS
            if self.match(COBOLTokenType.IS):
                self.advance()
            
            # Procedure name
            if not self.match(COBOLTokenType.IDENTIFIER):
                self.error("Expected procedure name")
            output_procedure = self.current_token().value
            self.advance()
            
            # Optional THROUGH/THRU
            if self.match(COBOLTokenType.THROUGH, COBOLTokenType.THRU):
                self.advance()
                if not self.match(COBOLTokenType.IDENTIFIER):
                    self.error("Expected procedure name after THROUGH/THRU")
                output_procedure_thru = self.current_token().value
                self.advance()
        
        elif keyword == 'GIVING':
            # Parse GIVING file
            self.advance()  # GIVING
            
            if not self.match(COBOLTokenType.IDENTIFIER):
                self.error("Expected file name after GIVING")
            giving_file = self.current_token().value
            self.advance()
        
        else:
            self.error(f"Expected OUTPUT PROCEDURE or GIVING, got {keyword}")
    
    # Rule 3: Error
    else:
        self.error("Expected OUTPUT PROCEDURE or GIVING in MERGE statement")
    
    # ═══════════════════════════════════════════════════════════════
    # PHASE 6: Complete statement
    # ═══════════════════════════════════════════════════════════════
    self.consume_optional_period()
    
    return COBOLMerge(
        target_file=target_file,
        sort_keys=sort_keys,
        collating_sequence=collating_sequence,
        input_files=input_files,
        output_procedure=output_procedure,
        output_procedure_thru=output_procedure_thru,
        giving_file=giving_file,
        line=token.line,
        column=token.column
    )



def parse_go_to(self, token: Token) -> COBOLASTNode:
    """Parse GO TO statement with truth table approach
    
    TRUTH TABLE: GO TO Statement Patterns
    ┌────────────────────────────────────┬─────────────────────────┐
    │ Pattern                            │ Action                  │
    ├────────────────────────────────────┼─────────────────────────┤
    │ GO TO paragraph-name               │ Simple GO TO            │
    │ GO TO paragraph-name DEPENDING ON  │ GO TO with DEPENDING    │
    │ GO paragraph-name (no TO)          │ Simple GO (COBOL-74)    │
    │ GO (alone - requires ALTER)        │ Altered GO              │
    └────────────────────────────────────┴─────────────────────────┘
    
    Examples:
    - GO TO PARA-1.
    - GO TO SECTION-A.
    - GO TO GO--A.           ← Paragraph name starts with GO
    - GO TO EXIT-RTN DEPENDING ON X.
    - GO TO PARA-1 PARA-2 PARA-3 DEPENDING ON IDX.  ← Multiple targets
    - GO.                    ← Altered GO (rarely used)
    """
    line, col = token.line, token.column
    
    # Step 1: Consume GO keyword (already at GO token)
    self.advance()
    
    # Step 2: Check for optional TO keyword
    # COBOL allows both "GO TO" and "GO" (COBOL-74 style)
    if self.match(COBOLTokenType.TO):
        self.advance()
    
    # Step 3: Parse paragraph name(s)
    # TRUTH TABLE: What comes after GO [TO]?
    # ┌─────────────────┬──────────────────────────────────┐
    # │ Token Type      │ Action                           │
    # ├─────────────────┼──────────────────────────────────┤
    # │ IDENTIFIER      │ Parse as paragraph name          │
    # │ NUMBER_LITERAL  │ Could be numeric paragraph name  │
    # │ PERIOD          │ Altered GO (no target)           │
    # │ Other           │ Error                            │
    # └─────────────────┴──────────────────────────────────┘
    
    # Check for altered GO (GO. with no target)
    if self.match(COBOLTokenType.PERIOD):
        self.consume(COBOLTokenType.PERIOD)
        # This is an altered GO - requires ALTER statement to work
        return COBOLPerformParagraph(
            paragraph_name=None,  # No target - will be set by ALTER
            line=line,
            column=col
        )
    
    # Parse paragraph name (can be IDENTIFIER or NUMBER_LITERAL)
    # CRITICAL: Do NOT check for paragraph declaration here!
    # The target of GO TO is always an identifier, never a new paragraph
    if not self.match(COBOLTokenType.IDENTIFIER, COBOLTokenType.NUMBER_LITERAL):
        current_tok = self.current_token()
        tok_info = f"{current_tok.type.name}='{current_tok.value}'" if current_tok else "EOF"
        self.error(f"Expected paragraph name after GO TO, got {tok_info}")
    
    paragraph_names = []
    paragraph_names.append(self.current_token().value)
    self.advance()
    
    # Step 4: Check for multiple paragraph names (GO TO p1 p2 p3 DEPENDING ON)
    # Keep parsing identifiers until we hit DEPENDING, PERIOD, or statement boundary
    while self.match(COBOLTokenType.IDENTIFIER, COBOLTokenType.NUMBER_LITERAL):
        # Check if this is DEPENDING keyword
        if self.match(COBOLTokenType.IDENTIFIER) and \
           self.current_token().value.upper() == 'DEPENDING':
            break
        
        # Check if next token is PERIOD (end of GO TO)
        next_tok = self.peek_token(1)
        if next_tok and next_tok.type == COBOLTokenType.PERIOD:
            # Don't consume this identifier - it might be next paragraph name
            break
        
        # Another paragraph name
        paragraph_names.append(self.current_token().value)
        self.advance()
    
    # Step 5: Check for DEPENDING ON clause
    depending_on = None
    if self.match(COBOLTokenType.IDENTIFIER):
        if self.current_token().value.upper() == 'DEPENDING':
            self.advance()  # Consume DEPENDING
            
            # ✅ FIX: Make ON keyword optional (lenient mode)
            # Check for optional ON keyword (can be token type or identifier)
            on_found = False
            
            if self.match(COBOLTokenType.ON):
                # ON as token type
                self.advance()
                on_found = True
            elif self.match(COBOLTokenType.IDENTIFIER) and \
                self.current_token().value.upper() == 'ON':
                # ON as identifier
                self.advance()
                on_found = True
            # else: No ON keyword - next token should be the variable name (lenient)
            
            # Parse the variable name (REQUIRED)
            if not self.match(COBOLTokenType.IDENTIFIER):
                self.error(f"Expected variable name after DEPENDING {'ON' if on_found else ''}")
            
            depending_on = self.current_token().value
            self.advance()
    
    # Step 6: Consume optional period
    self.consume_optional_period()
    
    # Step 7: Return appropriate node
    # If single paragraph name, return simple GO TO
    # If multiple names, return GO TO with depending (even if depending_on is None - invalid but parseable)
    if len(paragraph_names) == 1 and depending_on is None:
        return COBOLPerformParagraph(
            paragraph_name=paragraph_names[0],
            line=line,
            column=col
        )
    else:
        # Multiple targets or DEPENDING clause
        # Use a different node or extend COBOLPerformParagraph to handle this
        # For now, just use the first paragraph name
        return COBOLPerformParagraph(
            paragraph_name=paragraph_names[0] if paragraph_names else None,
            line=line,
            column=col
        )

def parse_exit(self, token: Token) -> COBOLExit:
    """
    Parse EXIT or EXIT PROGRAM statement
    
    Forms:
      EXIT.
      EXIT PROGRAM.
    """
    exit_token = self.current_token()
    print(f"[DEBUG EXIT] Line {exit_token.line}, consumed EXIT")
    self.consume(COBOLTokenType.EXIT)
    
    # Debug: What's the next token?
    next_tok = self.current_token()
    print(f"[DEBUG EXIT] Next token: type={next_tok.type.name}, value='{next_tok.value}'")
    
    is_program = False
    if self.match(COBOLTokenType.PROGRAM):
        print(f"[DEBUG EXIT] ✓ Matched PROGRAM keyword!")
        is_program = True
        self.advance()
    else:
        print(f"[DEBUG EXIT] ✗ Did NOT match PROGRAM keyword")
        print(f"[DEBUG EXIT]   Current token is: {self.current_token().type.name} = '{self.current_token().value}'")
    
    # Debug: What's after PROGRAM?
    if is_program:
        after_tok = self.current_token()
        print(f"[DEBUG EXIT] After PROGRAM: type={after_tok.type.name}, value='{after_tok.value}'")
    
    # Consume the required period
    if self.requires_period():
        print(f"[DEBUG EXIT] Requires period, current token: {self.current_token().type.name}")
        if not self.match(COBOLTokenType.PERIOD):
            print(f"[DEBUG EXIT] ⚠️  ERROR: Expected PERIOD but got {self.current_token().type.name} = '{self.current_token().value}'")
            # Show surrounding tokens for context
            print(f"[DEBUG EXIT] Token position: {self.pos}")
            if self.pos > 0:
                print(f"[DEBUG EXIT]   Previous: {self.tokens[self.pos-1].type.name} = '{self.tokens[self.pos-1].value}'")
            if self.pos < len(self.tokens) - 1:
                print(f"[DEBUG EXIT]   Next: {self.tokens[self.pos+1].type.name} = '{self.tokens[self.pos+1].value}'")
        self.consume(COBOLTokenType.PERIOD)
    
    print(f"[DEBUG EXIT] ✓ Successfully parsed EXIT (is_program={is_program})")
    return COBOLExit(is_program=is_program, line=exit_token.line, column=exit_token.column)

def parse_goback(self, token: Token) -> COBOLGoback:
    """Parse GOBACK statement"""
    self.consume(COBOLTokenType.GOBACK)
    
    if self.requires_period():
        self.consume(COBOLTokenType.PERIOD)
    
    return COBOLGoback(line=token.line, column=token.column)


def parse_call(self, token: Token) -> COBOLCall:
    """Parse CALL statement using state machine approach to prevent infinite recursion"""
    
    # State definitions
    STATE_EXPECT_PROGRAM = 0
    STATE_AFTER_PROGRAM = 1
    STATE_IN_USING = 2
    STATE_IN_OVERFLOW = 3
    STATE_DONE = 4
    
    print(f"[DEBUG] parse_call started at line {token.line}")
    
    state = STATE_EXPECT_PROGRAM
    prog_name = None
    parameters = []
    overflow_statements = []
    
    self.consume(COBOLTokenType.CALL)
    max_iterations = 100  # Safety limit
    iteration = 0
    
    while state != STATE_DONE and iteration < max_iterations:
        iteration += 1
        
        if state == STATE_EXPECT_PROGRAM:
            # Get program name (string literal or identifier)
            if self.match(COBOLTokenType.STRING_LITERAL):
                prog_name = self.consume(COBOLTokenType.STRING_LITERAL).value.strip('"\'')
            elif self.match(COBOLTokenType.IDENTIFIER):
                prog_name = self.consume(COBOLTokenType.IDENTIFIER).value
            else:
                print(f"[ERROR] parse_call: Expected program name at line {self.current_token().line}")
                state = STATE_DONE
                break
            state = STATE_AFTER_PROGRAM
            
        elif state == STATE_AFTER_PROGRAM:
            # Check what comes after program name
            if self.match(COBOLTokenType.PERIOD):
                state = STATE_DONE
            elif self.match(COBOLTokenType.EOF):
                state = STATE_DONE
            elif self.match(COBOLTokenType.USING):
                self.consume(COBOLTokenType.USING)
                state = STATE_IN_USING
            elif self.match(COBOLTokenType.IDENTIFIER):
                val = self.current_token().value.upper()
                if val == 'ON':
                    self.advance()  # consume ON
                    if self.match(COBOLTokenType.IDENTIFIER):
                        next_val = self.current_token().value.upper()
                        if next_val in ('OVERFLOW', 'EXCEPTION'):
                            self.advance()  # consume OVERFLOW/EXCEPTION
                            state = STATE_IN_OVERFLOW
                        else:
                            # Unknown keyword after ON
                            state = STATE_DONE
                    else:
                        state = STATE_DONE
                elif val in ('OVERFLOW', 'EXCEPTION'):
                    self.advance()  # consume OVERFLOW/EXCEPTION
                    state = STATE_IN_OVERFLOW
                else:
                    # Unknown identifier - probably next statement
                    print(f"[DEBUG] parse_call: Unknown identifier '{val}' at line {self.current_token().line}, ending CALL")
                    state = STATE_DONE
            else:
                # Unknown token type
                state = STATE_DONE
                
        elif state == STATE_IN_USING:
            # Collect USING parameters
            if self.match(COBOLTokenType.IDENTIFIER):
                parameters.append(self.consume(COBOLTokenType.IDENTIFIER).value)
                # Check for comma (optional in COBOL)
                if self.match(COBOLTokenType.COMMA):
                    self.advance()
                # Stay in USING state to get next parameter
            elif self.match(COBOLTokenType.PERIOD):
                state = STATE_DONE
            elif self.match(COBOLTokenType.EOF):
                state = STATE_DONE
            else:
                # Check if ON OVERFLOW/EXCEPTION follows USING
                if self.match(COBOLTokenType.IDENTIFIER):
                    val = self.current_token().value.upper()
                    if val in ('ON', 'OVERFLOW', 'EXCEPTION'):
                        # Transition to handle overflow
                        state = STATE_AFTER_PROGRAM
                        continue
                # Unknown token - done with USING
                state = STATE_DONE
                
        elif state == STATE_IN_OVERFLOW:
            # Parse overflow/exception statements
            # CRITICAL: Do NOT call parse_statement() recursively!
            
            if self.match(COBOLTokenType.PERIOD):
                state = STATE_DONE
            elif self.match(COBOLTokenType.EOF):
                state = STATE_DONE
            elif self.match(COBOLTokenType.IDENTIFIER):
                # Check if it's a new paragraph (IDENTIFIER followed by PERIOD)
                next_tok = self.peek_token()
                if next_tok and next_tok.type == COBOLTokenType.PERIOD:
                    # It's a paragraph declaration - we're done
                    print(f"[DEBUG] parse_call: Found paragraph '{self.current_token().value}' at line {self.current_token().line}, ending CALL")
                    state = STATE_DONE
                else:
                    # It's a variable reference or unknown - skip it
                    print(f"[DEBUG] parse_call: Skipping identifier '{self.current_token().value}' in OVERFLOW")
                    self.advance()
            elif self.match(COBOLTokenType.MOVE):
                # Parse MOVE statement directly
                stmt_token = self.current_token()
                stmt = self.parse_move(stmt_token)
                if stmt:
                    overflow_statements.append(stmt)
            elif self.match(COBOLTokenType.DISPLAY):
                stmt_token = self.current_token()
                stmt = self.parse_display(stmt_token)
                if stmt:
                    overflow_statements.append(stmt)
            elif self.match(COBOLTokenType.PERFORM):
                stmt_token = self.current_token()
                stmt = self.parse_perform(stmt_token)
                if stmt:
                    overflow_statements.append(stmt)
            elif self.match(COBOLTokenType.GO, COBOLTokenType.GOTO):
                stmt_token = self.current_token()
                stmt = self.parse_go_to(stmt_token)
                if stmt:
                    overflow_statements.append(stmt)
            else:
                # Unknown token in overflow - exit
                print(f"[DEBUG] parse_call: Unknown token {self.current_token().type.name} in OVERFLOW, ending CALL")
                state = STATE_DONE
    
    if iteration >= max_iterations:
        print(f"[WARNING] parse_call: Hit iteration limit at line {token.line}")
    
    # Consume optional trailing period
    if self.match(COBOLTokenType.PERIOD):
        self.consume(COBOLTokenType.PERIOD)
    
    print(f"[DEBUG] parse_call completed: program={prog_name}, params={len(parameters)}, overflow_stmts={len(overflow_statements)}")
    
    return COBOLCall(
        program_name=prog_name,
        parameters=parameters if parameters else [],
        overflow_statements=overflow_statements if overflow_statements else None,
        line=token.line, 
        column=token.column
    )


def parse_if(self, token: Token) -> COBOLIf:
    """Parse IF statement with comprehensive truth table approach
    
    TRUTH TABLE: IF Statement Structure
    ┌────────────────────────────────────────┬─────────────────────────┐
    │ Component                              │ Required?               │
    ├────────────────────────────────────────┼─────────────────────────┤
    │ IF condition                           │ YES                     │
    │ THEN (keyword)                         │ NO (optional)           │
    │ THEN statements                        │ YES (at least 1)        │
    │ ELSE                                   │ NO (optional)           │
    │ ELSE statements                        │ YES (if ELSE present)   │
    │ END-IF or PERIOD                       │ YES (one or the other)  │
    └────────────────────────────────────────┴─────────────────────────┘
    
    TRUTH TABLE: When to stop parsing THEN/ELSE statements
    ┌────────────────────────────────────────┬─────────────────────────┐
    │ Token                                  │ Action                  │
    ├────────────────────────────────────────┼─────────────────────────┤
    │ ELSE                                   │ Break - start ELSE      │
    │ END-IF                                 │ Break - end of IF       │
    │ PERIOD                                 │ Break - statement end   │
    │ EOF                                    │ Break - emergency       │
    │ SECTION                                │ Break - boundary        │
    │ IDENTIFIER SECTION                     │ Break - boundary        │
    │ IDENTIFIER PERIOD (new paragraph)      │ Break - boundary        │
    │ Division/Program boundary              │ Break - emergency       │
    │ Other                                  │ Continue - parse stmt   │
    └────────────────────────────────────────┴─────────────────────────┘
    """
    self.consume(COBOLTokenType.IF)
    
    # Parse condition
    condition = self.parse_condition()
    
    # Optional THEN keyword
    if self.match(COBOLTokenType.THEN):
        self.advance()
    
    self.push_context(ParseContext.INSIDE_IF)
    
    # Parse THEN statements
    then_statements = self._parse_if_statements_block()
    
    # Parse optional ELSE block
    else_statements = None
    if self.match(COBOLTokenType.ELSE):
        self.advance()
        else_statements = self._parse_if_statements_block()
    
    # Consume terminator (END-IF or PERIOD)
    if self.match(COBOLTokenType.END_IF):
        self.consume(COBOLTokenType.END_IF)
        if self.requires_period() and self.match(COBOLTokenType.PERIOD):
            self.consume(COBOLTokenType.PERIOD)
    elif self.match(COBOLTokenType.PERIOD):
        self.consume(COBOLTokenType.PERIOD)
    elif self.match(COBOLTokenType.EOF):
        self.pop_context()
        raise ParserError("IF statement missing END-IF or PERIOD", 
                        token.line, token.column)
    # If we hit a boundary, that's OK - old-style COBOL allows IF without explicit end
    
    self.pop_context()
    
    return COBOLIf(
        condition=condition,
        then_statements=then_statements,
        else_statements=else_statements,
        line=token.line,
        column=token.column
    )


def _parse_if_statements_block(self) -> List[COBOLASTNode]:
    """Parse a block of statements inside IF/ELSE using truth table
    
    TRUTH TABLE: Exit conditions for IF statement block
    ┌─────────────────────────────┬──────────────────────────────┐
    │ Current Token Pattern       │ Action                       │
    ├─────────────────────────────┼──────────────────────────────┤
    │ ELSE                        │ Break - next clause          │
    │ END-IF                      │ Break - end of IF            │
    │ PERIOD                      │ Break - statement end        │
    │ EOF                         │ Break - end of input         │
    │ SECTION (token)             │ Break - section boundary     │
    │ IDENTIFIER + SECTION        │ Break - section declaration  │
    │ IDENTIFIER + PERIOD + stmt  │ Break - new paragraph        │
    │ Division boundary           │ Break - emergency            │
    │ Program boundary            │ Break - emergency            │
    │ Statement keyword           │ Continue - parse it          │
    └─────────────────────────────┴──────────────────────────────┘
    """
    statements = []
    max_iterations = 10000
    iteration = 0
    
    while iteration < max_iterations:
        iteration += 1
        
        # TRUTH TABLE: Check exit conditions
        
        # Rule 1: IF/ELSE terminators
        if self.match(COBOLTokenType.ELSE, COBOLTokenType.END_IF, 
                      COBOLTokenType.PERIOD, COBOLTokenType.EOF):
            break
        
        # Rule 2: Section boundaries (bare SECTION token)
        if self.match(COBOLTokenType.SECTION):
            break
        
        # Rule 3: Division/Program boundaries
        if self._is_division_boundary() or self._is_program_boundary():
            break
        
        # Rule 4: Check for IDENTIFIER patterns (section or paragraph)
        if self.match(COBOLTokenType.IDENTIFIER):
            next_tok = self.peek_token(1)
            
            # IDENTIFIER SECTION. → section boundary
            if next_tok and next_tok.type == COBOLTokenType.SECTION:
                break
            
            # IDENTIFIER PERIOD → might be new paragraph
            if next_tok and next_tok.type == COBOLTokenType.PERIOD:
                # Check if it's a new paragraph declaration
                if self._is_new_paragraph_declaration():
                    break
                # Otherwise it's a statement, continue parsing
        
        # Rule 5: Parse the statement
        stmt = self.parse_statement()
        if stmt:
            statements.append(stmt)
        else:
            # parse_statement returned None - might have hit boundary
            break
    
    if iteration >= max_iterations:
        print(f"[WARNING] IF statement block iteration limit reached")
    
    return statements


def parse_copy_statement(self, token: Token) -> None:
    """Parse and skip COPY statement (preprocessor directive)
    
    TRUTH TABLE: COPY Statement Parsing
    ┌────────────────────────────────┬───────────────────────────────┐
    │ Current Token                  │ Action                        │
    ├────────────────────────────────┼───────────────────────────────┤
    │ PERIOD                         │ Consume and return (done)     │
    │ EOF                            │ Return (unterminated)         │
    │ EQUALS_SIGN                    │ Skip (part of == delimiter)   │
    │ Any other token                │ Skip                          │
    └────────────────────────────────┴───────────────────────────────┘
    
    COPY statements are preprocessor directives included in NIST tests.
    Pattern: COPY library-name [REPLACING ==text== BY ==replacement==].
    
    The == delimiters are lexed as individual EQUALS_SIGN tokens, so we
    skip everything until we hit a PERIOD.
    
    Examples:
    1. COPY TEXTFILE.
    2. COPY KP004 REPLACING ==OLD== BY ==NEW==.
    3. COPY LIB REPLACING ==A== BY ==B== ==C== BY ==D==.
    """
    print(f"[COPY] Skipping COPY statement at line {token.line}")
    self.advance()  # Consume COPY keyword
    
    # Skip everything until we find a period
    max_iterations = 1000
    iteration = 0
    equals_count = 0  # Debug counter
    
    while iteration < max_iterations and not self.match(COBOLTokenType.EOF):
        current = self.current_token()
        
        # ══════════════════════════════════════════════════════════════
        # ROW 1: PERIOD → end of COPY statement
        # ══════════════════════════════════════════════════════════════
        if current.type == COBOLTokenType.PERIOD:
            self.advance()  # Consume the period
            print(f"[COPY] Skipped COPY statement ({iteration} tokens, {equals_count} equals)")
            return None
        
        # ══════════════════════════════════════════════════════════════
        # ROW 2: EOF → error (unterminated COPY)
        # ══════════════════════════════════════════════════════════════
        if current.type == COBOLTokenType.EOF:
            print(f"[ERROR] Unterminated COPY statement starting at line {token.line}")
            return None
        
        # ══════════════════════════════════════════════════════════════
        # ROW 3: EQUALS_SIGN → part of == delimiter
        # ══════════════════════════════════════════════════════════════
        if current.type == COBOLTokenType.EQUALS_SIGN:
            equals_count += 1
        
        # ══════════════════════════════════════════════════════════════
        # ROW 4: Any other token → skip it
        # ══════════════════════════════════════════════════════════════
        self.advance()
        iteration += 1
    
    # Safety check
    if iteration >= max_iterations:
        print(f"[WARNING] COPY statement hit iteration limit at line {token.line}")
    
    return None


def parse_perform(self, token: Token) -> COBOLASTNode:
    """Parse PERFORM statement with truth table approach
    
    TRUTH TABLE: PERFORM Statement Patterns
    ┌─────────────────────────────────────┬────────────────────────┐
    │ Pattern                             │ Action                 │
    ├─────────────────────────────────────┼────────────────────────┤
    │ PERFORM UNTIL condition             │ parse_perform_until    │
    │ PERFORM VARYING var...              │ parse_perform_varying  │
    │ PERFORM paragraph UNTIL condition   │ parse_perform_until    │
    │ PERFORM paragraph VARYING var...    │ parse_perform_varying  │
    │ PERFORM paragraph TIMES expr        │ parse_perform_times    │
    │ PERFORM paragraph THRU paragraph    │ THRU variant           │
    │ PERFORM 00                          │ Numeric paragraph name │
    │ PERFORM paragraph                   │ Simple paragraph call  │
    │ PERFORM expr TIMES (inline)         │ parse_perform_times    │
    └─────────────────────────────────────┴────────────────────────┘
    
    TRUTH TABLE: Paragraph Name Recognition
    ┌────────────────┬──────────────────────────────────┐
    │ Token Type     │ Is Valid Paragraph Name?         │
    ├────────────────┼──────────────────────────────────┤
    │ IDENTIFIER     │ YES (standard paragraph name)    │
    │ NUMBER_LITERAL │ YES (COBOL-74 numeric sections)  │
    │ Other          │ NO                               │
    └────────────────┴──────────────────────────────────┘
    """
    self.consume(COBOLTokenType.PERFORM)
    
    # TRUTH TABLE: Determine which PERFORM variant based on next tokens
    
    # Rule 1: PERFORM UNTIL (no paragraph name)
    if self.match(COBOLTokenType.UNTIL):
        return self.parse_perform_until(token, None)
    
    # Rule 2: PERFORM VARYING (no paragraph name)
    if self.match(COBOLTokenType.VARYING):
        return self.parse_perform_varying(token)
    
    # Rule 2.5: PERFORM with inline statements (no paragraph, no UNTIL/VARYING)
    # Pattern: PERFORM MOVE... ADD... END-PERFORM
    if self.match(COBOLTokenType.MOVE, COBOLTokenType.ADD, COBOLTokenType.SUBTRACT,
                COBOLTokenType.MULTIPLY, COBOLTokenType.DIVIDE, COBOLTokenType.COMPUTE,
                COBOLTokenType.IF, COBOLTokenType.DISPLAY, COBOLTokenType.ACCEPT,
                COBOLTokenType.SET, COBOLTokenType.STRING, COBOLTokenType.UNSTRING):
        
        self.push_context(ParseContext.INSIDE_LOOP)
        statements = []
        
        while not self.match(COBOLTokenType.END_PERFORM, COBOLTokenType.EOF):
            stmt = self.parse_statement()
            if stmt:
                statements.append(stmt)
        
        if self.match(COBOLTokenType.END_PERFORM):
            self.consume(COBOLTokenType.END_PERFORM)
        else:
            self.pop_context()
            raise ParserError("PERFORM inline missing END-PERFORM", token.line, token.column)
        
        self.pop_context()
        self.consume_optional_period()
        
        return COBOLPerformInline(
            statements=statements,
            line=token.line,
            column=token.column
        )
        
    
    # Rule 3: PERFORM number/variable TIMES (inline statements follow)
    # NOTE: This must come BEFORE Rule 4 to avoid conflict with numeric paragraph names
    if self.match(COBOLTokenType.NUMBER_LITERAL, COBOLTokenType.IDENTIFIER):
        peek1 = self.peek_token(1)
        if peek1 and peek1.type == COBOLTokenType.TIMES:
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
    
    # ═══════════════════════════════════════════════════════════════
    # Rule 4: PERFORM paragraph-name [THRU] [TIMES] [UNTIL] [VARYING]
    # ✅ FIXED: Accept both IDENTIFIER and NUMBER_LITERAL as paragraph names
    # ═══════════════════════════════════════════════════════════════
    if self.match(COBOLTokenType.IDENTIFIER, COBOLTokenType.NUMBER_LITERAL):
        # Get paragraph name (can be identifier or number)
        para_name = self.current_token().value
        self.advance()
        
        # ───────────────────────────────────────────────────────────
        # Sub-truth table: What comes after paragraph name?
        # ───────────────────────────────────────────────────────────
        # | Next Token | Action                              |
        # |------------|-------------------------------------|
        # | THRU       | Parse THRU paragraph                |
        # | TIMES      | Parse TIMES count                   |
        # | UNTIL      | Parse UNTIL condition               |
        # | VARYING    | Parse VARYING clause                |
        # | PERIOD/EOF | Simple paragraph call               |
        # | Statement  | Simple paragraph call (no period)   |
        # ───────────────────────────────────────────────────────────
        
        # Check for THRU clause
        if self.match(COBOLTokenType.THRU):
            self.advance()
            
            # THRU paragraph name can also be numeric!
            if not self.match(COBOLTokenType.IDENTIFIER, COBOLTokenType.NUMBER_LITERAL):
                self.error("Expected paragraph name after THRU")
            
            thru_para = self.current_token().value
            self.advance()
            self.consume_optional_period()
            
            return COBOLPerformParagraph(
                paragraph_name=para_name,
                thru_paragraph=thru_para,
                line=token.line,
                column=token.column
            )
        
        # Check for TIMES
        elif self.match(COBOLTokenType.TIMES):
            return self.parse_perform_times_inline(token)
        
        # Check for UNTIL
        elif self.match(COBOLTokenType.UNTIL):
            return self.parse_perform_until(token, para_name)
        
        # Check for VARYING
        elif self.match(COBOLTokenType.VARYING):
            return self.parse_perform_varying(token, para_name)
        
        # Simple paragraph call
        else:
            # Check if next token is a statement keyword (boundary)
            # Pattern: "PERFORM PASS GO TO..." → PERFORM ends, GO starts new statement
            if self.match(COBOLTokenType.GO, COBOLTokenType.MOVE, COBOLTokenType.IF,
                        COBOLTokenType.ADD, COBOLTokenType.SUBTRACT, COBOLTokenType.MULTIPLY,
                        COBOLTokenType.DIVIDE, COBOLTokenType.COMPUTE, COBOLTokenType.ACCEPT,
                        COBOLTokenType.DISPLAY, COBOLTokenType.CALL, COBOLTokenType.STOP,
                        COBOLTokenType.EXIT, COBOLTokenType.EVALUATE, COBOLTokenType.PERFORM):
                # Next token is a statement keyword - PERFORM is done
                return COBOLPerformParagraph(
                    paragraph_name=para_name,
                    line=token.line,
                    column=token.column
                )
            
            # Otherwise consume optional period as normal
            self.consume_optional_period()
            return COBOLPerformParagraph(
                paragraph_name=para_name,
                line=token.line,
                column=token.column
            )
            
    # If we get here, invalid syntax
    self.error(f"Invalid PERFORM statement syntax at line {token.line}")


def parse_perform_until(self, token: Token, paragraph_name: Optional[str]) -> COBOLPerformUntil:
    """Parse PERFORM [paragraph] UNTIL condition [statements END-PERFORM]"""
    self.consume(COBOLTokenType.UNTIL)
    condition = self.parse_condition()
    
    statements = []
    if paragraph_name is None:
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
    """Parse PERFORM VARYING with truth table approach and full subscript support
    
    TRUTH TABLE: PERFORM VARYING Syntax Patterns
    ┌────────────────────────────────────────────┬─────────────────────────────┐
    │ Pattern                                    │ Form                        │
    ├────────────────────────────────────────────┼─────────────────────────────┤
    │ PERFORM VARYING var FROM x BY y UNTIL cond │ Inline (no paragraph)       │
    │ PERFORM para VARYING var FROM x BY y...    │ Paragraph call with VARYING │
    └────────────────────────────────────────────┴─────────────────────────────┘
    
    TRUTH TABLE: Variable Patterns in VARYING Clause
    ┌────────────────────────┬────────────────────────────────────────┐
    │ Variable Form          │ Example                                │
    ├────────────────────────┼────────────────────────────────────────┤
    │ Simple identifier      │ VARYING I                              │
    │ Subscripted variable   │ VARYING ARRAY(INDEX)                   │
    │ Multi-subscript        │ VARYING TABLE(I, J)                    │
    │ Qualified name         │ VARYING FIELD OF RECORD                │
    └────────────────────────┴────────────────────────────────────────┘
    
    TRUTH TABLE: FROM/BY Expression Patterns
    ┌────────────────────────┬────────────────────────────────────────┐
    │ Expression Type        │ Example                                │
    ├────────────────────────┼────────────────────────────────────────┤
    │ Literal                │ FROM 1                                 │
    │ Variable               │ FROM START-VALUE                       │
    │ Subscripted variable   │ FROM ARRAY(1)                          │
    │ Arithmetic expression  │ FROM START-VAL + 10                    │
    └────────────────────────┴────────────────────────────────────────┘
    
    Examples:
        1. Simple inline loop:
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
               DISPLAY I
           END-PERFORM.
        
        2. Subscripted variable:
           PERFORM VARYING ARRAY(INDEX) FROM 10 BY INCREMENT UNTIL ARRAY(INDEX) > 70
               COMPUTE X = ARRAY(INDEX) * 2
           END-PERFORM.
        
        3. Paragraph call:
           PERFORM PROCESS-ITEM VARYING COUNTER FROM 1 BY 1 UNTIL COUNTER > MAX-ITEMS.
        
        4. Complex expressions:
           PERFORM VARYING TABLE(I, J) FROM START-VAL BY STEP-SIZE(K) UNTIL TABLE(I, J) = LIMIT.
    
    CRITICAL: Variable, FROM, and BY can all be subscripted expressions, not just identifiers!
    """
    if self.debug:
        print(f"[PARSE_PERFORM_VARYING] Starting at pos={self.pos}")
    
    # STEP 1: Consume VARYING keyword
    self.consume(COBOLTokenType.VARYING)
    
    # ═══════════════════════════════════════════════════════════════════════════
    # STEP 2: Parse variable (can be simple identifier or subscripted expression)
    # ═══════════════════════════════════════════════════════════════════════════
    # TRUTH TABLE: What can follow VARYING?
    # ┌──────────────────┬────────────────────────────────────────────────────┐
    # │ Token Type       │ Action                                             │
    # ├──────────────────┼────────────────────────────────────────────────────┤
    # │ IDENTIFIER       │ Parse as expression (handles subscripts, OF, etc.) │
    # │ Other            │ Error                                              │
    # └──────────────────┴────────────────────────────────────────────────────┘
    
    if not self.match(COBOLTokenType.IDENTIFIER):
        self.error(f"Expected variable name after VARYING")
    
    # ✅ FIX: Use parse_expression() to handle subscripts like ARRAY(INDEX)
    # This supports:
    # - Simple: I
    # - Subscripted: ARRAY(INDEX)
    # - Multi-subscript: TABLE(I, J)
    # - Qualified: FIELD OF RECORD
    variable = self.parse_expression()
    
    if self.debug:
        print(f"[PARSE_PERFORM_VARYING] Variable: {variable}")
    
    # ═══════════════════════════════════════════════════════════════════════════
    # STEP 3: Parse FROM clause (required)
    # ═══════════════════════════════════════════════════════════════════════════
    if not self.match(COBOLTokenType.FROM):
        self.error(f"Expected FROM after VARYING variable")
    
    self.consume(COBOLTokenType.FROM)
    from_expr = self.parse_expression()
    
    if self.debug:
        print(f"[PARSE_PERFORM_VARYING] FROM: {from_expr}")
    
    # ═══════════════════════════════════════════════════════════════════════════
    # STEP 4: Parse BY clause (required)
    # ═══════════════════════════════════════════════════════════════════════════
    if not self.match(COBOLTokenType.BY):
        self.error(f"Expected BY after FROM expression")
    
    self.consume(COBOLTokenType.BY)
    by_expr = self.parse_expression()
    
    if self.debug:
        print(f"[PARSE_PERFORM_VARYING] BY: {by_expr}")
    
    # ═══════════════════════════════════════════════════════════════════════════
    # STEP 5: Parse UNTIL clause (required)
    # ═══════════════════════════════════════════════════════════════════════════
    if not self.match(COBOLTokenType.UNTIL):
        self.error(f"Expected UNTIL after BY expression")
    
    self.consume(COBOLTokenType.UNTIL)
    until_condition = self.parse_condition()
    
    if self.debug:
        print(f"[PARSE_PERFORM_VARYING] UNTIL condition parsed")
    
    # ═══════════════════════════════════════════════════════════════════════════
    # STEP 6: Parse body statements (inline) or skip (paragraph call)
    # ═══════════════════════════════════════════════════════════════════════════
    # TRUTH TABLE: Statement Body Patterns
    # ┌───────────────────────┬──────────────────────────────────────────────┐
    # │ paragraph_name value  │ Action                                       │
    # ├───────────────────────┼──────────────────────────────────────────────┤
    # │ None                  │ Parse inline statements until END-PERFORM    │
    # │ Not None              │ Skip statements (calls paragraph)            │
    # └───────────────────────┴──────────────────────────────────────────────┘
    
    self.push_context(ParseContext.INSIDE_LOOP)
    
    statements = []
    
    if paragraph_name is None:
        # Inline PERFORM VARYING - parse statements until END-PERFORM
        if self.debug:
            print(f"[PARSE_PERFORM_VARYING] Parsing inline statements...")
        
        # Safety limit for infinite loops
        max_iterations = 10000
        iteration = 0
        
        while not self.match(COBOLTokenType.END_PERFORM, COBOLTokenType.EOF):
            iteration += 1
            if iteration >= max_iterations:
                self.pop_context()
                self.error("PERFORM VARYING: Too many statements (possible infinite loop)")
            
            stmt = self.parse_statement()
            if stmt:
                statements.append(stmt)
        
        # TRUTH TABLE: Loop Termination
        # ┌──────────────────┬─────────────────────────────────────────────┐
        # │ Current Token    │ Action                                      │
        # ├──────────────────┼─────────────────────────────────────────────┤
        # │ END-PERFORM      │ Consume and continue (valid termination)    │
        # │ EOF              │ Error - missing END-PERFORM                 │
        # │ Other            │ Should not reach (loop condition prevents)  │
        # └──────────────────┴─────────────────────────────────────────────┘
        
        if self.match(COBOLTokenType.END_PERFORM):
            self.consume(COBOLTokenType.END_PERFORM)
            if self.debug:
                print(f"[PARSE_PERFORM_VARYING] ✓ Found END-PERFORM, parsed {len(statements)} statements")
        elif self.match(COBOLTokenType.EOF):
            self.pop_context()
            raise ParserError(
                "PERFORM VARYING missing END-PERFORM before end of file",
                token.line, token.column
            )
    else:
        # Paragraph call - no inline statements
        if self.debug:
            print(f"[PARSE_PERFORM_VARYING] Paragraph call: {paragraph_name}")
    
    self.pop_context()
    
    # ═══════════════════════════════════════════════════════════════════════════
    # STEP 7: Consume optional period (context-dependent)
    # ═══════════════════════════════════════════════════════════════════════════
    self.consume_optional_period()
    
    # ═══════════════════════════════════════════════════════════════════════════
    # STEP 8: Return AST node
    # ═══════════════════════════════════════════════════════════════════════════
    if self.debug:
        print(f"[PARSE_PERFORM_VARYING] ✓ Complete")
    
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


def parse_evaluate(self, token: Token) -> COBOLEvaluate:
    """Parse EVALUATE statement with truth table approach
    
    TRUTH TABLE: EVALUATE Subject Patterns
    ┌────────────────────────────────────┬─────────────────────────┐
    │ Pattern                            │ Type                    │
    ├────────────────────────────────────┼─────────────────────────┤
    │ TRUE                               │ Boolean literal         │
    │ FALSE                              │ Boolean literal         │
    │ identifier [NOT] class-condition   │ Class test              │
    │ expression                         │ General expression      │
    └────────────────────────────────────┴─────────────────────────┘
    
    Class conditions: NUMERIC, ALPHABETIC, ALPHABETIC-LOWER, 
                     ALPHABETIC-UPPER, CLASS class-name
    
    Examples:
    - EVALUATE TRUE
    - EVALUATE X
    - EVALUATE WRK-XN-00001-1 NOT NUMERIC
    - EVALUATE STATUS-CODE ALSO RECORD-TYPE
    """
    self.consume(COBOLTokenType.EVALUATE)
    
    # Emergency exit for program boundaries
    if self.match(COBOLTokenType.IDENTIFICATION, COBOLTokenType.ENVIRONMENT,
                  COBOLTokenType.DATA, COBOLTokenType.PROCEDURE,
                  COBOLTokenType.DIVISION,
                  COBOLTokenType.NIST_HEADER, COBOLTokenType.NIST_END_OF):
        print(f"[BOUNDARY] Hit boundary in EVALUATE at line {self.current_token().line}")
        return COBOLEvaluate(
            subject=None,
            when_clauses=[],
            line=token.line,
            column=token.column
        )
    
    # Parse the EVALUATE subject with class condition support
    subject = self._parse_evaluate_subject()
    
    # Parse WHEN clauses
    when_clauses = []
    self.push_context(ParseContext.INSIDE_EVALUATE)
    
    max_when_clauses = 1000
    clause_count = 0
    
    while self.match(COBOLTokenType.WHEN) and clause_count < max_when_clauses:
        clause_count += 1
        
        # Emergency exit inside loop
        if self.match(COBOLTokenType.IDENTIFICATION, COBOLTokenType.ENVIRONMENT,
                      COBOLTokenType.DATA, COBOLTokenType.PROCEDURE,
                      COBOLTokenType.DIVISION,
                      COBOLTokenType.NIST_HEADER, COBOLTokenType.NIST_END_OF):
            print(f"[BOUNDARY] Hit boundary in EVALUATE WHEN loop at line {self.current_token().line}")
            break
        
        clause = self.parse_when_clause()
        if clause:
            when_clauses.append(clause)
        else:
            break
        
        if self.match(COBOLTokenType.END_EVALUATE, COBOLTokenType.EOF):
            break
    
    if clause_count >= max_when_clauses:
        print(f"[WARNING] EVALUATE loop limit reached at line {token.line}")
    
    self.pop_context()
    
    if self.match(COBOLTokenType.END_EVALUATE):
        self.consume(COBOLTokenType.END_EVALUATE)
    
    if self.requires_period() and self.match(COBOLTokenType.PERIOD):
        self.consume(COBOLTokenType.PERIOD)
    
    return COBOLEvaluate(
        subject=subject,
        when_clauses=when_clauses,
        line=token.line,
        column=token.column
    )


def _parse_evaluate_subject(self):
    """Parse EVALUATE subject with class condition support
    
    TRUTH TABLE: Subject Pattern Detection
    ┌──────────────────────┬────────────────────────────────────┐
    │ Current Token        │ Action                             │
    ├──────────────────────┼────────────────────────────────────┤
    │ TRUE                 │ Return boolean literal             │
    │ FALSE                │ Return boolean literal             │
    │ identifier + NOT     │ Parse class condition              │
    │ identifier + class   │ Parse class condition              │
    │ Other                │ Parse as expression                │
    └──────────────────────┴────────────────────────────────────┘
    
    Class conditions after identifier:
    - NUMERIC, ALPHABETIC, ALPHABETIC-LOWER, ALPHABETIC-UPPER
    - Can be preceded by NOT
    """
    
    print(f"[DEBUG EVAL SUBJECT] Starting, current token: {self.current_token().type.name} = {self.current_token().value}")
    # Check for TRUE/FALSE literals
    if self.match(COBOLTokenType.IDENTIFIER):
        id_val = self.current_token().value.upper()
        
        if id_val == 'TRUE':
            token = self.current_token()
            self.advance()
            return COBOLIdentifier(name='TRUE', line=token.line, column=token.column)
        
        elif id_val == 'FALSE':
            token = self.current_token()
            self.advance()
            return COBOLIdentifier(name='FALSE', line=token.line, column=token.column)
    
    # Save position to check for class condition pattern
    start_token = self.current_token()
    
    # Parse the base expression (identifier or complex expression)
    expr = self.parse_primary()  # Use parse_primary instead of parse_expression
    print(f"[DEBUG EVAL SUBJECT] After parse_primary, current token: {self.current_token().type.name} = {self.current_token().value}")
    # Now check if followed by NOT or class condition keyword
    has_not = False
    if self.match(COBOLTokenType.NOT):
        has_not = True
        self.advance()
    
    # Check for class condition keywords
    class_conditions = ['NUMERIC', 'ALPHABETIC', 'ALPHABETIC-LOWER', 
                       'ALPHABETIC-UPPER', 'CLASS']
    
    if self.match(COBOLTokenType.IDENTIFIER):
        class_name = self.current_token().value.upper()
        
        if class_name in class_conditions:
            self.advance()
            
            # If CLASS keyword, expect another identifier for the class name
            if class_name == 'CLASS':
                if not self.match(COBOLTokenType.IDENTIFIER):
                    self.error("Expected class name after CLASS keyword")
                actual_class = self.current_token().value
                self.advance()
                class_name = f"CLASS {actual_class}"
            
            # Build class condition string
            # Extract identifier name from expr
            if hasattr(expr, 'name'):
                expr_name = expr.name
            elif hasattr(expr, 'identifier'):
                expr_name = expr.identifier
            else:
                expr_name = str(expr)
            
            if has_not:
                condition_str = f"{expr_name} NOT {class_name}"
            else:
                condition_str = f"{expr_name} {class_name}"
            
            # Return as identifier with the condition string
            return COBOLIdentifier(
                name=condition_str,
                line=start_token.line,
                column=start_token.column
            )
    
    # If we consumed NOT but no class condition followed, that's an error
    if has_not:
        self.error(f"Expected class condition (NUMERIC, ALPHABETIC, etc.) after NOT in EVALUATE subject")
    
    # Check if followed by comparison operator or other expression continuation
    # If so, parse as full condition
    if self.match(COBOLTokenType.EQUALS, COBOLTokenType.EQUALS_SIGN,
                  COBOLTokenType.GT_SIGN, COBOLTokenType.LT_SIGN,
                  COBOLTokenType.GREATER, COBOLTokenType.LESS):
        # This is a comparison - need to parse the full condition
        # Back up and parse as full expression from start
        # Actually, we can't back up easily, so just continue with comparison
        self.error("Comparison in EVALUATE subject not yet supported - use WHEN clause instead")
    
    # Return the simple expression
    return expr



def parse_when_clause(self) -> Optional[COBOLWhenClause]:
    """Parse a WHEN clause in EVALUATE statement with truth table approach
    
    TRUTH TABLE: WHEN Clause Patterns
    ┌──────────────────────────────────┬────────────────────────────┐
    │ Pattern                          │ Action                     │
    ├──────────────────────────────────┼────────────────────────────┤
    │ WHEN OTHER                       │ Match-all case             │
    │ WHEN value                       │ Simple value match         │
    │ WHEN [NOT] class-condition       │ Class test match           │
    │ WHEN value1 THRU value2          │ Range match                │
    │ WHEN value1 ALSO value2          │ Multiple values            │
    │ Not WHEN                         │ Return None (not a clause) │
    └──────────────────────────────────┴────────────────────────────┘
    
    Class conditions: NUMERIC, ALPHABETIC, ALPHABETIC-LOWER,
                     ALPHABETIC-UPPER, CLASS class-name
    
    TRUTH TABLE: When to exit statement parsing
    ┌──────────────────────────────────┬────────────────────────────┐
    │ Token Type                       │ Action                     │
    ├──────────────────────────────────┼────────────────────────────┤
    │ WHEN                             │ Break - next clause        │
    │ END-EVALUATE                     │ Break - end of evaluate    │
    │ EOF                              │ Break - end of input       │
    │ Division/Program boundary        │ Break - emergency exit     │
    │ Other                            │ Parse as statement         │
    └──────────────────────────────────┴────────────────────────────┘
    
    Examples:
    - WHEN OTHER
    - WHEN 5
    - WHEN NOT NUMERIC
    - WHEN ALPHABETIC
    - WHEN 1 THRU 10
    - WHEN "A" ALSO "B"
    """
    token = self.current_token()
    
    # Check if this is actually a WHEN clause
    if not self.match(COBOLTokenType.WHEN):
        return None
    
    # Emergency boundary check before consuming WHEN
    if self._is_division_boundary() or self._is_program_boundary():
        print(f"[BOUNDARY] Hit boundary before WHEN clause at line {token.line}")
        return None
    
    self.consume(COBOLTokenType.WHEN)
    
    # Parse the WHEN value/condition
    value = None
    thru_value = None
    
    # CASE 1: WHEN OTHER
    if self.match(COBOLTokenType.OTHER):
        self.advance()
        value = None
    
    # CASE 2: WHEN NOT - could be class condition OR negated value
    elif self.match(COBOLTokenType.NOT):
        has_not = True
        self.advance()
        
        # Look ahead to determine: class condition vs negated value
        class_conditions = ['NUMERIC', 'ALPHABETIC', 'ALPHABETIC-LOWER', 
                           'ALPHABETIC-UPPER', 'CLASS']
        
        if self.match(COBOLTokenType.IDENTIFIER):
            cond_name = self.current_token().value.upper()
            
            if cond_name in class_conditions:
                # WHEN NOT NUMERIC - class condition
                self.advance()
                
                # If CLASS keyword, expect class name
                if cond_name == 'CLASS':
                    if not self.match(COBOLTokenType.IDENTIFIER):
                        self.error("Expected class name after CLASS keyword")
                    actual_class = self.current_token().value
                    self.advance()
                    cond_name = f"CLASS {actual_class}"
                
                # Build class condition value
                value = COBOLIdentifier(name=f"NOT {cond_name}", 
                                       line=token.line, column=token.column)
            else:
                # WHEN NOT identifier - negated value match
                # Parse the identifier/expression as the value
                neg_value = self.parse_expression()
                
                # Wrap in NOT operator
                value = COBOLUnaryOp(operator='NOT', operand=neg_value,
                                    line=token.line, column=token.column)
        else:
            # NOT followed by non-identifier (number, string, etc)
            neg_value = self.parse_expression()
            value = COBOLUnaryOp(operator='NOT', operand=neg_value,
                                line=token.line, column=token.column)
    
    # CASE 3: WHEN class-condition (without NOT)
    elif self.match(COBOLTokenType.IDENTIFIER):
        cond_name = self.current_token().value.upper()
        class_conditions = ['NUMERIC', 'ALPHABETIC', 'ALPHABETIC-LOWER', 
                           'ALPHABETIC-UPPER', 'CLASS']
        
        if cond_name in class_conditions:
            # Direct class condition: WHEN NUMERIC
            self.advance()
            
            if cond_name == 'CLASS':
                if not self.match(COBOLTokenType.IDENTIFIER):
                    self.error("Expected class name after CLASS keyword")
                actual_class = self.current_token().value
                self.advance()
                cond_name = f"CLASS {actual_class}"
            
            value = COBOLIdentifier(name=cond_name,
                                   line=token.line, column=token.column)
        else:
            # Regular expression
            value = self.parse_expression()
            
            # Check for THRU (range)
            if self.match(COBOLTokenType.THRU, COBOLTokenType.THROUGH):
                self.advance()
                thru_value = self.parse_expression()
            
            # Handle ALSO (multiple values)
            while self.match(COBOLTokenType.IDENTIFIER):
                peek_val = self.current_token().value.upper()
                if peek_val in ['ALSO', 'THROUGH']:
                    self.advance()
                    self.parse_expression()
                else:
                    break
    
    # CASE 4: WHEN expression (number, string, etc)
    else:
        # Parse first value as expression
        value = self.parse_expression()
        
        # Check for THRU (range)
        if self.match(COBOLTokenType.THRU, COBOLTokenType.THROUGH):
            self.advance()
            thru_value = self.parse_expression()
        
        # Handle ALSO (multiple values)
        while self.match(COBOLTokenType.IDENTIFIER):
            peek_val = self.current_token().value.upper()
            if peek_val in ['ALSO', 'THROUGH']:
                self.advance()
                self.parse_expression()
            else:
                break
    
    # Parse statements in this WHEN clause
    statements = []
    max_iterations = 10000
    iteration = 0
    
    while iteration < max_iterations:
        iteration += 1
        
        # TRUTH TABLE: When to stop parsing statements
        current = self.current_token()
        
        # Rule 1: End of EVALUATE or next WHEN
        if self.match(COBOLTokenType.WHEN, COBOLTokenType.END_EVALUATE, COBOLTokenType.EOF):
            break
        
        # Rule 2: Emergency boundaries
        if self._is_division_boundary() or self._is_program_boundary():
            print(f"[BOUNDARY] Hit boundary in WHEN statements at line {current.line}")
            break
        
        # Rule 3: Parse statement
        stmt = self.parse_statement()
        if stmt:
            statements.append(stmt)
        else:
            # parse_statement returned None - check if we hit a boundary
            if self.match(COBOLTokenType.WHEN, COBOLTokenType.END_EVALUATE, COBOLTokenType.EOF):
                break
    
    if iteration >= max_iterations:
        print(f"[WARNING] WHEN clause iteration limit at line {token.line}")
    
    return COBOLWhenClause(
        value=value,
        statements=statements,
        line=token.line,
        column=token.column
    )


def parse_stop_run(self, token: Token) -> COBOLStopRun:
    """Parse STOP statement with truth table for variants
    
    TRUTH TABLE: STOP Statement Patterns
    ┌──────────────────────┬─────────────────────────────┐
    │ Pattern              │ Action                      │
    ├──────────────────────┼─────────────────────────────┤
    │ STOP RUN             │ Parse as COBOLStopRun       │
    │ STOP RUN.            │ Parse as COBOLStopRun       │
    │ STOP 'literal'       │ Parse as COBOLStopRun       │
    │ STOP identifier      │ Parse as COBOLStopRun       │
    │ STOP.                │ Parse as COBOLStopRun       │
    └──────────────────────┴─────────────────────────────┘
    
    Note: All variants map to COBOLStopRun for simplicity
    The message/type distinction is for documentation only
    """
    self.consume(COBOLTokenType.STOP)
    
    next_token = self.current_token()
    
    # ROW 1-2: STOP RUN [.]
    if next_token.type == COBOLTokenType.RUN:
        self.consume(COBOLTokenType.RUN)
        
        # Optional: WITH ERROR / WITH NORMAL (consume and ignore)
        if self.match(COBOLTokenType.WITH):
            self.advance()
            if self.match(COBOLTokenType.IDENTIFIER):
                self.advance()
        
        if self.requires_period():
            self.consume(COBOLTokenType.PERIOD)
        
        return COBOLStopRun(line=token.line, column=token.column)
    
    # ROW 3: STOP 'literal'
    elif next_token.type == COBOLTokenType.STRING_LITERAL:
        # Consume the literal (we ignore it, just terminate)
        self.advance()
        
        if self.requires_period():
            self.consume(COBOLTokenType.PERIOD)
        
        return COBOLStopRun(line=token.line, column=token.column)
    
    # ROW 4: STOP identifier
    elif next_token.type == COBOLTokenType.IDENTIFIER:
        # Consume the identifier (we ignore it, just terminate)
        self.advance()
        
        if self.requires_period():
            self.consume(COBOLTokenType.PERIOD)
        
        return COBOLStopRun(line=token.line, column=token.column)
    
    # ROW 5: STOP.
    elif next_token.type == COBOLTokenType.PERIOD:
        self.consume(COBOLTokenType.PERIOD)
        return COBOLStopRun(line=token.line, column=token.column)
    
    # Error case: unexpected token
    else:
        self.error(
            f"Expected RUN, literal, identifier, or period after STOP. "
            f"Got {next_token.type.name} = '{next_token.value}'"
        )


def parse_string_statement(self, token: Token) -> COBOLStringConcat:
    """Parse STRING statement"""
    self.consume(COBOLTokenType.STRING)
    
    source_fields = []
    delimiters = []
    
    while not self.match(COBOLTokenType.INTO):
        if self.match(COBOLTokenType.STRING_LITERAL, COBOLTokenType.IDENTIFIER):
            source_fields.append(self.parse_expression())
        else:
            self.error("Expected source field in STRING statement")
        
        if self.match(COBOLTokenType.DELIMITED):
            self.consume(COBOLTokenType.DELIMITED)
            self.consume(COBOLTokenType.BY)
            
            if self.match(COBOLTokenType.SIZE):
                self.consume(COBOLTokenType.SIZE)
                delimiters.append(None)
            else:
                delimiters.append(self.parse_expression())
        else:
            delimiters.append(None)
    
    self.consume(COBOLTokenType.INTO)
    if not self.match(COBOLTokenType.IDENTIFIER):
        self.error("Expected target identifier after INTO")
    target = self.consume(COBOLTokenType.IDENTIFIER).value
    
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


def parse_unstring_statement(self, token: Token) -> COBOLUnstring:
    """Parse UNSTRING statement with multiple delimiters"""
    self.consume(COBOLTokenType.UNSTRING)
    
    if not self.match(COBOLTokenType.IDENTIFIER, COBOLTokenType.STRING_LITERAL):
        self.error("Expected source field in UNSTRING statement")
    source = self.parse_expression()
    
    delimiters = []
    delimiter_all_flags = []
    
    if self.match(COBOLTokenType.DELIMITED):
        self.consume(COBOLTokenType.DELIMITED)
        self.consume(COBOLTokenType.BY)
        
        has_all = False
        if self.match(COBOLTokenType.ALL):
            self.advance()
            has_all = True
        
        delimiter = self.parse_expression()
        delimiters.append(delimiter)
        delimiter_all_flags.append(has_all)
        
        while self.match(COBOLTokenType.OR):
            self.advance()
            
            has_all = False
            if self.match(COBOLTokenType.ALL):
                self.advance()
                has_all = True
            
            delimiter = self.parse_expression()
            delimiters.append(delimiter)
            delimiter_all_flags.append(has_all)
    
    while not self.match(COBOLTokenType.INTO, COBOLTokenType.EOF, COBOLTokenType.PERIOD):
        self.advance()

    self.consume(COBOLTokenType.INTO)
    targets = []
    while self.match(COBOLTokenType.IDENTIFIER):
        targets.append(self.consume(COBOLTokenType.IDENTIFIER).value)
        if self.match(COBOLTokenType.TALLYING, COBOLTokenType.PERIOD):
            break
    
    if not targets:
        self.error("Expected at least one target field in UNSTRING INTO clause")
    
    counter = None
    if self.match(COBOLTokenType.TALLYING):
        self.consume(COBOLTokenType.TALLYING)
        self.consume(COBOLTokenType.IN)
        if self.match(COBOLTokenType.IDENTIFIER):
            counter = self.consume(COBOLTokenType.IDENTIFIER).value
    
    self.consume_optional_period()
    
    return COBOLUnstring(
        source=source,
        delimiters=delimiters,
        delimiter_all_flags=delimiter_all_flags,
        targets=targets,
        count=counter,
        line=token.line,
        column=token.column
    )


def parse_inspect(self, token: Token) -> COBOLInspect:
    """Parse INSPECT statement with truth table approach
    
    TRUTH TABLE: INSPECT Statement Patterns
    ┌──────────────────────────────────────┬─────────────────────────────┐
    │ Pattern                              │ Action                      │
    ├──────────────────────────────────────┼─────────────────────────────┤
    │ INSPECT identifier REPLACING...      │ Simple target               │
    │ INSPECT identifier(subscript) REPL...│ Subscripted target          │
    │ INSPECT identifier TALLYING...       │ Simple target, count mode   │
    │ INSPECT identifier(subscript) TALL...│ Subscripted target, count   │
    │ INSPECT identifier CONVERTING...     │ Character conversion        │
    └──────────────────────────────────────┴─────────────────────────────┘
    
    INSPECT Subclauses:
    - REPLACING: Replace pattern with replacement
      - ALL pattern BY replacement
      - FIRST pattern BY replacement  
      - LEADING pattern BY replacement
      - CHARACTERS BY replacement [AFTER/BEFORE INITIAL string]
    - TALLYING: Count occurrences
      - counter FOR ALL pattern
    - CONVERTING: Convert characters
      - from-string TO to-string
    
    Examples:
    1. INSPECT field REPLACING ALL "A" BY "B".
    2. INSPECT TABLE-ITEM(INDEX) REPLACING ALL "X" BY "Y".
    3. INSPECT PROC-NAME(STACK-INDEX) REPLACING CHARACTERS BY " ".
    """
    # ═══════════════════════════════════════════════════════════════
    # STEP 1: Parse target identifier (with optional subscript)
    # ═══════════════════════════════════════════════════════════════
    target = self.consume(COBOLTokenType.IDENTIFIER).value
    
    # ✅ CRITICAL FIX: Handle optional subscript
    # Pattern: PROC-NAME (STACK-INDEX)
    target_subscript = None
    if self.match(COBOLTokenType.LPAREN):
        self.advance()  # Consume LPAREN
        target_subscript = self.parse_expression()
        if not self.match(COBOLTokenType.RPAREN):
            self.error(f"Expected RPAREN after subscript in INSPECT target '{target}'")
        self.advance()  # Consume RPAREN
    
    # ═══════════════════════════════════════════════════════════════
    # STEP 2: Determine operation type
    # ═══════════════════════════════════════════════════════════════
    operation_token = self.current_token()
    
    # TRUTH TABLE ROW 1-2: REPLACING operation
    if operation_token.type == COBOLTokenType.REPLACING:
        self.consume(COBOLTokenType.REPLACING)
        
        # Check for CHARACTERS keyword (special case)
        # Pattern: REPLACING CHARACTERS BY " " [AFTER/BEFORE INITIAL " "]
        if self.match(COBOLTokenType.IDENTIFIER):
            if self.current_token().value.upper() == 'CHARACTERS':
                self.advance()  # Consume CHARACTERS
                
                self.consume(COBOLTokenType.BY)
                replacement = self.parse_expression()
                
                # Optional AFTER/BEFORE INITIAL clause
                after_before = None
                initial_string = None
                if self.match(COBOLTokenType.IDENTIFIER):
                    keyword = self.current_token().value.upper()
                    if keyword in ['AFTER', 'BEFORE']:
                        after_before = keyword
                        self.advance()
                        
                        # Expect INITIAL keyword
                        if self.match(COBOLTokenType.IDENTIFIER):
                            if self.current_token().value.upper() == 'INITIAL':
                                self.advance()
                                initial_string = self.parse_expression()
                
                return COBOLInspect(
                    token.line, token.column,
                    target=target,
                    operation='REPLACING',
                    pattern='CHARACTERS',  # Special marker
                    replacement=replacement,
                    scope='CHARACTERS',
                    target_subscript=target_subscript,
                    after_before=after_before,
                    initial_string=initial_string
                )
        
        # Normal REPLACING with scope (ALL/FIRST/LEADING)
        scope = 'ALL'  # Default
        if self.match(COBOLTokenType.IDENTIFIER):
            scope_value = self.current_token().value.upper()
            if scope_value in ['ALL', 'FIRST', 'LEADING']:
                self.advance()
                scope = scope_value
        elif self.match(COBOLTokenType.ALL):
            self.consume(COBOLTokenType.ALL)
            scope = 'ALL'
        
        pattern = self.parse_expression()
        
        self.consume(COBOLTokenType.BY)
        
        replacement = self.parse_expression()
        
        return COBOLInspect(
            token.line, token.column,
            target=target,
            operation='REPLACING',
            pattern=pattern,
            replacement=replacement,
            scope=scope,
            target_subscript=target_subscript
        )
    
    # TRUTH TABLE ROW 3-4: TALLYING operation
    elif operation_token.type == COBOLTokenType.TALLYING:
        self.consume(COBOLTokenType.TALLYING)
        counter = self.parse_expression()  # ✅ FIX: Handle WRK-DU-999(INDEX2)
        
        self.consume(COBOLTokenType.FOR)
        
        scope = 'ALL'
        if self.match(COBOLTokenType.IDENTIFIER):
            scope_value = self.current_token().value.upper()
            if scope_value == 'ALL':
                self.advance()
        elif self.match(COBOLTokenType.ALL):
            self.consume(COBOLTokenType.ALL)
        
        pattern = self.parse_expression()
        
        return COBOLInspect(
            token.line, token.column,
            target=target,
            operation='TALLYING',
            pattern=pattern,
            counter=counter,
            scope=scope,
            target_subscript=target_subscript
        )
    
    # TRUTH TABLE ROW 5: CONVERTING operation (future enhancement)
    elif self.match(COBOLTokenType.IDENTIFIER) and \
         self.current_token().value.upper() == 'CONVERTING':
        self.advance()  # Consume CONVERTING
        
        from_string = self.parse_expression()
        
        # Expect TO keyword
        if not self.match(COBOLTokenType.TO):
            self.error("Expected TO in INSPECT CONVERTING")
        self.advance()
        
        to_string = self.parse_expression()
        
        return COBOLInspect(
            token.line, token.column,
            target=target,
            operation='CONVERTING',
            pattern=from_string,
            replacement=to_string,
            scope='CONVERTING',
            target_subscript=target_subscript
        )
    
    # ERROR: Unknown operation
    else:
        self.error(f"Expected REPLACING, TALLYING, or CONVERTING in INSPECT, got {operation_token.type.name}")


def parse_inspect_statement(self, token: Token) -> COBOLInspect:
    """Wrapper to keep existing call structure"""
    self.consume(COBOLTokenType.INSPECT)
    result = self.parse_inspect(token)
    self.consume_optional_period()
    return result


def _is_statement_boundary(self) -> bool:
    """Truth table for statement boundary detection
    
    Returns True if the current token marks the end of a statement.
    This prevents statements from consuming tokens that belong to the next statement.
    
    TRUTH TABLE:
    | Token Type        | Is Boundary? | Reason |
    |-------------------|--------------|--------|
    | PERIOD            | YES          | Statement terminator |
    | EOF               | YES          | End of input |
    | Statement keyword | YES          | New statement starts |
    | NEWLINE           | NO           | Just whitespace |
    | IDENTIFIER        | MAYBE        | Could be statement or operand |
    
    For IDENTIFIER: Check if it's a known statement keyword.
    """
    token = self.current_token()
    token_type = token.type
    
    # Rule 1: Explicit terminators
    if token_type in (COBOLTokenType.PERIOD, COBOLTokenType.EOF):
        return True
    
    # Rule 2: Statement keywords that start new statements
    STATEMENT_KEYWORDS_TOKENS = {
        COBOLTokenType.MOVE, COBOLTokenType.ADD, COBOLTokenType.SUBTRACT,
        COBOLTokenType.MULTIPLY, COBOLTokenType.DIVIDE, COBOLTokenType.COMPUTE,
        COBOLTokenType.IF, COBOLTokenType.ELSE, COBOLTokenType.END_IF,
        COBOLTokenType.PERFORM, COBOLTokenType.END_PERFORM,
        COBOLTokenType.DISPLAY, COBOLTokenType.ACCEPT,
        COBOLTokenType.CALL, COBOLTokenType.EVALUATE, COBOLTokenType.END_EVALUATE,
        COBOLTokenType.STOP, COBOLTokenType.GOBACK, COBOLTokenType.EXIT,
        COBOLTokenType.GO, COBOLTokenType.STRING, COBOLTokenType.UNSTRING,
        COBOLTokenType.INSPECT, COBOLTokenType.READ, COBOLTokenType.WRITE,
        COBOLTokenType.OPEN, COBOLTokenType.CLOSE, COBOLTokenType.SET
        # Note: CONTINUE and SEARCH are handled as IDENTIFIER keywords below
    }
    
    if token_type in STATEMENT_KEYWORDS_TOKENS:
        return True
    
    # Rule 3: IDENTIFIER that's a statement keyword (for keywords lexed as IDENTIFIER)
    if token_type == COBOLTokenType.IDENTIFIER:
        keyword_value = token.value.upper()
        STATEMENT_KEYWORDS_TEXT = {
            'MOVE', 'ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE', 'COMPUTE',
            'DISPLAY', 'ACCEPT', 'PERFORM', 'IF', 'ELSE', 'STOP',
            'CALL', 'EVALUATE', 'GOBACK', 'EXIT', 'GO', 'GOTO',
            'STRING', 'UNSTRING', 'INSPECT', 'READ', 'WRITE',
            'OPEN', 'CLOSE', 'SET', 'CONTINUE', 'SEARCH'
        }
        if keyword_value in STATEMENT_KEYWORDS_TEXT:
            return True
    
    # Rule 4: Division/Section boundaries (should never appear in statements)
    if token_type in (COBOLTokenType.IDENTIFICATION, COBOLTokenType.ENVIRONMENT,
                     COBOLTokenType.DATA, COBOLTokenType.PROCEDURE,
                     COBOLTokenType.DIVISION, COBOLTokenType.SECTION):
        return True
    
    # Rule 5: NIST markers
    if token_type in (COBOLTokenType.NIST_HEADER, COBOLTokenType.NIST_END_OF):
        return True
    
    return False




def parse_move(self, token: Token) -> COBOLMove:
    """Parse MOVE statement with truth table approach
    
    TRUTH TABLE: MOVE Statement Patterns
    ┌────────────────────────────────────┬──────────────────────────┐
    │ Pattern                            │ Action                   │
    ├────────────────────────────────────┼──────────────────────────┤
    │ MOVE source TO target              │ Single target            │
    │ MOVE source TO target, target, ... │ Multiple targets         │
    │ MOVE CORRESPONDING source TO target│ Group move               │
    │ MOVE CORR source, target           │ TO optional (legacy)     │
    │ MOVE source TO target MOVE ...     │ Multiple statements/line │
    └────────────────────────────────────┴──────────────────────────┘
    
    TRUTH TABLE: Statement Termination
    ┌──────────────────────┬──────────────────────────────┐
    │ Next Token           │ Action                       │
    ├──────────────────────┼──────────────────────────────┤
    │ PERIOD               │ Consume, end statement       │
    │ Statement keyword    │ Don't consume, end statement │
    │ COMMA                │ Continue parsing targets     │
    │ EOF/boundary         │ End statement                │
    └──────────────────────┴──────────────────────────────┘
    """
    self.consume(COBOLTokenType.MOVE)
    
    # Check for CORRESPONDING/CORR keyword
    is_corresponding = False
    if self.match(COBOLTokenType.IDENTIFIER):
        peek = self.current_token().value.upper()
        if peek in ['CORRESPONDING', 'CORR']:
            is_corresponding = True
            self.advance()
    
    # Parse source expression
    source = self.parse_expression()
    
    # 🔥 FIX: TO keyword is optional in some COBOL dialects for CORRESPONDING
    # Pattern 1: MOVE CORRESPONDING source TO target (standard)
    # Pattern 2: MOVE CORRESPONDING source, target (legacy, TO implied)
    if self.match(COBOLTokenType.TO):
        self.consume(COBOLTokenType.TO)
    elif is_corresponding and self.match(COBOLTokenType.COMMA):
        # Legacy CORRESPONDING without TO: MOVE CORR source, target
        # The comma separates source from target
        if self.debug:
            print(f"[PARSE_MOVE] CORRESPONDING without TO keyword (legacy syntax)")
        self.advance()  # consume comma
    elif not self.match(COBOLTokenType.COMMA):
        # Not CORRESPONDING or no comma - TO is required
        self.error(f"Expected TO after MOVE source at line {token.line}")
    
    # Parse target list (comma-separated)
    targets = []
    max_targets = 100
    iteration = 0
    
    while iteration < max_targets:
        iteration += 1
        
        # ✅ CRITICAL FIX: Check boundary BEFORE trying to parse target
        # This prevents parse_expression() from seeing PERFORM keyword
        # TRUTH TABLE: When to stop parsing targets
        # Row 1: Hit statement boundary (includes PERIOD, EOF, statement keywords)
        if self._is_statement_boundary():
            break
        
        # Row 2: Parse next target (only if we haven't hit a boundary)
        target = self.parse_expression()
        if target:
            targets.append(target)
        else:
            break
        
        # Row 3: Check for comma (more targets follow)
        if self.match(COBOLTokenType.COMMA):
            self.advance()
            continue  # Parse next target
        
        # Row 4: Check again for statement boundary after parsing target
        # This catches: MOVE X TO Y PERFORM Z (where PERFORM comes after target)
        if self._is_statement_boundary():
            break
        
        # Row 5: No comma and no boundary, stop parsing targets
        break
    
    if iteration >= max_targets:
        print(f"[WARNING] MOVE target parsing hit iteration limit at line {token.line}")
    
    if not targets:
        self.error(f"MOVE statement has no targets at line {token.line}")
    
    # TRUTH TABLE: Period consumption
    # Row 1: Next token is PERIOD → consume it
    # Row 2: Next token is statement keyword → don't consume (multi-stmt line)
    # Row 3: Other → don't consume
    if self.match(COBOLTokenType.PERIOD):
        self.advance()
    
    return COBOLMove(
        source=source,
        targets=targets,
        is_corresponding=is_corresponding,
        line=token.line,
        column=token.column
    )
    
    
def _is_statement_start(self):
    """Check if current token starts a new statement
    
    TRUTH TABLE: Statement Start Detection
    ┌────────────────────┬────────────────────┐
    │ Token Type         │ Is Statement Start │
    ├────────────────────┼────────────────────┤
    │ MOVE               │ YES                │
    │ ADD/SUBTRACT/etc   │ YES                │
    │ IF/PERFORM/etc     │ YES                │
    │ Other              │ NO                 │
    └────────────────────┴────────────────────┘
    """
    return self.match(
        COBOLTokenType.MOVE, COBOLTokenType.ADD, COBOLTokenType.SUBTRACT,
        COBOLTokenType.MULTIPLY, COBOLTokenType.DIVIDE, COBOLTokenType.COMPUTE,
        COBOLTokenType.IF, COBOLTokenType.PERFORM, COBOLTokenType.DISPLAY,
        COBOLTokenType.ACCEPT, COBOLTokenType.CALL, COBOLTokenType.GO,
        COBOLTokenType.STOP, COBOLTokenType.EXIT, COBOLTokenType.CONTINUE
    )
    
    

# ============================================================================
# UPDATE parse_add() similarly
# ============================================================================

def parse_add(self, token: Token) -> COBOLArithmetic:
    self.consume(COBOLTokenType.ADD)
    
    operands = []
    max_operands = 100
    operand_count = 0
    
    # Parse operands until we hit TO or GIVING
    while operand_count < max_operands:
        if self.match(COBOLTokenType.TO, COBOLTokenType.GIVING):
            break
        if self._is_statement_boundary():
            break
        
        operands.append(self.parse_expression())
        operand_count += 1
    
    if not operands:
        self.error(f"ADD statement has no operands at line {token.line}")
    
    if self.match(COBOLTokenType.GIVING):
        self.advance()
        giving = self.parse_expression()
        target = None
    else:
        self.consume(COBOLTokenType.TO)
        to_value = self.parse_expression()
        
        giving = None
        target = None
        if self.match(COBOLTokenType.GIVING):
            self.advance()
            giving = self.parse_expression()
            operands.append(to_value)
        else:
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


COBOLMultiProgramParser.parse_exit = parse_exit
COBOLMultiProgramParser.parse_goback = parse_goback
COBOLMultiProgramParser.parse_statement = parse_statement
COBOLMultiProgramParser.parse_display = parse_display
COBOLMultiProgramParser.parse_accept = parse_accept
COBOLMultiProgramParser.parse_move = parse_move
COBOLMultiProgramParser._is_statement_start = _is_statement_start
COBOLMultiProgramParser.parse_compute = parse_compute
COBOLMultiProgramParser.parse_add = parse_add
COBOLMultiProgramParser.parse_subtract = parse_subtract
COBOLMultiProgramParser.parse_multiply = parse_multiply
COBOLMultiProgramParser.parse_divide = parse_divide
COBOLMultiProgramParser.parse_read_statement = parse_read_statement
COBOLMultiProgramParser.parse_go_to = parse_go_to
COBOLMultiProgramParser.parse_call = parse_call
COBOLMultiProgramParser.parse_if = parse_if
COBOLMultiProgramParser.parse_perform = parse_perform
COBOLMultiProgramParser.parse_perform_until = parse_perform_until
COBOLMultiProgramParser.parse_perform_varying = parse_perform_varying
COBOLMultiProgramParser.parse_evaluate = parse_evaluate
COBOLMultiProgramParser.parse_when_clause = parse_when_clause
COBOLMultiProgramParser.parse_stop_run = parse_stop_run
COBOLMultiProgramParser.parse_string_statement = parse_string_statement
COBOLMultiProgramParser.parse_unstring_statement = parse_unstring_statement
COBOLMultiProgramParser.parse_inspect = parse_inspect
COBOLMultiProgramParser.parse_inspect_statement = parse_inspect_statement
COBOLMultiProgramParser.parse_perform_times_inline = parse_perform_times_inline
COBOLMultiProgramParser._is_statement_boundary = _is_statement_boundary
COBOLMultiProgramParser._parse_if_statements_block = _parse_if_statements_block
COBOLMultiProgramParser.parse_merge = parse_merge
COBOLMultiProgramParser._parse_evaluate_subject = _parse_evaluate_subject