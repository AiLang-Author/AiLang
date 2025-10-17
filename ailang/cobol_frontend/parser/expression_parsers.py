#!/usr/bin/env python3

# Copyright (c) 2025 Sean Collins, 2 Paws Machine and Engineering. All rights reserved.
#
# Licensed under the Sean Collins Software License (SCSL). See the LICENSE file in the root directory of this project
# for the full terms and conditions, including restrictions on forking, corporate use, and permissions for private/teaching purposes.

"""
COBOL Expression Parsers - Handles all expression types and conditions
"""

import re
from typing import Optional
from cobol_frontend.cobol_lexer import Token, COBOLTokenType  # ✅ FIXED
from .ast_nodes import *
from .parser_core import COBOLMultiProgramParser



def parse_variable_decl(self) -> COBOLVariableDecl:
    """Parse COBOL variable declaration - Main orchestrator with truth table
    
    TRUTH TABLE: Variable Declaration by Level Number
    ┌────────────┬─────────────────────────────────┬──────────────────────────┐
    │ Level      │ Type                            │ Handler Function         │
    ├────────────┼─────────────────────────────────┼──────────────────────────┤
    │ 66         │ RENAMES clause                  │ _parse_renames_clause    │
    │ 88         │ Condition name (VALUE clause)   │ _parse_condition_name    │
    │ 01-49, 77  │ USAGE IS INDEX                  │ _create_index_variable   │
    │ 01-49, 77  │ COPY statement present          │ Handle as GROUP item     │
    │ 01-49, 77  │ No PIC, has PERIOD/VALUE        │ _parse_group_item        │
    │ 01-49, 77  │ Has PIC clause                  │ _parse_elementary_item   │
    └────────────┴─────────────────────────────────┴──────────────────────────┘
    
    TRUTH TABLE: Item Type Detection (UPDATED with USAGE support)
    ┌──────────┬──────────────┬────────────┬──────────┬─────────────────────┐
    │ Has PIC? │ Has PERIOD   │ Has VALUE? │ Has      │ Item Type           │
    │          │ with children│            │ USAGE?   │                     │
    ├──────────┼──────────────┼────────────┼──────────┼─────────────────────┤
    │ YES      │ *            │ *          │ *        │ Elementary Item     │
    │ NO       │ *            │ *          │ YES      │ Elementary Item     │
    │ NO       │ YES          │ *          │ NO       │ Group Item          │
    │ NO       │ NO           │ YES        │ NO       │ Group Item w/VALUE  │
    │ NO       │ NO           │ NO         │ NO       │ Check EXTERNAL/SIGN │
    └──────────┴──────────────┴────────────┴──────────┴─────────────────────┘
    """
    token = self.current_token()
    
    if self.debug:
        print(f"[PARSE_VAR] Starting at line {token.line}, token: {token.type.name} = '{token.value}'")
    
    # STEP 1: Parse level number and name
    level, name = self._parse_level_and_name()
    
    if self.debug:
        print(f"[PARSE_VAR] Parsed level {level}, name '{name}'")
    
    # ═══════════════════════════════════════════════════════════════
    # TRUTH TABLE ROW 1: Level 66 → RENAMES clause (special handling)
    # ═══════════════════════════════════════════════════════════════
    if level == 66:
        if self.debug:
            print(f"[PARSE_VAR] Level 66 detected → RENAMES clause")
        return self._parse_renames_clause(level, name, token)
    
    # ═══════════════════════════════════════════════════════════════
    # TRUTH TABLE ROW 2: Level 88 → Condition name (special handling)
    # ═══════════════════════════════════════════════════════════════
    if level == 88:
        if self.debug:
            print(f"[PARSE_VAR] Level 88 detected → Condition name")
        return self._parse_condition_name(level, name, token)
    
    # ═══════════════════════════════════════════════════════════════
    # STEP 2: Parse optional clauses (for normal levels 01-49, 77)
    # ═══════════════════════════════════════════════════════════════
    if self.debug:
        print(f"[PARSE_VAR] Next token: {self.current_token().type.name}")
    
    self._skip_whitespace()
    
    # Parse REDEFINES clause
    redefines_target = self._parse_redefines()
    if self.debug and redefines_target:
        print(f"[PARSE_VAR] Found REDEFINES: {redefines_target}")
    
    # Parse EXTERNAL/GLOBAL
    is_external, is_global = self._parse_external_global()
    if self.debug and (is_external or is_global):
        print(f"[PARSE_VAR] Found EXTERNAL={is_external}, GLOBAL={is_global}")
    
    # ═══════════════════════════════════════════════════════════════
    # 🔥 NEW: TRUTH TABLE ROW 2.5: Check for COPY statement
    # Pattern: 01  TST-TEST                    COPY K101A.
    # This indicates a group item whose children come from COPY
    # 
    # CRITICAL: Distinguish from "77 COPY K1W02." where COPY is the name!
    # If name == "COPY", this is NOT a copybook statement
    # ═══════════════════════════════════════════════════════════════
    if self.match(COBOLTokenType.COPY) and name.upper() != "COPY":
        if self.debug:
            print(f"[PARSE_VAR] Detected COPY statement for '{name}' at level {level}")
            print(f"[PARSE_VAR] → This is a GROUP item (children from COPY)")
        
        # Skip the COPY statement
        self.advance()  # consume COPY
        if self.match(COBOLTokenType.IDENTIFIER):
            copy_name = self.current_token().value
            self.advance()
            if self.debug:
                print(f"[PARSE_VAR] COPY {copy_name} (copybook not expanded in parser)")
        
        # Consume period if present
        if self.match(COBOLTokenType.PERIOD):
            self.advance()
        
        # Since COPY brings in children, this is a GROUP item
        # Parse children that follow (they come from the COPY expansion)
        children = []
        while self.match(COBOLTokenType.LEVEL_NUMBER):
            child_level = int(self.current_token().value)
            if child_level <= level:
                break  # End of this group's children
            
            child = self.parse_variable_decl()
            if child:
                children.append(child)
        
        # Return group item
        return COBOLVariableDecl(
            level=level,
            name=name,
            pic_clause=None,  # Group items don't have PIC
            value=None,
            occurs_count=0,
            decimal_places=0,
            usage_type=None,
            is_signed=False,
            children=children,
            is_external=is_external,
            is_global=is_global,
            redefines_target=redefines_target,
            line=token.line,
            column=token.column
        )
    
    # ═══════════════════════════════════════════════════════════════
    # TRUTH TABLE ROW 3: USAGE IS INDEX (special handling)
    # ═══════════════════════════════════════════════════════════════
    usage_is_index = self._check_usage_index()
    if usage_is_index:
        if self.debug:
            print(f"[PARSE_VAR] USAGE IS INDEX detected")
        return self._create_index_variable(level, name, redefines_target, 
                                           is_external, is_global, token)
    
    # Parse OCCURS clause (can appear before or after PIC)
    occurs_info = self._parse_occurs_clause()
    if self.debug and occurs_info['occurs_count']:
        print(f"[PARSE_VAR] Found OCCURS: {occurs_info['occurs_count']}")

    # ═══════════════════════════════════════════════════════════════
    # STEP 3: Determine item type (Group vs Elementary) using truth table
    # ═══════════════════════════════════════════════════════════════
    # CRITICAL: Skip any trailing separators before lookahead
    while self.match(COBOLTokenType.COMMA, COBOLTokenType.SEMICOLON):
        self.advance()
        self._skip_whitespace()   
    
    
    # returns 5 values including next_is_usage
    next_is_pic, next_is_period, next_is_value, next_is_sign, next_is_usage = self._peek_ahead(current_level=level)

    if self.debug:
        print(f"[PARSE_VAR] Item type detection: PIC={next_is_pic}, "
              f"PERIOD+CHILDREN={next_is_period}, VALUE={next_is_value}, "
              f"SIGN={next_is_sign}, USAGE={next_is_usage}")

    # SPECIAL CASE: Level 77 is ALWAYS elementary (never a group)
    if level == 77:
        if self.debug:
            print(f"[PARSE_VAR] → Elementary item (level 77 must be elementary)")
        return self._parse_elementary_item(level, name, occurs_info, redefines_target,
                                        is_external, is_global, token)
    
    
    # TRUTH TABLE ROW 4: Has PIC clause → Elementary Item
    if next_is_pic:
        if self.debug:
            print(f"[PARSE_VAR] → Elementary item (has PIC)")
        return self._parse_elementary_item(level, name, occurs_info, redefines_target,
                                           is_external, is_global, token)

    # ✅ NEW ROW: Has USAGE clause → Elementary Item
    # This handles cases like: OCCURS 10 TIMES INDEXED BY IN1 USAGE DISPLAY.
    # USAGE is checked BEFORE group item checks because it's definitive
    if next_is_usage:
        if self.debug:
            print(f"[PARSE_VAR] → Elementary item (has USAGE, will parse it)")
        return self._parse_elementary_item(level, name, occurs_info, redefines_target,
                                           is_external, is_global, token)

    # TRUTH TABLE ROW 5-6: No PIC/USAGE, has PERIOD+CHILDREN/VALUE/EXTERNAL/GLOBAL/SIGN → Group Item
    # ✅ UPDATED: next_is_period now means "has children"
    if next_is_period or next_is_value or is_external or is_global or next_is_sign:
        if self.debug:
            reason = []
            if next_is_period: reason.append("PERIOD+CHILDREN")
            if next_is_value: reason.append("VALUE")
            if is_external: reason.append("EXTERNAL")
            if is_global: reason.append("GLOBAL")
            if next_is_sign: reason.append("SIGN")
            print(f"[PARSE_VAR] → Group item (no PIC, has {', '.join(reason)})")
        return self._parse_group_item(level, name, occurs_info, redefines_target,
                                      is_external, is_global, token)

    # ═══════════════════════════════════════════════════════════════
    # FALLBACK: Default to elementary item if ambiguous
    # ═══════════════════════════════════════════════════════════════
    if self.debug:
        print(f"[PARSE_VAR] → Defaulting to elementary item")
    return self._parse_elementary_item(level, name, occurs_info, redefines_target,
                                       is_external, is_global, token)
    
    
def _parse_renames_clause(self, level, name, token):
    """Parse level 66 RENAMES clause
    
    TRUTH TABLE: RENAMES Clause Patterns
    ┌────────────────────────────────────────┬─────────────────────────┐
    │ Pattern                                │ Action                  │
    ├────────────────────────────────────────┼─────────────────────────┤
    │ 66 name RENAMES field1.                │ Single field rename     │
    │ 66 name RENAMES field1 THRU field2.    │ Range rename            │
    │ 66 name RENAMES field1 THROUGH field2. │ Range rename (synonym)  │
    └────────────────────────────────────────┴─────────────────────────┘
    
    Examples:
    - 66 LONG-NARRATIVE RENAMES LAG-TIME THRU MSG.
    - 66 SHORT-NARRATIVE RENAMES IDLE-COUNT THROUGH MSG.
    - 66 TOTAL-FIELDS RENAMES FIELD-A.
    """
    if self.debug:
        print(f"[PARSE_VAR] Parsing level 66 RENAMES: {name}")
    
    self._skip_whitespace()
    
    # Expect RENAMES keyword
    if not self.match(COBOLTokenType.RENAMES):
        # Check if RENAMES is an IDENTIFIER (lexer might not tokenize it)
        if self.match(COBOLTokenType.IDENTIFIER):
            if self.current_token().value.upper() != 'RENAMES':
                self.error(f"Expected RENAMES keyword after level 66 name '{name}'")
        else:
            self.error(f"Expected RENAMES keyword after level 66 name '{name}'")
    
    self.advance()  # Consume RENAMES
    self._skip_whitespace()
    
    # Parse first field name
    if not self.match(COBOLTokenType.IDENTIFIER):
        self.error(f"Expected field name after RENAMES in '{name}'")
    
    renames_from = self.current_token().value
    self.advance()
    self._skip_whitespace()
    
    # Check for THRU/THROUGH keyword (optional)
    renames_thru = None
    if self.match(COBOLTokenType.THRU, COBOLTokenType.THROUGH):
        self.advance()  # Consume THRU/THROUGH
        self._skip_whitespace()
        
        # Parse second field name
        if not self.match(COBOLTokenType.IDENTIFIER):
            self.error(f"Expected field name after THRU/THROUGH in '{name}'")
        
        renames_thru = self.current_token().value
        self.advance()
        self._skip_whitespace()
    
    # Consume terminating PERIOD
    # ✅ FIX: Be more lenient about PERIOD - it might be optional or delayed
    if not self.match(COBOLTokenType.PERIOD):
        # TRUTH TABLE: What can follow RENAMES clause?
        # ┌──────────────────┬─────────────────────────────┐
        # │ Next Token       │ Action                      │
        # ├──────────────────┼─────────────────────────────┤
        # │ PERIOD           │ Consume it (standard)       │
        # │ LEVEL_NUMBER     │ OK - start of next variable │
        # │ EOF              │ OK - end of section         │
        # │ PROCEDURE/DATA   │ OK - division boundary      │
        # │ Other            │ Warning, try to recover     │
        # └──────────────────┴─────────────────────────────┘
        
        next_tok = self.current_token()
        
        # Check if we're at a natural boundary
        if next_tok.type in (COBOLTokenType.LEVEL_NUMBER, COBOLTokenType.EOF,
                            COBOLTokenType.PROCEDURE, COBOLTokenType.DATA):
            # Missing period, but at a valid boundary - issue warning
            if self.debug:
                print(f"[PARSE_VAR] ⚠ Missing PERIOD after RENAMES '{name}' at line {token.line}")
                print(f"[PARSE_VAR]   But next token is {next_tok.type.name}, so continuing")
            # Don't error - just continue without period
        else:
            # Unexpected token - still try to find period
            if self.debug:
                print(f"[PARSE_VAR] ⚠ Expected PERIOD after RENAMES '{name}', got {next_tok.type.name}")
            
            # Try to recover by looking ahead
            saved_pos = self.pos
            found_period = False
            for _ in range(5):  # Look ahead up to 5 tokens
                if self.match(COBOLTokenType.PERIOD):
                    found_period = True
                    break
                if self.match(COBOLTokenType.LEVEL_NUMBER, COBOLTokenType.EOF):
                    break
                self.advance()
            
            if found_period:
                self.consume(COBOLTokenType.PERIOD)
            else:
                # Restore position and continue without period
                self.pos = saved_pos
                if self.debug:
                    print(f"[PARSE_VAR]   Could not find PERIOD, continuing anyway")
    else:
        # Period found - consume it normally
        self.consume(COBOLTokenType.PERIOD)
    
    if self.debug:
        if renames_thru:
            print(f"[PARSE_VAR] Level 66: {name} RENAMES {renames_from} THRU {renames_thru}")
        else:
            print(f"[PARSE_VAR] Level 66: {name} RENAMES {renames_from}")
    
    # Create a special variable declaration for RENAMES
    # Store the renames info in the 'value' field as a string for now
    renames_value = f"{renames_from} THRU {renames_thru}" if renames_thru else renames_from
    
    return COBOLVariableDecl(
        level=66,
        name=name,
        pic_clause=None,
        value=renames_value,  # Store RENAMES info here
        occurs_count=None,
        decimal_places=None,
        usage_type='RENAMES',  # Mark as RENAMES type
        is_signed=False,
        children=[],
        occurs_min=None,
        occurs_max=None,
        depending_on=None,
        is_external=False,
        is_global=False,
        redefines_target=None,
        index_names=[],
        line=token.line,
        column=token.column
    )

def _parse_level_and_name(self):
    """Parse level number and variable name with truth table approach
    
    TRUTH TABLE: Variable declaration start
    ┌─────────────────────────────────┬────────────────────────────┐
    │ Token Sequence                  │ Action                     │
    ├─────────────────────────────────┼────────────────────────────┤
    │ LEVEL_NUMBER IDENTIFIER         │ Standard variable          │
    │ LEVEL_NUMBER IDENTIFIER COMMA   │ Skip comma (noise word)    │
    │ LEVEL_NUMBER KEYWORD            │ Keyword as variable name   │
    │ LEVEL_NUMBER USAGE/COMP/etc     │ Anonymous FILLER           │
    │ Other                           │ Error                      │
    └─────────────────────────────────┴────────────────────────────┘
    
    CRITICAL: COBOL allows keywords as variable names (e.g., 77 PROCEDURE).
    This is insane but required for NIST test compliance.
    
    Examples:
    - 77 PROCEDURE PIC X(10).         → name="PROCEDURE"
    - 77 COPY K1W02.                  → name="COPY"
    - 01 DIVISION.                    → name="DIVISION"
    - 77 RCD-1 COPY K1W01.            → name="RCD-1" (normal)
    """
    # Parse level number (required)
    level_token = self.consume(COBOLTokenType.LEVEL_NUMBER)
    level = int(level_token.value)
    
    name_token = self.current_token()
    name = None
    
    # TRUTH TABLE: Determine variable name
    
    # Rule 1: IDENTIFIER (standard variable name)
    if self.match(COBOLTokenType.IDENTIFIER):
        name = self.consume(COBOLTokenType.IDENTIFIER).value
        
        # ✅ Skip optional comma after name (COBOL noise word)
        if self.match(COBOLTokenType.COMMA, COBOLTokenType.SEMICOLON):
            self.advance()
        
        return level, name
    
    # Rule 2: USAGE/COMP keywords (anonymous FILLER)
    elif self.match(
        COBOLTokenType.USAGE, COBOLTokenType.COMP, COBOLTokenType.COMP_1,
        COBOLTokenType.COMP_2, COBOLTokenType.COMP_3, COBOLTokenType.COMPUTATIONAL,
        COBOLTokenType.COMPUTATIONAL_3, COBOLTokenType.BINARY,
        COBOLTokenType.PACKED_DECIMAL, COBOLTokenType.DISPLAY
    ):
        name = name_token.value
        self.advance()
        return level, name
    
    # ══════════════════════════════════════════════════════════════
    # Rule 3: KEYWORDS as variable names (COBOL allows this!)
    # Pattern: 77 PROCEDURE, 01 DIVISION, 77 COPY, etc.
    # This is insane but legal in COBOL and tested by NIST
    # ══════════════════════════════════════════════════════════════
    elif name_token.type not in (
        COBOLTokenType.EOF,
        COBOLTokenType.PERIOD,
        COBOLTokenType.LPAREN,
        COBOLTokenType.RPAREN,
        COBOLTokenType.COMMA
    ):
        # ANY other token type can be a variable name in COBOL
        # (except obvious delimiters like EOF, PERIOD, parens)
        name = name_token.value
        self.advance()
        
        if self.debug:
            print(f"[PARSE_VAR] Keyword '{name}' used as variable name at level {level}")
        
        # Skip optional comma
        if self.match(COBOLTokenType.COMMA, COBOLTokenType.SEMICOLON):
            self.advance()
        
        return level, name
    
    # Rule 4: No valid name found → error
    else:
        self.error(f"Expected variable name after level {level}, got {name_token.type.name}")

def _parse_level88_values(self):
    """Parse level 88 VALUE list with truth table approach - HARDENED
    
    TRUTH TABLE: Level 88 Value Patterns
    ┌─────────────────────────────────────┬──────────────────────────┐
    │ Pattern                             │ Action                   │
    ├─────────────────────────────────────┼──────────────────────────┤
    │ literal                             │ Single value             │
    │ literal THRU literal                │ Range                    │
    │ literal, literal, ...               │ Comma-separated list     │
    │ literal literal ...                 │ Space-separated list     │
    │ Mixed: lit THRU lit, lit, lit ...   │ Mixed separators         │
    │ Continuation lines                  │ Skip whitespace/newlines │
    └─────────────────────────────────────┴──────────────────────────┘
    
    Examples:
    - VALUE 1
    - VALUE 2 THRU 4
    - VALUE .01, .11, .21 .81          (mixed comma/space)
    - VALUE 100 THRU 128 1000 THRU 1280 -9 THRU -2  (multiple ranges)
    - VALUES ARE 06 THRU 10            (continuation line ->)
                16 THRU 20  00         (multiple values on next line)
    """
    values = []
    
    # CRITICAL: Skip all whitespace including newlines before first value
    self._skip_whitespace()
    
    # Parse first value (required)
    first_val = self._parse_level88_single_value()
    if first_val is None:
        return None
    
    # Check if first value is part of a THRU range
    self._skip_whitespace()  # CRITICAL: Skip after each value
    if self.match(COBOLTokenType.THRU, COBOLTokenType.THROUGH):
        self.advance()
        self._skip_whitespace()
        end_val = self._parse_level88_single_value()
        if end_val:
            values.append(f"{first_val} THRU {end_val}")
        else:
            self.error("Expected value after THRU")
    else:
        values.append(first_val)
    
    # Parse additional values (comma or space separated)
    max_iterations = 100
    iteration = 0
    
    while iteration < max_iterations:
        iteration += 1
        
        # CRITICAL: Always skip whitespace at start of loop
        # This handles continuation lines
        self._skip_whitespace()
        
        # TRUTH TABLE: When to stop parsing values
        # Row 1: Hit PERIOD → end of statement
        if self.match(COBOLTokenType.PERIOD):
            break
        
        # Row 2: Hit EOF → end of values
        if self.match(COBOLTokenType.EOF):
            break
        
        # Row 3: Check for statement keywords that end the value list
        # (shouldn't happen, but be defensive)
        if self.match(
            COBOLTokenType.MOVE, COBOLTokenType.ADD, COBOLTokenType.IF,
            COBOLTokenType.PERFORM, COBOLTokenType.DISPLAY
        ):
            break
        
        # Row 4: Hit comma → expect another value
        has_comma = False
        if self.match(COBOLTokenType.COMMA):
            self.advance()
            self._skip_whitespace()
            has_comma = True
        
        # Row 5: Check if another value follows (space-separated)
        # Only continue if we see a valid value token
        if not has_comma:
            if not self.match(
                COBOLTokenType.NUMBER_LITERAL,
                COBOLTokenType.STRING_LITERAL,
                COBOLTokenType.PLUS,
                COBOLTokenType.MINUS,
                COBOLTokenType.IDENTIFIER,
                COBOLTokenType.ALL,
                COBOLTokenType.LEVEL_NUMBER, 
            ):
                break
        
        # Parse the next value
        next_val = self._parse_level88_single_value()
        if next_val is None:
            if has_comma:
                self.error("Expected value after comma in VALUE list")
            break
        
        # CRITICAL: Skip whitespace after value
        self._skip_whitespace()
        
        # Check if this is part of a THRU range
        if self.match(COBOLTokenType.THRU, COBOLTokenType.THROUGH):
            self.advance()
            self._skip_whitespace()
            end_val = self._parse_level88_single_value()
            if end_val:
                values.append(f"{next_val} THRU {end_val}")
                self._skip_whitespace()  # Skip after range end
            else:
                self.error("Expected value after THRU")
        else:
            values.append(next_val)
    
    if iteration >= max_iterations:
        print(f"[WARNING] Level 88 value parsing hit iteration limit")
    
    # Combine all values into a single string
    if len(values) == 0:
        return None
    elif len(values) == 1:
        return values[0]
    else:
        return ", ".join(values)


def _parse_level88_single_value(self):
    """Parse a single value in a level 88 VALUE list - HARDENED
    
    TRUTH TABLE: Single Value Types
    ┌──────────────────────────┬──────────────────────────┐
    │ Token Pattern            │ Action                   │
    ├──────────────────────────┼──────────────────────────┤
    │ ALL literal              │ Return "ALL literal"     │
    │ NUMBER_LITERAL           │ Return as-is             │
    │ STRING_LITERAL           │ Return as-is             │
    │ PLUS NUMBER              │ Return "+123"            │
    │ MINUS NUMBER             │ Return "-123"            │
    │ IDENTIFIER (ZERO, etc)   │ Return identifier        │
    │ Other                    │ Return None              │
    └──────────────────────────┴──────────────────────────┘
    """
    # CRITICAL: Don't call _skip_whitespace here - caller handles it
    
    # Handle ALL literal (e.g., ALL "BAC", ALL QUOTE)
    if self.match(COBOLTokenType.ALL):
        self.advance()  # Consume ALL
        
        # Skip whitespace between ALL and literal
        while self.current_token() and self.current_token().type in (
            COBOLTokenType.NEWLINE, COBOLTokenType.COMMENT
        ):
            self.advance()
        
        if self.match(COBOLTokenType.STRING_LITERAL):
            literal = self.current_token().value
            self.advance()
            return f"ALL {literal}"
        
        elif self.match(COBOLTokenType.IDENTIFIER):
            # ALL QUOTE, ALL SPACE, etc.
            literal = self.current_token().value
            self.advance()
            return f"ALL {literal}"
        
        else:
            self.error("Expected literal after ALL keyword")
            return None
    
    elif self.match(COBOLTokenType.NUMBER_LITERAL):
        val = self.current_token().value
        self.advance()
        return val
    
    elif self.match(COBOLTokenType.LEVEL_NUMBER):
        # Inside level 88 VALUE lists, LEVEL_NUMBER tokens
        # should be treated as NUMBER_LITERAL (continuation line edge case)
        # Example: VALUES ARE 06 THRU 10
        #                     16 THRU 20  <- 16 gets lexed as LEVEL_NUMBER
        val = self.current_token().value
        self.advance()
        return val    
    
    elif self.match(COBOLTokenType.STRING_LITERAL):
        val = self.current_token().value
        self.advance()
        return val
    
    elif self.match(COBOLTokenType.PLUS, COBOLTokenType.MINUS):
        sign = self.current_token().value
        self.advance()
        if self.match(COBOLTokenType.NUMBER_LITERAL):
            val = sign + self.current_token().value
            self.advance()
            return val
        else:
            self.error(f"Expected number after {sign}")
            return None
    
    elif self.match(COBOLTokenType.IDENTIFIER):
        # Figurative constants like ZERO, SPACES, QUOTE, HIGH-VALUE, etc.
        val = self.current_token().value
        # Check it's not a keyword that ends the value list
        if val.upper() not in ('THRU', 'THROUGH', 'VALUE', 'VALUES'):
            self.advance()
            return val
        return None
    
    else:
        return None


def _parse_condition_name(self, level, name, token):
    """Parse level 88 condition name with VALUE clause - HARDENED
    
    TRUTH TABLE: Level 88 VALUE Patterns
    ┌────────────────────────────────────┬──────────────────────────┐
    │ Pattern                            │ Action                   │
    ├────────────────────────────────────┼──────────────────────────┤
    │ VALUE [IS] values...               │ Parse values             │
    │ VALUES [ARE] values...             │ Parse values (plural)    │
    │ No VALUE clause                    │ value = None             │
    └────────────────────────────────────┴──────────────────────────┘
    """
    if self.debug:
        print(f"[PARSE_VAR] Level 88 condition name: {name}")
    
    # CRITICAL: Skip whitespace before looking for VALUE keyword
    while self.current_token() and self.current_token().type in (
        COBOLTokenType.NEWLINE, COBOLTokenType.COMMENT
    ):
        self.advance()
    
    value = None
    
    # STEP 1: Check for VALUE or VALUES keyword
    has_value_keyword = False
    
    if self.match(COBOLTokenType.VALUE):
        has_value_keyword = True
        self.advance()
        
    elif self.match(COBOLTokenType.IDENTIFIER) and \
         self.current_token().value.upper() == 'VALUES':
        has_value_keyword = True
        self.advance()
    
    # STEP 2: Handle optional IS or ARE keyword
    if has_value_keyword:
        # Skip whitespace after VALUE/VALUES
        while self.current_token() and self.current_token().type in (
            COBOLTokenType.NEWLINE, COBOLTokenType.COMMENT
        ):
            self.advance()
        
        if self.match(COBOLTokenType.IS):
            self.advance()
        elif self.match(COBOLTokenType.IDENTIFIER) and \
             self.current_token().value.upper() == 'ARE':
            self.advance()
    
    # STEP 3: Parse the value(s) using helper function
    if has_value_keyword:
        value = self._parse_level88_values()
    
    # STEP 4: Skip any trailing whitespace before PERIOD
    while self.current_token() and self.current_token().type in (
        COBOLTokenType.NEWLINE, COBOLTokenType.COMMENT
    ):
        self.advance()
    
    # STEP 5: Consume terminating PERIOD
    if not self.match(COBOLTokenType.PERIOD):
        current_tok = self.current_token()
        tok_info = f"{current_tok.type.name}='{current_tok.value}'" if current_tok else "EOF"
        self.error(f"Expected PERIOD after level 88 condition name '{name}', got {tok_info}")
    
    self.consume(COBOLTokenType.PERIOD)
    
    if self.debug:
        print(f"[PARSE_VAR] Level 88 complete: {name}, VALUE={value}")
    
    return COBOLVariableDecl(
        level=88,
        name=name,
        pic_clause=None,
        value=value,
        occurs_count=None,
        decimal_places=None,
        usage_type='CONDITION',
        is_signed=False,
        children=[],
        occurs_min=None,
        occurs_max=None,
        depending_on=None,
        is_external=False,
        is_global=False,
        redefines_target=None,
        index_names=[],
        line=token.line,
        column=token.column
    )


def _skip_whitespace(self):
    """Skip newlines and comments"""
    while self.match(COBOLTokenType.NEWLINE, COBOLTokenType.COMMENT):
        self.advance()

def _skip_separators(self):
    """Skip optional COMMA and SEMICOLON separators
    
    COBOL X3.23-1985 Section 2.1.6: Comma and semicolon are
    optional separators and are interchangeable.
    
    Can appear multiple times: "; ; ," is valid in COBOL.
    
    TRUTH TABLE: Separator Skipping
    ┌──────────────┬─────────────────────────────────┐
    │ Token Type   │ Action                          │
    ├──────────────┼─────────────────────────────────┤
    │ COMMA        │ Skip, check next token          │
    │ SEMICOLON    │ Skip, check next token          │
    │ Other        │ Stop, return                    │
    └──────────────┴─────────────────────────────────┘
    
    Examples:
    - After TIMES: "OCCURS 5 TIMES; INDEXED BY IX"
    - Multiple: "OCCURS 5 TIMES;; INDEXED BY IX"
    - Mixed: "OCCURS 5 TIMES;, INDEXED BY IX"
    """
    while self.match(COBOLTokenType.COMMA, COBOLTokenType.SEMICOLON):
        if self.debug:
            sep = self.current_token().type.name
            print(f"[SKIP_SEP] Skipping {sep}")
        self.advance()
        self._skip_whitespace()  # Also skip whitespace after separator


# Add this to the monkey-patching section at the bottom of expression_parsers.py:
# COBOLMultiProgramParser._skip_separators = _skip_separators


def _parse_redefines(self):
    """Parse optional REDEFINES clause
    
    TRUTH TABLE: REDEFINES Clause Parsing
    ┌──────────────────────────┬──────────┬─────────────────────────┐
    │ Token Sequence           │ Result   │ Position After          │
    ├──────────────────────────┼──────────┼─────────────────────────┤
    │ REDEFINES identifier     │ name     │ After identifier        │
    │ REDEFINES <missing id>   │ Error    │ Error raised            │
    │ <not REDEFINES>          │ None     │ Unchanged               │
    └──────────────────────────┴──────────┴─────────────────────────┘
    
    Examples:
        02  NEW-FIELD REDEFINES OLD-FIELD PIC X(10).
        77  BINARY-NUM REDEFINES DISPLAY-NUM PIC 9(4) COMP.
    
    CRITICAL: REDEFINES must be followed by an identifier (the target).
    Missing identifier is a syntax error.
    
    Returns:
        str: Name of the field being redefined, or None if no REDEFINES
    """
    # ROW 3: Not REDEFINES → Return None, position unchanged
    if not self.match(COBOLTokenType.REDEFINES):
        return None
    
    if self.debug:
        print(f"[PARSE_REDEFINES] Found REDEFINES keyword")
    
    # Consume REDEFINES keyword
    self.advance()
    
    # Skip whitespace/comments after REDEFINES
    while self.current_token() and self.current_token().type in (
        COBOLTokenType.COMMENT, COBOLTokenType.NEWLINE
    ):
        self.advance()
    
    # ROW 2: REDEFINES without identifier → Error
    if not self.match(COBOLTokenType.IDENTIFIER):
        current_tok = self.current_token()
        tok_info = f"{current_tok.type.name}='{current_tok.value}'" if current_tok else "EOF"
        self.error(f"Expected identifier after REDEFINES keyword, got {tok_info}")
    
    # ROW 1: REDEFINES identifier → Return target name
    redefines_target = self.current_token().value
    self.advance()  # Consume identifier
    
    # Skip whitespace after identifier
    while self.current_token() and self.current_token().type in (
        COBOLTokenType.COMMENT, COBOLTokenType.NEWLINE
    ):
        self.advance()
    
    if self.debug:
        print(f"[PARSE_REDEFINES] Target: '{redefines_target}'")
    
    return redefines_target


def _parse_external_global(self):
    """Parse optional EXTERNAL/GLOBAL flags using truth table approach
    
    TRUTH TABLE: EXTERNAL/GLOBAL Clause Parsing
    ┌─────────────────────┬──────────────────────────────────────┐
    │ Current Token       │ Action                               │
    ├─────────────────────┼──────────────────────────────────────┤
    │ IS                  │ Advance, check next for EXTERNAL/GLOBAL │
    │ IDENTIFIER("IS")    │ Advance, check next for EXTERNAL/GLOBAL │
    │ IDENTIFIER("EXT")   │ Set flag, advance                    │
    │ IDENTIFIER("GLOBAL")│ Set flag, advance                    │
    │ Other               │ Return (False, False)                │
    └─────────────────────┴──────────────────────────────────────┘
    """
    is_external = False
    is_global = False
    
    # ✅ FIX: Check for IS token type FIRST
    if self.match(COBOLTokenType.IS):
        self.advance()
        self._skip_whitespace()
        
        # After IS, check for EXTERNAL/GLOBAL (as IDENTIFIER)
        if self.match(COBOLTokenType.IDENTIFIER):
            next_val = self.current_token().value.upper()
            if next_val == 'EXTERNAL':
                is_external = True
                self.advance()
            elif next_val == 'GLOBAL':
                is_global = True
                self.advance()
        
        self._skip_whitespace()
        if self.debug and (is_external or is_global):
            print(f"[PARSE_VAR] Variable is {'EXTERNAL' if is_external else ''}{'GLOBAL' if is_global else ''}")
        return is_external, is_global
    
    # Check for IDENTIFIER tokens
    if not self.match(COBOLTokenType.IDENTIFIER):
        return is_external, is_global
    
    peek_val = self.current_token().value.upper()
    
    # Handle "IS EXTERNAL" or "IS GLOBAL" when IS is an IDENTIFIER
    if peek_val == 'IS':
        self.advance()
        self._skip_whitespace()
        
        if self.match(COBOLTokenType.IDENTIFIER):
            next_val = self.current_token().value.upper()
            if next_val == 'EXTERNAL':
                is_external = True
                self.advance()
            elif next_val == 'GLOBAL':
                is_global = True
                self.advance()
    
    # Direct EXTERNAL or GLOBAL (no IS)
    elif peek_val == 'EXTERNAL':
        is_external = True
        self.advance()
    elif peek_val == 'GLOBAL':
        is_global = True
        self.advance()
    
    self._skip_whitespace()
    
    if self.debug and (is_external or is_global):
        print(f"[PARSE_VAR] Variable is {'EXTERNAL' if is_external else ''}{'GLOBAL' if is_global else ''}")
    
    return is_external, is_global


def _check_usage_index(self):
    """Check for USAGE IS INDEX clause
    
    TRUTH TABLE: USAGE IS INDEX Detection
    ┌─────────────────────────┬──────────┬─────────────────────┐
    │ Token Sequence          │ Result   │ Position After      │
    ├─────────────────────────┼──────────┼─────────────────────┤
    │ USAGE IS INDEX          │ True     │ After INDEX         │
    │ USAGE INDEX             │ True     │ After INDEX         │
    │ USAGE <other>           │ False    │ Before USAGE        │
    │ <not USAGE>             │ False    │ Unchanged           │
    └─────────────────────────┴──────────┴─────────────────────┘
    
    CRITICAL: This function must NOT consume tokens unless it finds INDEX.
    If not USAGE INDEX, position must be restored to before USAGE.
    
    Returns:
        bool: True if USAGE IS INDEX found, False otherwise
    """
    # ROW 4: Not USAGE → Return False immediately
    if not self.match(COBOLTokenType.USAGE):
        return False
    
    # Save position to restore if not INDEX
    saved_pos = self.pos
    
    # Advance past USAGE
    self.advance()
    
    # Skip whitespace/newlines
    while self.current_token() and self.current_token().type in (
        COBOLTokenType.COMMENT, COBOLTokenType.NEWLINE
    ):
        self.advance()
    
    # Check for optional IS
    if self.current_token() and self.current_token().value and \
       self.current_token().value.upper() == 'IS':
        self.advance()
        # Skip whitespace after IS
        while self.current_token() and self.current_token().type in (
            COBOLTokenType.COMMENT, COBOLTokenType.NEWLINE
        ):
            self.advance()
    
    # Check for INDEX keyword
    if self.current_token() and self.current_token().value and \
       self.current_token().value.upper() == 'INDEX':
        # ROW 1-2: Found USAGE [IS] INDEX → Return True, keep position
        if self.debug:
            print(f"[CHECK_USAGE_INDEX] Found USAGE IS INDEX")
        return True
    
    # ROW 3: USAGE but not INDEX → Restore position and return False
    if self.debug:
        token_info = f"{self.current_token().type.name}='{self.current_token().value}'" if self.current_token() else "EOF"
        print(f"[CHECK_USAGE_INDEX] USAGE found but not INDEX (found {token_info}), restoring position")
    
    self.pos = saved_pos  # CRITICAL: Restore to before USAGE
    return False


def _create_index_variable(self, level, name, redefines_target, is_external, is_global, token):
    """Create a USAGE INDEX variable
    
    TRUTH TABLE: USAGE INDEX Variable Creation
    ┌──────────────────────┬────────────────────────────────────┐
    │ Current State        │ Action                             │
    ├──────────────────────┼────────────────────────────────────┤
    │ At INDEX keyword     │ Consume INDEX                      │
    │ After INDEX          │ Skip whitespace, expect PERIOD     │
    │ At PERIOD            │ Consume PERIOD                     │
    │ Missing PERIOD       │ Error: INDEX must end with PERIOD  │
    └──────────────────────┴────────────────────────────────────┘
    
    CRITICAL: This function is called after _check_usage_index() returns True,
    so we should be positioned right after the INDEX keyword.
    
    Args:
        level: Level number (01-49, 77)
        name: Variable name
        redefines_target: REDEFINES target if present
        is_external: EXTERNAL flag
        is_global: GLOBAL flag
        token: Original token for line/column info
        
    Returns:
        COBOLVariableDecl: Variable declaration with USAGE INDEX
    """
    if self.debug:
        print(f"[CREATE_INDEX_VAR] Creating USAGE INDEX variable '{name}' at level {level}")
        print(f"[CREATE_INDEX_VAR] Current token: {self.current_token().type.name} = '{self.current_token().value}'")
    
    # We should already be past INDEX keyword from _check_usage_index
    # But let's verify and consume it if we're still on it
    if self.current_token() and self.current_token().value and \
       self.current_token().value.upper() == 'INDEX':
        if self.debug:
            print(f"[CREATE_INDEX_VAR] Consuming INDEX keyword")
        self.advance()
    
    # Skip whitespace/comments before PERIOD
    while self.current_token() and self.current_token().type in (
        COBOLTokenType.COMMENT, COBOLTokenType.NEWLINE
    ):
        self.advance()
    
    # CRITICAL: Must end with PERIOD
    if not self.match(COBOLTokenType.PERIOD):
        current_tok = self.current_token()
        tok_info = f"{current_tok.type.name}='{current_tok.value}'" if current_tok else "EOF"
        self.error(f"Expected PERIOD after USAGE INDEX for '{name}', got {tok_info}")
    
    self.consume(COBOLTokenType.PERIOD)
    
    if self.debug:
        print(f"[CREATE_INDEX_VAR] Successfully created USAGE INDEX variable '{name}'")
    
    return COBOLVariableDecl(
        level=level,
        name=name,
        pic_clause=None,
        value=None,
        occurs_count=None,
        decimal_places=None,
        usage_type='INDEX',
        is_signed=False,
        children=[],
        redefines_target=redefines_target,
        is_external=is_external,
        is_global=is_global,
        line=token.line,
        column=token.column
    )


def _parse_occurs_clause(self):
    """Parse OCCURS clause with truth table for optional components
    
    TRUTH TABLE: OCCURS Clause Component Detection (Order Matters!)
    ┌──────┬────────────────────┬────────────────────────────────────┐
    │ Step │ Looking For        │ Action                             │
    ├──────┼────────────────────┼────────────────────────────────────┤
    │  1   │ OCCURS keyword     │ If not found, return empty dict    │
    │  2   │ NUMBER_LITERAL     │ Parse occurs count (required)      │
    │  3   │ TO NUMBER          │ If found, parse min/max range      │
    │  4   │ TIMES keyword      │ If found, consume (optional)       │
    │  4b  │ COMMA/SEMICOLON    │ Skip separators                    │
    │  5   │ DEPENDING ON var   │ If found, parse depending clause   │
    │  5b  │ COMMA/SEMICOLON    │ Skip separators                    │
    │  6   │ ASCENDING KEY      │ Loop: parse all ASCENDING clauses  │
    │  7   │ DESCENDING KEY     │ Loop: parse all DESCENDING clauses │
    │  7b  │ COMMA/SEMICOLON    │ Skip separators                    │
    │  8   │ INDEXED BY         │ If found, parse index names        │
    │  8b  │ COMMA/SEMICOLON    │ Skip separators                    │
    │  9   │ End of OCCURS      │ Return dict with all parsed info   │
    └──────┴────────────────────┴────────────────────────────────────┘
    
    TRUTH TABLE: Token Type Detection for Optional Clauses
    ┌────────────────┬──────────────┬───────────────────────────────┐
    │ Current Token  │ Value (UP)   │ Action                        │
    ├────────────────┼──────────────┼───────────────────────────────┤
    │ COMMA/SEMICOLON│ *            │ Skip (separator)              │
    │ IDENTIFIER     │ "DEPENDING"  │ Call _parse_depending_on()    │
    │ IDENTIFIER     │ "ASCENDING"  │ Parse ASCENDING KEY clause    │
    │ IDENTIFIER     │ "DESCENDING" │ Parse DESCENDING KEY clause   │
    │ IDENTIFIER     │ "INDEXED"    │ Call _parse_indexed_by()      │
    │ PIC/PICTURE    │ *            │ Done with OCCURS              │
    │ PERIOD         │ *            │ Done with OCCURS              │
    │ Other          │ *            │ Done with OCCURS              │
    └────────────────┴──────────────┴───────────────────────────────┘
    
    Example: OCCURS 15 TIMES; INDEXED BY IN1, INDEX1, USAGE COMP.
                            ^ skip    ^ skip
    """
    self._skip_whitespace()
    
    # STEP 1: Check for OCCURS keyword
    if not self.match(COBOLTokenType.OCCURS):
        return {
            'occurs_count': None,
            'occurs_min': None,
            'occurs_max': None,
            'depending_on': None,
            'index_names': [],
            'ascending_keys': [],
            'descending_keys': []
        }
    
    self.advance()  # Consume OCCURS
    
    if self.debug:
        print(f"[PARSE_OCCURS] After consuming OCCURS, pos={self.pos}")
        print(f"[PARSE_OCCURS] Current token = {self.current_token().type.name} '{self.current_token().value}'")
    
    # STEP 2: Parse required number
    first_num = int(self.consume(COBOLTokenType.NUMBER_LITERAL).value)
    
    occurs_count = None
    occurs_min = None
    occurs_max = None
    
    # STEP 3: Check for TO keyword (variable-length table)
    if self.match(COBOLTokenType.TO):
        if self.debug:
            print(f"[PARSE_OCCURS] Found TO keyword - variable-length table")
        self.advance()
        occurs_min = first_num
        occurs_max = int(self.consume(COBOLTokenType.NUMBER_LITERAL).value)
        occurs_count = occurs_max
    else:
        if self.debug:
            print(f"[PARSE_OCCURS] Fixed-size table")
        occurs_count = first_num
    
    self._skip_whitespace()
    
    # STEP 4: Optional TIMES keyword
    if self.match(COBOLTokenType.TIMES):
        if self.debug:
            print(f"[PARSE_OCCURS] Found TIMES keyword")
        self.advance()
    
    # STEP 4b: Skip separators after TIMES
    self._skip_whitespace()
    self._skip_separators()
    
    # STEP 5: Optional DEPENDING ON clause
    depending_on = None
    if self.match(COBOLTokenType.IDENTIFIER):
        if self.current_token().value.upper() == 'DEPENDING':
            if self.debug:
                print(f"[PARSE_OCCURS] Found DEPENDING ON clause")
            depending_on = self._parse_depending_on()
    
    # STEP 5b: Skip separators after DEPENDING ON
    self._skip_whitespace()
    self._skip_separators()
    
    # ═══════════════════════════════════════════════════════════════
    # STEP 6 & 7: Parse ASCENDING KEY and DESCENDING KEY clauses
    # These can appear multiple times and in any order
    # ═══════════════════════════════════════════════════════════════
    ascending_keys = []
    descending_keys = []
    
    # Loop to handle multiple KEY clauses
    max_iterations = 20  # Safety limit
    iteration = 0
    
    while iteration < max_iterations:
        iteration += 1
        self._skip_whitespace()
        
        # ✅ FIX: Check for ASCENDING/DESCENDING as token types FIRST
        is_ascending = self.match(COBOLTokenType.ASCENDING)
        is_descending = self.match(COBOLTokenType.DESCENDING)
        
        # Fallback: Also check for IDENTIFIER with value ASCENDING/DESCENDING
        if not is_ascending and not is_descending:
            if self.match(COBOLTokenType.IDENTIFIER):
                token_val = self.current_token().value.upper()
                is_ascending = (token_val == 'ASCENDING')
                is_descending = (token_val == 'DESCENDING')
            else:
                break  # No more KEY clauses
        
        if not is_ascending and not is_descending:
            break  # No more KEY clauses
        
        # TRUTH TABLE ROW 1: ASCENDING KEY
        if is_ascending:
            if self.debug:
                print(f"[PARSE_OCCURS] Found ASCENDING KEY clause")
            self.advance()  # Consume ASCENDING
            self._skip_whitespace()
            
            # Expect KEY keyword (could be IDENTIFIER or KEY token type)
            key_found = False
            if self.match(COBOLTokenType.KEY):
                self.advance()
                key_found = True
            elif self.match(COBOLTokenType.IDENTIFIER) and self.current_token().value.upper() == 'KEY':
                self.advance()
                key_found = True
            
            if key_found:
                self._skip_whitespace()
                
                # ✅ FIX: Check for optional IS keyword
                if self.match(COBOLTokenType.IS):
                    if self.debug:
                        print(f"[PARSE_OCCURS]   Found IS after KEY")
                    self.advance()
                    self._skip_whitespace()
                elif self.match(COBOLTokenType.IDENTIFIER) and self.current_token().value.upper() == 'IS':
                    if self.debug:
                        print(f"[PARSE_OCCURS]   Found IS (as identifier) after KEY")
                    self.advance()
                    self._skip_whitespace()
                
                # Parse the field name(s)
                if self.match(COBOLTokenType.IDENTIFIER):
                    key_field = self.current_token().value
                    ascending_keys.append(key_field)
                    self.advance()
                    if self.debug:
                        print(f"[PARSE_OCCURS]   Added ascending key: {key_field}")
            continue

        # TRUTH TABLE ROW 2: DESCENDING KEY
        elif is_descending:
            if self.debug:
                print(f"[PARSE_OCCURS] Found DESCENDING KEY clause")
            self.advance()  # Consume DESCENDING
            self._skip_whitespace()
            
            # Expect KEY keyword (could be IDENTIFIER or KEY token type)
            key_found = False
            if self.match(COBOLTokenType.KEY):
                self.advance()
                key_found = True
            elif self.match(COBOLTokenType.IDENTIFIER) and self.current_token().value.upper() == 'KEY':
                self.advance()
                key_found = True
            
            if key_found:
                self._skip_whitespace()
                
                # ✅ FIX: Check for optional IS keyword
                if self.match(COBOLTokenType.IS):
                    if self.debug:
                        print(f"[PARSE_OCCURS]   Found IS after KEY")
                    self.advance()
                    self._skip_whitespace()
                elif self.match(COBOLTokenType.IDENTIFIER) and self.current_token().value.upper() == 'IS':
                    if self.debug:
                        print(f"[PARSE_OCCURS]   Found IS (as identifier) after KEY")
                    self.advance()
                    self._skip_whitespace()
                
                # Parse the field name(s)
                if self.match(COBOLTokenType.IDENTIFIER):
                    key_field = self.current_token().value
                    descending_keys.append(key_field)
                    self.advance()
                    if self.debug:
                        print(f"[PARSE_OCCURS]   Added descending key: {key_field}")
            continue
    
    # STEP 7b: Skip separators after KEY clauses
    self._skip_whitespace()
    self._skip_separators()
    
    # ═══════════════════════════════════════════════════════════════
    # STEP 8: Optional INDEXED BY clause
    # ═══════════════════════════════════════════════════════════════
    index_names = []
    if self.match(COBOLTokenType.IDENTIFIER):
        if self.current_token().value.upper() == 'INDEXED':
            if self.debug:
                print(f"[PARSE_OCCURS] Found INDEXED BY clause")
            index_names = self._parse_indexed_by()
    
    # STEP 8b: Skip separators after INDEXED BY (for completeness)
    self._skip_whitespace()
    self._skip_separators()
    
    # STEP 9: Return complete OCCURS information
    if self.debug:
        print(f"[PARSE_OCCURS] Complete: count={occurs_count}, min={occurs_min}, max={occurs_max}")
        print(f"[PARSE_OCCURS]   depending_on={depending_on}")
        print(f"[PARSE_OCCURS]   ascending_keys={ascending_keys}")
        print(f"[PARSE_OCCURS]   descending_keys={descending_keys}")
        print(f"[PARSE_OCCURS]   index_names={index_names}")
    
    return {
        'occurs_count': occurs_count,
        'occurs_min': occurs_min,
        'occurs_max': occurs_max,
        'depending_on': depending_on,
        'index_names': index_names,
        'ascending_keys': ascending_keys,
        'descending_keys': descending_keys
    }




def _parse_depending_on(self):
    """Parse DEPENDING ON clause with lenient mode for missing ON
    
    TRUTH TABLE: DEPENDING [ON] Clause Patterns (Lenient Mode)
    ┌────────────────────────────────┬─────────────────────────────┐
    │ Token Sequence                 │ Action                      │
    ├────────────────────────────────┼─────────────────────────────┤
    │ DEPENDING ON identifier        │ Parse normally (standard)   │
    │ DEPENDING identifier           │ Accept without ON (lenient) │
    │ DEPENDING [WS] ON [WS] id      │ Skip whitespace between     │
    └────────────────────────────────┴─────────────────────────────┘
    
    Examples:
    1. OCCURS 1 TO 15 DEPENDING ON DN3        ✓ Standard COBOL
    2. OCCURS 1 TO 26 DEPENDING TBL-LENGTH    ✓ Lenient (missing ON)
    
    CRITICAL: The lexer can tokenize "ON" as either:
    - COBOLTokenType.ON (keyword token)
    - COBOLTokenType.IDENTIFIER with value "ON"
    
    We must handle BOTH cases, plus the case where ON is missing!
    """
    if self.debug:
        print(f"[PARSE_DEPENDING] Starting, pos={self.pos}")
        print(f"[PARSE_DEPENDING] Current: {self.current_token().type.name} = '{self.current_token().value}'")
    
    # STEP 1: Consume DEPENDING keyword (already verified by caller)
    self.advance()  # Consume DEPENDING
    self._skip_whitespace()
    
    if self.debug:
        print(f"[PARSE_DEPENDING] After DEPENDING, current: {self.current_token().type.name} = '{self.current_token().value}'")
    
    # STEP 2: Look for optional ON keyword (two possible token types)
    on_found = False
    
    # TRUTH TABLE ROW 1: ON as keyword token (COBOLTokenType.ON)
    if self.match(COBOLTokenType.ON):
        if self.debug:
            print(f"[PARSE_DEPENDING] Found ON as token type")
        self.advance()  # Consume ON token
        self._skip_whitespace()
        on_found = True
    
    # TRUTH TABLE ROW 2: ON as identifier token
    elif self.match(COBOLTokenType.IDENTIFIER):
        if self.current_token().value.upper() == 'ON':
            if self.debug:
                print(f"[PARSE_DEPENDING] Found ON as identifier")
            self.advance()  # Consume ON identifier
            self._skip_whitespace()
            on_found = True
        else:
            # TRUTH TABLE ROW 3: No ON keyword - variable name directly (LENIENT MODE)
            if self.debug:
                print(f"[PARSE_DEPENDING] ⚠️  Missing ON keyword - accepting variable directly (lenient mode)")
            # Don't advance - the current identifier IS the variable name
            # Fall through to parse it
    
    # STEP 3: Parse the variable name (REQUIRED)
    if not self.match(COBOLTokenType.IDENTIFIER):
        self.error(f"Expected variable name after DEPENDING {'ON' if on_found else ''}")
    
    depending_on = self.current_token().value
    if self.debug:
        print(f"[PARSE_DEPENDING] Variable name: {depending_on}")
    
    self.advance()  # Consume variable name
    return depending_on


def _parse_indexed_by(self):
    """Parse INDEXED BY clause - returns list of index names
    
    TRUTH TABLE: INDEXED BY Clause Patterns
    ┌─────────────────────────────────┬──────────────────────────────────┐
    │ Pattern                         │ Action                           │
    ├─────────────────────────────────┼──────────────────────────────────┤
    │ INDEXED BY IDX1                 │ Single index, return [IDX1]      │
    │ INDEXED IDX1 (no BY)            │ Parse without BY (dialect)       │
    │ INDEXED BY IDX1, IDX2           │ Multiple indices, return list    │
    │ INDEXED BY IDX1, IDX2,          │ Trailing comma (valid COBOL)     │
    │ INDEXED BY IDX1 IDX2            │ No comma (malformed, best effort)│
    │ INDEXED BY IDX1; IDX2           │ Semicolon separator (consume)    │
    └─────────────────────────────────┴──────────────────────────────────┘
    
    TRUTH TABLE: Loop State Machine for Index Name Parsing
    ┌─────────────┬───────────┬──────────────┬──────────────────────────┐
    │ Current     │ Next      │ State After  │ Action                   │
    │ Token       │ Token     │ Advance      │                          │
    ├─────────────┼───────────┼──────────────┼──────────────────────────┤
    │ IDENTIFIER  │ COMMA     │ After COMMA  │ Append, consume both     │
    │ IDENTIFIER  │ SEMICOLON │ After SEMI   │ Append, consume both     │
    │ IDENTIFIER  │ NEWLINE   │ After NL     │ Append, consume ID, skip │
    │ IDENTIFIER  │ OTHER     │ Exit loop    │ Append, exit             │
    │ COMMA       │ IDENTIFIER│ Before ID    │ Skip whitespace          │
    │ SEMICOLON   │ IDENTIFIER│ Before ID    │ Skip whitespace          │
    │ NEWLINE     │ IDENTIFIER│ Before ID    │ Skip whitespace          │
    │ NON-ID      │ *         │ Exit loop    │ Return list              │
    └─────────────┴───────────┴──────────────┴──────────────────────────┘
    
    CRITICAL FIX: BY keyword is OPTIONAL in some COBOL dialects
    Examples:
        INDEXED BY IN1, INDEX1, INDEX2    (standard form)
        INDEXED IDX-1 IDX-2 IDX-3         (without BY - legacy COBOL)
        INDEXED BY IX-A, IX-B,
        INDEXED BY IDX1; IDX2; IDX3
    
    Returns:
        list: Index names (may be empty if malformed)
    """
    index_names = []
    max_iterations = 100  # Safety limit
    iteration = 0
    
    if self.debug:
        print(f"[PARSE_INDEXED_BY] Starting parse")
    
    # STEP 1: Consume INDEXED keyword
    self.advance()  # Already matched INDEXED before calling
    self._skip_whitespace()
    
    # STEP 2: Consume BY keyword if present (OPTIONAL)
    # CRITICAL FIX: BY is optional in some COBOL dialects
    if self.match(COBOLTokenType.BY):
        if self.debug:
            print(f"[PARSE_INDEXED_BY] Found BY keyword")
        self.advance()
        self._skip_whitespace()
    else:
        # No BY keyword - this is valid in legacy COBOL
        # Continue directly to parsing index names
        if self.debug:
            print(f"[PARSE_INDEXED_BY] No BY keyword found - parsing without it (legacy dialect)")
    
    # STEP 3: Parse index names in loop with separator handling
    # ROW 1-4: Parse IDENTIFIER tokens until we hit a non-separator
    while iteration < max_iterations:
        iteration += 1
        
        # STATE CHECK: Are we at an identifier?
        if not self.match(COBOLTokenType.IDENTIFIER):
            # ROW 8: NON-ID token → Exit loop
            if self.debug:
                token_name = self.current_token().type.name if self.current_token() else "EOF"
                print(f"[PARSE_INDEXED_BY] Exit: next token is {token_name}")
            break
        
        # STEP 3A: Capture the index name
        index_name = self.current_token().value
        index_names.append(index_name)
        self.advance()  # Consume identifier
        
        if self.debug:
            print(f"[PARSE_INDEXED_BY]   Found index: {index_name}")
        
        # STEP 3B: Skip whitespace/newlines
        self._skip_whitespace()
        
        # STEP 3C: Check for separator (COMMA or SEMICOLON)
        # ROW 1: COMMA separator → consume and continue
        if self.match(COBOLTokenType.COMMA):
            self.advance()  # Consume comma
            self._skip_whitespace()
            if self.debug:
                print(f"[PARSE_INDEXED_BY]   Consumed COMMA, continuing...")
            continue  # Look for next index name
        
        # ROW 5: SEMICOLON separator → consume and continue
        elif self.match(COBOLTokenType.SEMICOLON):
            self.advance()  # Consume semicolon
            self._skip_whitespace()
            if self.debug:
                print(f"[PARSE_INDEXED_BY]   Consumed SEMICOLON, continuing...")
            continue  # Look for next index name
        
        # ROW 4: No separator → check if next token is also an identifier
        # In legacy COBOL: "INDEXED IDX-1 IDX-2 IDX-3" without separators is valid
        elif self.match(COBOLTokenType.IDENTIFIER):
            # Continue parsing - next token is another index name
            if self.debug:
                print(f"[PARSE_INDEXED_BY]   No separator, but next is identifier - continuing (space-separated)")
            continue
        
        else:
            if self.debug:
                next_token = self.current_token()
                token_info = f"{next_token.type.name}='{next_token.value}'" if next_token else "EOF"
                print(f"[PARSE_INDEXED_BY]   No separator, next={token_info}, done")
            break
    
    # STEP 4: Safety check for infinite loop
    if iteration >= max_iterations:
        print(f"[WARNING] _parse_indexed_by hit iteration limit!")
    
    if self.debug:
        print(f"[PARSE_INDEXED_BY]   Final index_names={index_names}")
    
    return index_names


def _peek_ahead(self, current_level=None):
    """Check what comes next: PIC, PERIOD, VALUE, SIGN, or USAGE clause
    
    🔧 FIXED: Better whitespace/newline handling for multi-line USAGE clauses
    🔧 FIXED: Second identifier detection for group items
    
    Example that was failing:
        02 U2   PICTURE 9 USAGE IS
        DISPLAY VALUE IS 9.
    
    Uses 2-token lookahead to detect if PERIOD is followed by children, and
    to distinguish between group items with USAGE and elementary items with USAGE.
    
    TRUTH TABLE: Item Type Detection After Optional Clauses
    ┌────────────────────┬──────────┬──────────┬─────────────────────────────────┐
    │ Token0             │ Token1   │ Next > ? │ Decision                        │
    ├────────────────────┼──────────┼──────────┼─────────────────────────────────┤
    │ PIC                │ *        │ *        │ Elementary (definitive)         │
    │ VALUE              │ *        │ *        │ Has VALUE clause                │
    │ SIGN               │ *        │ *        │ Has SIGN clause                 │
    │ USAGE ... PIC      │ *        │ *        │ Elementary (has PIC after)      │
    │ USAGE ... PERIOD   │ LEVEL_NO │ YES      │ Group with USAGE + children     │
    │ USAGE ... PERIOD   │ LEVEL_NO │ NO       │ Elementary USAGE (no PIC)       │
    │ PERIOD             │ LEVEL_NO │ YES      │ Group with children             │
    │ PERIOD             │ LEVEL_NO │ NO       │ Standalone item (no children)   │
    │ PERIOD             │ OTHER    │ *        │ Standalone item (no children)   │
    │ IDENTIFIER         │ not key  │ *        │ Group (second identifier)       │
    │ OTHER              │ *        │ *        │ Ambiguous                       │
    └────────────────────┴──────────┴──────────┴─────────────────────────────────┘
    
    Args:
        current_level: Current item's level number (for child detection)
    
    Returns:
        tuple: (next_is_pic, next_is_period, next_is_value, next_is_sign, next_is_usage)
        
        Note: next_is_period is True ONLY if period is followed by children
              next_is_usage is True ONLY for elementary items with USAGE (no children)
    """
    # Helper to skip whitespace in lookahead
    def skip_ws():
        while self.current_token() and self.current_token().type in (
            COBOLTokenType.COMMENT, COBOLTokenType.NEWLINE
        ):
            self.advance()
            
    # CRITICAL FIX: Skip any separators (COMMA, SEMICOLON) before checking clauses
    while self.match(COBOLTokenType.COMMA, COBOLTokenType.SEMICOLON):
        self.advance()
        skip_ws()
    
     # ROW 1: Check for PIC clause (definitive elementary item)
    next_is_pic = self.match(COBOLTokenType.PIC, COBOLTokenType.PICTURE)
    if next_is_pic:
        if self.debug:
            print(f"[PEEK_AHEAD] Found PIC → Elementary item")
        return True, False, False, False, False
    
    # ROW 1.5: ✅ FIXED - Check for USAGE clause with better whitespace handling
    # USAGE can indicate either:
    #   1. Elementary: USAGE COMP PIC 9(4). or USAGE COMP. (no children)
    #   2. Group: USAGE COMP. <level N+1 children>
    next_is_usage = False
    if self.match(COBOLTokenType.USAGE):
        saved_pos = self.pos
        try:
            self.advance()  # Move past USAGE
            skip_ws()  # 🔧 Skip whitespace BEFORE checking for IS
            
            # Skip optional IS
            if self.current_token() and self.current_token().value and \
               self.current_token().value.upper() == 'IS':
                self.advance()
                skip_ws()  # 🔧 Skip whitespace AFTER IS, BEFORE usage type
            
            # 🔧 CRITICAL FIX: Always skip whitespace before checking usage type!
            # This handles multi-line cases like:
            #   USAGE IS
            #   DISPLAY VALUE IS 9.
            skip_ws()
            
            # Skip the usage type (COMP, DISPLAY, COMPUTATIONAL, etc.)
            usage_type_found = False
            if self.match(COBOLTokenType.COMP, COBOLTokenType.COMP_1, COBOLTokenType.COMP_2,
                         COBOLTokenType.COMP_3, COBOLTokenType.COMPUTATIONAL,
                         COBOLTokenType.COMPUTATIONAL_3, COBOLTokenType.BINARY,
                         COBOLTokenType.PACKED_DECIMAL, COBOLTokenType.DISPLAY):
                usage_type_found = True
                self.advance()
                skip_ws()
            elif self.match(COBOLTokenType.IDENTIFIER):
                # Handle COMPUTATIONAL/DISPLAY as identifier
                val = self.current_token().value.upper()
                if 'COMP' in val or 'BINARY' in val or 'DISPLAY' in val:
                    usage_type_found = True
                    self.advance()
                    skip_ws()
            
            # 🔧 Additional safety: if no usage type found, might be malformed
            if not usage_type_found:
                if self.debug:
                    token_name = self.current_token().type.name if self.current_token() else "EOF"
                    print(f"[PEEK_AHEAD] USAGE without type, found {token_name}")
            
            # Now check what follows: PIC, PERIOD, or something else
            if self.match(COBOLTokenType.PIC, COBOLTokenType.PICTURE):
                # USAGE ... PIC → Definitely elementary
                next_is_usage = True
                if self.debug:
                    print(f"[PEEK_AHEAD] Found USAGE followed by PIC → Elementary item")
            elif self.match(COBOLTokenType.PERIOD):
                # USAGE ... PERIOD → Check for children
                if current_level is not None:
                    period_pos = self.pos  # Save PERIOD position
                    self.advance()  # Move past PERIOD
                    skip_ws()
                    
                    if self.match(COBOLTokenType.LEVEL_NUMBER):
                        next_level = int(self.current_token().value)
                        if next_level > current_level:
                            # USAGE ... PERIOD <children> → Group item with usage, NOT elementary
                            if self.debug:
                                print(f"[PEEK_AHEAD] Found USAGE ... PERIOD with L{next_level} > L{current_level} → Group item (not elementary)")
                            # CRITICAL FIX: Return immediately with next_is_period=True
                            self.pos = saved_pos  # Restore to before USAGE
                            return False, True, False, False, False  # Group with children
                        else:
                            # USAGE ... PERIOD (no children) → Elementary without PIC
                            next_is_usage = True
                            if self.debug:
                                print(f"[PEEK_AHEAD] Found USAGE ... PERIOD, L{next_level} <= L{current_level} → Elementary (no children)")
                    else:
                        # USAGE ... PERIOD (not followed by level) → Elementary without PIC
                        next_is_usage = True
                        if self.debug:
                            print(f"[PEEK_AHEAD] Found USAGE ... PERIOD (no level after) → Elementary")
                else:
                    # No current_level to check, conservatively assume elementary
                    next_is_usage = True
                    if self.debug:
                        print(f"[PEEK_AHEAD] Found USAGE ... PERIOD (no current_level) → Assume elementary")
            else:
                # 🔧 FIXED: USAGE not followed by PIC or PERIOD
                # Could be VALUE or other clause - this means it's an elementary item
                # Example: USAGE IS DISPLAY VALUE IS 9.
                #                          ^-- we're here, should see VALUE
                next_is_usage = True
                if self.debug:
                    token_name = self.current_token().type.name if self.current_token() else "EOF"
                    print(f"[PEEK_AHEAD] Found USAGE followed by {token_name} → Elementary with USAGE clause")
        finally:
            # Always restore position
            self.pos = saved_pos
        
        if next_is_usage:
            return False, False, False, False, True
        # If not elementary (group with usage), fall through to check PERIOD
    
    # ROW 2: Check for VALUE clause
    next_is_value = self.match(COBOLTokenType.VALUE)
    if next_is_value:
        if self.debug:
            print(f"[PEEK_AHEAD] Found VALUE clause")
        return False, False, True, False, False
    
    # ROW 3: Check for SIGN or SYNCHRONIZED clause with lookahead for PICTURE
    next_is_sign = False
    if self.match(COBOLTokenType.IDENTIFIER):
        token_val = self.current_token().value.upper()
        
        # Check if this is SIGN, SYNCHRONIZED, or related keywords
        if token_val in ('SIGN', 'LEADING', 'TRAILING', 'SYNCHRONIZED', 'SYNC'):
            saved_pos = self.pos
            
            try:
                # Handle SIGN clause
                if token_val in ('SIGN', 'LEADING', 'TRAILING'):
                    if token_val == 'SIGN':
                        self.advance()  # Consume SIGN
                        skip_ws()
                        
                        # Skip optional IS
                        if self.match(COBOLTokenType.IS):
                            self.advance()
                            skip_ws()
                        elif self.match(COBOLTokenType.IDENTIFIER) and \
                            self.current_token().value.upper() == 'IS':
                            self.advance()
                            skip_ws()
                        
                        # Must have LEADING or TRAILING
                        if self.match(COBOLTokenType.IDENTIFIER):
                            pos_val = self.current_token().value.upper()
                            if pos_val in ('LEADING', 'TRAILING'):
                                self.advance()
                                skip_ws()
                    else:
                        # Shorthand: LEADING or TRAILING without SIGN
                        self.advance()  # Consume LEADING/TRAILING
                        skip_ws()
                    
                    # Skip optional SEPARATE [CHARACTER]
                    if self.match(COBOLTokenType.IDENTIFIER) and \
                    self.current_token().value.upper() == 'SEPARATE':
                        self.advance()
                        skip_ws()
                        
                        if self.match(COBOLTokenType.IDENTIFIER) and \
                        self.current_token().value.upper() == 'CHARACTER':
                            self.advance()
                            skip_ws()
                
                # Handle SYNCHRONIZED/SYNC clause
                elif token_val in ('SYNCHRONIZED', 'SYNC'):
                    self.advance()  # Consume SYNCHRONIZED/SYNC
                    skip_ws()
                    
                    # Check for optional LEFT/RIGHT
                    if self.match(COBOLTokenType.IDENTIFIER):
                        pos_val = self.current_token().value.upper()
                        if pos_val in ('LEFT', 'RIGHT'):
                            self.advance()
                            skip_ws()
                
                # Check what follows the SIGN or SYNC clause
                if self.match(COBOLTokenType.PIC, COBOLTokenType.PICTURE):
                    # SIGN/SYNC ... PICTURE → Elementary item
                    if self.debug:
                        clause_type = "SIGN" if token_val in ('SIGN', 'LEADING', 'TRAILING') else "SYNC"
                        print(f"[PEEK_AHEAD] {clause_type}+PIC → Elementary")
                    return True, False, False, False, False
                else:
                    # SIGN/SYNC without PIC → Group item
                    next_is_sign = True
                    if self.debug:
                        clause_type = "SIGN" if token_val in ('SIGN', 'LEADING', 'TRAILING') else "SYNC"
                        print(f"[PEEK_AHEAD] {clause_type} only → Group")
                    return False, False, False, True, False
            finally:
                # Restore position
                self.pos = saved_pos
    
    # ROWS 4-6: Check for PERIOD with 2-token lookahead
    next_is_period = False
    
    if self.match(COBOLTokenType.PERIOD):
        if self.debug:
            print(f"[PEEK_AHEAD] Found PERIOD, checking for children...")
        
        # If we don't know current level, conservatively assume no children
        if current_level is None:
            if self.debug:
                print(f"[PEEK_AHEAD] No current_level → assume no children")
            return False, False, False, False, False
        
        # Save position to restore after peeking
        saved_pos = self.pos
        
        try:
            # Advance past PERIOD to peek ahead
            self.advance()
            
            # Skip comments/newlines to find next meaningful token
            while self.current_token() and self.current_token().type in (
                COBOLTokenType.COMMENT,
                COBOLTokenType.NEWLINE
            ):
                self.advance()
            
            # Check if next token is a LEVEL_NUMBER
            if self.match(COBOLTokenType.LEVEL_NUMBER):
                next_level = int(self.current_token().value)
                
                # ROW 4: Next level > current → Has children (group item)
                if next_level > current_level:
                    next_is_period = True
                    if self.debug:
                        print(f"[PEEK_AHEAD] L{next_level} > L{current_level} → Group with children")
                # ROW 5: Next level <= current → No children (standalone)
                else:
                    next_is_period = False
                    if self.debug:
                        print(f"[PEEK_AHEAD] L{next_level} <= L{current_level} → Standalone (no children)")
            else:
                # ROW 6: PERIOD not followed by LEVEL_NUMBER → No children
                if self.debug:
                    token_name = self.current_token().type.name if self.current_token() else "EOF"
                    print(f"[PEEK_AHEAD] After PERIOD: {token_name} → Standalone (no children)")
                next_is_period = False
        
        finally:
            # Always restore position (don't consume PERIOD yet)
            self.pos = saved_pos
        
        return False, next_is_period, False, False, False
    
    # ═══════════════════════════════════════════════════════════════
    # 🔥 NEW ROW: Check for second IDENTIFIER → Group item pattern
    # This fixes 4 of the 6 failures:
    #   - 05 PIC X (PIC is name, X is second identifier)
    #   - 02 ENTRY-310 GRP (GRP is second identifier)  
    #   - 02 1ST-ENTRY FIELD-2 (FIELD-2 is second identifier)
    #   - 03 VKEY XXXXX086 (XXXXX086 is second identifier)
    # ═══════════════════════════════════════════════════════════════
    if self.match(COBOLTokenType.IDENTIFIER):
        token_val = self.current_token().value.upper()
        
        # All known clause keywords that should NOT trigger this
        clause_keywords = {
            'REDEFINES', 'OCCURS', 'DEPENDING', 'ON', 'INDEXED', 'BY',
            'ASCENDING', 'DESCENDING', 'KEY', 'IS', 'EXTERNAL', 'GLOBAL',
            'JUSTIFIED', 'BLANK', 'WHEN', 'ZERO', 'JUST',
            'CHARACTER', 'LEFT', 'RIGHT', 'SEPARATE'
        }
        
        # If NOT a clause keyword, it's a second identifier → Group item
        if token_val not in clause_keywords:
            if self.debug:
                print(f"[PEEK_AHEAD] Second IDENTIFIER '{token_val}' → Group item")
            return False, True, False, False, False  # next_is_period=True for group
    
    # ROW 7: No recognized pattern found
    if self.debug:
        token_info = f"{self.current_token().type.name}='{self.current_token().value}'" if self.current_token() else "EOF"
        print(f"[PEEK_AHEAD] Ambiguous token: {token_info}")
    
    return False, False, False, False, False


def _parse_group_item(self, level, name, occurs_info, redefines_target, 
                       is_external, is_global, token):
    """Parse a group item (no PIC clause)
    
    TRUTH TABLE: Group Item Clauses
    ┌───────────────────────────────┬────────────────────────────────┐
    │ Clause                        │ Action                         │
    ├───────────────────────────────┼────────────────────────────────┤
    │ SIGN → LEADING/TRAILING       │ Parse SIGN clause              │
    │ VALUE → literal               │ Parse VALUE clause             │
    │ USAGE → type                  │ Parse USAGE clause             │
    │ COMMA/SEMICOLON               │ Skip (separator)               │
    │ PERIOD                        │ Done - parse children          │
    └───────────────────────────────┴────────────────────────────────┘
    """
    if self.debug:
        print(f"[PARSE_VAR] Detected GROUP ITEM")
    
    # Parse optional clauses in a loop
    value = None
    sign_info = None
    usage_type = None
    
    max_iterations = 10
    iteration = 0
    
    while iteration < max_iterations and not self.match(COBOLTokenType.PERIOD):
        iteration += 1
        
        # Skip optional separators
        if self.match(COBOLTokenType.COMMA, COBOLTokenType.SEMICOLON):
            self.advance()
            continue
        
        # SIGN clause
        if self.match(COBOLTokenType.IDENTIFIER) and \
           self.current_token().value.upper() in ('SIGN', 'LEADING', 'TRAILING'):
            if sign_info is None:
                sign_info = self._parse_sign_clause()
            continue
        
        # VALUE clause
        if self.match(COBOLTokenType.VALUE):
            if value is None:
                self.advance()  # Consume VALUE
                
                # Optional IS keyword
                if self.current_token() and self.current_token().value and \
                   self.current_token().value.upper() == 'IS':
                    self.advance()
                
                # Parse the actual value
                if self.match(COBOLTokenType.STRING_LITERAL):
                    value = self.current_token().value
                    self.advance()
                elif self.match(COBOLTokenType.IDENTIFIER):
                    value = self.current_token().value
                    self.advance()
                else:
                    self.error(f"Expected value after VALUE keyword for group item '{name}'")
            continue
        
        # USAGE clause (rare on group items but valid)
        if self.match(COBOLTokenType.USAGE) or \
           self.match(COBOLTokenType.COMP, COBOLTokenType.COMP_1, COBOLTokenType.COMP_2,
                      COBOLTokenType.COMP_3, COBOLTokenType.COMPUTATIONAL,
                      COBOLTokenType.COMPUTATIONAL_3, COBOLTokenType.BINARY,
                      COBOLTokenType.PACKED_DECIMAL, COBOLTokenType.DISPLAY):
            if usage_type is None:
                usage_type = self._parse_usage_type_clause()
            continue
        
        # If we get here, we hit something unexpected - break and let PERIOD check handle it
        break
    
    # Consume the terminating PERIOD
    self.consume(COBOLTokenType.PERIOD)
    
    # Parse child declarations
    children = []
    while self.match(COBOLTokenType.LEVEL_NUMBER):
        child_level = int(self.current_token().value)
        if self.debug:
            print(f"👶 [CHILDREN] Parent L{level}:{name} checking child L{child_level}")
        
        if child_level <= level:
            if self.debug:
                print(f"👶 [CHILDREN] Breaking: {child_level} <= {level}")
            break
        
        if self.debug:
            print(f"👶 [CHILDREN] Parsing child...")
        child = self.parse_variable_decl()
        if self.debug:
            print(f"👶 [CHILDREN] Parsed: L{child.level}:{child.name}, has {len(child.children) if child.children else 0} children")
        children.append(child)
    
    return COBOLVariableDecl(
        level=level, name=name, pic_clause=None, value=value,
        occurs_count=occurs_info['occurs_count'],
        decimal_places=None, usage_type=usage_type,
        is_signed=False, children=children,
        redefines_target=redefines_target,
        occurs_min=occurs_info['occurs_min'],
        occurs_max=occurs_info['occurs_max'],
        depending_on=occurs_info['depending_on'],
        is_external=is_external,
        is_global=is_global,
        index_names=occurs_info['index_names'],
        sign_info=sign_info,
        line=token.line, column=token.column
    )


def _parse_sign_clause(self):
    """Parse optional SIGN clause with shorthand support
    
    TRUTH TABLE: SIGN Clause Patterns
    ┌─────────────────────────────────┬──────────────────────────┐
    │ Pattern                         │ Valid?                   │
    ├─────────────────────────────────┼──────────────────────────┤
    │ SIGN LEADING                    │ YES                      │
    │ SIGN IS LEADING                 │ YES (optional IS)        │
    │ SIGN TRAILING                   │ YES                      │
    │ SIGN IS TRAILING                │ YES (optional IS)        │
    │ SIGN LEADING SEPARATE CHARACTER │ YES                      │
    │ SIGN IS LEADING SEPARATE        │ YES                      │
    │ LEADING                         │ YES (shorthand - no SIGN)│
    │ TRAILING                        │ YES (shorthand - no SIGN)│
    │ LEADING SEPARATE                │ YES (shorthand)          │
    │ TRAILING SEPARATE CHARACTER     │ YES (shorthand)          │
    └─────────────────────────────────┴──────────────────────────┘
    
    Examples:
        1. 01 VAR SIGN IS LEADING SEPARATE.         (full form)
        2. 01 VAR LEADING SEPARATE.                 (shorthand)
        3. 01 GROUP-NAME TRAILING.                  (shorthand on group)
        4. 05 FIELD PIC S999 SIGN TRAILING.         (full form on field)
    
    ✅ FIX: Support shorthand form where SIGN IS is completely omitted
    """
    self._skip_whitespace()
    
    # Check current token
    if not self.match(COBOLTokenType.IDENTIFIER):
        return None
    
    token_value = self.current_token().value.upper()
    
    # Check if this is a SIGN clause at all
    if token_value not in ['SIGN', 'LEADING', 'TRAILING']:
        return None
    
    has_sign_keyword = False
    
    if token_value == 'SIGN':
        # Full form: SIGN [IS] LEADING/TRAILING
        has_sign_keyword = True
        self.advance()  # Consume SIGN
        self._skip_whitespace()
        
        # Optional IS keyword
        if self.match(COBOLTokenType.IS):
            self.advance()
            self._skip_whitespace()
        elif self.match(COBOLTokenType.IDENTIFIER) and self.current_token().value.upper() == 'IS':
            self.advance()
            self._skip_whitespace()
        
        # Now must have LEADING or TRAILING
        if not self.match(COBOLTokenType.IDENTIFIER):
            self.error("Expected LEADING or TRAILING after SIGN")
        
        token_value = self.current_token().value.upper()
    
    # Validate position
    if token_value not in ['LEADING', 'TRAILING']:
        self.error(f"Expected LEADING or TRAILING, got {token_value}")
    
    position = token_value
    self.advance()  # Consume LEADING/TRAILING
    self._skip_whitespace()
    
    # Optional SEPARATE [CHARACTER] clause
    is_separate = False
    if self.match(COBOLTokenType.IDENTIFIER):
        if self.current_token().value.upper() == 'SEPARATE':
            is_separate = True
            self.advance()
            self._skip_whitespace()
            
            # Optional CHARACTER keyword after SEPARATE
            if self.match(COBOLTokenType.IDENTIFIER):
                if self.current_token().value.upper() == 'CHARACTER':
                    self.advance()
                    self._skip_whitespace()
    
    if self.debug:
        print(f"[PARSE_SIGN] position={position}, separate={is_separate}, had_sign_keyword={has_sign_keyword}")
    
    return {
        'position': position,
        'is_separate': is_separate
    }



def _parse_elementary_item(self, level, name, occurs_info, redefines_target,
                            is_external, is_global, token):
    """Parse elementary item with truth table approach - FIXED VERSION    
    This pattern has SYNCHRONIZED before PIC, which was not handled!
    
    TRUTH TABLE: Elementary Item Clause Order
    ┌──────────────────┬──────────────┬──────────────────────┐
    │ Clause           │ Required?    │ Can Appear Multiple? │
    ├──────────────────┼──────────────┼──────────────────────┤
    │ PIC/PICTURE      │ YES          │ NO                   │
    │ SIGN             │ NO           │ NO                   │
    │ SYNCHRONIZED     │ NO           │ NO                   │
    │ USAGE            │ NO           │ NO                   │
    │ OCCURS           │ NO           │ NO (but check 2x)    │
    │ VALUE            │ NO           │ NO                   │
    │ JUSTIFIED/JUST   │ NO           │ NO                   │
    │ BLANK WHEN ZERO  │ NO           │ NO                   │
    │ COMMA/SEMICOLON  │ NO (skip)    │ YES (separators)     │
    │ PERIOD           │ YES          │ NO                   │
    │ Level 88 items   │ NO           │ YES (after period)   │
    └──────────────────┴──────────────┴──────────────────────┘
    
    Examples:
    1. 01 VAR-1 PIC X(10) VALUE "ABC".
    2. 01 VAR-2 PIC 9(5) SIGN LEADING USAGE COMP-3.
    3. 77 DATA-G SYNCHRONIZED RIGHT PICTURE X(5) VALUE "VWXYZ".
    4. 01 VAR-4 PIC 9(3) BLANK WHEN ZERO.
    5. 77 WRK1 PIC S999; COMPUTATIONAL, VALUE ZERO.
    """
    
    # ══════════════════════════════════════════════════════════════
    # INITIALIZATION: Track parsing state and metrics
    # ══════════════════════════════════════════════════════════════
    parse_start_pos = self.pos
    seen_clauses = set()  # Track which clauses we've encountered
    warnings = []  # Collect warnings for this item
    
    def check_duplicate(clause_name):
        """Helper to detect duplicate clauses"""
        if clause_name in seen_clauses:
            msg = f"Duplicate {clause_name} clause ignored at line {self.current_token().line}"
            warnings.append(msg)
            if self.debug:
                print(f"[WARNING] {msg}")
            return True
        seen_clauses.add(clause_name)
        return False
    
    # ══════════════════════════════════════════════════════════════
    # STEP 0: ✅ FIXED - Parse optional SIGN or SYNCHRONIZED BEFORE PIC
    # In COBOL-85, both SIGN and SYNCHRONIZED can appear before PICTURE
    # Examples:
    #   77 WRK-DS TRAILING SEPARATE PICTURE S9(12).
    #   77 DATA-G SYNCHRONIZED RIGHT PICTURE X(5).
    # ══════════════════════════════════════════════════════════════
    sign_info = None
    is_synchronized = False
    sync_direction = None
    
    # Check if SIGN or SYNCHRONIZED clause appears before PIC
    if self.match(COBOLTokenType.IDENTIFIER):
        token_val = self.current_token().value.upper()
        
        # Sub-case A: SIGN clause before PIC
        if token_val in ('SIGN', 'LEADING', 'TRAILING'):
            if self.debug:
                print(f"[PARSE_ELEM] Found SIGN clause before PIC: {token_val}")
            
            if not check_duplicate('SIGN'):
                sign_info = self._parse_sign_clause()
                if self.debug:
                    print(f"[PARSE_ELEM] Parsed SIGN: {sign_info}")
            else:
                # Consume duplicate but ignore
                self._parse_sign_clause()
        
        # Sub-case B: SYNCHRONIZED clause before PIC (🔧 NEW FIX!)
        elif token_val in ('SYNCHRONIZED', 'SYNC'):
            if self.debug:
                print(f"[PARSE_ELEM] Found SYNCHRONIZED clause before PIC: {token_val}")
            
            if not check_duplicate('SYNCHRONIZED'):
                self.advance()  # Consume SYNCHRONIZED/SYNC
                self._skip_whitespace()
                is_synchronized = True
                
                # Optional: LEFT or RIGHT
                if self.match(COBOLTokenType.IDENTIFIER):
                    sync_dir = self.current_token().value.upper()
                    if sync_dir in ('LEFT', 'RIGHT'):
                        sync_direction = sync_dir
                        self.advance()
                        self._skip_whitespace()
                        if self.debug:
                            print(f"[PARSE_ELEM] SYNCHRONIZED {sync_direction}")
            else:
                # Consume duplicate
                self.advance()
                self._skip_whitespace()
                if self.match(COBOLTokenType.IDENTIFIER):
                    sync_dir = self.current_token().value.upper()
                    if sync_dir in ('LEFT', 'RIGHT'):
                        self.advance()
                        self._skip_whitespace()
                        
                           
    
    # ══════════════════════════════════════════════════════════════
# STEP 1: Parse PIC clause (can come after VALUE in COBOL-85)
# ══════════════════════════════════════════════════════════════
    
    # Handle VALUE before PICTURE
    value = None
    if self.match(COBOLTokenType.VALUE):
        if self.debug:
            print(f"[PARSE_ELEM] Found VALUE before PIC")
        value = self._parse_value_clause()
        seen_clauses.add('VALUE')
        self._skip_whitespace()

    # Now require PICTURE
    if not self.match(COBOLTokenType.PIC, COBOLTokenType.PICTURE):
        self.error(
            f"Expected PIC clause for elementary item '{name}' at level {level}. "
            f"Elementary items must have a PICTURE clause. "
            f"Current token: {self.current_token().type.name} = '{self.current_token().value}'"
        )

    if self.debug:
        print(f"[PARSE_ELEM] Parsing PIC clause for '{name}'")

    # Parse PIC clause (required)
    self.advance()
    if self.match(COBOLTokenType.IS) or \
    (self.match(COBOLTokenType.IDENTIFIER) and self.current_token().value.upper() == 'IS'):
        self.advance()

    pic_clause = self.parse_pic_clause()
    decimal_places = self._calculate_decimal_places(pic_clause)
    is_signed = self._check_signed(pic_clause)

    # Determine PIC type for validation
    pic_upper = pic_clause.upper() if pic_clause else ""
    is_numeric = any(c in pic_upper for c in '9VPS')
    is_alphanumeric = 'X' in pic_upper
    is_alphabetic = 'A' in pic_upper

    if self.debug:
        pic_type = "numeric" if is_numeric else "alphanumeric" if is_alphanumeric else "alphabetic"
        print(f"[PARSE_ELEM] PIC = '{pic_clause}' (type: {pic_type}, signed: {is_signed})")

    seen_clauses.add('PIC')
        
    # ══════════════════════════════════════════════════════════════
    # STEP 2: Parse optional clauses using truth table loop
    # ══════════════════════════════════════════════════════════════
    
    usage_type = None
    is_justified = False
    is_blank_when_zero = False
    
    # Truth table loop: parse clauses until PERIOD
    max_iterations = 30  # Increased from 20 for safety
    iteration = 0
    
    while iteration < max_iterations:
        iteration += 1
        self._skip_whitespace()
        
        # ══════════════════════════════════════════════════════════════
        # TRUTH TABLE ROW 0: Optional separators (COMMA or SEMICOLON)
        # COBOL X3.23-1985 Section 2.1.6: Comma and semicolon are
        # optional separators and are interchangeable
        # Example: 77 WRK1 PIC S999; COMPUTATIONAL, VALUE ZERO.
        #                          ^ skip    ^ skip
        # ══════════════════════════════════════════════════════════════
        if self.match(COBOLTokenType.COMMA, COBOLTokenType.SEMICOLON):
            if self.debug:
                separator = self.current_token().type.name
                print(f"[PARSE_ELEM] Skipping {separator} separator")
            self.advance()  # Skip the separator
            continue  # Check next token
        
        # Check what clause comes next
        current = self.current_token()
        
        # TRUTH TABLE ROW 1: PERIOD → done with clauses
        if self.match(COBOLTokenType.PERIOD):
            break
        
        # TRUTH TABLE ROW 2: SIGN clause (with or without SIGN keyword)
        # Handles: "SIGN IS TRAILING", "TRAILING SEPARATE", "USAGE IS DISPLAY TRAILING"
        if self.match(COBOLTokenType.IDENTIFIER):
            token_val = current.value.upper()
            if token_val in ('SIGN', 'LEADING', 'TRAILING'):
                if not check_duplicate('SIGN'):
                    sign_info = self._parse_sign_clause()
                else:
                    # Consume duplicate but ignore
                    self._parse_sign_clause()
                continue
        
        # TRUTH TABLE ROW 3: USAGE clause
        if self.match(COBOLTokenType.USAGE) or \
           self.match(COBOLTokenType.COMP, COBOLTokenType.COMP_1, COBOLTokenType.COMP_2,
                      COBOLTokenType.COMP_3, COBOLTokenType.COMPUTATIONAL,
                      COBOLTokenType.COMPUTATIONAL_3, COBOLTokenType.BINARY,
                      COBOLTokenType.PACKED_DECIMAL, COBOLTokenType.DISPLAY):
            if not check_duplicate('USAGE'):
                usage_type = self._parse_usage_type_clause()
            else:
                # Consume duplicate but ignore
                self._parse_usage_type_clause()
            continue
        
        # TRUTH TABLE ROW 4: OCCURS clause (check if not already parsed)
        if self.match(COBOLTokenType.OCCURS):
            if occurs_info['occurs_count'] is None:
                if not check_duplicate('OCCURS'):
                    occurs_info_after_pic = self._parse_occurs_clause()
                    # Merge occurs info
                    for key in occurs_info_after_pic:
                        if occurs_info_after_pic[key] is not None:
                            occurs_info[key] = occurs_info_after_pic[key]
                else:
                    # Consume duplicate but ignore
                    self._parse_occurs_clause()
            else:
                # OCCURS was before PIC, check for duplicate
                check_duplicate('OCCURS')
                self._parse_occurs_clause()
            continue
        
        # TRUTH TABLE ROW 5: VALUE clause (skip if already parsed in STEP 1)
        if self.match(COBOLTokenType.VALUE):
            if 'VALUE' not in seen_clauses:  # Only parse if not already done
                if not check_duplicate('VALUE'):
                    value = self._parse_value_clause()
                else:
                    # Consume duplicate but ignore
                    self._parse_value_clause()
            else:
                # Already parsed, this is a duplicate
                if self.debug:
                    print(f"[PARSE_ELEM] VALUE already parsed, skipping duplicate")
                check_duplicate('VALUE')
                self._parse_value_clause()
            continue
        
        # TRUTH TABLE ROW 6: JUSTIFIED/JUST, BLANK WHEN ZERO, SYNCHRONIZED
        if self.match(COBOLTokenType.IDENTIFIER):
            id_val = current.value.upper()
            
            # Sub-case A: JUSTIFIED or JUST
            if id_val in ['JUSTIFIED', 'JUST']:
                if not check_duplicate('JUSTIFIED'):
                    self.advance()  # Consume JUSTIFIED/JUST
                    self._skip_whitespace()
                    is_justified = True
                    # Optional: RIGHT keyword (JUSTIFIED is always RIGHT, but keyword is optional)
                    if self.match(COBOLTokenType.IDENTIFIER):
                        if self.current_token().value.upper() == 'RIGHT':
                            self.advance()
                            self._skip_whitespace()
                    if self.debug:
                        print(f"[PARSE_ELEM] Parsed JUSTIFIED clause")
                else:
                    # Consume duplicate
                    self.advance()
                    self._skip_whitespace()
                    if self.match(COBOLTokenType.IDENTIFIER):
                        if self.current_token().value.upper() == 'RIGHT':
                            self.advance()
                            self._skip_whitespace()
                continue
            
            # Sub-case B: BLANK [WHEN] ZERO
            elif id_val == 'BLANK':
                if not check_duplicate('BLANK_WHEN_ZERO'):
                    self.advance()  # Consume BLANK
                    self._skip_whitespace()
                    
                    # ✅ FIX: WHEN keyword is OPTIONAL (COBOL-74 compatibility)
                    # TRUTH TABLE: BLANK Clause Variants
                    # ┌─────────────────────┬──────────────┐
                    # │ Pattern             │ Valid?       │
                    # ├─────────────────────┼──────────────┤
                    # │ BLANK WHEN ZERO     │ YES (COBOL-85)│
                    # │ BLANK ZERO          │ YES (COBOL-74)│
                    # │ BLANK WHEN ZEROS    │ YES          │
                    # │ BLANK ZEROS         │ YES          │
                    # └─────────────────────┴──────────────┘
                    
                    when_found = False
                    if self.match(COBOLTokenType.WHEN):
                        # WHEN as token type
                        when_found = True
                        self.advance()
                        self._skip_whitespace()
                    elif self.match(COBOLTokenType.IDENTIFIER):
                        if self.current_token().value.upper() == 'WHEN':
                            # WHEN as identifier
                            when_found = True
                            self.advance()
                            self._skip_whitespace()
                    
                    # WHEN is optional - if not found, that's OK
                    # Just proceed to parse ZERO
                    
                    # Expect ZERO/ZEROS/ZEROES
                    zero_found = False
                    if self.match(COBOLTokenType.IDENTIFIER):
                        zero_val = self.current_token().value.upper()
                        if zero_val in ['ZERO', 'ZEROS', 'ZEROES']:
                            self.advance()
                            self._skip_whitespace()
                            is_blank_when_zero = True
                            zero_found = True
                            if self.debug:
                                when_msg = " WHEN" if when_found else ""
                                print(f"[PARSE_ELEM] Parsed BLANK{when_msg} {zero_val} clause")
                    elif self.match(COBOLTokenType.ZERO):
                        # ZERO as a token type
                        self.advance()
                        self._skip_whitespace()
                        is_blank_when_zero = True
                        zero_found = True
                        if self.debug:
                            when_msg = " WHEN" if when_found else ""
                            print(f"[PARSE_ELEM] Parsed BLANK{when_msg} ZERO clause")
                    
                    if not zero_found:
                        # ✅ ERROR: Missing ZERO after BLANK [WHEN]
                        when_msg = " WHEN" if when_found else ""
                        self.error(
                            f"Expected ZERO/ZEROS/ZEROES after BLANK{when_msg} at line {current.line}. "
                            f"Syntax: BLANK [WHEN] ZERO"
                        )
                else:
                    # Consume duplicate
                    self.advance()
                    self._skip_whitespace()
                    if self.match(COBOLTokenType.WHEN) or \
                       (self.match(COBOLTokenType.IDENTIFIER) and 
                        self.current_token().value.upper() == 'WHEN'):
                        self.advance()
                        self._skip_whitespace()
                    if self.match(COBOLTokenType.IDENTIFIER):
                        zero_val = self.current_token().value.upper()
                        if zero_val in ['ZERO', 'ZEROS', 'ZEROES']:
                            self.advance()
                            self._skip_whitespace()
                    elif self.match(COBOLTokenType.ZERO):
                        self.advance()
                        self._skip_whitespace()
                continue
            
            # Sub-case C: SYNCHRONIZED/SYNC (🔧 FIXED - check if already parsed)
            elif id_val in ['SYNCHRONIZED', 'SYNC']:
                if is_synchronized:
                    # Already parsed in STEP 0, this is a duplicate
                    if self.debug:
                        print(f"[PARSE_ELEM] SYNCHRONIZED already parsed in STEP 0, skipping duplicate")
                    check_duplicate('SYNCHRONIZED')
                    self.advance()  # Consume it
                    self._skip_whitespace()
                    if self.match(COBOLTokenType.IDENTIFIER):
                        sync_dir = self.current_token().value.upper()
                        if sync_dir in ['LEFT', 'RIGHT']:
                            self.advance()
                            self._skip_whitespace()
                else:
                    # First occurrence
                    if not check_duplicate('SYNCHRONIZED'):
                        self.advance()  # Consume SYNCHRONIZED/SYNC
                        self._skip_whitespace()
                        is_synchronized = True
                        # Optional: LEFT or RIGHT
                        if self.match(COBOLTokenType.IDENTIFIER):
                            sync_dir = self.current_token().value.upper()
                            if sync_dir in ['LEFT', 'RIGHT']:
                                sync_direction = sync_dir
                                self.advance()
                                self._skip_whitespace()
                        if self.debug:
                            sync_msg = f"SYNCHRONIZED {sync_direction}" if sync_direction else "SYNCHRONIZED"
                            print(f"[PARSE_ELEM] Parsed {sync_msg} clause")
                    else:
                        # Consume duplicate
                        self.advance()
                        self._skip_whitespace()
                        if self.match(COBOLTokenType.IDENTIFIER):
                            sync_dir = self.current_token().value.upper()
                            if sync_dir in ['LEFT', 'RIGHT']:
                                self.advance()
                                self._skip_whitespace()
                continue
            
            
                 # Sub-case D: Handle unexpected special characters (< > =)
            # Some old COBOL uses < as less-than indicator in data definition
            elif current.type in (COBOLTokenType.LT_SIGN, COBOLTokenType.GT_SIGN, 
                                 COBOLTokenType.EQ_SIGN):
                if self.debug:
                    print(f"[PARSE_ELEM] ⚠ Unexpected {current.type.name} in data item '{name}'")
                    print(f"[PARSE_ELEM]   Likely obsolete COBOL syntax, skipping")
                
                # Skip the operator and any following token
                self.advance()
                self._skip_whitespace()
                
                # If followed by identifier or number, skip that too
                if self.match(COBOLTokenType.IDENTIFIER, COBOLTokenType.NUMBER_LITERAL):
                    self.advance()
                    self._skip_whitespace()
                
                continue
        
        # TRUTH TABLE ROW 7: Unknown token → error or break
        # ✅ HARDENING: Better error messages for unknown tokens
        if not self.match(COBOLTokenType.EOF):
            # Give helpful error message
            token_info = f"{current.type.name}"
            if hasattr(current, 'value'):
                token_info += f" '{current.value}'"
            
            msg = (
                f"Unexpected token {token_info} at line {current.line} while parsing "
                f"elementary item '{name}'. Expected one of: SIGN, USAGE, OCCURS, VALUE, "
                f"JUSTIFIED, BLANK, SYNCHRONIZED, or PERIOD"
            )
            
            if self.debug:
                print(f"[ERROR] {msg}")
            
            # Try to recover by looking for PERIOD
            recovery_attempts = 0
            max_recovery = 10
            while recovery_attempts < max_recovery and \
                  not self.match(COBOLTokenType.PERIOD, COBOLTokenType.LEVEL_NUMBER, 
                                COBOLTokenType.EOF):
                if self.debug:
                    print(f"[RECOVERY] Skipping token: {self.current_token().type.name}")
                self.advance()
                recovery_attempts += 1
            
            if self.match(COBOLTokenType.PERIOD):
                warnings.append(f"Recovered from error by finding PERIOD at line {self.current_token().line}")
                break
            else:
                self.error(msg)
        
        break
    
    # ✅ HARDENING: Check for iteration limit
    if iteration >= max_iterations:
        msg = (
            f"Elementary item '{name}' parsing exceeded {max_iterations} iterations "
            f"at line {token.line}. Possible malformed clause or missing PERIOD."
        )
        warnings.append(msg)
        if self.debug:
            print(f"[WARNING] {msg}")
        
        # Try to recover
        recovery_attempts = 0
        while recovery_attempts < 20 and \
              not self.match(COBOLTokenType.PERIOD, COBOLTokenType.LEVEL_NUMBER, COBOLTokenType.EOF):
            self.advance()
            recovery_attempts += 1
        
        if not self.match(COBOLTokenType.PERIOD):
            self.error(msg)
    
    # ══════════════════════════════════════════════════════════════
    # STEP 2.5: ✅ HARDENING - Validate clause combinations
    # ══════════════════════════════════════════════════════════════
    
    # JUSTIFIED only valid for alphanumeric (PIC X or A)
    if is_justified:
        if not (is_alphanumeric or is_alphabetic):
            msg = (
                f"JUSTIFIED clause on '{name}' is only valid for alphanumeric (PIC X) "
                f"or alphabetic (PIC A) items. Found PIC: {pic_clause}"
            )
            warnings.append(msg)
            if self.debug:
                print(f"[WARNING] {msg}")
    
    # BLANK WHEN ZERO only valid for numeric (PIC 9)
    if is_blank_when_zero:
        if not is_numeric:
            msg = (
                f"BLANK WHEN ZERO on '{name}' is only valid for numeric items (PIC 9). "
                f"Found PIC: {pic_clause}"
            )
            warnings.append(msg)
            if self.debug:
                print(f"[WARNING] {msg}")
    
    # SIGN clause should have signed PIC
    if sign_info and not is_signed:
        msg = (
            f"SIGN clause on '{name}' but PIC clause is unsigned. "
            f"Expected 'S' in PIC: {pic_clause}"
        )
        warnings.append(msg)
        if self.debug:
            print(f"[WARNING] {msg}")
    
    # ══════════════════════════════════════════════════════════════
    # STEP 3: Consume terminating PERIOD
    # ══════════════════════════════════════════════════════════════
    if not self.match(COBOLTokenType.PERIOD):
        self.error(
            f"Expected PERIOD to terminate elementary item '{name}' at line {self.current_token().line}. "
            f"Found: {self.current_token().type.name}"
        )
    
    self.consume(COBOLTokenType.PERIOD)
    
    # ══════════════════════════════════════════════════════════════
    # STEP 4: Parse level 88 condition name children (after PERIOD)
    # ══════════════════════════════════════════════════════════════
    children = []
    child_count = 0
    while self.match(COBOLTokenType.LEVEL_NUMBER):
        child_level = int(self.current_token().value)
        if child_level != 88:
            break
        child = self.parse_variable_decl()
        children.append(child)
        child_count += 1
    
    # ══════════════════════════════════════════════════════════════
    # STEP 5: ✅ ENHANCEMENT - Metrics and summary
    # ══════════════════════════════════════════════════════════════
    tokens_consumed = self.pos - parse_start_pos
    
    if self.debug:
        clause_summary = []
        if sign_info: clause_summary.append(f"SIGN={sign_info['position']}")
        if usage_type: clause_summary.append(f"USAGE={usage_type}")
        if value is not None: clause_summary.append(f"VALUE={repr(value)[:20]}")
        if is_justified: clause_summary.append("JUSTIFIED")
        if is_blank_when_zero: clause_summary.append("BLANK-WHEN-ZERO")
        if is_synchronized: 
            sync_msg = f"SYNC-{sync_direction}" if sync_direction else "SYNC"
            clause_summary.append(sync_msg)
        if occurs_info['occurs_count']: clause_summary.append(f"OCCURS={occurs_info['occurs_count']}")
        
        summary_str = ", ".join(clause_summary) if clause_summary else "(basic)"
        print(f"[PARSE_ELEM] ✓ Complete: {name}")
        print(f"              PIC={pic_clause}, {summary_str}")
        print(f"              Tokens={tokens_consumed}, Iterations={iteration}, Children={child_count}")
        
        if warnings:
            print(f"              Warnings: {len(warnings)}")
            for w in warnings:
                print(f"                - {w}")
    
    # ✅ FIXED: COBOLVariableDecl doesn't have is_synchronized field yet
    # Store SYNC info in a dict like sign_info for now
    sync_info = None
    if is_synchronized:
        sync_info = {
            'direction': sync_direction  # 'LEFT', 'RIGHT', or None
        }
    
    return COBOLVariableDecl(
        level=level, name=name, pic_clause=pic_clause, value=value,
        occurs_count=occurs_info['occurs_count'],
        decimal_places=decimal_places,
        usage_type=usage_type,
        is_signed=is_signed,
        children=children,
        occurs_min=occurs_info['occurs_min'],
        occurs_max=occurs_info['occurs_max'],
        depending_on=occurs_info['depending_on'],
        is_external=is_external,
        is_global=is_global,
        redefines_target=redefines_target,
        index_names=occurs_info['index_names'],
        sign_info=sign_info,
        # NOTE: sync_info could be stored in sign_info or add new field to AST later
        line=token.line, column=token.column
    )

def _calculate_decimal_places(self, pic_clause):
    """
    Calculate decimal places from PIC clause using truth table approach.
    
    Truth Table for Decimal Detection:
    ┌─────────────────┬─────────┬─────────┬──────────────────┐
    │ PIC Clause      │ Has '.' │ Has 'V' │ Result           │
    ├─────────────────┼─────────┼─────────┼──────────────────┤
    │ None/Empty      │ N/A     │ N/A     │ None             │
    │ 9999            │ No      │ No      │ None (integer)   │
    │ 99.99           │ Yes     │ No      │ 2                │
    │ 99.             │ Yes     │ No      │ 0                │
    │ 99V99           │ No      │ Yes     │ 2                │
    │ 99V             │ No      │ Yes     │ 0 ✓              │
    │ 999V            │ No      │ Yes     │ 0 ✓              │
    │ 99V9(3)         │ No      │ Yes     │ 3                │
    └─────────────────┴─────────┴─────────┴──────────────────┘
    
    Edge Case Handling:
    - PIC 999V: V at end with nothing after → returns 0 (not None)
    - PIC 999V.: V with only period after → returns 0
    - PIC V99: V at start → returns 2 (all digits after V)
    """
    import re
    
    # ══════════════════════════════════════════════════════════════
    # STEP 1: Validate input
    # ══════════════════════════════════════════════════════════════
    if not pic_clause:
        return None
    
    if not isinstance(pic_clause, str):
        return None
    
    pic_clause = pic_clause.strip()
    if not pic_clause:
        return None
    
    pic_upper = pic_clause.upper()
    
    # ══════════════════════════════════════════════════════════════
    # STEP 2: Check for non-numeric PIC (early exit)
    # ══════════════════════════════════════════════════════════════
    if 'X' in pic_upper or 'A' in pic_upper:
        return None
    
    # ══════════════════════════════════════════════════════════════
    # STEP 3: Process decimal indicators
    # ══════════════════════════════════════════════════════════════
    decimal_count = 0
    has_explicit_decimal = '.' in pic_clause
    has_implied_decimal = 'V' in pic_upper
    
    # Process explicit decimal (period)
    if has_explicit_decimal:
        parts = pic_clause.split('.', 1)
        
        if len(parts) >= 2 and parts[1]:
            after_period = parts[1]
            
            # Extract grouped nines: 9(n)
            grouped_matches = re.findall(r'9\((\d+)\)', after_period)
            for count_str in grouped_matches:
                if count_str:  # Check not empty
                    try:
                        decimal_count += int(count_str)
                    except ValueError:
                        pass
            
            # Remove grouped patterns and count individual 9s and Zs
            cleaned = re.sub(r'9\(\d*\)', '', after_period)
            cleaned = re.sub(r'Z\(\d*\)', '', cleaned)
            decimal_count += cleaned.count('9')
            decimal_count += cleaned.count('Z')
    
    # Process implied decimal (V)
    if has_implied_decimal:
        v_index = pic_upper.find('V')
        after_v = pic_upper[v_index + 1:]
        
        # ═════════════════════════════════════════════════════════
        # TRUTH TABLE for after_v processing:
        # ┌────────────────┬──────────────────────────────────┐
        # │ after_v        │ Action                           │
        # ├────────────────┼──────────────────────────────────┤
        # │ ""             │ decimal_count += 0 (999V case) ✓ │
        # │ "99"           │ decimal_count += 2               │
        # │ "9(3)"         │ decimal_count += 3               │
        # │ "9()"          │ decimal_count += 0 (malformed)   │
        # │ "."            │ decimal_count += 0 (just period) │
        # └────────────────┴──────────────────────────────────┘
        # ═════════════════════════════════════════════════════════
        
        if after_v:
            # Remove any trailing period (999V.)
            after_v = after_v.rstrip('.')
            
            if not after_v:
                # Case: 999V or 999V.
                # V at end means 0 decimal places
                pass  # decimal_count stays as-is (0 from initialization)
            
            else:
                # Try grouped pattern: V9(n)
                match = re.search(r'^9\((\d+)\)', after_v)
                if match:
                    count_str = match.group(1)
                    if count_str:
                        try:
                            decimal_count += int(count_str)
                        except ValueError:
                            pass
                else:
                    # Count consecutive 9s after V
                    match = re.search(r'^(9+)', after_v)
                    if match:
                        decimal_count += len(match.group(1))
        else:
            # Case: V at very end (999V)
            # This means 0 decimal places
            pass  # decimal_count stays as-is (0 from initialization)
    
    # ══════════════════════════════════════════════════════════════
    # STEP 4: Return based on truth table
    # ══════════════════════════════════════════════════════════════
    # ┌────────────────────┬─────────────────────────────────────┐
    # │ decimal_count      │ Return Value                        │
    # ├────────────────────┼─────────────────────────────────────┤
    # │ 0 and no decimals  │ None (integer, no decimal concept)  │
    # │ 0 and has decimals │ 0 (explicit zero decimals: 999V)    │
    # │ > 0                │ decimal_count                       │
    # └────────────────────┴─────────────────────────────────────┘
    
    if decimal_count == 0:
        if has_explicit_decimal or has_implied_decimal:
            return 0  # Explicit zero decimals (covers 999V case)
        else:
            return None  # Integer, no decimal concept (covers 9999 case)
    
    return decimal_count

def _check_signed(self, pic_clause):
    """Check if PIC clause indicates signed value"""
    if not pic_clause:
        return False
    
    pic_upper = pic_clause.upper()
    return pic_upper.startswith('S') or 'S9' in pic_upper


def _parse_usage_type_clause(self):
    """Parse USAGE clause with truth table approach - HARDENED VERSION
    
    🔧 FIXES:
    - ✅ Multi-line USAGE declarations (USAGE IS on one line, DISPLAY on next)
    - ✅ Better error handling with recovery
    - ✅ Duplicate usage type detection
    - ✅ Enhanced debugging and metrics
    - ✅ Comprehensive whitespace handling
    
    TRUTH TABLE: USAGE Clause Entry Points
    ┌────────────────────────────┬──────────────────────────────────┐
    │ Starting Token             │ Action                           │
    ├────────────────────────────┼──────────────────────────────────┤
    │ USAGE [IS] type            │ Parse full USAGE clause          │
    │ COMP/DISPLAY/BINARY (no    │ Shorthand usage (no USAGE word)  │
    │   USAGE keyword)           │                                  │
    │ IDENTIFIER matching type   │ Usage type as identifier         │
    │ Other                      │ No usage clause found            │
    └────────────────────────────┴──────────────────────────────────┘
    
    TRUTH TABLE: USAGE Type Recognition (After Optional IS)
    ┌────────────────────────────┬──────────────┬──────────────────┐
    │ Token Pattern              │ Result       │ Notes            │
    ├────────────────────────────┼──────────────┼──────────────────┤
    │ COMP                       │ 'COMP'       │ Binary           │
    │ COMP-1                     │ 'COMP-1'     │ Single float     │
    │ COMP-2                     │ 'COMP-2'     │ Double float     │
    │ COMP-3                     │ 'COMP-3'     │ Packed decimal   │
    │ COMPUTATIONAL              │ 'COMP'       │ Synonym for COMP │
    │ COMPUTATIONAL-3            │ 'COMP-3'     │ Synonym          │
    │ BINARY                     │ 'BINARY'     │ Same as COMP     │
    │ PACKED-DECIMAL             │ 'PACKED-DECIMAL' │ Same as COMP-3 │
    │ DISPLAY                    │ 'DISPLAY'    │ Character (default) │
    │ INDEX                      │ Handled separately (not here)    │
    └────────────────────────────┴──────────────┴──────────────────┘
    
    TRUTH TABLE: Whitespace Handling Strategy
    ┌────────────────────────────┬──────────────────────────────────┐
    │ Location                   │ Skip Whitespace?                 │
    ├────────────────────────────┼──────────────────────────────────┤
    │ After USAGE keyword        │ YES (handles multi-line)         │
    │ After optional IS          │ YES (handles multi-line)         │
    │ Before checking type       │ YES (CRITICAL for next line!)    │
    │ After type token           │ NO (caller handles)              │
    │ After optional comma       │ NO (caller handles)              │
    └────────────────────────────┴──────────────────────────────────┘
    
    Examples:
    1. USAGE COMP                  → 'COMP'
    2. USAGE IS COMPUTATIONAL      → 'COMP'
    3. COMP-3                      → 'COMP-3' (shorthand)
    4. USAGE IS                    → 'DISPLAY' (multi-line case)
       DISPLAY VALUE IS 9
    5. PACKED-DECIMAL              → 'PACKED-DECIMAL' (as identifier)
    """
    
    usage_type = None
    parse_start_pos = self.pos
    had_usage_keyword = False
    
    # ══════════════════════════════════════════════════════════════
    # STEP 1: Check for USAGE keyword (full form)
    # ══════════════════════════════════════════════════════════════
    if self.match(COBOLTokenType.USAGE):
        had_usage_keyword = True
        self.advance()  # Consume USAGE
        
        if self.debug:
            print(f"[PARSE_USAGE] Found USAGE keyword")
        
        # 🔧 FIX: Skip whitespace AFTER USAGE
        # Handles: USAGE
        #          COMP
        self._skip_whitespace()
        
        # Check for optional IS
        if self.match(COBOLTokenType.IS):
            self.advance()
            if self.debug:
                print(f"[PARSE_USAGE] Found IS after USAGE")
            # 🔧 FIX: Skip whitespace AFTER IS
            self._skip_whitespace()
        elif self.match(COBOLTokenType.IDENTIFIER) and \
             self.current_token().value.upper() == 'IS':
            self.advance()
            if self.debug:
                print(f"[PARSE_USAGE] Found IS (as identifier) after USAGE")
            # 🔧 FIX: Skip whitespace AFTER IS
            self._skip_whitespace()
        
        # Parse the usage type using the helper function
        usage_type = self.parse_usage_type()
        
        if not usage_type:
            # ✅ HARDENING: Better error for malformed USAGE
            current_tok = self.current_token()
            tok_info = f"{current_tok.type.name}='{current_tok.value}'" if current_tok else "EOF"
            
            if self.debug:
                print(f"[ERROR] Expected usage type after USAGE [IS], got {tok_info}")
            
            # Try to recover - maybe it's on next line?
            self._skip_whitespace()
            usage_type = self.parse_usage_type()
            
            if not usage_type:
                self.error(
                    f"Expected usage type (COMP, DISPLAY, etc.) after USAGE [IS], "
                    f"got {tok_info}"
                )
        
        # Skip optional comma after usage type
        if self.match(COBOLTokenType.COMMA):
            if self.debug:
                print(f"[PARSE_USAGE] Skipping comma after usage type")
            self.advance()
    
    # ══════════════════════════════════════════════════════════════
    # STEP 2: Check for shorthand usage (no USAGE keyword)
    # Pattern: PIC 9(5) COMP VALUE 100.
    #                   ^^^^ no USAGE keyword before it
    # ══════════════════════════════════════════════════════════════
    elif self.match(
        COBOLTokenType.COMP, COBOLTokenType.COMP_1, COBOLTokenType.COMP_2,
        COBOLTokenType.COMP_3, COBOLTokenType.COMPUTATIONAL,
        COBOLTokenType.COMPUTATIONAL_3, COBOLTokenType.BINARY,
        COBOLTokenType.PACKED_DECIMAL, COBOLTokenType.DISPLAY
    ):
        # Shorthand usage - type without USAGE keyword
        usage_map = {
            COBOLTokenType.COMP: 'COMP', 
            COBOLTokenType.COMP_1: 'COMP-1',
            COBOLTokenType.COMP_2: 'COMP-2', 
            COBOLTokenType.COMP_3: 'COMP-3',
            COBOLTokenType.COMPUTATIONAL: 'COMP', 
            COBOLTokenType.COMPUTATIONAL_3: 'COMP-3',
            COBOLTokenType.BINARY: 'BINARY', 
            COBOLTokenType.PACKED_DECIMAL: 'PACKED-DECIMAL',
            COBOLTokenType.DISPLAY: 'DISPLAY'
        }
        
        token_type = self.current_token().type
        usage_type = usage_map.get(token_type, 'COMP')
        
        if self.debug:
            print(f"[PARSE_USAGE] Found shorthand usage: {usage_type}")
        
        self.advance()
        
        # Skip optional comma
        if self.match(COBOLTokenType.COMMA):
            if self.debug:
                print(f"[PARSE_USAGE] Skipping comma after shorthand usage")
            self.advance()
    
    # ══════════════════════════════════════════════════════════════
    # STEP 3: Check for usage type as IDENTIFIER
    # Some compilers tokenize BINARY/PACKED-DECIMAL as identifiers
    # ══════════════════════════════════════════════════════════════
    elif self.match(COBOLTokenType.IDENTIFIER):
        val = self.current_token().value.upper()
        
        # ✅ HARDENING: More comprehensive identifier matching
        usage_keywords = {
            'BINARY', 'COMP', 'COMP-1', 'COMP-2', 'COMP-3', 'COMP-4', 'COMP-5',
            'COMPUTATIONAL', 'COMPUTATIONAL-1', 'COMPUTATIONAL-2', 
            'COMPUTATIONAL-3', 'COMPUTATIONAL-4', 'COMPUTATIONAL-5',
            'PACKED-DECIMAL', 'PACKED', 'DISPLAY', 'DISPLAY-1'
        }
        
        # Check if this identifier is a usage keyword
        if val in usage_keywords:
            usage_type = val
            if self.debug:
                print(f"[PARSE_USAGE] Found usage type as identifier: {usage_type}")
            self.advance()
            
            # Skip optional comma
            if self.match(COBOLTokenType.COMMA):
                if self.debug:
                    print(f"[PARSE_USAGE] Skipping comma after identifier usage")
                self.advance()
        # Also check for partial matches (e.g., identifiers containing these words)
        elif any(keyword in val for keyword in ['BINARY', 'PACKED', 'COMP', 'DISPLAY']):
            usage_type = val
            if self.debug:
                print(f"[PARSE_USAGE] Found usage-like identifier: {usage_type}")
            self.advance()
            
            # Skip optional comma
            if self.match(COBOLTokenType.COMMA):
                self.advance()
    
    # ══════════════════════════════════════════════════════════════
    # STEP 4: ✅ HARDENING - Metrics and validation
    # ══════════════════════════════════════════════════════════════
    if self.debug and usage_type:
        tokens_consumed = self.pos - parse_start_pos
        form = "full (USAGE keyword)" if had_usage_keyword else "shorthand"
        print(f"[PARSE_USAGE] ✓ Parsed usage type: '{usage_type}' ({form})")
        print(f"[PARSE_USAGE]   Tokens consumed: {tokens_consumed}")
    
    # ✅ HARDENING: Validate recognized usage types
    if usage_type:
        valid_usage_types = {
            'COMP', 'COMP-1', 'COMP-2', 'COMP-3', 'COMP-4', 'COMP-5',
            'COMPUTATIONAL', 'COMPUTATIONAL-1', 'COMPUTATIONAL-2',
            'COMPUTATIONAL-3', 'COMPUTATIONAL-4', 'COMPUTATIONAL-5',
            'BINARY', 'PACKED-DECIMAL', 'PACKED', 'DISPLAY', 'DISPLAY-1'
        }
        
        # Normalize for comparison
        usage_normalized = usage_type.upper().replace('_', '-')
        
        if usage_normalized not in valid_usage_types:
            # Check if it's a partial match (might be valid but non-standard)
            has_partial_match = any(
                valid in usage_normalized 
                for valid in ['COMP', 'BINARY', 'PACKED', 'DISPLAY']
            )
            
            if not has_partial_match and self.debug:
                print(f"[WARNING] Non-standard usage type: '{usage_type}'")
    
    return usage_type


def _parse_value_clause(self):
    """Parse VALUE clause with continuation line support
    
    TRUTH TABLE: VALUE Clause Patterns
    ┌─────────────────────────────────────┬──────────────────────────────┐
    │ Pattern                             │ Action                       │
    ├─────────────────────────────────────┼──────────────────────────────┤
    │ No VALUE keyword                    │ Return None                  │
    │ VALUE [WS] STRING_LITERAL           │ Return string                │
    │ VALUE [WS] IS [WS] STRING_LITERAL   │ Skip IS, return string       │
    │ VALUE [WS] +/- NUMBER               │ Return signed number         │
    │ VALUE [WS] NUMBER.NUMBER            │ Return decimal number        │
    │ VALUE [WS] NUMBER                   │ Return number                │
    │ VALUE [WS] ALL [WS] literal         │ Return "ALL literal"         │
    │ VALUE [WS] SPACES/ZEROS/etc         │ Return figurative constant   │
    │ VALUE [WS] identifier               │ Return identifier (XXXXX031) │
    └─────────────────────────────────────┴──────────────────────────────┘
    
    [WS] = whitespace/newlines (automatically skipped after keywords)
    
    CRITICAL: ALL is lexed as COBOLTokenType.ALL (not IDENTIFIER), so it needs
    a dedicated case BEFORE the IDENTIFIER check!
    
    Examples that must work:
    1. VALUE "ABC".
    2. VALUE IS "ABC".
    3. VALUE                    ← Line 43
       XXXXX031.                ← Line 44 (continuation)
    4. VALUE +123.
    5. VALUE SPACES.
    6. VALUE ALL "-".           ← Line 83: ALL is a token type!
    7. VALUE IS 9999.9.         ← Line 217271: Decimal number!
    """
    # TRUTH TABLE ROW 1: No VALUE keyword
    if not self.match(COBOLTokenType.VALUE):
        return None
    
    self.advance()  # Consume VALUE
    
    # ✅ CRITICAL FIX: Skip whitespace/newlines after VALUE keyword
    # This handles continuation lines like:
    #   77 PASSWORD1 PIC X(10) VALUE
    #      XXXXX031.
    self._skip_whitespace()
    
    # TRUTH TABLE ROW 2-3: Optional IS keyword
    if self.current_token() and self.current_token().value and \
       self.current_token().value.upper() == 'IS':
        self.advance()  # Consume IS
        self._skip_whitespace()  # ✅ Skip whitespace after IS too
    
    # TRUTH TABLE ROW 4: STRING_LITERAL
    if self.match(COBOLTokenType.STRING_LITERAL):
        value = self.current_token().value
        self.advance()
        return value
    
    # TRUTH TABLE ROW 5: Signed number (+123 or -456)
    elif self.match(COBOLTokenType.PLUS, COBOLTokenType.MINUS):
        sign = self.current_token().value
        self.advance()
        self._skip_whitespace()  # ✅ Skip whitespace after sign
        if self.match(COBOLTokenType.NUMBER_LITERAL):
            value = sign + self.current_token().value
            self.advance()
            return value
        else:
            # Sign without number - treat as identifier?
            return sign
    
    # TRUTH TABLE ROW 6: Unsigned number (with optional decimal)
    elif self.match(COBOLTokenType.NUMBER_LITERAL):
        number_val = self.current_token().value
        self.advance()
        
        # ✅ CRITICAL FIX: Check for decimal separator (both period and comma)
        # Pattern 1: 9999.9 (US notation - period as decimal)
        # Pattern 2: 9999,9 (European notation - comma as decimal)
        # The lexer tokenizes both as: NUMBER SEPARATOR NUMBER
        
        # Check for PERIOD as decimal separator
        if self.match(COBOLTokenType.PERIOD):
            # Peek ahead - is next token a number (decimal fraction)?
            peek = self.peek_token(1)
            if peek and peek.type == COBOLTokenType.NUMBER_LITERAL:
                # It's a decimal number! Consume period and fraction
                self.advance()  # Consume PERIOD
                fraction = self.current_token().value
                self.advance()  # Consume fraction
                return f"{number_val}.{fraction}"
            # Otherwise, don't consume PERIOD - it's the statement terminator
        
        # ✅ NEW: Check for COMMA as decimal separator (European notation)
        elif self.match(COBOLTokenType.COMMA):
            # Peek ahead - is next token a number (decimal fraction)?
            peek = self.peek_token(1)
            if peek and peek.type == COBOLTokenType.NUMBER_LITERAL:
                # It's a European decimal! Consume comma and fraction
                # Convert to US notation internally (use period)
                self.advance()  # Consume COMMA
                fraction = self.current_token().value
                self.advance()  # Consume fraction
                return f"{number_val}.{fraction}"  # Convert comma to period
            # Otherwise, don't consume COMMA - it might be a separator
        
        # Just an integer
        return number_val
    
    # TRUTH TABLE ROW 7: ALL keyword (handles both ALL "literal" and ALL QUOTE)
    elif self.match(COBOLTokenType.ALL):
        self.advance()  # Consume ALL token
        self._skip_whitespace()
        
        # ══════════════════════════════════════════════════════════════
        # Check what follows ALL:
        # - STRING_LITERAL: ALL "X" or ALL "-"
        # - IDENTIFIER figurative constant: ALL QUOTE, ALL SPACES, etc.
        # ══════════════════════════════════════════════════════════════
        
        if self.match(COBOLTokenType.STRING_LITERAL):
            # Pattern: VALUE ALL "X"
            literal = self.current_token().value
            self.advance()
            return f"ALL {literal}"
        
        elif self.match(COBOLTokenType.IDENTIFIER):
            # Pattern: VALUE ALL QUOTE or VALUE ALL SPACES
            # Check if it's a figurative constant
            val = self.current_token().value.upper()
            if val in ['QUOTE', 'QUOTES', 'SPACE', 'SPACES', 'ZERO', 'ZEROS', 'ZEROES',
                       'HIGH-VALUE', 'HIGH-VALUES', 'LOW-VALUE', 'LOW-VALUES']:
                figurative = val
                self.advance()
                return f"ALL {figurative}"
            else:
                # Not a figurative constant, just an identifier after ALL
                # This is probably an error, but return it anyway
                ident = self.current_token().value
                self.advance()
                return f"ALL {ident}"
        
        else:
            # ALL without anything after it - just return "ALL"
            return "ALL"
    
    # TRUTH TABLE ROW 8-9: Figurative constants or identifiers
    elif self.match(COBOLTokenType.IDENTIFIER):
        val = self.current_token().value.upper()
        
        # Note: ALL is handled above as COBOLTokenType.ALL
        # This section handles cases where ALL might appear as IDENTIFIER
        # (shouldn't happen with current lexer, but defensive programming)
        if val == 'ALL':
            self.advance()  # Consume ALL
            self._skip_whitespace()
            
            # Expect a string literal after ALL
            if self.match(COBOLTokenType.STRING_LITERAL):
                literal = self.current_token().value
                self.advance()
                return f"ALL {literal}"
            else:
                # ALL without literal - just return "ALL"
                return "ALL"
        
        # Check if it's a figurative constant
        elif val in ['SPACES', 'SPACE', 'ZEROS', 'ZEROES', 'ZERO',
                     'HIGH-VALUES', 'HIGH-VALUE', 'LOW-VALUES', 'LOW-VALUE',
                     'QUOTES', 'QUOTE', 'NULL', 'NULLS']:
            value = val
            self.advance()
            return value
        else:
            # Regular identifier (like XXXXX031 in NIST tests)
            # This is configuration-dependent or symbolic value
            value = self.current_token().value
            self.advance()
            return value
    
    # TRUTH TABLE ROW 10: Operator symbols as literal values
    # Pattern: VALUE '<' or VALUE '>' or VALUE '='
    # Some COBOL dialects allow operators as character literals without quotes
    elif self.match(COBOLTokenType.LT_SIGN, COBOLTokenType.GT_SIGN,
                   COBOLTokenType.EQUALS_SIGN, COBOLTokenType.LTE,
                   COBOLTokenType.GTE, COBOLTokenType.NOT_EQUALS,
                   COBOLTokenType.PLUS, COBOLTokenType.MINUS,
                   COBOLTokenType.ASTERISK, COBOLTokenType.SLASH):
        # Store the operator symbol as the value
        value = self.current_token().value
        self.advance()
        if self.debug:
            print(f"[PARSE_VALUE] Operator symbol as literal: '{value}'")
        return value
    
    # No valid value found after VALUE keyword
    return None


def parse_pic_clause(self) -> str:
    """Parse a PIC clause"""
    pic_str = ""
    
    start_line = self.current_token().line if self.current_token() else 0
    
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
        
        if self.match(COBOLTokenType.LEVEL_NUMBER):
            if token.line != start_line:
                break
            
            if token.column <= 12:
                break
        
        if self.match(COBOLTokenType.PERIOD):
            next_pos = self.pos + 1
            if next_pos < len(self.tokens):
                next_token = self.tokens[next_pos]
                is_decimal = False
                
                if next_token.type == COBOLTokenType.NUMBER_LITERAL:
                    is_decimal = True
                elif next_token.type == COBOLTokenType.LPAREN:
                    is_decimal = True
                elif next_token.type == COBOLTokenType.LEVEL_NUMBER:
                    if next_token.line == token.line and next_token.column > 12:
                        is_decimal = True
                    else:
                        is_decimal = False
                elif next_token.type == COBOLTokenType.IDENTIFIER:
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
                    break
            else:
                break
        
        if self.match(COBOLTokenType.LEVEL_NUMBER):
            if token.line == start_line and token.column > 12:
                pic_str += token.value
                seen_content = True
                last_was_closing_paren = False
                self.advance()
                continue
            else:
                break
        
        # ✅ PATCHED: Split IDENTIFIER check to detect SIGN and other keywords
        if self.match(COBOLTokenType.IDENTIFIER):
            # Check if this IDENTIFIER is a keyword that ends PIC
            id_val = token.value.upper()
            if id_val in ['SIGN', 'SYNCHRONIZED', 'SYNC', 'JUSTIFIED', 'JUST', 
                          'BLANK', 'EXTERNAL', 'GLOBAL']:
                break  # Stop parsing PIC, this is the next clause
            
            pic_str += token.value
            seen_content = True
            last_was_closing_paren = False
            self.advance()
        elif self.match(
            COBOLTokenType.NUMBER_LITERAL,
            COBOLTokenType.LPAREN,
            COBOLTokenType.RPAREN,
            COBOLTokenType.COMMA,
            COBOLTokenType.PLUS,
            COBOLTokenType.MINUS,
            COBOLTokenType.ASTERISK,
            COBOLTokenType.SLASH,
            COBOLTokenType.DOLLAR_SIGN
        ):
            pic_str += token.value
            seen_content = True
            last_was_closing_paren = (token.type == COBOLTokenType.RPAREN)
            self.advance()
        else:
            break
    
    return pic_str.strip()


def parse_usage_type(self) -> str:
    """Parse USAGE clause value"""
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
        ident_value = self.current_token().value.upper()
        self.advance()
        return ident_value
    else:
        return 'DISPLAY'


def parse_condition(self) -> COBOLASTNode:
    return self.parse_or_expression()


def parse_and_expression(self) -> COBOLASTNode:
    """Parse AND expression with support for abbreviated conditions
    
    TRUTH TABLE: AND Expression Patterns
    ┌──────────────────────────────────────┬─────────────────────────────┐
    │ Pattern                              │ Interpretation              │
    ├──────────────────────────────────────┼─────────────────────────────┤
    │ A > 10 AND A < 20                    │ Standard (explicit subject) │
    │ A > 10 AND < 20                      │ Abbreviated (reuse subject) │
    │ (A > 10 AND < 20)                    │ Parenthesized abbreviated   │
    │ A > 10 AND B > 20                    │ Different subjects          │
    └──────────────────────────────────────┴─────────────────────────────┘
    
    TRUTH TABLE: Token After AND Keyword
    ┌────────────────────────┬──────────────────────────────────────┐
    │ Next Token Type        │ Action                               │
    ├────────────────────────┼──────────────────────────────────────┤
    │ NOT                    │ Parse NOT expression (standard)      │
    │ LPAREN                 │ Parse parenthesized expr (standard)  │
    │ IDENTIFIER             │ Parse full comparison (standard)     │
    │ Relational Operator    │ Abbreviated condition (reuse subject)│
    │ (<, >, =, <=, >=, <>)  │ Extract subject from left side       │
    └────────────────────────┴──────────────────────────────────────┘
    """
    left = self.parse_not_expression()
    
    while self.match(COBOLTokenType.AND):
        op_token = self.current_token()
        self.advance()
        
        # ═══════════════════════════════════════════════════════════════
        # TRUTH TABLE: Check if next token is a relational operator
        # This indicates an abbreviated condition (COBOL-74/85 feature)
        # ═══════════════════════════════════════════════════════════════
        current = self.current_token()
        
        # ROW 1: Check for relational operators that start a comparison
        is_relational_operator = current.type in (
            COBOLTokenType.LT_SIGN,      # <
            COBOLTokenType.GT_SIGN,      # >
            COBOLTokenType.EQUAL,        # =
        )
        
        # Check for keyword forms: LESS, GREATER, EQUAL
        if current.type == COBOLTokenType.IDENTIFIER:
            keyword = current.value.upper()
            is_relational_operator = keyword in (
                'LESS', 'GREATER', 'EQUAL', 'EQUALS'
            )
        
        # ═══════════════════════════════════════════════════════════════
        # ROW 2: Abbreviated condition detected
        # Pattern: (A > 10 AND < 20) means (A > 10 AND A < 20)
        # ═══════════════════════════════════════════════════════════════
        if is_relational_operator:
            # Extract the subject from the left comparison
            # The left side should be a comparison node with a subject
            subject = self._extract_comparison_subject(left)
            
            if subject is None:
                # Fallback: if we can't extract subject, parse normally
                # This shouldn't happen, but fail gracefully
                right = self.parse_not_expression()
            else:
                # Build the abbreviated comparison
                # Save subject, parse the operator and right side
                right = self._parse_abbreviated_comparison(subject)
        
        # ═══════════════════════════════════════════════════════════════
        # ROW 3: Standard condition (full comparison)
        # ═══════════════════════════════════════════════════════════════
        else:
            right = self.parse_not_expression()
        
        # Build the AND node
        left = COBOLBinaryOp(
            operator='AND',
            left=left,
            right=right,
            line=op_token.line,
            column=op_token.column
        )
    
    return left


def parse_and_expression(self) -> COBOLASTNode:
    """Parse AND expression with support for abbreviated conditions
    
    TRUTH TABLE: AND Expression Patterns
    ┌──────────────────────────────────────┬─────────────────────────────┐
    │ Pattern                              │ Interpretation              │
    ├──────────────────────────────────────┼─────────────────────────────┤
    │ A > 10 AND A < 20                    │ Standard (explicit subject) │
    │ A > 10 AND < 20                      │ Abbreviated (reuse subject) │
    │ (A > 10 AND < 20)                    │ Parenthesized abbreviated   │
    │ A > 10 AND B > 20                    │ Different subjects          │
    └──────────────────────────────────────┴─────────────────────────────┘
    
    TRUTH TABLE: Token After AND Keyword
    ┌────────────────────────┬──────────────────────────────────────┐
    │ Next Token Type        │ Action                               │
    ├────────────────────────┼──────────────────────────────────────┤
    │ NOT                    │ Parse NOT expression (standard)      │
    │ LPAREN                 │ Parse parenthesized expr (standard)  │
    │ IDENTIFIER             │ Parse full comparison (standard)     │
    │ Relational Operator    │ Abbreviated condition (reuse subject)│
    │ (<, >, =, <=, >=, <>)  │ Extract subject from left side       │
    └────────────────────────┴──────────────────────────────────────┘
    """
    left = self.parse_not_expression()
    
    while self.match(COBOLTokenType.AND):
        op_token = self.current_token()
        self.advance()
        
        # ═══════════════════════════════════════════════════════════════
        # TRUTH TABLE: Check if next token is a relational operator
        # This indicates an abbreviated condition (COBOL-74/85 feature)
        # ═══════════════════════════════════════════════════════════════
        current = self.current_token()
        
        # ROW 1: Check for relational operators that start a comparison
        is_relational_operator = current.type in (
            COBOLTokenType.LT_SIGN,      # <
            COBOLTokenType.GT_SIGN,      # >
            COBOLTokenType.EQUAL,        # =
        )
        
        # Check for keyword forms: LESS, GREATER, EQUAL
        if current.type == COBOLTokenType.IDENTIFIER:
            keyword = current.value.upper()
            is_relational_operator = keyword in (
                'LESS', 'GREATER', 'EQUAL', 'EQUALS'
            )
        
        # ═══════════════════════════════════════════════════════════════
        # ROW 2: Abbreviated condition detected
        # Pattern: (A > 10 AND < 20) means (A > 10 AND A < 20)
        # ═══════════════════════════════════════════════════════════════
        if is_relational_operator:
            # Extract the subject from the left comparison
            # The left side should be a comparison node with a subject
            subject = self._extract_comparison_subject(left)
            
            if subject is None:
                # Fallback: if we can't extract subject, parse normally
                # This shouldn't happen, but fail gracefully
                right = self.parse_not_expression()
            else:
                # Build the abbreviated comparison
                # Save subject, parse the operator and right side
                right = self._parse_abbreviated_comparison(subject)
        
        # ═══════════════════════════════════════════════════════════════
        # ROW 3: Standard condition (full comparison)
        # ═══════════════════════════════════════════════════════════════
        else:
            right = self.parse_not_expression()
        
        # Build the AND node
        left = COBOLBinaryOp(
            operator='AND',
            left=left,
            right=right,
            line=op_token.line,
            column=op_token.column
        )
    
    return left


def _extract_comparison_subject(self, node: COBOLASTNode) -> Optional[COBOLASTNode]:
    """Extract the subject (left side) from a comparison node
    
    Given: (A > 10)
    Returns: A
    
    Given: ((A > 10) AND (B < 20))
    Returns: B (from the rightmost comparison)
    """
    # If it's a binary comparison, return the left side
    if isinstance(node, COBOLBinaryOp):
        # For AND/OR nodes, recurse to get the rightmost comparison's subject
        if node.operator in ('AND', 'OR'):
            return self._extract_comparison_subject(node.right)
        
        # For comparison operators, return the left operand
        if node.operator in ('<', '>', '=', '<=', '>=', '<>', 'LESS', 'GREATER', 'EQUAL'):
            return node.left
    
    # For other node types, we can't extract a subject
    return None


def _parse_abbreviated_comparison(self, subject: COBOLASTNode) -> COBOLASTNode:
    """Parse an abbreviated comparison, reusing the given subject
    
    Input stream: < 20
    Subject: A
    Returns: (A < 20)
    """
    # Parse the relational operator
    op_token = self.current_token()
    
    # Map token types to operators
    operator_map = {
        COBOLTokenType.LT_SIGN: '<',
        COBOLTokenType.GT_SIGN: '>',
        COBOLTokenType.EQUAL: '=',
    }
    
    operator = operator_map.get(op_token.type)
    
    # Handle keyword forms
    if op_token.type == COBOLTokenType.IDENTIFIER:
        keyword = op_token.value.upper()
        if keyword == 'LESS':
            operator = '<'
        elif keyword == 'GREATER':
            operator = '>'
        elif keyword in ('EQUAL', 'EQUALS'):
            operator = '='
    
    if operator is None:
        self.error(f"Expected relational operator, got {op_token.type.name}")
    
    self.advance()  # Consume the operator
    
    # Handle optional THAN keyword (LESS THAN, GREATER THAN)
    if self.match(COBOLTokenType.IDENTIFIER):
        if self.current_token().value.upper() == 'THAN':
            self.advance()
    
    # Parse the right side (the value to compare against)
    right = self.parse_arithmetic_expression()
    
    # Build the comparison node with the reused subject
    return COBOLBinaryOp(
        operator=operator,
        left=subject,
        right=right,
        line=op_token.line,
        column=op_token.column
    )     

def parse_or_expression(self) -> COBOLASTNode:
    """Parse OR expression with support for abbreviated conditions
    
    TRUTH TABLE: OR Expression Patterns
    ┌──────────────────────────────────────┬─────────────────────────────┐
    │ Pattern                              │ Interpretation              │
    ├──────────────────────────────────────┼─────────────────────────────┤
    │ A = 1 OR A = 2                       │ Standard (explicit subject) │
    │ A = 1 OR = 2                         │ Abbreviated (reuse subject) │
    │ (A < 10 OR > 20)                     │ Parenthesized abbreviated   │
    │ A = 1 OR B = 2                       │ Different subjects          │
    └──────────────────────────────────────┴─────────────────────────────┘
    
    TRUTH TABLE: Token After OR Keyword
    ┌────────────────────────┬──────────────────────────────────────┐
    │ Next Token Type        │ Action                               │
    ├────────────────────────┼──────────────────────────────────────┤
    │ NOT                    │ Parse AND expression (standard)      │
    │ LPAREN                 │ Parse parenthesized expr (standard)  │
    │ IDENTIFIER             │ Parse full comparison (standard)     │
    │ Relational Operator    │ Abbreviated condition (reuse subject)│
    │ (<, >, =, <=, >=, <>)  │ Extract subject from left side       │
    └────────────────────────┴──────────────────────────────────────┘
    """
    left = self.parse_and_expression()
    
    while self.match(COBOLTokenType.OR):
        op_token = self.current_token()
        self.advance()
        
        # ═══════════════════════════════════════════════════════════════
        # TRUTH TABLE: Check if next token is a relational operator
        # This indicates an abbreviated condition (COBOL-74/85 feature)
        # ═══════════════════════════════════════════════════════════════
        current = self.current_token()
        
        # ROW 1: Check for relational operators that start a comparison
        is_relational_operator = current.type in (
            COBOLTokenType.LT_SIGN,      # <
            COBOLTokenType.GT_SIGN,      # >
            COBOLTokenType.EQUAL,        # =
        )
        
        # Check for keyword forms: LESS, GREATER, EQUAL
        if current.type == COBOLTokenType.IDENTIFIER:
            keyword = current.value.upper()
            is_relational_operator = keyword in (
                'LESS', 'GREATER', 'EQUAL', 'EQUALS'
            )
        
        # ═══════════════════════════════════════════════════════════════
        # ROW 2: Abbreviated condition detected
        # Pattern: (A = 1 OR = 2) means (A = 1 OR A = 2)
        # ═══════════════════════════════════════════════════════════════
        if is_relational_operator:
            # Extract the subject from the left comparison
            # The left side should be a comparison node with a subject
            subject = self._extract_comparison_subject(left)
            
            if subject is None:
                # Fallback: if we can't extract subject, parse normally
                # This shouldn't happen, but fail gracefully
                right = self.parse_and_expression()
            else:
                # Build the abbreviated comparison
                # Reuse subject, parse the operator and right side
                right = self._parse_abbreviated_comparison(subject)
        
        # ═══════════════════════════════════════════════════════════════
        # ROW 3: Standard condition (full comparison)
        # ═══════════════════════════════════════════════════════════════
        else:
            right = self.parse_and_expression()
        
        # Build the OR node
        left = COBOLBinaryOp(
            operator='OR',
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
    """Parse comparison or standalone condition (level-88 condition name)
    
    TRUTH TABLE: What follows the left operand?
    ┌────────────────────────────────────┬──────────────────────────────┐
    │ Next Token                         │ Action                       │
    ├────────────────────────────────────┼──────────────────────────────┤
    │ Comparison operator (=, <, >, etc) │ Parse right side, return op  │
    │ AND, OR, THEN, ELSE, PERIOD, EOF   │ Return left (level-88 cond)  │
    │ IS (before operator)               │ Skip, then check operator    │
    │ NOT (before EQUAL)                 │ Parse NOT EQUAL TO           │
    └────────────────────────────────────┴──────────────────────────────┘
    
    Examples:
    - IF CON-YEAR AND CON-MONTH          → Returns CON-YEAR identifier
    - IF X = 5 THEN                      → Returns BinaryOp(X, =, 5)
    - IF IDN2 IS EQUAL TO IN3            → Returns BinaryOp(IDN2, =, IN3)
    """
    left = self.parse_arithmetic_expression()
    
    # ✅ Handle optional IS keyword before comparison operators
    # COBOL allows: "IF IDN2 IS EQUAL TO IN3" or "IF IDN2 EQUAL TO IN3"
    if self.match(COBOLTokenType.IS):
        self.advance()
    
    # Rule 1: GREATER THAN [OR EQUAL [TO]]
    if self.match(COBOLTokenType.GTE_SIGN, COBOLTokenType.GREATER):
        op_token = self.current_token()
        self.advance()
        
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
    
    # Rule 2: LESS THAN [OR EQUAL [TO]]
    elif self.match(COBOLTokenType.LTE_SIGN, COBOLTokenType.LESS):
        op_token = self.current_token()
        self.advance()
        
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
    
    # Rule 3: Greater than (symbolic)
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
    
    # Rule 4: Less than (symbolic)
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
    
    # Rule 5: EQUAL [TO]
    elif self.match(COBOLTokenType.EQUALS_SIGN, COBOLTokenType.EQUAL, COBOLTokenType.EQUALS):
        op_token = self.current_token()
        self.advance()
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
    
    # Rule 6: NOT EQUAL [TO]
    elif self.match(COBOLTokenType.NOT):
        op_token = self.current_token()
        self.advance()
        if self.match(COBOLTokenType.EQUALS_SIGN, COBOLTokenType.EQUAL, COBOLTokenType.EQUALS):
            self.advance()
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
        else:
            # NOT without EQUAL - this is a logic error, but let's handle gracefully
            # Put the NOT back by rewinding and let parse_not_expression handle it
            self.pos -= 1
            return left
    
    # Rule 7: No comparison operator found - return left as standalone condition
    # This handles level-88 condition names: IF CON-YEAR AND CON-MONTH THEN
    return left


def parse_arithmetic_expression(self) -> COBOLASTNode:
    return self.parse_additive()


def parse_additive(self) -> COBOLASTNode:
    """Parse additive expressions (+ and -) using truth table approach
    
    TRUTH TABLE: When to stop parsing additive operations
    
    | Current Token | Action | Reason |
    |---------------|--------|--------|
    | PLUS/MINUS    | Continue parsing | Valid operator |
    | PERIOD        | Return left | Statement boundary |
    | EOF           | Return left | End of input |
    | RPAREN        | Return left | End of sub-expression |
    | Statement keyword | Return left | Statement boundary |
    | Comparison op | Return left | Lower precedence |
    | Other         | Return left | Unknown token |
    """
    left = self.parse_multiplicative()
    
    max_iterations = 100  # Safety limit
    iteration = 0
    
    while iteration < max_iterations:
        iteration += 1
        token = self.current_token()
        token_type = token.type
        
        # TRUTH TABLE: Determine action based on current token
        
        # Rule 1: Additive operators → continue parsing
        if token_type in (COBOLTokenType.PLUS, COBOLTokenType.MINUS):
            op = '+' if token_type == COBOLTokenType.PLUS else '-'
            self.advance()
            right = self.parse_multiplicative()
            left = COBOLBinaryOp(
                operator=op, 
                left=left, 
                right=right, 
                line=token.line, 
                column=token.column
            )
            continue
        
        # Rule 2: Boundaries → stop and return
        if token_type in (COBOLTokenType.PERIOD, COBOLTokenType.EOF, 
                         COBOLTokenType.RPAREN):
            return left
        
        # Rule 3: Statement keywords → stop and return
        if token_type in (COBOLTokenType.MOVE, COBOLTokenType.ADD,
                         COBOLTokenType.SUBTRACT, COBOLTokenType.MULTIPLY,
                         COBOLTokenType.DIVIDE, COBOLTokenType.COMPUTE,
                         COBOLTokenType.IF, COBOLTokenType.PERFORM,
                         COBOLTokenType.DISPLAY, COBOLTokenType.ACCEPT,
                         COBOLTokenType.TO, COBOLTokenType.GIVING,
                         COBOLTokenType.UNTIL, COBOLTokenType.VARYING):
            return left
        
        # Rule 4: Comparison operators → stop (lower precedence)
        if token_type in (COBOLTokenType.EQUALS_SIGN, COBOLTokenType.EQUAL,
                         COBOLTokenType.GT_SIGN, COBOLTokenType.LT_SIGN,
                         COBOLTokenType.GREATER, COBOLTokenType.LESS,
                         COBOLTokenType.NOT):
            return left
        
        # Rule 5: Logical operators → stop (lower precedence)
        if token_type in (COBOLTokenType.AND, COBOLTokenType.OR):
            return left
        
        # Rule 6: Unknown token → stop
        return left
    
    # Safety: max iterations reached
    print(f"[WARNING] parse_additive hit iteration limit at line {token.line}")
    return left




def parse_exponential(self) -> COBOLASTNode:
    """Parse exponentiation (** operator) - RIGHT associative
    
    TRUTH TABLE: Exponentiation handling
    ┌──────────────────┬────────────────────────┐
    │ Pattern          │ Action                 │
    ├──────────────────┼────────────────────────┤
    │ expr ** expr     │ Parse as power         │
    │ expr * expr      │ Return (not **)        │
    │ Other            │ Return primary         │
    └──────────────────┴────────────────────────┘
    
    Note: Exponentiation is RIGHT associative in COBOL
    2 ** 3 ** 2 = 2 ** (3 ** 2) = 2 ** 9 = 512
    """
    left = self.parse_primary()
    
    # Check for ** operator (two consecutive ASTERISK tokens)
    if self.match(COBOLTokenType.ASTERISK):
        next_tok = self.peek_token(1)
        if next_tok and next_tok.type == COBOLTokenType.ASTERISK:
            # Consume both asterisks
            self.advance()  # First *
            self.advance()  # Second *
            
            # Right associative - recursively parse right side
            right = self.parse_exponential()
            
            return COBOLBinaryOp(
                operator='**',  # Store as ** for converter to translate
                left=left,
                right=right,
                line=left.line,
                column=left.column
            )
    
    return left

def parse_multiplicative(self) -> COBOLASTNode:
    """Parse multiplicative expressions (* and /) using truth table approach
    
    TRUTH TABLE: When to stop parsing multiplicative operations
    
    | Current Token | Action | Reason |
    |---------------|--------|--------|
    | ASTERISK/SLASH | Continue parsing | Valid operator |
    | PLUS/MINUS    | Return left | Lower precedence |
    | PERIOD        | Return left | Statement boundary |
    | EOF           | Return left | End of input |
    | RPAREN        | Return left | End of sub-expression |
    | Statement keyword | Return left | Statement boundary |
    | Comparison op | Return left | Lower precedence |
    | Other         | Return left | Unknown token |
    """
    left = self.parse_exponential()
    
    max_iterations = 100  # Safety limit
    iteration = 0
    
    while iteration < max_iterations:
        iteration += 1
        token = self.current_token()
        token_type = token.type
        
        # TRUTH TABLE: Determine action based on current token
        
        # Rule 1: Multiplicative operators → continue parsing
        if token_type in (COBOLTokenType.ASTERISK, COBOLTokenType.SLASH):
            op = '*' if token_type == COBOLTokenType.ASTERISK else '/'
            self.advance()
            
            # ✅ CRITICAL: Check if next token is a boundary before calling parse_primary
            next_token = self.current_token()
            if next_token.type in (COBOLTokenType.PERIOD, COBOLTokenType.EOF):
                self.error(
                    f"Missing operand after '{op}' operator at line {token.line}. "
                    f"Expression is incomplete."
                )
            
            right = self.parse_primary()
            left = COBOLBinaryOp(
                operator=op, 
                left=left, 
                right=right, 
                line=token.line, 
                column=token.column
            )
            continue
        
        # Rule 2: Additive operators → stop (lower precedence)
        if token_type in (COBOLTokenType.PLUS, COBOLTokenType.MINUS):
            return left
        
        # Rule 3: Boundaries → stop and return
        if token_type in (COBOLTokenType.PERIOD, COBOLTokenType.EOF, 
                         COBOLTokenType.RPAREN):
            return left
        
        # Rule 4: Statement keywords → stop and return
        if token_type in (COBOLTokenType.MOVE, COBOLTokenType.ADD,
                         COBOLTokenType.SUBTRACT, COBOLTokenType.MULTIPLY,
                         COBOLTokenType.DIVIDE, COBOLTokenType.COMPUTE,
                         COBOLTokenType.IF, COBOLTokenType.PERFORM,
                         COBOLTokenType.DISPLAY, COBOLTokenType.ACCEPT,
                         COBOLTokenType.TO, COBOLTokenType.GIVING,
                         COBOLTokenType.UNTIL, COBOLTokenType.VARYING):
            return left
        
        # Rule 5: Comparison operators → stop (lower precedence)
        if token_type in (COBOLTokenType.EQUALS_SIGN, COBOLTokenType.EQUAL,
                         COBOLTokenType.GT_SIGN, COBOLTokenType.LT_SIGN,
                         COBOLTokenType.GREATER, COBOLTokenType.LESS,
                         COBOLTokenType.NOT):
            return left
        
        # Rule 6: Logical operators → stop (lower precedence)
        if token_type in (COBOLTokenType.AND, COBOLTokenType.OR):
            return left
        
        # Rule 7: Unknown token → stop
        return left
    
    # Safety: max iterations reached
    print(f"[WARNING] parse_multiplicative hit iteration limit at line {token.line}")
    return left




# Helper function to check if token is a statement boundary
def _is_expression_boundary(self, token_type) -> bool:
    """Centralized truth table for expression boundaries
    
    Returns True if the token marks the end of an expression.
    """
    # Statement enders
    if token_type in (COBOLTokenType.PERIOD, COBOLTokenType.EOF):
        return True
    
    # Sub-expression enders
    if token_type in (COBOLTokenType.RPAREN, COBOLTokenType.COMMA):
        return True
    
    # Statement keywords
    if token_type in (COBOLTokenType.MOVE, COBOLTokenType.ADD,
                     COBOLTokenType.SUBTRACT, COBOLTokenType.MULTIPLY,
                     COBOLTokenType.DIVIDE, COBOLTokenType.COMPUTE,
                     COBOLTokenType.IF, COBOLTokenType.PERFORM,
                     COBOLTokenType.DISPLAY, COBOLTokenType.ACCEPT,
                     COBOLTokenType.TO, COBOLTokenType.GIVING,
                     COBOLTokenType.UNTIL, COBOLTokenType.VARYING,
                     COBOLTokenType.THEN, COBOLTokenType.ELSE):
        return True
    
    return False


def parse_primary(self) -> COBOLASTNode:
    """Parse primary expressions using truth table approach
    
    TRUTH TABLE: Token Type → Action
    | Token Type       | Action |
    |------------------|--------|
    | PLUS/MINUS       | Unary operator |
    | NUMBER_LITERAL   | Number value |
    | STRING_LITERAL   | String value |
    | ALL              | ALL keyword (arrays/strings) |
    | FUNCTION         | Function call |
    | IDENTIFIER       | Variable, array, or qualified name |
    | LPAREN           | Parenthesized expression |
    | Boundary tokens  | Error - incomplete expression |
    """
    token = self.current_token()
    
    print(f"[PARSE_PRIMARY] Line {token.line} Col {token.column}: {token.type.name} = '{token.value}'")
    
    if self.debug:
        print(f"DEBUG parse_primary: token={token.type.name} value={token.value}")
    
    # ============================================================================
    # UNARY OPERATORS
    # ============================================================================
    
    if self.match(COBOLTokenType.PLUS):
        self.advance()
        # Unary + is identity operation, just return the operand
        operand = self.parse_primary()
        return operand
    
    if self.match(COBOLTokenType.MINUS):
        self.advance()
        operand = self.parse_primary()
        return COBOLUnaryOp(operator='-', operand=operand, line=token.line, column=token.column)
    
    # ============================================================================
    # LITERALS
    # ============================================================================
    
    if self.match(COBOLTokenType.NUMBER_LITERAL):
        value = token.value
        self.advance()
        
        # ✅ Handle comma-separated numeric literals (COBOL formatting)
        # Example: 211113411,114311112 represents 211113411.114311112
        while self.match(COBOLTokenType.COMMA):
            saved_pos = self.pos
            self.advance()  # consume comma
            
            if self.match(COBOLTokenType.NUMBER_LITERAL):
                # Continuation - comma acts as decimal point in COBOL
                value += "." + self.current_token().value
                self.advance()
            else:
                # Not a continuation, restore position
                self.pos = saved_pos
                break
        
        return COBOLNumberLiteral(value=value, line=token.line, column=token.column)

    elif self.match(COBOLTokenType.STRING_LITERAL):
        value = token.value
        self.advance()
        return COBOLStringLiteral(value=value, line=token.line, column=token.column)
    
    # ============================================================================
    # ALL KEYWORD
    # ============================================================================
    
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
            if self.debug:
                print(f"WARNING: Standalone ALL keyword at line {token.line}, column {token.column}")
            return COBOLIdentifier(name="ALL", line=token.line, column=token.column)
    
    # ============================================================================
    # FUNCTION CALL
    # ============================================================================
    
    elif self.match(COBOLTokenType.FUNCTION):
        return self.parse_function_call()
    
    # ============================================================================
    # IDENTIFIER (Variable, Array, or Qualified Name)
    # ============================================================================
    
    elif self.match(COBOLTokenType.IDENTIFIER):
        name = token.value
        self.advance()
        
        # Check for qualified name (var OF parent or var IN parent)
        while self.match(COBOLTokenType.OF, COBOLTokenType.IN):
            self.advance()  # Consume OF or IN
            if not self.match(COBOLTokenType.IDENTIFIER):
                self.error("Expected identifier after OF/IN keyword")
            parent_name = self.current_token().value
            self.advance()  # Consume parent identifier
            
            # Build dotted qualification: child.parent
            qualified_name = f"{name}.{parent_name}"
            name = qualified_name
            
            if self.debug:
                print(f"[PARSE_PRIMARY] Qualified: {name}")
        
        # Check for array subscript OR reference modification
        if self.match(COBOLTokenType.LPAREN):
            self.advance()  # Consume first LPAREN
            result = self._parse_subscript_or_refmod(name, token)
            
            # Check for CHAINED reference modification: array(i,j)(start:len)
            # COBOL allows: TABLE(subscripts)(refmod) but NOT TABLE(refmod)(refmod)
            if self.match(COBOLTokenType.LPAREN):
                self.advance()  # Consume second LPAREN
                
                # The second LPAREN must be reference modification
                # Parse it manually here since we already have the base
                
                # Check if starts with COLON
                if self.match(COBOLTokenType.COLON):
                    self.advance()
                    length_expr = None
                    if not self.match(COBOLTokenType.RPAREN):
                        length_expr = self.parse_arithmetic_expression()
                    self.consume(COBOLTokenType.RPAREN)
                    
                    # Return chained refmod with result as base
                    return COBOLReferenceModification(
                        identifier=result,  # Base is array access
                        start=None,
                        length=length_expr,
                        line=token.line,
                        column=token.column
                    )
                
                # Parse start expression
                start_expr = self.parse_arithmetic_expression()
                
                # Must have COLON for reference modification
                if not self.match(COBOLTokenType.COLON):
                    self.error(f"Expected COLON in chained reference modification, got {self.current_token().type.name}")
                
                self.advance()  # Consume COLON
                
                # Parse optional length
                length_expr = None
                if not self.match(COBOLTokenType.RPAREN):
                    length_expr = self.parse_arithmetic_expression()
                
                self.consume(COBOLTokenType.RPAREN)
                
                # Return chained reference modification
                return COBOLReferenceModification(
                    identifier=result,  # Base is array access
                    start=start_expr,
                    length=length_expr,
                    line=token.line,
                    column=token.column
                )
            
            return result
        
        # Simple identifier (not qualified, not array, not refmod)
        return COBOLIdentifier(name=name, line=token.line, column=token.column)
    
    # ============================================================================
    # USAGE KEYWORDS AS IDENTIFIERS (COBOL allows this in some contexts)
    # ============================================================================
    
    elif self.match(
        COBOLTokenType.USAGE,
        COBOLTokenType.COMP, COBOLTokenType.COMP_1, COBOLTokenType.COMP_2, COBOLTokenType.COMP_3,
        COBOLTokenType.COMPUTATIONAL, COBOLTokenType.COMPUTATIONAL_3,
        COBOLTokenType.BINARY, COBOLTokenType.PACKED_DECIMAL, COBOLTokenType.DISPLAY
    ):
        name = token.value
        self.advance()
        if self.debug:
            print(f"WARNING: Using USAGE keyword '{name}' as identifier at line {token.line}")
        return COBOLIdentifier(name=name, line=token.line, column=token.column)
    
    # ============================================================================
    # PARENTHESIZED EXPRESSION
    # ============================================================================
    
    elif self.match(COBOLTokenType.LPAREN):
        self.advance()
        expr = self.parse_or_expression()  # Parse full boolean expression
        self.consume(COBOLTokenType.RPAREN)
        return expr
    
    # ============================================================================
    # ERROR CASES - Expression Boundaries
    # ============================================================================
    
    else:
        token_type = token.type
        
        # TRUTH TABLE: What to do when we can't parse a primary expression
        # 
        # | Token Type | Valid Boundary? | Action |
        # |------------|-----------------|--------|
        # | PERIOD     | YES             | Error - statement incomplete |
        # | COMMA      | YES             | Error - missing expression before comma |
        # | EOF        | YES             | Error |
        # | RPAREN     | YES             | Error (mismatched parens) |
        # | TO/FROM/GIVING | YES        | Error (missing operand) |
        # | Operator   | NO              | Error (can't start expr with operator) |
        # | Other      | NO              | Error (unexpected token) |
        
        if token_type == COBOLTokenType.PERIOD:
            self.error(
                f"Missing expression before PERIOD at line {token.line}. "
                f"Statement appears incomplete."
            )
        
        elif token_type == COBOLTokenType.COMMA:
            # This shouldn't happen if callers check boundaries properly
            self.error(
                f"Missing expression before COMMA at line {token.line}. "
                f"Expected value in list before comma separator."
            )
        
        elif token_type == COBOLTokenType.EOF:
            self.error(f"Unexpected end of file while parsing expression")
        
        elif token_type == COBOLTokenType.RPAREN:
            self.error(f"Unexpected closing parenthesis - expression expected")
        
        elif token_type in (COBOLTokenType.TO, COBOLTokenType.FROM, 
                           COBOLTokenType.GIVING, COBOLTokenType.BY):
            self.error(f"Expected expression before '{token.value}' keyword")
        
        # Operators that can't start an expression (PLUS/MINUS already handled above)
        elif token_type in (COBOLTokenType.ASTERISK, COBOLTokenType.SLASH,
                           COBOLTokenType.EQUALS_SIGN, COBOLTokenType.GT_SIGN,
                           COBOLTokenType.LT_SIGN):
            self.error(f"Unexpected operator '{token.value}' - expected expression")
        
        else:
            self.error(f"Unexpected token in expression: {token.type.name}")
            
    
    
def _parse_subscript_or_refmod(self, identifier_name, token):
    """Parse array subscript OR reference modification after LPAREN
    
    TRUTH TABLE: Subscript vs Reference Modification Detection
    ┌─────────────────────────────────┬──────────────────────────┐
    │ Pattern After LPAREN            │ Type                     │
    ├─────────────────────────────────┼──────────────────────────┤
    │ COLON (starts with :)           │ Reference mod (:length)  │
    │ expr COLON (has colon)          │ Reference mod (start:)   │
    │ expr COMMA/SEMICOLON/expr       │ Array subscript          │
    │ expr RPAREN (single value)      │ Array subscript (1D)     │
    └─────────────────────────────────┴──────────────────────────┘
    
    Examples:
    - var(3:)           → Reference modification from pos 3 to end
    - var(2:5)          → Reference modification pos 2, length 5
    - var(:3)           → Reference modification first 3 chars
    - var(i)            → Array subscript 1D
    - var(i, j)         → Array subscript 2D
    - var(i j k)        → Array subscript 3D (space-separated)
    """
    # Already consumed LPAREN, current token is first element or COLON
    
    # CASE 1: Starts with COLON → Reference modification (:length)
    if self.match(COBOLTokenType.COLON):
        self.advance()  # Consume COLON
        
        # Parse optional length (could be empty for :)
        length_expr = None
        if not self.match(COBOLTokenType.RPAREN):
            length_expr = self.parse_arithmetic_expression()
        
        self.consume(COBOLTokenType.RPAREN)
        
        # Return reference modification with start=None (means position 1)
        return COBOLReferenceModification(
            identifier=identifier_name,
            start=None,
            length=length_expr,
            line=token.line,
            column=token.column
        )
    
    # CASE 2: Parse first expression
    first_expr = self.parse_arithmetic_expression()
    
    # CASE 3: Check what follows the first expression
    if self.match(COBOLTokenType.COLON):
        # REFERENCE MODIFICATION: var(start:length)
        self.advance()  # Consume COLON
        
        # Parse optional length expression
        length_expr = None
        if not self.match(COBOLTokenType.RPAREN):
            length_expr = self.parse_arithmetic_expression()
        
        self.consume(COBOLTokenType.RPAREN)
        
        return COBOLReferenceModification(
            identifier=identifier_name,
            start=first_expr,
            length=length_expr,
            line=token.line,
            column=token.column
        )
    
    # CASE 4: ARRAY SUBSCRIPT - continue parsing subscripts
    subscripts = [first_expr]
    max_subscripts = 7  # COBOL max dimensions
    
    while not self.match(COBOLTokenType.RPAREN):
        # Skip separators (comma, semicolon)
        if self.match(COBOLTokenType.COMMA, COBOLTokenType.SEMICOLON):
            self.advance()
            # Check for trailing separator
            if self.match(COBOLTokenType.RPAREN):
                break
            continue
        
        # Parse next subscript (space-separated or after separator)
        subscripts.append(self.parse_arithmetic_expression())
        
        # Safety check
        if len(subscripts) > max_subscripts:
            self.error(f"Too many array subscripts: {len(subscripts)} (COBOL max is {max_subscripts})")
    
    self.consume(COBOLTokenType.RPAREN)
    
    return COBOLArraySubscript(
        array_name=identifier_name,
        indices=subscripts,
        line=token.line,
        column=token.column
    )

            

def parse_function_call(self) -> COBOLFunctionCall:
    """Parse COBOL intrinsic function call with truth table approach
    
    TRUTH TABLE: Function call structure
    ┌─────────────────────────────────┬──────────────────────┐
    │ Component                       │ Required?            │
    ├─────────────────────────────────┼──────────────────────┤
    │ FUNCTION keyword                │ YES                  │
    │ Function name (identifier)      │ YES                  │
    │ LPAREN                          │ NO (some functions)  │
    │ Arguments (comma-separated)     │ NO (can be empty)    │
    │ RPAREN                          │ YES (if LPAREN)      │
    └─────────────────────────────────┴──────────────────────┘
    
    TRUTH TABLE: Argument parsing loop exit conditions
    ┌─────────────────────────────────┬──────────────────────┐
    │ Current Token                   │ Action               │
    ├─────────────────────────────────┼──────────────────────┤
    │ RPAREN                          │ Break - end of args  │
    │ EOF                             │ Error - unclosed     │
    │ PERIOD                          │ Error - unclosed     │
    │ After parsing arg + no COMMA    │ Expect RPAREN        │
    │ After parsing arg + COMMA       │ Continue to next arg │
    └─────────────────────────────────┴──────────────────────┘
    """
    token = self.current_token()
    self.consume(COBOLTokenType.FUNCTION)
    
    # Parse function name
    if not self.match(COBOLTokenType.IDENTIFIER, COBOLTokenType.UPPER_CASE, 
                      COBOLTokenType.LOWER_CASE):
        self.error("Expected function name after FUNCTION keyword")
    
    function_name = self.current_token().value
    self.advance()
    
    arguments = []
    
    # Parse optional argument list
    if self.match(COBOLTokenType.LPAREN):
        self.consume(COBOLTokenType.LPAREN)
        
        # TRUTH TABLE: Parse arguments until RPAREN
        max_iterations = 100
        iteration = 0
        
        while iteration < max_iterations:
            iteration += 1
            
            # Rule 1: RPAREN → end of arguments
            if self.match(COBOLTokenType.RPAREN):
                break
            
            # Rule 2: EOF or PERIOD → error (unclosed parenthesis)
            if self.match(COBOLTokenType.EOF, COBOLTokenType.PERIOD):
                self.error(
                    f"Unclosed parenthesis in function call to '{function_name}'. "
                    f"Expected ')' but got {self.current_token().type.name}"
                )
            
            # Rule 3: Parse next argument
            arg = self.parse_arithmetic_expression()
            arguments.append(arg)
            
            # Rule 4: After argument, check what comes next
            # | Next Token | Action |
            # |------------|--------|
            # | COMMA      | Continue to next arg |
            # | RPAREN     | Done with args |
            # | Other      | Error |
            
            if self.match(COBOLTokenType.COMMA):
                self.advance()
                # Continue loop to parse next argument
            elif self.match(COBOLTokenType.RPAREN):
                # Will break on next iteration
                continue
            else:
                self.error(
                    f"Expected ',' or ')' after argument in function '{function_name}'. "
                    f"Got {self.current_token().type.name} = '{self.current_token().value}'"
                )
        
        if iteration >= max_iterations:
            print(f"[WARNING] Function argument parsing hit iteration limit at line {token.line}")
        
        self.consume(COBOLTokenType.RPAREN)
    
    return COBOLFunctionCall(
        function_name=function_name,
        arguments=arguments,
        line=token.line,
        column=token.column
    )


def parse_expression(self) -> COBOLASTNode:
    return self.parse_arithmetic_expression()

COBOLMultiProgramParser.parse_variable_decl = parse_variable_decl
COBOLMultiProgramParser._parse_level_and_name = _parse_level_and_name
COBOLMultiProgramParser._parse_condition_name = _parse_condition_name
COBOLMultiProgramParser._parse_level88_values = _parse_level88_values
COBOLMultiProgramParser._parse_level88_single_value = _parse_level88_single_value
COBOLMultiProgramParser._parse_renames_clause = _parse_renames_clause
COBOLMultiProgramParser._skip_whitespace = _skip_whitespace
COBOLMultiProgramParser._parse_redefines = _parse_redefines
COBOLMultiProgramParser._parse_external_global = _parse_external_global
COBOLMultiProgramParser._check_usage_index = _check_usage_index
COBOLMultiProgramParser._create_index_variable = _create_index_variable
COBOLMultiProgramParser._parse_occurs_clause = _parse_occurs_clause
COBOLMultiProgramParser._parse_depending_on = _parse_depending_on
COBOLMultiProgramParser._parse_indexed_by = _parse_indexed_by
COBOLMultiProgramParser._peek_ahead = _peek_ahead
COBOLMultiProgramParser._parse_group_item = _parse_group_item
COBOLMultiProgramParser._parse_elementary_item = _parse_elementary_item
COBOLMultiProgramParser._calculate_decimal_places = _calculate_decimal_places
COBOLMultiProgramParser._check_signed = _check_signed
COBOLMultiProgramParser._parse_usage_type_clause = _parse_usage_type_clause
COBOLMultiProgramParser._parse_sign_clause = _parse_sign_clause
COBOLMultiProgramParser._parse_value_clause = _parse_value_clause
COBOLMultiProgramParser.parse_variable_decl = parse_variable_decl
COBOLMultiProgramParser.parse_pic_clause = parse_pic_clause
COBOLMultiProgramParser.parse_usage_type = parse_usage_type
COBOLMultiProgramParser.parse_condition = parse_condition
COBOLMultiProgramParser.parse_or_expression = parse_or_expression
COBOLMultiProgramParser.parse_and_expression = parse_and_expression
COBOLMultiProgramParser.parse_not_expression = parse_not_expression
COBOLMultiProgramParser.parse_comparison = parse_comparison
COBOLMultiProgramParser.parse_arithmetic_expression = parse_arithmetic_expression
COBOLMultiProgramParser.parse_additive = parse_additive
COBOLMultiProgramParser.parse_multiplicative = parse_multiplicative
COBOLMultiProgramParser.parse_exponential = parse_exponential
COBOLMultiProgramParser.parse_primary = parse_primary
COBOLMultiProgramParser._parse_subscript_or_refmod = _parse_subscript_or_refmod
COBOLMultiProgramParser.parse_function_call = parse_function_call
COBOLMultiProgramParser.parse_expression = parse_expression
COBOLMultiProgramParser._skip_separators = _skip_separators