#!/usr/bin/env python3

# Copyright (c) 2025 Sean Collins, 2 Paws Machine and Engineering. All rights reserved.
#
# Licensed under the Sean Collins Software License (SCSL). See the LICENSE file in the root directory of this project
# for the full terms and conditions, including restrictions on forking, corporate use, and permissions for private/teaching purposes.

"""
Type System - Handles COBOL type inference, PIC clause parsing, and format handling
"""
import re
import sys
import os
from typing import List, Optional, Dict

# Add project root to path
project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '../..'))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

from cobol_frontend.parser.ast_nodes import COBOLVariableDecl
from ailang_parser.ailang_ast import Number, String

class TypeSystem:
    """Handles type inference, PIC clause parsing, and edited format detection"""
    
    def __init__(self, converter):
        self.converter = converter
    
    def calculate_pic_size(self, pic_clause: str) -> int:
        """Calculate the storage size of a PIC clause for DISPLAY usage."""
        if not pic_clause:
            return 0
        
        pic_upper = pic_clause.upper()
        
        # Remove non-storage characters
        pic_upper = pic_upper.replace('S', '').replace('V', '')
        
        size = 0
        # Find all instances of X(n) or 9(n)
        for match in re.finditer(r'([X9Z])\((\d+)\)', pic_upper):
            size += int(match.group(2))
        
        # Remove the handled parts to count remaining characters
        pic_upper = re.sub(r'[X9Z]\(\d+\)', '', pic_upper)
        
        # Count remaining characters like X, 9, Z, $, etc.
        size += len(pic_upper)
        
        return size

    def get_redefines_values(self, redefines_target: str, original_declarations: List[COBOLVariableDecl], 
                        redefines_decl: COBOLVariableDecl) -> List[str]:
        """Extract values from REDEFINES target"""
        target_name = self.converter.normalize_name(redefines_target)
        
        def search_for_target(decls):
            for decl in decls:
                if self.converter.normalize_name(decl.name) == target_name:
                    # Case 1: Target has children with values
                    if decl.children:
                        values = []
                        for child in decl.children:
                            if child.value:
                                values.append(child.value)
                        if values:
                            return values
                    
                    # Case 2: Target is single field - split the value
                    if decl.value and redefines_decl.occurs_count:
                        element_size = self.calculate_pic_size(redefines_decl.pic_clause)
                        value = decl.value.strip('"\'')
                        
                        # Split the value into chunks
                        values = []
                        for i in range(redefines_decl.occurs_count):
                            start = i * element_size
                            end = start + element_size
                            if start < len(value):
                                values.append(value[start:end])
                        return values
                    
                    return []
                # Search in children recursively
                if decl.children:
                    result = search_for_target(decl.children)
                    if result:
                        return result
            return None
        
        result = search_for_target(original_declarations)
        return result if result is not None else []

    def flatten_variable_declarations(self, declarations: List[COBOLVariableDecl]) -> List[COBOLVariableDecl]:
        """Flatten hierarchical COBOL variable declarations"""
        # ✅ Guard against None input
        if not declarations:
            return []
        
        result = []
        
        def visit(decl: COBOLVariableDecl):
            """Recursively visit variable declarations"""
            if decl.level == 88:
                return
            
            if decl.pic_clause:
                result.append(decl)
            elif decl.occurs_count:
                result.append(decl)
            elif decl.children:  # 🔧 NEW: Include group items with children
                result.append(decl)
            
            if decl.children:
                for child in decl.children:
                    visit(child)
        
        for decl in declarations:
            visit(decl)
        
        return result
  

    def infer_type(self, pic_clause: Optional[str], decimal_places: Optional[int]) -> str:
        """Infer Ailang type from PIC clause and decimal places"""
        if not pic_clause:
            return 'Integer'
        
        pic_upper = pic_clause.upper()
        
        if decimal_places is not None and decimal_places > 0:
            return 'Float'
        
        if 'X' in pic_upper or 'A' in pic_upper:
            return 'String'
        elif '9' in pic_upper:
            return 'Integer'
        else:
            return 'Integer'

    def get_storage_info(self, pic_clause: Optional[str], usage_type: Optional[str], 
                    is_signed: bool, decimal_places: Optional[int]) -> dict:
        """Determine how to store the variable based on PIC and USAGE"""
        info = {
            'ailang_type': 'Integer',
            'storage': 'DISPLAY',
            'is_signed': is_signed,
            'precision': decimal_places or 0
        }
        
        # Determine Ailang type
        if pic_clause:
            pic_upper = pic_clause.upper()
            if 'X' in pic_upper or 'A' in pic_upper:
                info['ailang_type'] = 'String'
            elif decimal_places and decimal_places > 0:
                info['ailang_type'] = 'Float'
            else:
                info['ailang_type'] = 'Integer'
        
        # Determine storage format
        if usage_type:
            usage_upper = usage_type.upper()
            if usage_upper in ['COMP', 'COMP-1', 'COMP-2', 'BINARY', 'COMPUTATIONAL']:
                info['storage'] = 'BINARY'
            elif usage_upper in ['COMP-3', 'PACKED-DECIMAL', 'COMPUTATIONAL-3']:
                info['storage'] = 'PACKED'
        
        return info

    def is_edited_format(self, pic_clause: str) -> bool:
        """Check if PIC clause is display-edited format"""
        if not pic_clause:
            return False
        
        pic_upper = pic_clause.upper()
        
        # Check for edit characters
        edit_chars = ['$', 'Z', '*', '+', '-', 'CR', 'DB']
        
        for char in edit_chars:
            if char in pic_upper:
                return True
        
        # Check for insertion characters (comma not in V9 context)
        if ',' in pic_clause and 'V' not in pic_upper:
            return True
        
        return False

    def parse_edit_format(self, pic_clause: str) -> dict:
        """Parse display-edited PIC clause to extract format info"""
        pic_upper = pic_clause.upper()
        
        # Calculate total field width
        width = 0
        width += pic_clause.count('9')
        width += pic_upper.count('Z')
        width += pic_clause.count('*')
        width += pic_clause.count('$')
        
        # Count decimal places (after period)
        decimals = 0
        if '.' in pic_clause:
            after_period = pic_clause.split('.')[1]
            decimals = after_period.count('9')
        
        format_info = {
            'type': 'simple',
            'decimals': decimals,
            'width': width,
            'has_commas': ',' in pic_clause,
            'symbol': '',
            'float_symbol': False
        }
        
        # Detect format type
        if '$' in pic_clause:
            format_info['type'] = 'currency'
            format_info['symbol'] = '$'
            format_info['float_symbol'] = pic_clause.count('$') > 1
        
        elif 'Z' in pic_upper:
            format_info['type'] = 'zero-suppress'
        
        elif '*' in pic_clause:
            format_info['type'] = 'asterisk'
        
        elif 'CR' in pic_upper:
            format_info['type'] = 'indicator'
            format_info['symbol'] = 'CR'
        
        elif 'DB' in pic_upper:
            format_info['type'] = 'indicator'
            format_info['symbol'] = 'DB'
        
        elif '+' in pic_clause or '-' in pic_clause:
            format_info['type'] = 'signed'
            format_info['symbol'] = '+' if '+' in pic_clause else '-'
        
        return format_info

    def get_initial_value_node(self, var_info: Dict):
        """Helper to get an AST node for a variable's initial value
        
        TRUTH TABLE: Value Type Detection and Conversion
        ┌──────────────────────────┬───────────────┬─────────────────────────┐
        │ Input Value              │ Variable Type │ Output                  │
        ├──────────────────────────┼───────────────┼─────────────────────────┤
        │ None/Empty/Whitespace    │ Any           │ Default by type         │
        │ Edited format (PIC w/$Z) │ Any           │ String("")              │
        │ ZERO/ZEROS/ZEROES        │ String        │ String("0")             │
        │ ZERO/ZEROS/ZEROES        │ Integer/Float │ Number(0)               │
        │ SPACE/SPACES             │ Any           │ String(" ")             │
        │ QUOTE/QUOTES             │ Any           │ String('"')             │
        │ HIGH-VALUE(S)            │ Any           │ String("\xFF")          │
        │ LOW-VALUE(S)             │ Any           │ String("\x00")          │
        │ ALL "literal"            │ Any           │ String(literal repeated)│
        │ Numeric string           │ Float         │ FixedPoint value        │
        │ Numeric string           │ Integer       │ Number(int)             │
        │ "Quoted string"          │ String        │ String(unquoted)        │
        │ Identifier (XXXXX031)    │ Any           │ String(as-is)           │
        │ Unrecognized             │ Any           │ Default by type         │
        └──────────────────────────┴───────────────┴─────────────────────────┘
        """
        # ═══════════════════════════════════════════════════════════════
        # STEP 1: Extract and validate input
        # ═══════════════════════════════════════════════════════════════
        value_str = var_info.get('value', None)
        var_type = var_info.get('type', 'String')
        
        # TRUTH TABLE ROW 1: None, empty, or whitespace-only value
        if not value_str or (isinstance(value_str, str) and not value_str.strip()):
            # Return type-appropriate default
            if var_type == 'String':
                return String(1, 1, "")
            else:
                return Number(1, 1, 0)
        
        # Normalize to string for comparison
        value_str = str(value_str).strip()
        value_upper = value_str.upper()
        
        # ═══════════════════════════════════════════════════════════════
        # TRUTH TABLE ROW 2: Edited format (always string)
        # ═══════════════════════════════════════════════════════════════
        if var_info.get('is_edited'):
            return String(1, 1, "")
        
        # ═══════════════════════════════════════════════════════════════
        # TRUTH TABLE ROWS 3-8: COBOL Figurative Constants
        # ═══════════════════════════════════════════════════════════════
        
        # ROW 3: ZERO/ZEROS/ZEROES
        if value_upper in ['ZERO', 'ZEROS', 'ZEROES']:
            if var_type == 'String':
                return String(1, 1, "0")
            else:
                return Number(1, 1, 0)
        
        # ROW 4: SPACE/SPACES
        if value_upper in ['SPACE', 'SPACES']:
            return String(1, 1, " ")
        
        # ROW 5: QUOTE/QUOTES
        if value_upper in ['QUOTE', 'QUOTES']:
            return String(1, 1, '"')
        
        # ROW 6: HIGH-VALUE(S)
        if value_upper in ['HIGH-VALUE', 'HIGH-VALUES']:
            return String(1, 1, "\xFF")
        
        # ROW 7: LOW-VALUE(S)
        if value_upper in ['LOW-VALUE', 'LOW-VALUES']:
            return String(1, 1, "\x00")
        
        # ROW 8: ALL "literal" pattern
        if value_upper.startswith('ALL '):
            # Extract the literal after ALL
            literal_part = value_str[4:].strip()
            # Remove quotes if present
            if literal_part.startswith('"') and literal_part.endswith('"'):
                literal_part = literal_part[1:-1]
            elif literal_part.startswith("'") and literal_part.endswith("'"):
                literal_part = literal_part[1:-1]
            
            # For ALL, we just use the literal once (runtime will handle repetition)
            return String(1, 1, literal_part)
        
        # ═══════════════════════════════════════════════════════════════
        # TRUTH TABLE ROW 9-10: Numeric values
        # ═══════════════════════════════════════════════════════════════
        
        # ROW 9: Float with decimal places
        if var_type == 'Float' and var_info.get('decimal_places'):
            try:
                fixedpoint_value = self.converter.decimal_support.convert_decimal_value(
                    value_str, var_info['decimal_places']
                )
                return Number(1, 1, fixedpoint_value)
            except (ValueError, AttributeError):
                # Fallback to 0.0 if conversion fails
                return Number(1, 1, 0)
        
        # ROW 10: Integer
        elif var_type == 'Integer':
            try:
                # Remove any decimal part and whitespace
                clean_value = value_str.split('.')[0].strip()
                # Handle empty string after cleaning
                if not clean_value or clean_value == '':
                    return Number(1, 1, 0)
                # Handle signed numbers
                if clean_value.startswith(('+', '-')):
                    return Number(1, 1, int(clean_value))
                else:
                    return Number(1, 1, int(clean_value))
            except (ValueError, AttributeError):
                # Fallback to 0 if conversion fails
                return Number(1, 1, 0)
        
        # ═══════════════════════════════════════════════════════════════
        # TRUTH TABLE ROW 11-12: String values
        # ═══════════════════════════════════════════════════════════════
        
        # ROW 11: Quoted string literal
        elif var_type == 'String':
            # Remove quotes if present
            if value_str.startswith('"') and value_str.endswith('"'):
                return String(1, 1, value_str[1:-1])
            elif value_str.startswith("'") and value_str.endswith("'"):
                return String(1, 1, value_str[1:-1])
            else:
                # ROW 12: Unquoted string (like XXXXX031 identifier)
                return String(1, 1, value_str)
        
        # ═══════════════════════════════════════════════════════════════
        # TRUTH TABLE ROW 13: Fallback default by type
        # ═══════════════════════════════════════════════════════════════
        else:
            if var_type == 'String':
                return String(1, 1, "")
            else:
                return Number(1, 1, 0)

    def format_storage_display(self, var_name: str, var_info: dict) -> str:
        """Format variable storage info for debug output"""
        parts = [var_name + ":"]
        parts.append(var_info['type'])
        
        if var_info.get('is_signed'):
            parts.append("SIGNED")
        
        if var_info.get('decimal_places'):
            parts.append(f"(V{var_info['decimal_places']})")
        
        if var_info.get('occurs'):
            parts.append(f"OCCURS {var_info['occurs']}")
        
        if var_info.get('storage') and var_info['storage'] != 'DISPLAY':
            parts.append(f"STORAGE {var_info['storage']}")
        
        parts.append(f"= {var_info.get('value', '0')}")
        
        return " ".join(parts)