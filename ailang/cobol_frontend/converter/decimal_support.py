#!/usr/bin/env python3

# Copyright (c) 2025 Sean Collins, 2 Paws Machine and Engineering. All rights reserved.
#
# Licensed under the Sean Collins Software License (SCSL). See the LICENSE file in the root directory of this project
# for the full terms and conditions, including restrictions on forking, corporate use, and permissions for private/teaching purposes.

"""
Decimal Support - Handles COBOL decimal/FixedPoint arithmetic
UPDATED: Now passes source_decimals to format functions
"""
import sys
import os
from typing import Dict

# Add project root to path
project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '../..'))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

from cobol_frontend.parser.ast_nodes import COBOLIdentifier
from ailang_parser.ailang_ast import FunctionCall, Number


class DecimalSupport:
    """Handles decimal arithmetic and FixedPoint operations"""
    
    def __init__(self, converter):
        self.converter = converter
    
    def is_decimal_operation(self, left, right, variables: Dict) -> bool:
        """Check if operation involves decimal variables"""
        # Check if left operand is a decimal variable
        if isinstance(left, COBOLIdentifier):
            var_name = self.converter.normalize_name(left.name)
            if var_name in variables:
                var_info = variables[var_name]
                if var_info.get('type') == 'Float':
                    return True
        
        # Check if right operand is a decimal variable
        if isinstance(right, COBOLIdentifier):
            var_name = self.converter.normalize_name(right.name)
            if var_name in variables:
                var_info = variables[var_name]
                if var_info.get('type') == 'Float':
                    return True
        
        return False

    def get_arithmetic_function(self, operation: str, is_decimal: bool) -> str:
        """Get the appropriate arithmetic function name"""
        if is_decimal:
            # Map to FixedPoint library functions
            decimal_ops = {
                'ADD': 'FixedPoint.Add',
                'SUBTRACT': 'FixedPoint.Subtract',
                'MULTIPLY': 'FixedPoint.Multiply',
                'DIVIDE': 'FixedPoint.Divide'
            }
            return decimal_ops.get(operation, 'Add')
        else:
            # Regular integer operations
            int_ops = {
                'ADD': 'Add',
                'SUBTRACT': 'Subtract',
                'MULTIPLY': 'Multiply',
                'DIVIDE': 'Divide'
            }
            return int_ops.get(operation, 'Add')

    def convert_decimal_value(self, value_str: str, decimal_places: int) -> int:
        """Convert COBOL decimal value to FixedPoint integer"""
        FIXEDPOINT_SCALE = 10000  # FixedPoint library uses 4 decimal places
        
        # Handle sign separately
        is_negative = value_str.startswith('-')
        value_str = value_str.lstrip('+-').strip('"').strip("'")
        
        if '.' in value_str:
            # Has explicit decimal point: "123.45"
            parts = value_str.split('.')
            integer_part = int(parts[0]) if parts[0] else 0
            decimal_part = parts[1] if len(parts) > 1 else ''
            
            # Pad or truncate decimal part to match scale
            decimal_part = decimal_part.ljust(decimal_places, '0')[:decimal_places]
            
            # Build COBOL-scaled value (unsigned first)
            scale = 10 ** decimal_places
            cobol_value = integer_part * scale + (int(decimal_part) if decimal_part else 0)
            
            # Apply sign AFTER calculation
            if is_negative:
                cobol_value = -cobol_value
        else:
            # No decimal point, treat as integer: "123"
            scale = 10 ** decimal_places
            cobol_value = int(value_str) * scale
            
            # Apply sign for integer case too
            if is_negative:
                cobol_value = -cobol_value
        
        # Convert from COBOL scale to FixedPoint scale
        scale_multiplier = FIXEDPOINT_SCALE // (10 ** decimal_places)
        fixedpoint_value = cobol_value * scale_multiplier
        
        return fixedpoint_value

    def create_format_function(self, value_expr, format_info: dict, source_decimals: int = 4) -> 'FunctionCall':
        """
        Create appropriate Ailang function call for formatting
        
        Args:
            value_expr: The value expression to format
            format_info: Format information from parse_edit_format
            source_decimals: How many decimal places in the source value (default 4 for FixedPoint)
        """
        format_type = format_info['type']
        decimals = format_info.get('decimals', 0)
        width = format_info.get('width', 4)
        
        if format_type == 'currency':
            # FormatCurrency(value, source_decimals, width, decimals, symbol, has_commas, float_symbol)
            from ailang_parser.ailang_ast import String
            return FunctionCall(1, 1, 'FormatCurrency', [
                value_expr,
                Number(1, 1, source_decimals),  # ✅ NEW: Pass source decimal precision
                Number(1, 1, width),
                Number(1, 1, decimals),
                String(1, 1, format_info.get('symbol', '$')),
                Number(1, 1, 1 if format_info.get('has_commas') else 0),
                Number(1, 1, 1 if format_info.get('float_symbol') else 0)
            ])
        
        elif format_type == 'zero-suppress':
            # FormatZeroSuppress(value, source_decimals, width, decimals)
            return FunctionCall(1, 1, 'FormatZeroSuppress', [
                value_expr,
                Number(1, 1, source_decimals),  # ✅ NEW: Pass source decimal precision
                Number(1, 1, width),
                Number(1, 1, decimals)
            ])
        
        elif format_type == 'asterisk':
            # FormatAsteriskFill(value, source_decimals, width, decimals)
            return FunctionCall(1, 1, 'FormatAsteriskFill', [
                value_expr,
                Number(1, 1, source_decimals),  # ✅ NEW: Pass source decimal precision
                Number(1, 1, width),
                Number(1, 1, decimals)
            ])
        
        elif format_type == 'indicator':
            # FormatWithIndicator(value, indicator, source_decimals, width, decimals)
            from ailang_parser.ailang_ast import String
            return FunctionCall(1, 1, 'FormatWithIndicator', [
                value_expr,
                String(1, 1, format_info.get('symbol', 'CR')),
                Number(1, 1, source_decimals),  # ✅ NEW: Pass source decimal precision
                Number(1, 1, width),
                Number(1, 1, decimals)
            ])
        
        elif format_type == 'signed':
            # FormatSigned(value, source_decimals, width, decimals, position)
            from ailang_parser.ailang_ast import String
            sign_pos = 'trailing' if format_info.get('symbol') == '-' else 'leading'
            return FunctionCall(1, 1, 'FormatSigned', [
                value_expr,
                Number(1, 1, source_decimals),  # ✅ NEW: Pass source decimal precision
                Number(1, 1, width),
                Number(1, 1, decimals),
                String(1, 1, sign_pos)
            ])
        
        else:
            # Simple format with commas
            if format_info.get('has_commas'):
                return FunctionCall(1, 1, 'FormatWithCommas', [
                    value_expr,
                    Number(1, 1, source_decimals),  # ✅ NEW: Pass source decimal precision
                    Number(1, 1, width),
                    Number(1, 1, decimals)
                ])
            else:
                # Just convert to string - no formatting needed
                return FunctionCall(1, 1, 'NumberToString', [value_expr])