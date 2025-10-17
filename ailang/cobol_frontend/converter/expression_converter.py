#!/usr/bin/env python3

# Copyright (c) 2025 Sean Collins, 2 Paws Machine and Engineering. All rights reserved.
#
# Licensed under the Sean Collins Software License (SCSL). See the LICENSE file in the root directory of this project
# for the full terms and conditions, including restrictions on forking, corporate use, and permissions for private/teaching purposes.

"""
Expression Converter - Handles COBOL expression to Ailang conversion
"""
import sys
import os
from typing import Dict, Any

# Add project root to path
project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '../..'))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

from cobol_frontend.parser.ast_nodes import (
    COBOLBinaryOp,
    COBOLUnaryOp,
    COBOLIdentifier,
    COBOLNumberLiteral,
    COBOLStringLiteral,
    COBOLArraySubscript,
    COBOLFunctionCall,
)
from ailang_parser.ailang_ast import FunctionCall, Identifier, Number, String


class ExpressionConverter:
    """Handles conversion of COBOL expressions to Ailang AST"""
    
    def __init__(self, converter):
        self.converter = converter
        self.operator_mappings = {
            '+': 'Add',
            '-': 'Subtract',
            '*': 'Multiply',
            '/': 'Divide',
            '=': 'EqualTo',
            '>': 'GreaterThan',
            '<': 'LessThan',
            '>=': 'GreaterEqual',
            '<=': 'LessEqual',
            'NOT =': 'NotEqual',
            'NOT >': lambda l, r: FunctionCall(1, 1, 'Not', [FunctionCall(1, 1, 'GreaterThan', [l, r])]),
            'NOT <': lambda l, r: FunctionCall(1, 1, 'Not', [FunctionCall(1, 1, 'LessThan', [l, r])]),
        }
    
    def convert_expression(self, expr, variables: Dict) -> Any:
        """Convert a COBOL expression to Ailang AST"""
        if isinstance(expr, COBOLBinaryOp):
            # Check if it's an arithmetic operator for COMPUTE
            if expr.operator in ['+', '-', '*', '/', '**']:
                return self.convert_binary_op(expr, variables)
            else:
                # Handle comparison operators
                left = self.convert_expression(expr.left, variables)
                right = self.convert_expression(expr.right, variables)
                func_name = self.operator_mappings.get(expr.operator, 'EqualTo')
                return FunctionCall(1, 1, func_name, [left, right])
        
        elif isinstance(expr, COBOLUnaryOp):
            operand = self.convert_expression(expr.operand, variables)
            if expr.operator == '-':
                return FunctionCall(1, 1, 'Subtract', [Number(1, 1, 0), operand])
            else:
                return operand
        
        elif isinstance(expr, COBOLIdentifier):
            name_upper = expr.name.upper()
    
            # Handle COBOL figurative constants
            if name_upper in ['SPACES', 'SPACE']:
                return String(1, 1, " ")
            elif name_upper in ['ZEROS', 'ZEROES', 'ZERO']:
                return Number(1, 1, 0)
            elif name_upper in ['QUOTES', 'QUOTE']:
                return String(1, 1, '"')
            elif name_upper in ['HIGH-VALUES', 'HIGH-VALUE']:
                return String(1, 1, "\xFF")
            elif name_upper in ['LOW-VALUES', 'LOW-VALUE']:
                return String(1, 1, "\x00")
    
            name = self.converter.normalize_name(expr.name)
    
            # Check if this is a linkage parameter (uses its own pool)
            if name in variables and variables[name].get('is_linkage'):
                pool_name = f"COBOL_{self.converter.current_program_name}_LINKAGE"
                return Identifier(expr.line, expr.column, f"{pool_name}.{name}")
            
            # Check if we're using a variable pool
            if self.converter.current_pool_name and name in variables:
                return Identifier(expr.line, expr.column, f"{self.converter.current_pool_name}.{name}")
            
            return Identifier(expr.line, expr.column, name)
        
        elif isinstance(expr, COBOLNumberLiteral):
            if '.' in expr.value:
                return Number(1, 1, int(float(expr.value)))
            else:
                return Number(1, 1, int(expr.value))
        
        elif isinstance(expr, COBOLStringLiteral):
            return String(1, 1, expr.value)
        
        elif isinstance(expr, COBOLArraySubscript):
            return self.convert_array_subscript(expr, variables)
        
        elif isinstance(expr, COBOLFunctionCall):
            return self.convert_function_call(expr, variables)
        
        else:
            return Number(1, 1, 0)

    def convert_binary_op(self, expr: COBOLBinaryOp, variables: Dict) -> FunctionCall:
        """Convert binary operations in COMPUTE expressions"""
        left = self.convert_expression(expr.left, variables)
        right = self.convert_expression(expr.right, variables)
        
        # Map operator to operation name
        op_map = {
            '+': 'ADD',
            '-': 'SUBTRACT',
            '*': 'MULTIPLY',
            '/': 'DIVIDE',
            '**': 'POWER'
        }
        
        operation = op_map.get(expr.operator, 'ADD')
        
        # Check if this operation involves decimal variables
        is_decimal = self.converter.decimal_support.is_decimal_operation(expr.left, expr.right, variables)
        
        # Get the appropriate function name
        if expr.operator == '**':
            # Power doesn't have a FixedPoint version
            func_name = 'Power'
        else:
            # Use helper to get correct function (FixedPoint or regular)
            func_name = self.converter.decimal_support.get_arithmetic_function(operation, is_decimal)
        
        # ✅ NEW: If using FixedPoint arithmetic, convert any Integer operands to FixedPoint
        if is_decimal and 'FixedPoint' in func_name:
            # Check if left operand is Integer and needs conversion
            if isinstance(expr.left, COBOLIdentifier):
                left_name = self.converter.normalize_name(expr.left.name)
                if left_name in variables and variables[left_name].get('type') == 'Integer':
                    # Convert Integer to FixedPoint: multiply by 10000
                    left = FunctionCall(1, 1, 'Multiply', [left, Number(1, 1, 10000)])
            
            # Check if right operand is Integer and needs conversion
            if isinstance(expr.right, COBOLIdentifier):
                right_name = self.converter.normalize_name(expr.right.name)
                if right_name in variables and variables[right_name].get('type') == 'Integer':
                    # Convert Integer to FixedPoint: multiply by 10000
                    right = FunctionCall(1, 1, 'Multiply', [right, Number(1, 1, 10000)])
        
        return FunctionCall(1, 1, func_name, [left, right])

    def convert_array_subscript(self, node: COBOLArraySubscript, variables: Dict):
        """Convert COBOL array subscript to Ailang ArrayGet"""
        array_name = self.converter.normalize_name(node.array_name)
        
        # Check if this is a condition name with array subscript
        if array_name in self.converter.condition_names:
            cond_info = self.converter.condition_names[array_name]
            parent_var = cond_info['parent']
            
            # Build reference to parent variable with proper pool prefix
            if parent_var in variables:
                if variables[parent_var].get('is_linkage'):
                    pool_name = f"COBOL_{self.converter.current_program_name}_LINKAGE"
                    parent_ref = f"{pool_name}.{parent_var}"
                elif self.converter.current_pool_name:
                    parent_ref = f"{self.converter.current_pool_name}.{parent_var}"
                else:
                    parent_ref = parent_var
            else:
                # Fallback: use is_linkage flag from condition info
                if cond_info['is_linkage']:
                    pool_name = f"COBOL_{self.converter.current_program_name}_LINKAGE"
                    parent_ref = f"{pool_name}.{parent_var}"
                elif self.converter.current_pool_name:
                    parent_ref = f"{self.converter.current_pool_name}.{parent_var}"
                else:
                    parent_ref = parent_var
            
            # Convert index expression
            index_expr = self.convert_expression(node.indices[0], variables)
            zero_based_index = FunctionCall(1, 1, 'Subtract', [
                index_expr,
                Number(1, 1, 1)
            ])
            
            # Build: ArrayGet(parent, index) = value
            array_access = FunctionCall(1, 1, 'ArrayGet', [
                Identifier(1, 1, parent_ref),
                zero_based_index
            ])
            
            value_str = cond_info['value'].strip('"\'')
            value_node = String(1, 1, value_str)
            
            return FunctionCall(1, 1, 'EqualTo', [array_access, value_node])
        
        # Regular array subscript - check for subscript parent redirect
        if array_name in self.converter.subscript_parent_map:
            actual_array_name = self.converter.subscript_parent_map[array_name]
            if self.converter.debug:
                print(f"  Subscript redirect: {array_name}(idx) -> {actual_array_name}(idx)")
            array_name = actual_array_name
        
        # ✅ FIX: Always qualify with pool when available
        # Build pool-prefixed array reference
        if array_name in variables and variables[array_name].get('is_linkage'):
            pool_name = f"COBOL_{self.converter.current_program_name}_LINKAGE"
            array_ref = f"{pool_name}.{array_name}"
        elif self.converter.current_pool_name and array_name in variables:
            # Qualify with pool regardless of 'occurs' flag
            array_ref = f"{self.converter.current_pool_name}.{array_name}"
        else:
            # Only use bare name if not in any pool
            array_ref = array_name
        
        index_expr = self.convert_expression(node.indices[0], variables)
        zero_based_index = FunctionCall(1, 1, 'Subtract', [
            index_expr,
            Number(1, 1, 1)
        ])
        
        return FunctionCall(1, 1, 'ArrayGet', [
            Identifier(1, 1, array_ref),
            zero_based_index
        ])


    def convert_identifier(self, node: COBOLIdentifier, variables: Dict) -> Identifier:
        """Convert COBOL identifier to Ailang identifier"""
        var_name = self.converter.normalize_name(node.name)
        
        # ✅ Check if this is a condition name (level 88)
        if var_name in self.converter.condition_names:
            # This is a condition name - expand to comparison
            cond_info = self.converter.condition_names[var_name]
            parent_var = cond_info['parent']
            
            # Build reference to parent variable with proper pool prefix
            if cond_info['is_linkage']:
                pool_name = f"COBOL_{self.converter.current_program_name}_LINKAGE"
                parent_ref = Identifier(1, 1, f"{pool_name}.{parent_var}")
            elif self.converter.current_pool_name and parent_var in variables:
                parent_ref = Identifier(1, 1, f"{self.converter.current_pool_name}.{parent_var}")
            else:
                parent_ref = Identifier(1, 1, parent_var)
            
            # Create comparison: parent = value
            value_str = cond_info['value'].strip('"\'')
            value_node = String(1, 1, value_str)
            
            return FunctionCall(1, 1, 'EqualTo', [parent_ref, value_node])
        
        # 🔧 FIX: Check if this is a LINKAGE variable
        if var_name in variables and variables[var_name].get('is_linkage'):
            pool_name = f"COBOL_{self.converter.current_program_name}_LINKAGE"
            return Identifier(1, 1, f"{pool_name}.{var_name}")
        
        # Regular variable - existing logic
        if self.converter.current_pool_name and var_name in variables:
            return Identifier(1, 1, f"{self.converter.current_pool_name}.{var_name}")
        else:
            return Identifier(1, 1, var_name)
            
        
    


    def convert_function_call(self, expr: COBOLFunctionCall, variables: Dict) -> FunctionCall:
        """Convert COBOL function calls"""
        func_name = expr.function_name
        
        if func_name == 'UPPER-CASE':
            arg = self.convert_expression(expr.arguments[0], variables)
            return FunctionCall(1, 1, 'StringToUpper', [arg])
        
        elif func_name == 'NUMVAL':
            # String to number conversion
            arg = self.convert_expression(expr.arguments[0], variables)
            return FunctionCall(1, 1, 'StringToNumber', [arg])
        
        elif func_name == 'ANNUITY' or func_name == 'PRESENT-VALUE':
            # Financial functions - stub them for now (return 0)
            return Number(1, 1, 0)
        
        else:
            raise ValueError(f"Unsupported FUNCTION: {expr.function_name}")
    
    def _is_string_expr(self, left, right, variables: Dict):
        """Check if either operand is a string"""
        # Check if either is a string literal
        if isinstance(left, COBOLStringLiteral) or isinstance(right, COBOLStringLiteral):
            return True
        # Check if either is a string function call
        if isinstance(left, COBOLFunctionCall) and left.function_name in ['UPPER-CASE', 'LOWER-CASE']:
            return True
        if isinstance(right, COBOLFunctionCall) and right.function_name in ['UPPER-CASE', 'LOWER-CASE']:
            return True
        # Check if either is a string variable
        if isinstance(left, COBOLIdentifier):
            var_name = self.converter.normalize_name(left.name)
            if var_name in variables and variables[var_name]['type'] == 'String':
                return True
        if isinstance(right, COBOLIdentifier):
            var_name = self.converter.normalize_name(right.name)
            if var_name in variables and variables[var_name]['type'] == 'String':
                return True
        return False