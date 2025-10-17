#!/usr/bin/env python3

# Copyright (c) 2025 Sean Collins, 2 Paws Machine and Engineering. All rights reserved.
#
# Licensed under the Sean Collins Software License (SCSL). See the LICENSE file in the root directory of this project
# for the full terms and conditions, including restrictions on forking, corporate use, and permissions for private/teaching purposes.

"""
Statement Converter - Handles COBOL statement to Ailang conversion
"""
import sys
import os
from typing import Dict, List, Any, Optional

# Add project root to path
project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '../..'))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

from cobol_frontend.parser.ast_nodes import (
    COBOLDisplay,
    COBOLMove,
    COBOLCompute,
    COBOLArithmetic,
    COBOLIf,
    COBOLPerformTimes,
    COBOLPerformUntil,
    COBOLPerformVarying,
    COBOLPerformParagraph,
    COBOLEvaluate,
    COBOLAccept,
    COBOLCall,
    COBOLStopRun,
    COBOLGoback,
    COBOLExit,
    COBOLStringConcat,
    COBOLUnstring,
    COBOLInspect,
    COBOLMerge,
    COBOLASTNode,
    COBOLIdentifier,
    COBOLArraySubscript,
    COBOLNumberLiteral,
    COBOLStringLiteral,
    COBOLFunctionCall,
)
from ailang_parser.ailang_ast import (
    Assignment,
    FunctionCall,
    Identifier,
    Number,
    String,
    If,
    While,
    ReturnValue,
    RunTask,
)

class StatementConverter:
    """Handles conversion of COBOL statements to Ailang AST"""
    
    def __init__(self, converter):
        self.converter = converter
        self.debug = converter.debug_emitter
        
    def convert_statement(self, stmt: COBOLASTNode, variables: Dict) -> Any:
        """Main statement conversion dispatcher"""
        try:
            if isinstance(stmt, COBOLDisplay):
                print(f"    DEBUG: Converting COBOLDisplay...")
                result = self.convert_display(stmt, variables)
                print(f"    DEBUG: convert_display returned list with {len(result)} items")
                return result
            elif isinstance(stmt, COBOLMove):
                print(f"    DEBUG: Converting COBOLMove...")
                result = self.convert_move(stmt, variables)
                print(f"    DEBUG: convert_move returned: {type(result).__name__}")
                return result
            elif isinstance(stmt, COBOLCompute):
                print(f"    DEBUG: Converting COBOLCompute...")
                result = self.convert_compute(stmt, variables)
                print(f"    DEBUG: convert_compute returned: {type(result).__name__}")
                return result
            elif isinstance(stmt, COBOLArithmetic):
                print(f"    DEBUG: Converting COBOLArithmetic (op={stmt.operation})...")
                result = self.convert_arithmetic(stmt, variables)
                print(f"    DEBUG: convert_arithmetic returned: {type(result).__name__ if result else 'None'}")
                return result
            elif isinstance(stmt, COBOLIf):
                print(f"    DEBUG: Converting COBOLIf...")
                result = self.convert_if(stmt, variables)
                print(f"    DEBUG: convert_if returned: {type(result).__name__}")
                return result
            elif isinstance(stmt, COBOLPerformUntil):
                print(f"    DEBUG: Converting COBOLPerformUntil...")
                result = self.convert_perform_until(stmt, variables)
                print(f"    DEBUG: convert_perform_until returned While")
                return result
            elif isinstance(stmt, COBOLPerformTimes):
                print(f"    DEBUG: Converting COBOLPerformTimes...")
                result = self.convert_perform_times(stmt, variables)
                print(f"    DEBUG: convert_perform_times returned list")
                return result
            elif isinstance(stmt, COBOLPerformVarying):
                print(f"    DEBUG: Converting COBOLPerformVarying...")
                result = self.convert_perform_varying(stmt, variables)
                print(f"    DEBUG: convert_perform_varying returned list")
                return result
            elif isinstance(stmt, COBOLPerformParagraph):
                print(f"    DEBUG: Converting COBOLPerformParagraph...")
                result = self.convert_perform_paragraph(stmt, variables)
                print(f"    DEBUG: convert_perform_paragraph returned: {type(result).__name__}")
                return result
            elif isinstance(stmt, COBOLEvaluate):
                print(f"    DEBUG: Converting COBOLEvaluate...")
                result = self.convert_evaluate(stmt, variables)
                print(f"    DEBUG: convert_evaluate returned: {type(result).__name__}")
                return result
            elif isinstance(stmt, COBOLAccept):
                print(f"    DEBUG: Converting COBOLAccept...")
                result = self.convert_accept(stmt, variables)
                print(f"    DEBUG: convert_accept returned: {type(result).__name__ if not isinstance(result, list) else 'List'}")
                return result
            elif isinstance(stmt, COBOLStopRun):
                if self.converter.debug:
                    print(f"    DEBUG: Converting COBOLStopRun (ends subroutine)...")
                return None
            elif isinstance(stmt, COBOLGoback):
                if self.converter.debug:
                    print(f"    DEBUG: Converting COBOLGoback (ends subroutine)...")
                return None
            elif isinstance(stmt, COBOLCall):
                if self.converter.debug:
                    print(f"    DEBUG: Converting COBOLCall...")
                result = self.convert_call(stmt, variables)
                if self.converter.debug:
                    result_type = "List" if isinstance(result, list) else type(result).__name__
                    print(f"    DEBUG: convert_call returned: {result_type}")
                return result
            elif isinstance(stmt, COBOLExit):
                if self.converter.debug:
                    exit_type = "EXIT PROGRAM" if stmt.is_program else "EXIT"
                    print(f"    DEBUG: Converting {exit_type} (returns None)...")
                return None
            elif isinstance(stmt, COBOLStringConcat):
                if self.converter.debug:
                    print(f"    DEBUG: Converting COBOLStringConcat...")
                result = self.convert_string_concat(stmt, variables)
                print(f"    DEBUG: convert_string_concat returned list with {len(result)} items")
                return result
            elif isinstance(stmt, COBOLUnstring):
                if self.converter.debug:
                    print(f"    DEBUG: Converting COBOLUnstring...")
                result = self.convert_unstring(stmt, variables)
                print(f"    DEBUG: convert_unstring returned list with {len(result)} items")
                return result
            elif isinstance(stmt, COBOLInspect):
                if self.converter.debug:
                    print(f"    DEBUG: Converting COBOLInspect...")
                result = self.convert_inspect(stmt, variables)
                print(f"    DEBUG: convert_inspect returned list with {len(result)} items")
                return result
            elif isinstance(stmt, COBOLMerge):
                if self.converter.debug:
                    print(f"    DEBUG: Converting COBOLMerge...")
                result = self.convert_merge(stmt, variables)
                return result
            else:
                print(f"    DEBUG: Unknown statement type: {type(stmt).__name__}")
                return None
        except Exception as e:
            print(f"    ERROR: Exception in convert_statement for {type(stmt).__name__}: {e}")
            import traceback
            traceback.print_exc()
            return None

    def convert_display(self, stmt: COBOLDisplay, variables: Dict) -> List[Any]:
        """Convert COBOL DISPLAY to Ailang PrintMessage"""
        if not stmt.expressions:
            return [FunctionCall(1, 1, 'PrintMessage', [String(1, 1, "")])]

        string_parts = []

        for expr in stmt.expressions:
            converted_expr = self.converter.expression_converter.convert_expression(expr, variables)

            # Check if this is a variable reference
            if isinstance(expr, COBOLIdentifier):
                var_name = self.converter.normalize_name(expr.name)
                if var_name in variables:
                    var_info = variables[var_name]

                    # Check if it's an edited format (already a string)
                    if var_info.get('is_edited'):
                        string_parts.append(converted_expr)
                    # Format decimal variables for display
                    elif var_info.get('type') == 'Float' and var_info.get('decimal_places'):
                        source_decimals = Number(1, 1, 4)  # FixedPoint always uses 4
                        target_decimals = Number(1, 1, var_info['decimal_places'])
                        formatted = FunctionCall(
                            1, 1,
                            'FormatDecimal',
                            [converted_expr, source_decimals, target_decimals]
                        )
                        string_parts.append(formatted)
                    # Convert integers to strings
                    elif var_info.get('type') == 'Integer':
                        string_parts.append(FunctionCall(1, 1, 'NumberToString', [converted_expr]))
                    else:
                        string_parts.append(converted_expr)
                else:
                    string_parts.append(converted_expr)
            # Check if this is an array subscript (needs special handling)
            elif isinstance(expr, COBOLArraySubscript):
                array_name = self.converter.normalize_name(expr.array_name)
                if array_name in variables:
                    var_info = variables[array_name]
                    # Array elements need type conversion too!
                    if var_info.get('type') == 'Integer':
                        string_parts.append(FunctionCall(1, 1, 'NumberToString', [converted_expr]))
                    elif var_info.get('type') == 'Float' and var_info.get('decimal_places'):
                        source_decimals = Number(1, 1, 4)
                        target_decimals = Number(1, 1, var_info['decimal_places'])
                        formatted = FunctionCall(
                            1, 1,
                            'FormatDecimal',
                            [converted_expr, source_decimals, target_decimals]
                        )
                        string_parts.append(formatted)
                    else:
                        string_parts.append(converted_expr)
                else:
                    string_parts.append(converted_expr)
            elif isinstance(expr, COBOLNumberLiteral):
                string_parts.append(FunctionCall(1, 1, 'NumberToString', [converted_expr]))
            else:
                string_parts.append(converted_expr)

        # Build concatenated string
        if len(string_parts) == 0:
            return [FunctionCall(1, 1, 'PrintMessage', [String(1, 1, "")])]
        elif len(string_parts) == 1:
            return [FunctionCall(1, 1, 'PrintMessage', [string_parts[0]])]
        else:
            result = string_parts[0]
            for part in string_parts[1:]:
                result = FunctionCall(1, 1, 'StringConcat', [result, part])
            return [FunctionCall(1, 1, 'PrintMessage', [result])]


    def convert_move(self, stmt: COBOLMove, variables: Dict):
        """Convert MOVE statement"""
        source = self.converter.expression_converter.convert_expression(stmt.source, variables)
        
        assignments = []
        
        for target in stmt.targets:
            # Check if target is an array element
            if isinstance(target, COBOLArraySubscript):
                array_name = self.converter.normalize_name(target.array_name)
                
                # Check if this variable is a child of an OCCURS parent
                if array_name in self.converter.subscript_parent_map:
                    actual_array_name = self.converter.subscript_parent_map[array_name]
                    if self.converter.debug:
                        print(f"  ArraySet redirect: {array_name}(idx) -> {actual_array_name}(idx)")
                    array_name = actual_array_name
                
                array_ref = self.converter.make_assignment_target(array_name, variables)
                index_expr = self.converter.expression_converter.convert_expression(target.indices[0], variables)
                
                zero_based_index = FunctionCall(1, 1, 'Subtract', [
                    index_expr,
                    Number(1, 1, 1)
                ])
                assignments.append(FunctionCall(1, 1, 'ArraySet', [
                    Identifier(1, 1, array_ref),
                    zero_based_index,
                    source
                ]))
                continue

            # Regular scalar assignment
            if isinstance(target, COBOLIdentifier):
                target_name = self.converter.normalize_name(target.name)
            else:
                target_name = str(self.converter.expression_converter.convert_expression(target, variables))

            # Check if target is an edited format field
            target_source = source
            if isinstance(target, COBOLIdentifier):
                if target_name in variables:
                    var_info = variables[target_name]

                    if var_info.get('is_edited') and var_info.get('edit_format'):
                        if self.converter.debug:
                            print(f"    Applying {var_info['edit_format']['type']} format to {target_name}")

                        source_decimals = 4  # Default FixedPoint precision
                        
                        if isinstance(stmt.source, COBOLIdentifier):
                            source_name = self.converter.normalize_name(stmt.source.name)
                            if source_name in variables:
                                source_var_info = variables[source_name]
                                if source_var_info.get('type') == 'Integer':
                                    source_decimals = 0
                        
                        target_source = self.converter.decimal_support.create_format_function(
                            source, 
                            var_info['edit_format'],
                            source_decimals
                        )

            assignments.append(Assignment(1, 1, self.converter.make_assignment_target(target_name, variables), target_source))

        return assignments 

    
    
    
    def convert_compute(self, stmt: COBOLCompute, variables: Dict) -> Assignment:
        """Convert COMPUTE statement"""
        expression = self.converter.expression_converter.convert_expression(stmt.expression, variables)
        
        # Check if target is an array subscript
        if isinstance(stmt.target, COBOLArraySubscript):
            array_name = self.converter.normalize_name(stmt.target.array_name)
            
            if self.converter.current_pool_name and array_name in variables and variables[array_name].get('occurs'):
                array_ref = f"{self.converter.current_pool_name}.{array_name}"
            else:
                array_ref = array_name
            
            index_expr = self.converter.expression_converter.convert_expression(stmt.target.indices[0], variables)
            zero_based_index = FunctionCall(1, 1, 'Subtract', [
                index_expr,
                Number(1, 1, 1)
            ])
            return FunctionCall(1, 1, 'ArraySet', [
                Identifier(1, 1, array_ref),
                zero_based_index,
                expression
            ])
        
        # Regular scalar compute
        target = self.converter.expression_converter.convert_expression(stmt.target, variables)
        target_name = target.name if isinstance(target, Identifier) else str(target)
        return Assignment(1, 1, self.converter.make_assignment_target(target_name, variables), expression)
    
    def convert_arithmetic(self, stmt: COBOLArithmetic, variables: Dict) -> Optional[Assignment]:
        """Convert COBOL arithmetic statements (ADD, SUBTRACT, etc.) with truth table approach
        
        TRUTH TABLE: Arithmetic Statement Structure
        ┌─────────────────────────┬──────────────────────────────────┐
        │ Statement Pattern       │ Operands & Target                │
        ├─────────────────────────┼──────────────────────────────────┤
        │ ADD A B GIVING C        │ ops=[A,B], target=C              │
        │ ADD A TO B              │ ops=[A], target=B (in-place)     │
        │ SUBTRACT A FROM B       │ ops=[A], target=B (in-place)     │
        │ MULTIPLY A BY B         │ ops=[A], target=B (in-place)     │
        │ DIVIDE A BY B           │ ops=[A], target=B (in-place)     │
        │ DIVIDE A INTO B         │ ops=[A], target=B (in-place)     │
        └─────────────────────────┴──────────────────────────────────┘
        
        TRUTH TABLE: Target Expression Handling
        ┌─────────────────────┬────────────────────────────────────┐
        │ Target Type         │ Action                             │
        ├─────────────────────┼────────────────────────────────────┤
        │ Variable (has .name)│ Extract .name, create Assignment   │
        │ Number literal      │ Warning + skip (invalid COBOL)     │
        │ String literal      │ Warning + skip (invalid COBOL)     │
        │ Other expression    │ Warning + skip (unexpected type)   │
        └─────────────────────┴────────────────────────────────────┘
        """
        from ailang_parser.ast_modules.ast_expressions import Number, String
        
        # ═══════════════════════════════════════════════════════════════
        # PHASE 1: Convert operands to Ailang expressions
        # ═══════════════════════════════════════════════════════════════
        all_operands = [
            self.converter.expression_converter.convert_expression(op, variables) 
            for op in stmt.operands
        ]
        
        # ═══════════════════════════════════════════════════════════════
        # PHASE 2: Determine target based on GIVING vs in-place
        # ═══════════════════════════════════════════════════════════════
        
        # Truth table: Which node is the target?
        # ┌─────────────────┬──────────────────────────────────────┐
        # │ Has GIVING?     │ Target Node                          │
        # ├─────────────────┼──────────────────────────────────────┤
        # │ YES             │ stmt.giving                          │
        # │ NO              │ stmt.target                          │
        # └─────────────────┴──────────────────────────────────────┘
        
        if stmt.giving:
            # GIVING form: ADD A B GIVING C
            target_node = stmt.giving
            
            # For ADD/SUBTRACT with GIVING, target also becomes an operand
            if stmt.operation in ['ADD', 'SUBTRACT'] and stmt.target:
                all_operands.append(
                    self.converter.expression_converter.convert_expression(stmt.target, variables)
                )
        else:
            # In-place form: ADD A TO B (B is both operand and target)
            target_node = stmt.target
            all_operands.insert(0, 
                self.converter.expression_converter.convert_expression(target_node, variables)
            )

        if not all_operands:
            return None

        # ═══════════════════════════════════════════════════════════════
        # PHASE 3: Check if decimal arithmetic is needed
        # ═══════════════════════════════════════════════════════════════
        is_decimal = False
        for op_node in stmt.operands + [stmt.target, stmt.giving]:
            if op_node and self.converter.decimal_support.is_decimal_operation(op_node, None, variables):
                is_decimal = True
                break
        
        func_name = self.converter.decimal_support.get_arithmetic_function(stmt.operation, is_decimal)
        
        # ═══════════════════════════════════════════════════════════════
        # PHASE 4: Build expression tree (left-to-right chaining)
        # ═══════════════════════════════════════════════════════════════
        expr = all_operands[0]
        for op in all_operands[1:]:
            expr = FunctionCall(1, 1, func_name, [expr, op])
        
        # ═══════════════════════════════════════════════════════════════
        # PHASE 5: Convert target with truth table validation
        # ═══════════════════════════════════════════════════════════════
        target_expr = self.converter.expression_converter.convert_expression(target_node, variables)
        
        # Truth table: Validate and extract target
        # ┌─────────────────────┬──────────────────────────────────┐
        # │ Condition           │ Action                           │
        # ├─────────────────────┼──────────────────────────────────┤
        # │ has .name attribute │ Use .name → create Assignment    │
        # │ isinstance Number   │ Warning → return None (skip)     │
        # │ isinstance String   │ Warning → return None (skip)     │
        # │ other type          │ Warning → return None (skip)     │
        # └─────────────────────┴──────────────────────────────────┘
        
        # Rule 1: Normal case - Variable with .name attribute
        if hasattr(target_expr, 'name'):
            target_name = target_expr.name
            return Assignment(
                1, 1, 
                self.converter.make_assignment_target(target_name, variables), 
                expr
            )
        
        # Rule 2: Literal number (invalid COBOL - graceful skip)
        elif isinstance(target_expr, Number):
            print(f"WARNING: Arithmetic {stmt.operation} has literal number as target - skipping statement")
            return None
        
        # Rule 3: Literal string (invalid COBOL - graceful skip)
        elif isinstance(target_expr, String):
            print(f"WARNING: Arithmetic {stmt.operation} has string literal as target - skipping statement")
            return None
        
        # Rule 4: Unknown type (unexpected - graceful skip)
        else:
            print(f"WARNING: Arithmetic {stmt.operation} target is {type(target_expr).__name__}, expected Variable - skipping statement")
            return None
    
    def convert_if(self, stmt: COBOLIf, variables: Dict):
        """Convert IF statement with debug"""
        condition = self.converter.expression_converter.convert_expression(stmt.condition, variables)
        
        then_body = []
        for then_stmt in stmt.then_statements:
            converted = self.convert_statement(then_stmt, variables)
            if isinstance(converted, list):
                then_body.extend(converted)
            elif converted:
                then_body.append(converted)
        
        else_body = []
        if stmt.else_statements:
            for else_stmt in stmt.else_statements:
                converted = self.convert_statement(else_stmt, variables)
                if isinstance(converted, list):
                    else_body.extend(converted)
                elif converted:
                    else_body.append(converted)
        
        if_stmt = If(1, 1, condition, then_body, else_body if else_body else None)
        
        # Wrap with debug - returns the If wrapped with debug message
        condition_str = getattr(stmt.condition, 'name', 'condition')
        wrapped = self.debug.wrap_if(if_stmt, condition=condition_str)
        
        # wrap_if returns [debug_msg, if_stmt] so we need to return just the if_stmt
        # BUT the calling code expects to handle lists properly
        # So return the wrapped result which is already a list
        return wrapped if isinstance(wrapped, list) else if_stmt
    
    
    def convert_call(self, stmt: COBOLCall, variables: Dict) -> List:
        """Convert CALL statement with debug"""
        statements = []
        
        # Determine called program name
        if isinstance(stmt.program, COBOLStringLiteral):
            program_name = stmt.program.value.strip('"\'')
        elif isinstance(stmt.program, COBOLIdentifier):
            var_name = self.converter.normalize_name(stmt.program.name)
            if var_name in variables:
                program_name = var_name
            else:
                program_name = stmt.program.name
        else:
            program_name = "UNKNOWN"
        
        called_name = self.converter.normalize_name(program_name)
        
        # Check if called program has linkage
        if called_name in self.converter.program_linkage_params:
            pool_name = f"COBOL_{called_name}_LINKAGE"
            callee_params = self.converter.program_linkage_params[called_name]
            caller_params = [self.converter.normalize_name(p.name if hasattr(p, 'name') else str(p)) 
                            for p in stmt.using_params] if stmt.using_params else []
            
            # Copy caller params to linkage pool
            for i, caller_param in enumerate(caller_params):
                if i >= len(callee_params):
                    break
                
                if caller_param in variables:
                    caller_var = self.converter.expression_converter.convert_expression(
                        COBOLIdentifier(1, 1, caller_param), variables
                    )
                    caller_var_name = caller_var.name
                else:
                    caller_var_name = caller_param
                
                pool_var = f"{pool_name}.{callee_params[i]}"
                statements.append(Assignment(1, 1, pool_var, Identifier(1, 1, caller_var_name)))
            
            # Call the program
            full_name = called_name
            run_task = FunctionCall(1, 1, 'RunTask', [Identifier(1, 1, full_name)])
            
            # Wrap with debug - returns [debug_msg, run_task]
            wrapped = self.debug.wrap_call(run_task, program=program_name)
            # Flatten the wrapped result into statements list
            statements.extend(self.debug.flatten(wrapped))
            
            # Copy results back
            for i, caller_param in enumerate(caller_params):
                if i >= len(callee_params):
                    break
                
                if caller_param in variables:
                    caller_target = self.converter.make_assignment_target(caller_param, variables)
                else:
                    caller_target = caller_param
                
                pool_var = Identifier(1, 1, f"{pool_name}.{callee_params[i]}")
                statements.append(Assignment(1, 1, caller_target, pool_var))
        else:
            # Simple call without parameters
            run_task = FunctionCall(1, 1, 'RunTask', [Identifier(1, 1, called_name)])
            
            # Wrap with debug - returns [debug_msg, run_task]
            wrapped = self.debug.wrap_call(run_task, program=program_name)
            statements.extend(self.debug.flatten(wrapped))
        
        return statements


    def convert_perform_paragraph(self, stmt: COBOLPerformParagraph, variables: Dict):
        """Convert PERFORM paragraph-name with debug"""
        para_name = self.converter.normalize_name(stmt.paragraph_name)
        full_name = f"{self.converter.current_program_name}_{para_name}"
        
        run_task = FunctionCall(1, 1, 'RunTask', [Identifier(1, 1, full_name)])
        
        # Wrap with debug - returns [debug_msg, run_task]
        wrapped = self.debug.wrap_perform(run_task, para_name=stmt.paragraph_name)
        
        # Return wrapped result (which is a list)
        return wrapped if isinstance(wrapped, list) else run_task
        
    def convert_perform_times(self, stmt: COBOLPerformTimes, variables: Dict):
        """Convert PERFORM...TIMES to Ailang while loop"""
        counter_var = f"TEMP_TIMES_CTR_{id(stmt)}"
        
        times_value = self.converter.expression_converter.convert_expression(stmt.times_expr, variables)
        
        init_counter = Assignment(1, 1, counter_var, Number(1, 1, 0))
        
        loop_body = []
        
        if stmt.paragraph_name:
            para_name = self.converter.normalize_name(stmt.paragraph_name)
            full_name = f"{self.converter.current_program_name}_{para_name}"
            loop_body.append(FunctionCall(1, 1, 'RunTask', [Identifier(1, 1, full_name)]))
        elif stmt.statements:
            for inline_stmt in stmt.statements:
                converted = self.convert_statement(inline_stmt, variables)
                if isinstance(converted, list):
                    loop_body.extend(converted)
                elif converted is not None:
                    loop_body.append(converted)
        
        loop_body.append(
            Assignment(1, 1, counter_var,
                      FunctionCall(1, 1, 'Add', [
                          Identifier(1, 1, counter_var),
                          Number(1, 1, 1)
                      ]))
        )
        
        condition = FunctionCall(1, 1, 'LessThan', [
            Identifier(1, 1, counter_var),
            times_value
        ])
        
        while_loop = While(1, 1, condition, loop_body)
        
        return [init_counter, while_loop]
    
    def convert_perform_until(self, stmt: COBOLPerformUntil, variables: Dict):
        """Convert PERFORM UNTIL to Ailang WhileLoop"""
        # Invert condition: UNTIL x > 5 becomes While Not(x > 5)
        ailang_condition = FunctionCall(1, 1, 'Not', [
            self.converter.expression_converter.convert_expression(stmt.condition, variables)
        ])
        
        loop_body = []
        
        if stmt.paragraph_name:
            para_name = self.converter.normalize_name(stmt.paragraph_name)
            full_name = f"{self.converter.current_program_name}_{para_name}"
            loop_body.append(FunctionCall(1, 1, 'RunTask', [Identifier(1, 1, full_name)]))
        else:
            for inline_stmt in stmt.statements:
                converted = self.convert_statement(inline_stmt, variables)
                if isinstance(converted, list):
                    loop_body.extend(converted)
                elif converted is not None:
                    loop_body.append(converted)
        
        return While(1, 1, ailang_condition, loop_body)

    def convert_perform_varying(self, stmt: COBOLPerformVarying, variables: Dict) -> Any:
        """Convert PERFORM VARYING"""
        var_name = self.converter.normalize_name(stmt.variable)
        var_target = self.converter.make_assignment_target(var_name, variables)
        
        from_value = self.converter.expression_converter.convert_expression(stmt.from_expr, variables)
        by_value = self.converter.expression_converter.convert_expression(stmt.by_expr, variables)
        condition = self.converter.expression_converter.convert_expression(stmt.until_condition, variables)
        
        negated_condition = FunctionCall(1, 1, 'Not', [condition])
        
        loop_body = []
        
        if stmt.statements is not None:
            for loop_stmt in stmt.statements:
                converted = self.convert_statement(loop_stmt, variables)
                if isinstance(converted, list):
                    loop_body.extend(converted)
                elif converted:
                    loop_body.append(converted)
        elif stmt.paragraph_name:
            para_name = self.converter.normalize_name(stmt.paragraph_name)
            full_name = f"{self.converter.current_program_name}_{para_name}"
            loop_body.append(FunctionCall(1, 1, 'RunTask', [Identifier(1, 1, full_name)]))
        
        loop_body.append(
            Assignment(1, 1, var_target,
                    FunctionCall(1, 1, 'Add', [
                        Identifier(1, 1, var_target),
                        by_value
                    ]))
        )
        
        return [
            Assignment(1, 1, var_target, from_value),
            While(1, 1, negated_condition, loop_body)
        ]
    
    def convert_evaluate(self, stmt: COBOLEvaluate, variables: Dict) -> If:
        """Convert EVALUATE to nested If/ElseIf chain"""
        subject = self.converter.expression_converter.convert_expression(stmt.subject, variables)
        
        # Determine comparison function
        comparison_func = 'EqualTo'
        
        if isinstance(stmt.subject, COBOLIdentifier):
            var_name = self.converter.normalize_name(stmt.subject.name)
            if var_name in variables and variables[var_name]['type'] == 'String':
                comparison_func = 'StringEquals'
        elif isinstance(stmt.subject, COBOLStringLiteral):
            comparison_func = 'StringEquals'
        elif isinstance(stmt.subject, COBOLFunctionCall):
            if stmt.subject.function_name in ['UPPER-CASE', 'LOWER-CASE']:
                comparison_func = 'StringEquals'
        
        result = None
        
        for i in range(len(stmt.when_clauses) - 1, -1, -1):
            when_clause = stmt.when_clauses[i]
            
            then_body = []
            for when_stmt in when_clause.statements:
                converted = self.convert_statement(when_stmt, variables)
                if isinstance(converted, list):
                    then_body.extend(converted)
                elif converted:
                    then_body.append(converted)
            
            if when_clause.value is None:
                # WHEN OTHER
                if result is None:
                    result = then_body[0] if len(then_body) == 1 else then_body
                else:
                    result = [If(1, 1, FunctionCall(1, 1, 'EqualTo', [Number(1, 1, 1), Number(1, 1, 1)]), then_body, [result] if not isinstance(result, list) else result)]
            else:
                value = self.converter.expression_converter.convert_expression(when_clause.value, variables)
                condition = FunctionCall(1, 1, comparison_func, [subject, value])
                
                if result is None:
                    result = If(1, 1, condition, then_body, None)
                else:
                    result = If(1, 1, condition, then_body, [result] if not isinstance(result, list) else result)
        
        return result
    
    def convert_accept(self, stmt: COBOLAccept, variables: Dict):
        """Convert ACCEPT with debug"""
        var_name = self.converter.normalize_name(stmt.variable)
        
        input_call = FunctionCall(1, 1, 'GetUserInput', [])
        
        if var_name in variables and variables[var_name]['type'] == 'Integer':
            input_value = FunctionCall(1, 1, 'StringToNumber', [input_call])
        else:
            input_value = input_call
        
        target_var = self.converter.make_assignment_target(var_name, variables)
        assignment = Assignment(1, 1, target_var, input_value)
        
        # Wrap with debug - returns [debug_msg, assignment]
        wrapped = self.debug.wrap_accept(assignment, variable=stmt.variable)
        
        # Return wrapped result (which is a list)
        return wrapped if isinstance(wrapped, list) else assignment


    def convert_string_concat(self, stmt: COBOLStringConcat, variables: Dict) -> List:
        """Convert STRING statement with debug"""
        statements = []
        
        parts = []
        for field in stmt.source_fields:
            parts.append(self.converter.expression_converter.convert_expression(field, variables))
        
        if len(parts) == 0:
            concat_result = String(1, 1, "")
        elif len(parts) == 1:
            concat_result = parts[0]
        else:
            concat_result = parts[0]
            for part in parts[1:]:
                concat_result = FunctionCall(1, 1, 'StringConcat', [concat_result, part])
        
        target_var = self.converter.normalize_name(stmt.target)
        target_ref = self.converter.make_assignment_target(target_var, variables)
        
        assignment = Assignment(1, 1, target_ref, concat_result)
        
        # Wrap with debug - returns [debug_msg, assignment]
        wrapped = self.debug.wrap_string(assignment, target=stmt.target)
        statements.extend(self.debug.flatten(wrapped))
        
        return statements

    def convert_unstring(self, stmt: COBOLUnstring, variables: Dict) -> List:
        """Convert UNSTRING statement with debug"""
        statements = []
        
        source_expr = self.converter.expression_converter.convert_expression(stmt.source, variables)
        
        delimiter_expr = self.converter.expression_converter.convert_expression(
            stmt.delimiters[0], variables
        ) if stmt.delimiters else String(1, 1, " ")
        
        use_all = stmt.delimiter_all_flags[0] if stmt.delimiter_all_flags else False
        
        split_result = FunctionCall(1, 1, "CStringSplit", [
            source_expr, 
            delimiter_expr, 
            Number(1, 1, 1 if use_all else 0)
        ])
        
        temp_var = f"TEMP_UNSTRING_PARTS_{id(stmt)}"
        statements.append(Assignment(1, 1, temp_var, split_result))
        
        for i, target in enumerate(stmt.targets):
            normalized_target = self.converter.normalize_name(target)
            target_with_prefix = self.converter.make_assignment_target(normalized_target, variables)
            
            get_call = FunctionCall(1, 1, 'ArrayGet', [Identifier(1, 1, temp_var), Number(1, 1, i)])
            
            if normalized_target in variables and variables[normalized_target]['type'] == 'Integer':
                get_call = FunctionCall(1, 1, 'StringToNumber', [get_call])
            
            statements.append(Assignment(1, 1, target_with_prefix, get_call))
        
        if stmt.count:
            counter_name = self.converter.normalize_name(stmt.count)
            counter_ref = self.converter.make_assignment_target(counter_name, variables)
            length_call = FunctionCall(1, 1, "ArrayLength", [Identifier(1, 1, temp_var)])
            statements.append(Assignment(1, 1, counter_ref, length_call))
        
        # Wrap entire UNSTRING with debug - returns just a debug message (not wrapping statements)
        source_str = getattr(stmt.source, 'name', 'source')
        msg = self.debug.wrap_unstring(None, source=source_str)
        if msg:
            statements.insert(0, msg)
        
        return statements


    def convert_inspect(self, stmt: COBOLInspect, variables: Dict) -> List:
        """Convert INSPECT statement with debug"""
        statements = []
        target_var = self.converter.normalize_name(stmt.target)
        pattern_expr = self.converter.expression_converter.convert_expression(stmt.pattern, variables)
        
        if stmt.operation == 'REPLACING':
            replacement_expr = self.converter.expression_converter.convert_expression(stmt.replacement, variables)
            
            if self.converter.current_pool_name and target_var in variables:
                target_read = Identifier(1, 1, f"{self.converter.current_pool_name}.{target_var}")
            else:
                target_read = Identifier(1, 1, target_var)
            
            replace_call = FunctionCall(1, 1, 'StringReplaceAll', 
                                        [target_read, pattern_expr, replacement_expr])
            
            target_write = self.converter.make_assignment_target(target_var, variables)
            assignment = Assignment(1, 1, target_write, replace_call)
            
            # Wrap with debug - returns [debug_msg, assignment]
            wrapped = self.debug.wrap_inspect(assignment, target=stmt.target, operation='REPLACING')
            statements.extend(self.debug.flatten(wrapped))
        
        elif stmt.operation == 'TALLYING':
            counter_var = self.converter.normalize_name(stmt.counter)
            
            if self.converter.current_pool_name and target_var in variables:
                target_read = Identifier(1, 1, f"{self.converter.current_pool_name}.{target_var}")
            else:
                target_read = Identifier(1, 1, target_var)
            
            count_call = FunctionCall(1, 1, 'StringCount', [target_read, pattern_expr])
            
            counter_write = self.converter.make_assignment_target(counter_var, variables)
            assignment = Assignment(1, 1, counter_write, count_call)
            
            # Wrap with debug - returns [debug_msg, assignment]
            wrapped = self.debug.wrap_inspect(assignment, target=stmt.target, operation='TALLYING')
            statements.extend(self.debug.flatten(wrapped))
        
        return statements
    
    def convert_merge(self, stmt: COBOLMerge, variables: Dict):
        """
        Convert MERGE to Ailang.
        For now: stub implementation that calls a FileMerge library function.
        """
        # Generate placeholder merge call
        merge_call = FunctionCall(
            1, 1,
            'FileMerge_Sort',
            [
                String(1, 1, stmt.target_file),
                String(1, 1, ','.join(stmt.input_files)),
                String(1, 1, stmt.giving_file if stmt.giving_file else stmt.output_procedure or '')
            ]
        )
        
        # Wrap with debug message
        debug_msg = self.debug.message(f"MERGE {stmt.target_file}", level=2)
        
        return [debug_msg, merge_call] if debug_msg else [merge_call]

    def convert_stop_run(self, stmt: COBOLStopRun) -> None:
        """Convert STOP RUN to program termination"""
        return None

    def convert_goback(self, stmt: COBOLGoback) -> None:
        """Convert GOBACK to return"""
        return None

    def convert_exit(self, stmt: COBOLExit) -> None:
        """Convert EXIT to program termination"""
        return None