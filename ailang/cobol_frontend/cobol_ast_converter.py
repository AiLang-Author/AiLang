#!/usr/bin/env python3

from __future__ import annotations

"""
COBOL to Ailang AST Converter - Complete with EVALUATE
"""

import sys
import os
from typing import List, Optional, Any, Dict

project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

from ailang_parser.ailang_ast import (
    Program, Function, If, While, ForEvery, ReturnValue,
    Assignment, FunctionCall, Identifier, Number, String, SubRoutine,
    PrintMessage, RunTask
)
from ailang_parser.ast_modules.ast_pools import Pool, ResourceItem

from cobol_parser import (
    COBOLCompilationUnit, COBOLProgram, COBOLDataDivision, COBOLVariableDecl,
    COBOLProcedureDivision, COBOLParagraph, COBOLDisplay, COBOLArraySubscript, COBOLCall,
    COBOLMove, COBOLCompute, COBOLArithmetic, COBOLIf, COBOLFunctionCall, COBOLPerformTimes,
    COBOLPerformUntil, COBOLPerformParagraph, COBOLPerformTimes, COBOLPerformVarying,
    COBOLEvaluate, COBOLWhenClause, COBOLAccept,  
    COBOLStopRun, COBOLGoback, COBOLExit, COBOLBinaryOp, COBOLUnaryOp,
    COBOLIdentifier, COBOLNumberLiteral, COBOLStringLiteral,
    COBOLASTNode, COBOLStringConcat, COBOLUnstring, COBOLInspect  
)

class COBOLToAilangMultiProgramConverter:
    """
    Converts COBOL compilation unit (possibly multiple programs) to Ailang AST.
    
    Key improvements:
    - Each PROGRAM-ID gets its own SubRoutine
    - Variables are scoped per-program
    - Paragraphs inherit their parent program's variables
    - Main() calls the first program
    """
    
    def __init__(self, debug=False):
        self.debug = debug
        # These will be reset for each program
        self.variables = {}
        self.paragraphs = {}
        self.program_linkage_params = {}  # NEW: Track linkage params per program
        self.program_ws_pools = {}        # NEW: Track WORKING-STORAGE pools
        # Global accumulator for all subroutines
        self.all_subroutines = []
        self.string_literals = {} 
        self.operator_mappings = {
            '+': 'Add',
            '-': 'Subtract', # PATCH 4: This will be changed to a lambda
            '*': 'Multiply',
            '/': 'Divide',
            '=': 'EqualTo',
            '>': 'GreaterThan',      # Add this
            '<': 'LessThan',          # Add this
            '>=': 'GreaterEqual',
            '<=': 'LessEqual',
            'NOT =': 'NotEqual',
            'NOT >': lambda l, r: FunctionCall(1, 1, 'Not', [FunctionCall(1, 1, 'GreaterThan', [l, r])]),
            'NOT <': lambda l, r: FunctionCall(1, 1, 'Not', [FunctionCall(1, 1, 'LessThan', [l, r])]),
        }

    def normalize_name(self, cobol_name: str) -> str:
        return cobol_name.replace('-', '_')
    
    def infer_type(self, pic_clause: Optional[str], decimal_places: Optional[int]) -> str:
        """Infer Ailang type from PIC clause and decimal places"""
        if not pic_clause:
            return 'Integer'
        
        pic_upper = pic_clause.upper()
        
        # NEW: Check for decimal places first
        if decimal_places is not None and decimal_places > 0:
            return 'Float'
        
        if 'X' in pic_upper or 'A' in pic_upper:
            return 'String'
        elif '9' in pic_upper:
            return 'Integer'
        else:
            return 'Integer'
    
    def is_decimal_operation(self, left, right, variables: Dict) -> bool:
        """
        Check if operation involves decimal variables.
        Returns True if either operand is a decimal type.
        """
        # Check if left operand is a decimal variable
        if isinstance(left, COBOLIdentifier):
            var_name = self.normalize_name(left.name)
            if var_name in variables:
                var_info = variables[var_name]
                if var_info.get('type') == 'Float':
                    return True
        
        # Check if right operand is a decimal variable
        if isinstance(right, COBOLIdentifier):
            var_name = self.normalize_name(right.name)
            if var_name in variables:
                var_info = variables[var_name]
                if var_info.get('type') == 'Float':
                    return True
        
        return False

    def get_arithmetic_function(self, operation: str, is_decimal: bool) -> str:
        """
        Get the appropriate arithmetic function name.
        Uses FixedPoint operations for decimal types.
        """
        if is_decimal:
            # Map to FixedPoint library functions
            decimal_ops = {
                'ADD': 'FixedPoint.Add',
                'SUBTRACT': 'FixedPoint.Subtract',
                'MULTIPLY': 'FixedPoint.Multiply',
                'DIVIDE': 'FixedPoint.Divide'
            }
            return decimal_ops.get(operation, 'Add')  # Default to Add
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
        """
        Convert COBOL decimal value to FixedPoint integer.
        
        COBOL scale: 10^decimal_places
        FixedPoint scale: 10000 (always 4 decimals)
        
        E.g., "123.45" with 2 decimal places:
        - COBOL internal: 12345 (scale 100)
        - FixedPoint needs: 1234500 (scale 10000)
        - Conversion: multiply by 100
        
        E.g., "-123.45" with 2 decimal places:
        - COBOL internal: -12345 (scale 100)
        - FixedPoint needs: -1234500 (scale 10000)
        """
        FIXEDPOINT_SCALE = 10000  # FixedPoint library uses 4 decimal places
        
        # ✅ FIX: Handle sign separately
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
            
            # ✅ FIX: Apply sign AFTER calculation
            if is_negative:
                cobol_value = -cobol_value
        else:
            # No decimal point, treat as integer: "123"
            scale = 10 ** decimal_places
            cobol_value = int(value_str) * scale
            
            # ✅ FIX: Apply sign for integer case too
            if is_negative:
                cobol_value = -cobol_value
        
        # Convert from COBOL scale to FixedPoint scale
        scale_multiplier = FIXEDPOINT_SCALE // (10 ** decimal_places)
        fixedpoint_value = cobol_value * scale_multiplier
        
        return fixedpoint_value

    def get_storage_info(self, pic_clause: Optional[str], usage_type: Optional[str], 
                    is_signed: bool, decimal_places: Optional[int]) -> dict:
        """
        Determine how to store the variable based on PIC and USAGE.
        
        Returns: {
            'ailang_type': 'Integer' | 'Float' | 'String',
            'storage': 'BINARY' | 'PACKED' | 'DISPLAY',
            'is_signed': bool,
            'precision': int (for decimals)
        }
        """
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
        """
        Check if PIC clause is display-edited format.
        
        Display-edited formats use special characters:
        - $ (currency)
        - Z (zero suppression)
        - * (asterisk fill)
        - + - (signs)
        - CR DB (credit/debit)
        - Comma/period insertion
        
        Returns True if any edit characters present.
        """
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
        """
        Parse display-edited PIC clause to extract format info.
        
        Returns: {
            'type': 'currency' | 'zero-suppress' | 'asterisk' | 'signed' | 'indicator',
            'decimals': int,
            'width': int,  # NEW: total field width
            'has_commas': bool,
            'symbol': str (e.g., '$', 'CR', 'DB'),
            'float_symbol': bool  (True for $$$$, False for $999)
        }
        """
        pic_upper = pic_clause.upper()
        
        # Calculate total field width (count all digit positions)
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
            'width': width,  # NEW
            'has_commas': ',' in pic_clause,
            'symbol': '',
            'float_symbol': False
        }
        
        # Detect format type
        if '$' in pic_clause:
            format_info['type'] = 'currency'
            format_info['symbol'] = '$'
            # Check if floating ($$$$) or fixed ($999)
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

    def create_format_function(self, value_expr, format_info: dict) -> 'FunctionCall':
        """
        Create appropriate Ailang function call for formatting.
        
        Args:
            value_expr: The value to format (Ailang AST node)
            format_info: Dict from parse_edit_format()
        
        Returns:
            FunctionCall node for the formatter
        """
        format_type = format_info['type']
        decimals = format_info.get('decimals', 0)
        width = format_info.get('width', 4)
        
        if format_type == 'currency':
            # FormatCurrency(value, width, decimals, symbol, has_commas, float_symbol)
            return FunctionCall(1, 1, 'FormatCurrency', [
                value_expr,
                Number(1, 1, width),
                Number(1, 1, decimals),
                String(1, 1, format_info.get('symbol', '$')),
                Number(1, 1, 1 if format_info.get('has_commas') else 0),
                Number(1, 1, 1 if format_info.get('float_symbol') else 0)
            ])
        
        elif format_type == 'zero-suppress':
            # FormatZeroSuppress(value, width, decimals)
            return FunctionCall(1, 1, 'FormatZeroSuppress', [
                value_expr,
                Number(1, 1, width),
                Number(1, 1, decimals)
            ])
        
        elif format_type == 'asterisk':
            # FormatAsteriskFill(value, width, decimals)
            return FunctionCall(1, 1, 'FormatAsteriskFill', [
                value_expr,
                Number(1, 1, width),
                Number(1, 1, decimals)
            ])
        
        elif format_type == 'indicator':
            # FormatWithIndicator(value, indicator, width, decimals)
            return FunctionCall(1, 1, 'FormatWithIndicator', [
                value_expr,
                String(1, 1, format_info.get('symbol', 'CR')),
                Number(1, 1, width),
                Number(1, 1, decimals)
            ])
        
        elif format_type == 'signed':
            # FormatSigned(value, width, decimals, sign_position)
            sign_pos = 'trailing' if format_info.get('symbol') == '-' else 'leading'
            return FunctionCall(1, 1, 'FormatSigned', [
                value_expr,
                Number(1, 1, width),
                Number(1, 1, decimals),
                String(1, 1, sign_pos)
            ])
        
        else:
            # Simple format with commas
            if format_info.get('has_commas'):
                return FunctionCall(1, 1, 'FormatWithCommas', [
                    value_expr,
                    Number(1, 1, width),
                    Number(1, 1, decimals)
                ])
            else:
                # Just convert to string
                return FunctionCall(1, 1, 'NumberToString', [value_expr])
        
        
    def format_storage_display(self, var_name: str, var_info: dict) -> str:
        """
        Format variable storage info for debug output.
        
        Example: "TOTAL: Float SIGNED (V2) USAGE COMP-3 = 0"
        """
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

    def convert(self, ast_node) -> Program:
        """
        Convert COBOL AST to Ailang AST.
        
        Now handles both:
        - COBOLCompilationUnit (multiple programs)
        - COBOLProgram (single program - backward compatible)
        """
        if isinstance(ast_node, COBOLCompilationUnit):
            return self.convert_compilation_unit(ast_node)
        elif isinstance(ast_node, COBOLProgram):
            # Backward compatible: wrap single program in compilation unit
            compilation_unit = COBOLCompilationUnit(programs=[ast_node])
            return self.convert_compilation_unit(compilation_unit)
        else:
            raise ValueError(f"Expected COBOLCompilationUnit or COBOLProgram, got {type(ast_node)}")
    
    def convert_compilation_unit(self, unit: COBOLCompilationUnit) -> Program:
        """
        Convert a complete COBOL compilation unit (possibly multiple programs).
        
        Each PROGRAM-ID becomes a SubRoutine with its own variable scope.
        """
        # PRE-SCAN: Build linkage parameter registry first
        if self.debug:
            print("\n=== PRE-SCAN: Building linkage parameter registry ===")
        
        for cobol_program in unit.programs:
            prog_name = self.normalize_name(cobol_program.program_id)
            
            # Extract linkage section params
            if cobol_program.data_division and hasattr(cobol_program.data_division, 'linkage_section') and cobol_program.data_division.linkage_section:
                linkage_params = []
                for var_decl in cobol_program.data_division.linkage_section:
                    if isinstance(var_decl, COBOLVariableDecl):
                        var_name = self.normalize_name(var_decl.name)
                        linkage_params.append(var_name)
                
                if linkage_params:
                    self.program_linkage_params[prog_name] = linkage_params
                    if self.debug:
                        print(f"  Registered {prog_name}: {linkage_params}")
        
        if self.debug:
            print(f"\n=== Linkage registry complete: {list(self.program_linkage_params.keys())} ===\n")
        
        # NOW convert each program
        if self.debug:
            print(f"\n{'='*70}")
            print(f"Converting {len(unit.programs)} COBOL program(s) to Ailang")
            print(f"{'='*70}\n")
        
        self.all_subroutines = []
        for i, cobol_program in enumerate(unit.programs):
            if self.debug:
                print(f"\n>>> PROGRAM {i+1}: {cobol_program.program_id}")
            
            program_subroutines = self.convert_cobol_program(cobol_program)
            self.all_subroutines.extend(program_subroutines)

        # Create Main() that calls ALL programs in sequence for testing
        main_body = []
        for program in unit.programs:
            # Only call top-level programs that are not meant to be just subroutines
            if not program.is_nested and program.program_id != "ADD-NUMS":
                program_name = self.normalize_name(program.program_id)
                main_body.append(RunTask(1, 1, program_name, []))
        
        main_subroutine = SubRoutine(1, 1, 'Main', main_body)
        
        # Generate FixedPool declarations for programs with linkage parameters
        linkage_pools = []
        for prog_name, params in self.program_linkage_params.items():
            pool_fields = []
            for param in params:
                # Create ResourceItem: line, column, key, value, attributes
                attributes = {"Initialize": Number(1, 1, 0)}
                item = ResourceItem(1, 1, key=param, value=None, attributes=attributes)
                pool_fields.append(item)
            
            # Create FixedPool AST node
            pool_name = f"COBOL_{prog_name}_LINKAGE"
            pool = Pool(1, 1, pool_type="FixedPool", name=pool_name, body=pool_fields)
            linkage_pools.append(pool)
        
        # NEW: Generate FixedPool declarations for programs with shared variables
        ws_pools = []
        for prog_name, pool_info in self.program_ws_pools.items():
            pool_name = pool_info['name']
            pool_fields = []
            for var_name, var_info in pool_info['variables'].items():
                init_val = self.get_initial_value_node(var_info)
                attributes = {"Initialize": init_val}
                item = ResourceItem(1, 1, key=var_name, value=None, attributes=attributes)
                pool_fields.append(item)
            ws_pools.append(Pool(1, 1, pool_type="FixedPool", name=pool_name, body=pool_fields))
        
        # Combine all into declarations
        all_declarations = linkage_pools + ws_pools + self.all_subroutines + [main_subroutine]
        
        if self.debug:
            print(f"\n{'='*70}")
            print(f"Generated {len(all_declarations)} Ailang subroutines:")
            for decl in all_declarations:
                print(f"  - SubRoutine.{decl.name}")
            print(f"{'='*70}\n")
        
        return Program(declarations=all_declarations, line=1, column=1)

    def convert_cobol_program(self, program: COBOLProgram) -> List[SubRoutine]:
        """
        Convert ONE COBOL PROGRAM-ID to Ailang subroutines.
        
        Returns:
            List of subroutines:
            - One for the program itself (PROGRAM_ID)
            - One for each paragraph within the program (PROGRAM_ID_PARAGRAPH)
            - Recursively for any nested programs
        """
        subroutines = []
        
        # CRITICAL: Reset variable/paragraph tracking for THIS program
        # Each PROGRAM-ID has its own WORKING-STORAGE scope
        self.variables = {}
        self.paragraphs = {}
        
        program_name = self.normalize_name(program.program_id)
        
        if self.debug:
            print(f"\n  Converting PROGRAM-ID: {program.program_id}")
            print(f"  Ailang name: {program_name}")
        
        # === PHASE 1: Collect variables from THIS program's DATA DIVISION ===
        if program.data_division and program.data_division.working_storage:
            if self.debug:
                print(f"\n  WORKING-STORAGE SECTION:")

            for decl in program.data_division.working_storage:
                if isinstance(decl, COBOLVariableDecl):
                    var_name = self.normalize_name(decl.name)
                    # Get complete storage information
                    storage_info = self.get_storage_info(
                        decl.pic_clause, 
                        decl.usage_type,
                        decl.is_signed,
                        decl.decimal_places
                    )
                    var_value = decl.value if decl.value else ('""' if storage_info['ailang_type'] == 'String' else '0')

                    # Check if this is a display-edited format
                    is_edited = self.is_edited_format(decl.pic_clause) if decl.pic_clause else False
                    edit_format = None
                    if is_edited:
                        edit_format = self.parse_edit_format(decl.pic_clause)

                    self.variables[var_name] = {
                        'type': storage_info['ailang_type'],
                        'value': var_value,
                        'occurs': decl.occurs_count,
                        'decimal_places': decl.decimal_places,
                        'storage': storage_info['storage'],      # NEW
                        'is_signed': storage_info['is_signed'],  # NEW
                        'precision': storage_info['precision'],   # NEW
                        'is_edited': is_edited,                   
                        'edit_format': edit_format,              
                        'pic_clause': decl.pic_clause             
                    }

                    if self.debug:
                        occurs_str = f" OCCURS {decl.occurs_count}" if decl.occurs_count else ""
                        decimal_str = f" (V{decl.decimal_places})" if decl.decimal_places else ""
                        usage_str = f" {decl.usage_type}" if decl.usage_type else ""
                        sign_str = " SIGNED" if decl.is_signed else ""
                        edit_str = f" EDITED({edit_format['type']})" if is_edited else ""
                        print(f"    {var_name}: {storage_info['ailang_type']}{sign_str}{decimal_str}{usage_str}{edit_str}{occurs_str} = {var_value}")

        # NEW: Create FixedPool for ALL programs with WORKING-STORAGE to avoid zero-init bug
        pool_name = None
        if self.variables:  # Create a pool for any program with variables
            pool_name = f"COBOL_{program_name}_VARS"
            self.program_ws_pools[program_name] = {'name': pool_name, 'variables': self.variables}
            if self.debug:
                print(f"\n  Program has paragraphs - creating FixedPool: {pool_name}")
        
        # NEW: Store pool name for variable reference conversion
        self.current_pool_name = pool_name
        
        # NEW: Extract linkage section parameters (don't initialize them)
        linkage_params = []
        if program.data_division and hasattr(program.data_division, 'linkage_section') and program.data_division.linkage_section:
            # linkage_section is now a list, not an object with .variables
            for var_decl in program.data_division.linkage_section:
                var_name = self.normalize_name(var_decl.name)
                linkage_params.append(var_name)
                # Track but don't initialize - these are parameters
                self.variables[var_name] = {
                    'type': self.infer_type(var_decl.pic_clause, var_decl.decimal_places),
                    'is_linkage': True  # Mark as linkage parameter
                }
                if self.debug:
                    print(f"    Linkage param: {var_name} ({self.variables[var_name]['type']})")
        
        # Store linkage params for this program (for later CALL mapping)
        if linkage_params:
            self.program_linkage_params[program_name] = linkage_params
            if self.debug:
                print(f"  Registered linkage params for {program_name}: {linkage_params}")
        
        # Get procedure division using params if present
        procedure_using_params = []
        if program.procedure_division and hasattr(program.procedure_division, 'using_params') and program.procedure_division.using_params:
            procedure_using_params = [self.normalize_name(p) for p in program.procedure_division.using_params]

        # === PHASE 2: Collect paragraphs from THIS program's PROCEDURE DIVISION ===
        if self.debug and program.procedure_division.paragraphs:
            named_paras = [p for p in program.procedure_division.paragraphs if p.name]
            if named_paras:
                print(f"\n  PARAGRAPHS:")
                for para in named_paras:
                    print(f"    {para.name}")
        
        for para in program.procedure_division.paragraphs:
            if isinstance(para, COBOLParagraph) and para.name:
                para_name = self.normalize_name(para.name)
                self.paragraphs[para_name] = para
        
        # === PHASE 3: Create SubRoutine for the PROGRAM-ID itself ===
        program_subroutine = self.create_program_subroutine(
            program,  # Pass the whole program object
            self.variables,
            pool_name # NEW: Pass pool name
        )
        subroutines.append(program_subroutine)
        
        # === PHASE 4: Create SubRoutines for each PARAGRAPH ===
        # SET CURRENT PROGRAM NAME for paragraph creation
        self.current_program_name = program_name
        for para in self.paragraphs.values():
            # Paragraphs also need the pool_name
            para_subroutine = self.create_paragraph_subroutine(
                para,
                self.variables,
                program_name,  # Namespace paragraphs under their program
                pool_name
            )
            subroutines.append(para_subroutine)
        
        # === PHASE 5: Recursively handle nested programs ===
        for nested_program in program.contained_programs:
            if self.debug:
                print(f"\n  >>> NESTED PROGRAM: {nested_program.program_id}")
            nested_subroutines = self.convert_cobol_program(nested_program)
            subroutines.extend(nested_subroutines)
        
        # === PHASE 5: Create FixedPool if needed ===
        if pool_name:
            # This method doesn't exist, but the logic is in convert_compilation_unit
            # We will create the pool here based on that logic.
            # This part of the logic is actually handled at the `convert_compilation_unit` level
            # where `self.program_ws_pools` is processed. This comment serves as a placeholder
            # for the conceptual step.
            pass
        
        return subroutines
    
    def make_variable_reference(self, var_name: str, variables: Dict) -> Identifier:
        """
        Create a variable reference for READING (not assignment).
        Uses pool prefix if variable is in a FixedPool.
        """
        normalized = self.normalize_name(var_name)
        
        if self.current_pool_name and normalized in variables:
            # Pool variable - use prefix for reading
            return Identifier(1, 1, f"{self.current_pool_name}.{normalized}")
        else:
            # Temp variable or literal - no prefix
            return Identifier(1, 1, normalized)   
    
    

    def create_program_subroutine(self, program: COBOLProgram, variables: Dict, pool_name: Optional[str]) -> SubRoutine:
        """Create the main subroutine for a COBOL program"""
        """
        Create the main SubRoutine for a COBOL PROGRAM-ID.
        Contains: variable initializations + inline (unnamed paragraph) code.
        """
        name = self.normalize_name(program.program_id)
        paragraphs = program.procedure_division.paragraphs # Get paragraphs from program object

        statements = []
        
        # Initialize variables (only if not in a pool)
        statements.extend(self.create_variable_initializations(variables, pool_name))
        
        # SET CURRENT PROGRAM NAME
        self.current_program_name = name
        
        # NEW: Get PROCEDURE DIVISION USING params
        procedure_params = []
        if program.procedure_division and hasattr(program.procedure_division, 'using_params') and program.procedure_division.using_params:
            procedure_params = [self.normalize_name(p) for p in program.procedure_division.using_params]
        
        # Convert inline (unnamed) paragraphs
        # Convert inline code (unnamed paragraphs = direct PROCEDURE DIVISION code)
        for para in paragraphs:
            if not para.name:  # Unnamed = inline code
            
                for stmt in para.statements: # Pass variables to convert_statement
                    converted = self.convert_statement(stmt, variables)  # ADD variables here
                    if isinstance(converted, list):
                        statements.extend(converted)
                    elif converted is not None:
                        statements.append(converted)
        
        # âœ… ADD: Call all named paragraphs in sequence
        for para in paragraphs:
            if para.name:  # Named paragraph
                para_name = self.normalize_name(para.name)
                full_name = f"{name}_{para_name}"
                statements.append(RunTask(1, 1, full_name, []))
        
        # Add return
        # Add return if missing
        if not statements or not isinstance(statements[-1], ReturnValue):
            statements.append(ReturnValue(1, 1, Number(1, 1, 0)))
        
        # NEW: Add parameters to SubRoutine if present
        subroutine = SubRoutine(1, 1, name, statements)
        if procedure_params:
            subroutine.input_params = procedure_params  # Add parameter list
        return subroutine

    def create_variable_pool(self, pool_name: str, variables: Dict):
        """Create a FixedPool for shared WORKING-STORAGE variables"""
        
        pool_fields = []
        for var_name, var_info in variables.items():
            # Skip linkage params and arrays (arrays handled differently)
            if var_info.get('is_linkage') or var_info.get('occurs'):
                continue
            
            # Initialize based on type
            if var_info['type'] == 'String':
                init_value = String(1, 1, var_info['value'].strip('"') if '"' in var_info['value'] else var_info['value'])
            else:
                init_value = Number(1, 1, int(var_info['value']) if var_info['value'] else 0)
            
            attributes = {"Initialize": init_value}
            item = ResourceItem(1, 1, key=var_name, value=None, attributes=attributes)
            pool_fields.append(item)
        
        return Pool(1, 1, pool_type="FixedPool", name=pool_name, body=pool_fields)

    def create_paragraph_subroutine(self, para: COBOLParagraph, variables: Dict, parent_program: str, pool_name: Optional[str]) -> SubRoutine:
        """
        Create a subroutine for a COBOL paragraph.
        
        CRITICAL: Paragraphs share their parent program's variables!
        CRITICAL: Paragraphs share their parent program's WORKING-STORAGE!
        So we initialize the same variables here.
        
        Naming: PROGRAM_PARAGRAPH (to avoid collisions across programs)
        """
        para_name = self.normalize_name(para.name)
        # Namespace paragraphs under their program: PROGRAM_PARA
        full_name = f"{parent_program}_{para_name}"
        
        statements = []
        
        # DON'T initialize variables - paragraphs share WORKING-STORAGE with parent
        # Variables are already initialized in the parent program subroutine
        
        # SET CURRENT PROGRAM NAME
        self.current_program_name = parent_program
        
        # Convert paragraph statements
        # Convert paragraph's statements
        for stmt in para.statements: # Pass variables to convert_statement
            converted = self.convert_statement(stmt, variables)  # ADD variables here
            if isinstance(converted, list):
                statements.extend(converted)
            elif converted is not None:
                statements.append(converted)
        
        # Add return
        # Add return if missing
        if not statements or not isinstance(statements[-1], ReturnValue):
            statements.append(ReturnValue(1, 1, Number(1, 1, 0)))
        
        return SubRoutine(1, 1, full_name, statements)

    def create_variable_initializations(self, variables: Dict, pool_name: Optional[str] = None) -> List:
        """Create Ailang initialization statements for a variable dict"""
        initializations = []
        
        for var_name, var_info in variables.items():
            # Skip linkage parameters
            if var_info.get('is_linkage'):
                continue
            
            # Arrays still need local initialization
            if var_info.get('occurs'):
                occurs_count = var_info['occurs']
                if pool_name:
                    # Use pool reference for array
                    initializations.append(
                        Assignment(1, 1, f"{pool_name}.{var_name}", 
                                  FunctionCall(1, 1, 'ArrayCreate', [Number(1, 1, occurs_count)]))
                    )
                else:
                    initializations.append(
                        Assignment(1, 1, var_name, 
                                  FunctionCall(1, 1, 'ArrayCreate', [Number(1, 1, occurs_count)]))
                    )
                continue
            
            # If using pool, variables are already initialized in the pool
            if pool_name:
                continue
            
            # Regular initialization (no pool case)
            if var_info['type'] == 'String':
                value = var_info['value'].strip('"') if '"' in var_info['value'] else var_info['value']
                initializations.append(
                    Assignment(1, 1, var_name, String(1, 1, value))
                )
            else:
                if 'value' in var_info and var_info['value'] is not None:
                    initializations.append(
                        Assignment(1, 1, var_name, Number(1, 1, int(var_info['value'])))
                    )
                else:
                    initializations.append(
                        Assignment(1, 1, var_name, Number(1, 1, 0))
                    )
        
        return initializations

    def get_initial_value_node(self, var_info: Dict):
        """Helper to get an AST node for a variable's initial value."""
        value_str = var_info.get('value', '0')
        var_type = var_info['type']
        
        # NEW: Edited formats are always strings, initialize as empty string
        if var_info.get('is_edited'):
            # Even if they have a VALUE clause, it's for formatting, not initial state
            return String(1, 1, "")
    
        # Handle decimal types
        if var_type == 'Float' and var_info.get('decimal_places'):
            # Use FixedPoint for decimals
            fixedpoint_value = self.convert_decimal_value(value_str, var_info['decimal_places'])
            return Number(1, 1, fixedpoint_value)
        
        # Handle integers
        elif var_type == 'Integer':
            # ✅ FIX: Don't strip sign! Just remove decimal part if present
            clean_value = value_str.split('.')[0]
            return Number(1, 1, int(clean_value))
        
        # Handle strings
        elif var_type == 'String':
            # Strip quotes for string literals
            return String(1, 1, value_str.strip('"'))
        
        # Default
        return Number(1, 1, 0)

    def make_assignment_target(self, var_name: str, variables: Dict) -> str:
        """Create assignment target with pool prefix if needed"""
        # Skip if already has pool prefix
        if '.' in var_name:
            return var_name
        
        # Skip if it's a linkage parameter (uses different pool)
        if var_name in variables and variables[var_name].get('is_linkage'):
            pool_name = f"COBOL_{self.current_program_name}_LINKAGE"
            return f"{pool_name}.{var_name}"
        
        # Skip if it's an array (arrays handled separately with ArraySet)
        if var_name in variables and variables[var_name].get('occurs'):
            return var_name
        
        # Add pool prefix for regular variables
        if self.current_pool_name and var_name in variables:
            return f"{self.current_pool_name}.{var_name}"
        
        return var_name

    def convert_statement(self, stmt: COBOLASTNode, variables: Dict) -> Any:
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
                print(f"    DEBUG: convert_accept returned: {type(result).__name__}")
                return result
            elif isinstance(stmt, COBOLStopRun):
                if self.debug:
                    print(f"    DEBUG: Converting COBOLStopRun (ends subroutine)...")
                # STOP RUN in COBOL = program termination
                # In Ailang SubRoutines, we just end naturally (no return needed)
                return None
            elif isinstance(stmt, COBOLGoback):
                if self.debug:
                    print(f"    DEBUG: Converting COBOLGoback (ends subroutine)...")
                # GOBACK in COBOL = return to caller
                # In Ailang SubRoutines, we just end naturally (no return needed)
                return None
            elif isinstance(stmt, COBOLCall):
                if self.debug:
                    print(f"    DEBUG: Converting COBOLCall...")
                result = self.convert_call(stmt, variables)
                if self.debug:
                    result_type = "List" if isinstance(result, list) else type(result).__name__
                    print(f"    DEBUG: convert_call returned: {result_type}")
                return result  # May be a list of statements for parameter mapping
            elif isinstance(stmt, COBOLExit):
                print(f"    DEBUG: Converting COBOLExit (returns None)...")
                return None
            elif isinstance(stmt, COBOLStringConcat):
                if self.debug:
                    print(f"    DEBUG: Converting COBOLStringConcat...")
                result = self.convert_string_concat(stmt, variables)
                print(f"    DEBUG: convert_string_concat returned: {type(result).__name__}")
                return result
            elif isinstance(stmt, COBOLStringConcat):
                if self.debug:
                    print(f"    DEBUG: Converting COBOLStringConcat...")
                result = self.convert_string_concat(stmt, variables)
                print(f"    DEBUG: convert_string_concat returned list with {len(result)} items")
                return result
            elif isinstance(stmt, COBOLUnstring):
                if self.debug:
                    print(f"    DEBUG: Converting COBOLUnstring...")
                result = self.convert_unstring(stmt, variables)
                print(f"    DEBUG: convert_unstring returned list with {len(result)} items")
                return result
            elif isinstance(stmt, COBOLInspect):
                if self.debug:
                    print(f"    DEBUG: Converting COBOLInspect...")
                result = self.convert_inspect(stmt, variables)
                print(f"    DEBUG: convert_inspect returned list with {len(result)} items")
                return result
            else:
                print(f"    DEBUG: Unknown statement type: {type(stmt).__name__}")
                return None
        except Exception as e:
            print(f"    ERROR: Exception in convert_statement for {type(stmt).__name__}: {e}")
            import traceback
            traceback.print_exc()
            return None

    def _collect_literals_recursive(self, node):
        """Walk AST collecting string literals"""
        if node is None:
            return
        if isinstance(node, COBOLStringLiteral):
            self.get_string_literal_id(node.value)
        elif isinstance(node, list):
            for item in node:
                if isinstance(item, COBOLASTNode):
                    self._collect_literals_recursive(item)
        elif isinstance(node, COBOLASTNode):
            for field_name, field_value in node.__dict__.items():
                if isinstance(field_value, (COBOLASTNode, list)):
                    self._collect_literals_recursive(field_value)

    def convert_display(self, stmt: COBOLDisplay, variables: Dict) -> List[Any]:
        """
        Convert COBOL DISPLAY to Ailang PrintMessage.
        Multiple expressions are concatenated into a single output.
        """
        if not stmt.expressions:
            return [FunctionCall(1, 1, 'PrintMessage', [String(1, 1, "")])]

        string_parts = []

        for expr in stmt.expressions:
            converted_expr = self.convert_expression(expr, variables)

            # Check if this is a variable reference
            if isinstance(expr, COBOLIdentifier):
                var_name = self.normalize_name(expr.name)
                if var_name in variables:
                    var_info = variables[var_name]

                    # Check if it's an edited format (already a string)
                    if var_info.get('is_edited'):
                        string_parts.append(converted_expr)
                    # NEW: Format decimal variables for display
                    elif var_info.get('type') == 'Float' and var_info.get('decimal_places'):
                        # Use FormatDecimal to display with decimal point
                        decimal_places = Number(1, 1, var_info['decimal_places'])
                        formatted = FunctionCall(
                            1, 1,
                            'FormatDecimal',
                            [converted_expr, decimal_places]
                        )
                        string_parts.append(formatted)
                    elif var_info['type'] == 'Integer':
                        # Convert integer to string
                        string_parts.append(FunctionCall(1, 1, 'NumberToString', [converted_expr]))
                    else:
                        # String type - use as-is
                        string_parts.append(converted_expr)
                else:
                    # Unknown variable - treat as string
                    string_parts.append(converted_expr)

            elif isinstance(expr, COBOLNumberLiteral):
                # Convert number to string
                string_parts.append(FunctionCall(1, 1, 'NumberToString', [converted_expr]))

            elif isinstance(expr, COBOLStringLiteral):
                # String literal - use as-is
                string_parts.append(converted_expr)

            else:
                # Other expression types (e.g. function calls, arithmetic)
                string_parts.append(FunctionCall(1, 1, 'NumberToString', [converted_expr]))

        # Build concatenated string
        if len(string_parts) == 0:
            return [FunctionCall(1, 1, 'PrintMessage', [String(1, 1, "")])]
        elif len(string_parts) == 1:
            # Single expression - just print it
            return [FunctionCall(1, 1, 'PrintMessage', [string_parts[0]])]
        else:
            # Multiple expressions - concatenate them
            # Build nested StringConcat: StringConcat(StringConcat(a, b), c)
            result = string_parts[0]
            for part in string_parts[1:]:
                result = FunctionCall(1, 1, 'StringConcat', [result, part])
            return [FunctionCall(1, 1, 'PrintMessage', [result])]

    def convert_inspect(self, stmt: COBOLInspect, variables: Dict) -> List[Statement]:
        """Convert INSPECT - uses Library.Cobol functions"""
        statements = []
        target_var = self.normalize_name(stmt.target)
        pattern_expr = self.convert_expression(stmt.pattern, variables)
        
        if stmt.operation == 'REPLACING':
            replacement_expr = self.convert_expression(stmt.replacement, variables)
            
            # Read from target with pool prefix
            if self.current_pool_name and target_var in variables:
                target_read = Identifier(1, 1, f"{self.current_pool_name}.{target_var}")
            else:
                target_read = Identifier(1, 1, target_var)
            
            # Call StringReplaceAll (library already imported)
            replace_call = FunctionCall(
                1, 1,
                'StringReplaceAll',  # â† CHANGED: removed 'Library.Cobol.' prefix
                [target_read, pattern_expr, replacement_expr]
            )
            
            target_write = self.make_assignment_target(target_var, variables)
            statements.append(Assignment(1, 1, target_write, replace_call))
        
        elif stmt.operation == 'TALLYING':
            counter_var = self.normalize_name(stmt.counter)
            
            # Read from target
            if self.current_pool_name and target_var in variables:
                target_read = Identifier(1, 1, f"{self.current_pool_name}.{target_var}")
            else:
                target_read = Identifier(1, 1, target_var)
            
            # Call StringCount (library already imported)
            count_call = FunctionCall(
                1, 1,
                'StringCount',  # â† CHANGED: removed 'Library.Cobol.' prefix
                [target_read, pattern_expr]
            )
            
            counter_write = self.make_assignment_target(counter_var, variables)
            statements.append(Assignment(1, 1, counter_write, count_call))
        
        return statements
    
    def convert_move(self, stmt: COBOLMove, variables: Dict) -> Assignment:
        """Convert MOVE statement
        
        Returns Assignment for scalar variables
        Returns FunctionCall for array element assignments
        """
        source = self.convert_expression(stmt.source, variables)
        
        # Check if target is an array element
        if isinstance(stmt.target, COBOLArraySubscript):
            # MOVE value TO ARRAY(index)
            # Becomes: ArraySet(ARRAY, index-1, value)
            array_name = self.normalize_name(stmt.target.array_name)

            # NEW: Use pool prefix for arrays
            if self.current_pool_name and array_name in self.variables and self.variables[array_name].get('occurs'):
                array_ref = f"{self.current_pool_name}.{array_name}"
            else:
                array_ref = array_name

            index_expr = self.convert_expression(stmt.target.index, variables)

            # Convert to 0-based indexing
            zero_based_index = FunctionCall(1, 1, 'Subtract', [
                index_expr,
                Number(1, 1, 1)
            ])
            return FunctionCall(1, 1, 'ArraySet', [
                Identifier(1, 1, array_ref),
                zero_based_index,
                source
            ])

        # Regular scalar assignment - get target name
        if isinstance(stmt.target, COBOLIdentifier):
            target_name = self.normalize_name(stmt.target.name)
        else:
            # This case should be rare for MOVE targets
            target_name = str(self.convert_expression(stmt.target, variables))

        # Check if target is an edited format field
        if isinstance(stmt.target, COBOLIdentifier):
            if target_name in variables:
                var_info = variables[target_name]

                # If target is edited format, apply formatting
                if var_info.get('is_edited') and var_info.get('edit_format'):
                    if self.debug:
                        print(f"    Applying {var_info['edit_format']['type']} format to {target_name}")

                    # Wrap source in format function
                    source = self.create_format_function(
                        source, 
                        var_info['edit_format']
                    )

        return Assignment(1, 1, self.make_assignment_target(target_name, variables), source)
    
    def convert_compute(self, stmt: COBOLCompute, variables: Dict) -> Assignment:
        expression = self.convert_expression(stmt.expression, variables)
        
        # Check if target is an array subscript
        if isinstance(stmt.target, COBOLArraySubscript):
            # COMPUTE ARRAY(index) = expr
            # Becomes: ArraySet(ARRAY, index-1, expr)
            array_name = self.normalize_name(stmt.target.array_name)
            
            # NEW: Use pool prefix for arrays
            if self.current_pool_name and array_name in self.variables and self.variables[array_name].get('occurs'):
                array_ref = f"{self.current_pool_name}.{array_name}"
            else:
                array_ref = array_name
            
            index_expr = self.convert_expression(stmt.target.index, variables)
            # Convert to 0-based indexing
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
        target = self.convert_expression(stmt.target, variables)
        target_name = target.name if isinstance(target, Identifier) else str(target)
        return Assignment(1, 1, self.make_assignment_target(target_name, variables), expression)
    
    def convert_arithmetic(self, stmt: COBOLArithmetic, variables: Dict) -> Optional[Assignment]:
        """Convert COBOL arithmetic statements (ADD, SUBTRACT, etc.)"""
        
        # Determine operands and target based on operation
        all_operands = [self.convert_expression(op, variables) for op in stmt.operands]
        
        if stmt.giving:
            # GIVING form: ADD A B GIVING C -> C = A + B
            target_node = stmt.giving
            if stmt.operation in ['ADD', 'SUBTRACT'] and stmt.target:
                 # ADD A TO B GIVING C -> C = A + B
                 all_operands.append(self.convert_expression(stmt.target, variables))
        else:
            # TO/FROM form: ADD A TO B -> B = B + A
            target_node = stmt.target
            all_operands.insert(0, self.convert_expression(target_node, variables))

        if not all_operands:
            return None

        # Check if this is a decimal operation by checking all involved nodes
        is_decimal = False
        for op_node in stmt.operands + [stmt.target, stmt.giving]:
            if op_node and self.is_decimal_operation(op_node, None, variables):
                is_decimal = True
                break
        
        func_name = self.get_arithmetic_function(stmt.operation, is_decimal)
        
        # Build the expression tree
        # For SUBTRACT, it's op1 - op2 - op3...
        # For others, it's op1 + op2 + op3...
        expr = all_operands[0]
        for op in all_operands[1:]:
            expr = FunctionCall(1, 1, func_name, [expr, op])
        
        # Determine target with pool prefix
        target_name = self.convert_expression(target_node, variables).name
        return Assignment(1, 1, self.make_assignment_target(target_name, variables), expr)
    
    def convert_if(self, stmt: COBOLIf, variables: Dict) -> If:
        condition = self.convert_expression(stmt.condition, variables)
        
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
        
        return If(1, 1, condition, then_body, else_body if else_body else None)
    
    def convert_call(self, stmt: COBOLCall, variables: Dict) -> List:
        """
    CALL "PROG-NAME" â†’ RunTask(PROG_NAME)
    """
        prog_name = self.normalize_name(stmt.program_name)
        # Don't namespace - programs are top-level subroutines
        full_name = prog_name
        
        statements = []
        
        if self.debug:
            print(f"  DEBUG convert_call: program={full_name}")
            print(f"  DEBUG using_params={stmt.using_params}")
            print(f"  DEBUG registry={self.program_linkage_params.get(full_name)}")
        
        # If USING clause, map via FixedPool
        if stmt.using_params and full_name in self.program_linkage_params:
            caller_params = [self.normalize_name(p) for p in stmt.using_params]
            callee_params = self.program_linkage_params[full_name]
            pool_name = f"COBOL_{full_name}_LINKAGE"
            
            if self.debug:
                print(f"  CALL {full_name} USING {caller_params} -> FixedPool.{pool_name}.{callee_params}")
            
            # Map caller variables to FixedPool before call
            for i in range(min(len(caller_params), len(callee_params))):
                caller_var_name = caller_params[i]
                
                # NEW: Add pool prefix for caller variable
                if self.current_pool_name and caller_var_name in variables:
                    caller_ref = f"{self.current_pool_name}.{caller_var_name}"
                else:
                    caller_ref = caller_var_name
                
                caller_var = Identifier(1, 1, caller_ref)
                statements.append(Assignment(1, 1, f"{pool_name}.{callee_params[i]}", caller_var))
            
            # Make the call
            statements.append(
                FunctionCall(1, 1, 'RunTask', [Identifier(1, 1, full_name)])
            )
            
            # Map FixedPool back to caller variables after call
            # (This is for parameters that might be modified by the callee)
            for i in range(min(len(caller_params), len(callee_params))):
                caller_var_name = caller_params[i]
                
                # NEW: Add pool prefix for caller variable target
                if self.current_pool_name and caller_var_name in variables:
                    caller_target = f"{self.current_pool_name}.{caller_var_name}"
                else:
                    caller_target = caller_var_name
                
                pool_var = Identifier(1, 1, f"{pool_name}.{callee_params[i]}")
                statements.append(
                    Assignment(1, 1, caller_target, pool_var)
                )
        else:
            # Simple call without parameters
            statements.append(
                FunctionCall(1, 1, 'RunTask', [Identifier(1, 1, full_name)])
            )
        
        return statements

    def convert_perform_paragraph(self, stmt: COBOLPerformParagraph, variables: Dict) -> FunctionCall:
        para_name = self.normalize_name(stmt.paragraph_name)
        full_name = f"{self.current_program_name}_{para_name}"
        return FunctionCall(1, 1, 'RunTask', [Identifier(1, 1, full_name)])
    
    def convert_perform_times(self, stmt: COBOLPerformTimes, variables: Dict):
        """
        Convert PERFORM...TIMES to Ailang while loop with counter.
        """
        # Generate unique counter variable
        counter_var = f"TEMP_TIMES_CTR_{id(stmt)}"
        
        # Convert the times expression
        times_value = self.convert_expression(stmt.times_expr, variables)
        
        # Initialize counter to 0
        init_counter = Assignment(1, 1, counter_var, Number(1, 1, 0))
        
        # Build loop body
        loop_body = []
        
        if stmt.paragraph_name:
            # Calling a paragraph
            para_name = self.normalize_name(stmt.paragraph_name)
            full_name = f"{self.current_program_name}_{para_name}"
            loop_body.append(FunctionCall(1, 1, 'RunTask', [Identifier(1, 1, full_name)]))
        elif stmt.statements:
            # Inline statements
            for inline_stmt in stmt.statements:
                converted = self.convert_statement(inline_stmt, variables)
                if isinstance(converted, list):
                    loop_body.extend(converted)
                elif converted is not None:
                    loop_body.append(converted)
        
        # Increment counter
        loop_body.append(
            Assignment(1, 1, counter_var,
                      FunctionCall(1, 1, 'Add', [
                          Identifier(1, 1, counter_var),
                          Number(1, 1, 1)
                      ]))
        )
        
        # Create while loop: counter < times
        condition = FunctionCall(1, 1, 'LessThan', [
            Identifier(1, 1, counter_var),
            times_value
        ])
        
        while_loop = While(1, 1, condition, loop_body)
        
        return [init_counter, while_loop]
    
    def convert_perform_until(self, stmt: COBOLPerformUntil, variables: Dict):
        """
        Convert PERFORM UNTIL to Ailang WhileLoop.
        COBOL UNTIL = While NOT condition
        """
        # Invert condition: UNTIL x > 5 becomes While Not(x > 5)
        ailang_condition = FunctionCall(1, 1, 'Not', [
            self.convert_expression(stmt.condition, variables)
        ])
        
        # Build loop body
        loop_body = []
        
        if stmt.paragraph_name:
            # Calling a paragraph
            para_name = self.normalize_name(stmt.paragraph_name)
            full_name = f"{self.current_program_name}_{para_name}"
            loop_body.append(FunctionCall(1, 1, 'RunTask', [Identifier(1, 1, full_name)]))
        else:
            # Inline statements
            for inline_stmt in stmt.statements:
                converted = self.convert_statement(inline_stmt, variables)
                if isinstance(converted, list):
                    loop_body.extend(converted)
                elif converted is not None:
                    loop_body.append(converted)
        
        return While(1, 1, ailang_condition, loop_body)

    def convert_perform_varying(self, stmt: COBOLPerformVarying, variables: Dict) -> Any:
        # âœ… FIX: Add pool prefix to loop variable
        var_name = self.normalize_name(stmt.variable)
        target_var = self.make_assignment_target(var_name, variables)

        from_value = self.convert_expression(stmt.from_expr, variables)
        by_value = self.convert_expression(stmt.by_expr, variables)
        condition = self.convert_expression(stmt.until_condition, variables)

        negated_condition = FunctionCall(1, 1, 'Not', [condition])

        loop_body = []
        for loop_stmt in stmt.statements:
            converted = self.convert_statement(loop_stmt, variables)
            if isinstance(converted, list):
                loop_body.extend(converted)
            elif converted:
                loop_body.append(converted)

        loop_body.append(
            Assignment(1, 1, target_var,
                      FunctionCall(1, 1, 'Add', [
                          Identifier(1, 1, target_var),  # âœ… Also fix the read
                          by_value
                      ]))
        )

        return [
            Assignment(1, 1, target_var, from_value),
            While(1, 1, negated_condition, loop_body)
        ]
    
    def convert_evaluate(self, stmt: COBOLEvaluate, variables: Dict) -> If:
        """Convert EVALUATE to nested If/ElseIf chain"""
        subject = self.convert_expression(stmt.subject, variables)
        
        # Determine comparison function based on subject type
        comparison_func = 'EqualTo'
        
        # Check if subject is a string
        if isinstance(stmt.subject, COBOLIdentifier):
            var_name = self.normalize_name(stmt.subject.name)
            if var_name in variables and variables[var_name]['type'] == 'String':
                comparison_func = 'StringEquals'
        elif isinstance(stmt.subject, COBOLStringLiteral):
            comparison_func = 'StringEquals'
        elif isinstance(stmt.subject, COBOLFunctionCall):
            # String functions return strings
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
                value = self.convert_expression(when_clause.value, variables)
                condition = FunctionCall(1, 1, comparison_func, [subject, value])
                
                if result is None:
                    result = If(1, 1, condition, then_body, None)
                else:
                    result = If(1, 1, condition, then_body, [result] if not isinstance(result, list) else result)
        
        return result
    
    def convert_accept(self, stmt: COBOLAccept, variables: Dict) -> Assignment:
        """Convert ACCEPT to GetUserInput assignment"""
        var_name = self.normalize_name(stmt.variable)
        
        # Get user input
        input_call = FunctionCall(1, 1, 'GetUserInput', [])
        
        # Check if variable is numeric - if so, convert the input
        if var_name in variables and variables[var_name]['type'] == 'Integer':
            input_value = FunctionCall(1, 1, 'StringToNumber', [input_call])
        else:
            input_value = input_call
        
        # NEW: Check if we should use pool reference
        target_var = self.make_assignment_target(var_name, variables)
        return Assignment(1, 1, target_var, input_value)
    
    def convert_stop_run(self, stmt: COBOLStopRun) -> None:
        """Convert STOP RUN to program termination"""
        return None

    def convert_goback(self, stmt: COBOLGoback) -> None:
        """Convert GOBACK to return"""
        return None


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
        is_decimal = self.is_decimal_operation(expr.left, expr.right, variables)
        
        # Get the appropriate function name
        if expr.operator == '**':
            # Power doesn't have a FixedPoint version
            func_name = 'Power'
        else:
            # Use helper to get correct function (FixedPoint or regular)
            func_name = self.get_arithmetic_function(operation, is_decimal)
        
        return FunctionCall(1, 1, func_name, [left, right])

    def convert_expression(self, expr: COBOLASTNode, variables: Dict) -> Any:
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
            name = self.normalize_name(expr.name)

            # Check if this is a linkage parameter (uses its own pool)
            if name in self.variables and self.variables[name].get('is_linkage'):
                pool_name = f"COBOL_{self.current_program_name}_LINKAGE"
                return Identifier(expr.line, expr.column, f"{pool_name}.{name}")
            
            # Check if we're using a variable pool (for all non-array variables)
            if self.current_pool_name and name in self.variables and not self.variables[name].get('occurs'):
                return Identifier(expr.line, expr.column, f"{self.current_pool_name}.{name}")
            
            return Identifier(expr.line, expr.column, name)
        
        elif isinstance(expr, COBOLNumberLiteral):
            if '.' in expr.value:
                return Number(1, 1, int(float(expr.value)))
            else:
                return Number(1, 1, int(expr.value))
        
        elif isinstance(expr, COBOLStringLiteral):
            # âœ“ Returns direct string literal
            return String(1, 1, expr.value)
        elif isinstance(expr, COBOLArraySubscript):
            return self.convert_array_subscript(expr, variables)
        elif isinstance(expr, COBOLFunctionCall):
            return self.convert_function_call(expr, variables)
        
        else:
            return Number(1, 1, 0)

    def convert_function_call(self, expr: COBOLFunctionCall, variables: Dict) -> FunctionCall:
            if expr.function_name == 'UPPER-CASE':
                arg = self.convert_expression(expr.arguments[0], variables)
                return FunctionCall(1, 1, 'StringToUpper', [arg])
            # Add more functions here later << sorry claude updated the patches on me last minute. 
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
            var_name = self.normalize_name(left.name)
            if var_name in variables and variables[var_name]['type'] == 'String':
                return True
        if isinstance(right, COBOLIdentifier):
            var_name = self.normalize_name(right.name)
            if var_name in variables and variables[var_name]['type'] == 'String':
                return True
        return False

    def convert_array_subscript(self, node: COBOLArraySubscript, variables: Dict) -> FunctionCall:
        """Convert array subscript to Ailang ArrayGet
        
        COBOL: ARRAY-NAME(5) or ARRAY-NAME(INDEX)
        Ailang: ArrayGet(ARRAY_NAME, Subtract(index, 1))
        
        Note: COBOL uses 1-based indexing, Ailang uses 0-based
        """
        array_name = self.normalize_name(node.array_name)
        
        # NEW: Check if array is in a pool
        if self.current_pool_name and array_name in self.variables and self.variables[array_name].get('occurs'):
            array_ref = f"{self.current_pool_name}.{array_name}"
        else:
            array_ref = array_name
        
        index_expr = self.convert_expression(node.index, variables)
        
        # Convert 1-based to 0-based indexing
        zero_based_index = FunctionCall(1, 1, 'Subtract', [
            index_expr,
            Number(1, 1, 1)
        ])
        return FunctionCall(1, 1, 'ArrayGet', [
            Identifier(1, 1, array_ref),  # Use array_ref instead of array_name
            zero_based_index
        ])


    def convert_unstring(self, stmt: COBOLUnstring, variables: Dict) -> List[Assignment]:
        """Convert COBOL UNSTRING to Ailang StringSplit + array access
        
        UNSTRING source DELIMITED BY "," INTO field1 field2
        becomes:
        TEMP_PARTS = StringSplit(source, ",")
        field1 = Get(TEMP_PARTS, 0)
        field2 = Get(TEMP_PARTS, 1)
        """
        # Convert source and delimiter
        source_expr = self.convert_expression(stmt.source, variables)
        delimiter_expr = self.convert_expression(stmt.delimiter, variables)
        
        # Generate temporary variable for split result
        temp_var = f"TEMP_UNSTRING_PARTS_{id(stmt)}"
        
        # Split the string
        split_call = FunctionCall(1, 1, 'StringSplit', [source_expr, delimiter_expr])
        split_assign = Assignment(1, 1, temp_var, split_call)
        
        # Create assignments for each target field
        result = [split_assign]
        
        for i, target in enumerate(stmt.targets):
            normalized_target = self.normalize_name(target)
            # âœ… FIX: Add pool prefix to target field assignment
            target_with_prefix = self.make_assignment_target(normalized_target, variables)
            
            # Get the i-th element from the split result
            # Use AIMacro.Get if available, otherwise XArray.XGet
            get_call = FunctionCall(
                1, 1, 
                'ArrayGet',  # âœ… Built-in array accessor
                [Identifier(1, 1, temp_var), Number(1, 1, i)]
            )
            
            # Check if target is numeric and needs conversion
            if normalized_target in variables and variables[normalized_target]['type'] == 'Integer':
                get_call = FunctionCall(1, 1, 'StringToNumber', [get_call])
            
            result.append(Assignment(1, 1, target_with_prefix, get_call))
        
        # If TALLYING IN was specified, add count assignment
        if stmt.count:
            count_var = self.normalize_name(stmt.count)
            # Count = length of parts array
            count_call = FunctionCall(1, 1, 'ArrayLength', [Identifier(1, 1, temp_var)])
            result.append(Assignment(1, 1, self.make_assignment_target(count_var, variables), count_call)) # This was already correct
        
        return result

    

    def convert_string_concat(self, stmt: COBOLStringConcat, variables: Dict) -> List[Statement]:
        """
        Convert COBOL STRING statement to Ailang
        STRING field1 field2 DELIMITED BY SIZE INTO target
        becomes: target = StringConcat(field1, StringConcat(field2, ...))
        """
        statements = []
        
        # Build a series of StringConcat calls
        # Start with first field
        if not stmt.source_fields:
            return statements
        
        # Convert first source field
        result = self.convert_expression(stmt.source_fields[0], variables)
        
        # Chain concatenations for remaining fields
        for i in range(1, len(stmt.source_fields)):
            field_expr = self.convert_expression(stmt.source_fields[i], variables)
            # Create StringConcat function call
            result = FunctionCall(1, 1, "StringConcat", [result, field_expr])
        
        # Assign result to target
        target_name = self.normalize_name(stmt.target)
        assign = Assignment(1, 1, self.make_assignment_target(target_name, variables), result)
        statements.append(assign)
        
        return statements



class AILangASTSerializer:
    
    def __init__(self):
        self.indent_level = 0
        self.indent_str = "    "
    
    def indent(self):
        return self.indent_str * self.indent_level
    
    def serialize(self, ast: Program) -> str:
        lines = []
        
        # Always add FixedPoint library for now (can optimize later)
        lines.append("LibraryImport.Cobol")
        lines.append("LibraryImport.Library.FixedPoint") # NEW
        lines.append("")
        lines.append("// Generated from COBOL source")
        lines.append("")
        
        for func in ast.declarations:
            lines.extend(self.serialize_function(func))
            lines.append("")
        
        lines.append("RunTask(Main)")
        lines.append("")
        
        return '\n'.join(lines)
    
    def serialize_function(self, func) -> List[str]:
        lines = []
        
        if func.__class__.__name__ == 'SubRoutine':
            param_str = ""
            if hasattr(func, 'input_params') and func.input_params:
                params = ', '.join(func.input_params)
                param_str = f"({params})"
            lines.append(f"SubRoutine.{func.name}{param_str} {{")
            self.indent_level += 1
            for stmt in func.body:
                stmt_lines = self.serialize_statement(stmt)
                for line in stmt_lines:
                    lines.append(self.indent() + line)
            self.indent_level -= 1
            lines.append("}")
        elif func.name == 'Main' and not func.input_params and not func.output_type:
            lines.append(f"SubRoutine.Main {{")
            self.indent_level += 1
            for stmt in func.body:
                stmt_lines = self.serialize_statement(stmt)
                for line in stmt_lines:
                    lines.append(self.indent() + line)
            self.indent_level -= 1
            lines.append("}")
        else:
            lines.append(f"Function.{func.name} {{")
            if func.input_params:
                for param in func.input_params:
                    lines.append(f"{self.indent_str}Input: {param}")
            if func.output_type:
                lines.append(f"{self.indent_str}Output: {func.output_type}")
            lines.append(f"{self.indent_str}Body: {{")
            self.indent_level += 1
            for stmt in func.body:
                stmt_lines = self.serialize_statement(stmt)
                for line in stmt_lines:
                    lines.append(self.indent() + line)
            self.indent_level -= 1
            lines.append(f"{self.indent_str}}}")
            lines.append("}")
        
        return lines
    
    def serialize_statement(self, stmt: Any) -> List[str]:
        if isinstance(stmt, Assignment):
            if isinstance(stmt.target, str):
                target_str = stmt.target
            else:
                target_str = self.serialize_expr(stmt.target)
            return [f"{target_str} = {self.serialize_expr(stmt.value)}"]
        elif isinstance(stmt, RunTask):
            if stmt.arguments:
                args = ', '.join(f"{k} => {self.serialize_expr(v)}" for k, v in stmt.arguments)
                return [f"RunTask({stmt.task_name}, {args})"]
            else:
                return [f"RunTask({stmt.task_name})"]
        elif isinstance(stmt, FunctionCall):
            func_name = stmt.function if isinstance(stmt.function, str) else self.serialize_expr(stmt.function)
            args = ', '.join(self.serialize_expr(arg) for arg in stmt.arguments)
            return [f"{func_name}({args})"]
        
        elif isinstance(stmt, PrintMessage):
            msg = self.serialize_expr(stmt.message)
            return [f"PrintMessage({msg})"]
        
        elif isinstance(stmt, If):
            lines = [f"IfCondition {self.serialize_expr(stmt.condition)} ThenBlock: {{"]
            self.indent_level += 1
            for then_stmt in stmt.then_body:
                for line in self.serialize_statement(then_stmt):
                    lines.append(self.indent() + line)
            self.indent_level -= 1
            
            if stmt.else_body:
                lines.append(self.indent() + "} ElseBlock: {")
                self.indent_level += 1
                for else_stmt in stmt.else_body:
                    for line in self.serialize_statement(else_stmt):
                        lines.append(self.indent() + line)
                self.indent_level -= 1
            
            lines.append(self.indent() + "}")
            return lines
        
        elif isinstance(stmt, While):
            lines = [f"WhileLoop {self.serialize_expr(stmt.condition)} {{"]
            self.indent_level += 1
            for body_stmt in stmt.body:
                for line in self.serialize_statement(body_stmt):
                    lines.append(self.indent() + line)
            self.indent_level -= 1
            lines.append(self.indent() + "}")
            return lines
        
        elif isinstance(stmt, ReturnValue):
            return [f"ReturnValue({self.serialize_expr(stmt.value)})"]
        
        else:
            print(f"WARNING: Unknown statement type: {type(stmt).__name__}")
            return [f"// Unknown statement type: {type(stmt).__name__}"]
    
    def serialize_expr(self, expr: Any) -> str:
        if isinstance(expr, Identifier):
            return expr.name
        elif isinstance(expr, Number):
            return str(expr.value)
        elif isinstance(expr, String):
            return f'"{expr.value}"'
        elif isinstance(expr, FunctionCall):
            func_name = expr.function if isinstance(expr.function, str) else self.serialize_expr(expr.function)
            args = ', '.join(self.serialize_expr(arg) for arg in expr.arguments)
            return f"{func_name}({args})"
        else:
            print(f"WARNING: Unknown expression type: {type(expr).__name__}, expr={expr}")
            return f"/* UNKNOWN: {type(expr).__name__} */"
        
        loop_body = [
            FunctionCall(1, 1, 'RunTask', [Identifier(1, 1, para_name)]),
            Assignment(1, 1, counter_name, 
                      FunctionCall(1, 1, 'Add', [
                          Identifier(1, 1, counter_name),
                          Number(1, 1, 1)
                      ]))
        ]
        
        condition = FunctionCall(1, 1, 'LessThan', [
            Identifier(1, 1, counter_name),
            times_value
        ])
        
        return [
            Assignment(1, 1, counter_name, Number(1, 1, 0)),
            While(1, 1, condition, loop_body)
        ]
    
    def convert_perform_varying(self, stmt: COBOLPerformVarying, variables: Dict) -> Any:
        var_name = self.normalize_name(stmt.variable)
        from_value = self.convert_expression(stmt.from_expr, variables)
        by_value = self.convert_expression(stmt.by_expr, variables)
        condition = self.convert_expression(stmt.until_condition, variables)
        
        negated_condition = FunctionCall(1, 1, 'Not', [condition])
        
        loop_body = []
        for loop_stmt in stmt.statements:
            converted = self.convert_statement(loop_stmt, variables)
            if isinstance(converted, list):
                loop_body.extend(converted)
            elif converted:
                loop_body.append(converted)
        
        loop_body.append(
            Assignment(1, 1, var_name,
                      FunctionCall(1, 1, 'Add', [
                          Identifier(1, 1, var_name),
                          by_value
                      ]))
        )
        
        return [
            Assignment(1, 1, var_name, from_value),
            While(1, 1, negated_condition, loop_body)
        ]
    
    def convert_evaluate(self, stmt: COBOLEvaluate, variables: Dict) -> If:
        """Convert EVALUATE to nested If/ElseIf chain"""
        subject = self.convert_expression(stmt.subject, variables)
        
        # Determine comparison function based on subject type
        comparison_func = 'EqualTo'
        
        # Check if subject is a string
        if isinstance(stmt.subject, COBOLIdentifier):
            var_name = self.normalize_name(stmt.subject.name)
            if var_name in variables and variables[var_name]['type'] == 'String':
                comparison_func = 'StringEquals'
        elif isinstance(stmt.subject, COBOLStringLiteral):
            comparison_func = 'StringEquals'
        elif isinstance(stmt.subject, COBOLFunctionCall):
            # String functions return strings
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
                value = self.convert_expression(when_clause.value, variables)
                condition = FunctionCall(1, 1, comparison_func, [subject, value])
                
                if result is None:
                    result = If(1, 1, condition, then_body, None)
                else:
                    result = If(1, 1, condition, then_body, [result] if not isinstance(result, list) else result)
        
        return result
    
    def convert_accept(self, stmt: COBOLAccept, variables: Dict) -> Assignment:
        """Convert ACCEPT to GetUserInput assignment
        
        ACCEPT USER-NAME becomes:
        USER_NAME = GetUserInput()
        
        Note: For numeric variables, we need to convert the string input
        ACCEPT USER-AGE becomes:
        USER_AGE = StringToNumber(GetUserInput())
        """
        var_name = self.normalize_name(stmt.variable)
        
        # Get user input
        input_call = FunctionCall(1, 1, 'GetUserInput', [])
        
        # Check if variable is numeric - if so, convert the input
        if var_name in variables and variables[var_name]['type'] == 'Integer':
            input_value = FunctionCall(1, 1, 'StringToNumber', [input_call])
        else:
            input_value = input_call
        
        return Assignment(1, 1, var_name, input_value)
    
   

    def convert_expression(self, expr: COBOLASTNode, variables: Dict) -> Any:
        if isinstance(expr, COBOLBinaryOp):
            left = self.convert_expression(expr.left, variables)
            right = self.convert_expression(expr.right, variables)
            
            func_name = self.operator_mappings.get(expr.operator, 'Add')
            
            # Special case: string comparison needs StringEquals, not EqualTo
            if expr.operator == '=' and self._is_string_expr(expr.left, expr.right, variables):
                func_name = 'StringEquals'
            
            # Check if it's a callable (lambda)
            if callable(func_name):
                return func_name(left, right)  # Call the lambda
            else:
                return FunctionCall(1, 1, func_name, [left, right])
        elif isinstance(expr, COBOLUnaryOp):
            operand = self.convert_expression(expr.operand, variables)
            if expr.operator == '-':
                return FunctionCall(1, 1, 'Subtract', [Number(1, 1, 0), operand])
            else:
                return operand
        
        elif isinstance(expr, COBOLIdentifier):
            return Identifier(1, 1, self.normalize_name(expr.name))
        
        elif isinstance(expr, COBOLNumberLiteral):
            if '.' in expr.value:
                return Number(1, 1, int(float(expr.value)))
            else:
                return Number(1, 1, int(expr.value))
        
        elif isinstance(expr, COBOLStringLiteral):
            # âœ“ Returns direct string literal
            return String(1, 1, expr.value)
        elif isinstance(expr, COBOLArraySubscript):
            return self.convert_array_subscript(expr, variables)
        elif isinstance(expr, COBOLFunctionCall):
            return self.convert_function_call(expr, variables)
        
        else:
            return Number(1, 1, 0)

    def convert_function_call(self, expr: COBOLFunctionCall, variables: Dict) -> FunctionCall:
            if expr.function_name == 'UPPER-CASE':
                arg = self.convert_expression(expr.arguments[0], variables)
                return FunctionCall(1, 1, 'StringToUpper', [arg])
            # Add more functions here later << sorry claude updated the patches on me last minute. 
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
            var_name = self.normalize_name(left.name)
            if var_name in variables and variables[var_name]['type'] == 'String':
                return True
        if isinstance(right, COBOLIdentifier):
            var_name = self.normalize_name(right.name)
            if var_name in variables and variables[var_name]['type'] == 'String':
                return True
        return False

    def convert_array_subscript(self, node: COBOLArraySubscript, variables: Dict) -> FunctionCall:
        """Convert array subscript to Ailang ArrayGet
        
        COBOL: ARRAY-NAME(5) or ARRAY-NAME(INDEX)
        Ailang: ArrayGet(ARRAY_NAME, Subtract(index, 1))
        
        Note: COBOL uses 1-based indexing, Ailang uses 0-based
        """
        array_name = self.normalize_name(node.array_name)
        index_expr = self.convert_expression(node.index, variables)
        
        # Convert 1-based to 0-based indexing
        zero_based_index = FunctionCall(1, 1, 'Subtract', [
            index_expr,
            Number(1, 1, 1)
        ])
        
        return FunctionCall(1, 1, 'ArrayGet', [
            Identifier(1, 1, array_name),
            zero_based_index
        ])

    def convert_unstring(self, stmt: COBOLUnstring, variables: Dict) -> List[Assignment]:
        """Convert COBOL UNSTRING to Ailang StringSplit + array access
        
        UNSTRING source DELIMITED BY "," INTO field1 field2
        becomes:
        TEMP_PARTS = StringSplit(source, ",")
        field1 = Get(TEMP_PARTS, 0)
        field2 = Get(TEMP_PARTS, 1)
        """
        # Convert source and delimiter
        source_expr = self.convert_expression(stmt.source, variables)
        delimiter_expr = self.convert_expression(stmt.delimiter, variables)
        
        # Generate temporary variable for split result
        temp_var = f"TEMP_UNSTRING_PARTS_{id(stmt)}"
        
        # Split the string
        split_call = FunctionCall(1, 1, 'StringSplit', [source_expr, delimiter_expr])
        split_assign = Assignment(1, 1, temp_var, split_call)
        
        # Create assignments for each target field
        result = [split_assign]
        
        for i, target in enumerate(stmt.targets):
            normalized_target = self.normalize_name(target)
            
            # Get the i-th element from the split result
            # Use AIMacro.Get if available, otherwise XArray.XGet
            get_call = FunctionCall(
                1, 1, 
                'XArray.XGet',  # Could also be 'AIMacro.Get'
                [Identifier(1, 1, temp_var), Number(1, 1, i)]
            )
            
            # Check if target is numeric and needs conversion
            if normalized_target in variables and variables[normalized_target]['type'] == 'Integer':
                get_call = FunctionCall(1, 1, 'StringToNumber', [get_call])
            
            result.append(Assignment(1, 1, normalized_target, get_call))
        
        # If TALLYING IN was specified, add count assignment
        if stmt.count:
            count_var = self.normalize_name(stmt.count)
            # Count = length of parts array
            count_call = FunctionCall(1, 1, 'XArray.XSize', [Identifier(1, 1, temp_var)])
            result.append(Assignment(1, 1, count_var, count_call))
        
        return result

    def convert_string_concat(self, stmt: COBOLStringConcat, variables: Dict) -> Assignment:
        """
        Convert COBOL STRING statement to Ailang
        STRING field1 field2 DELIMITED BY SIZE INTO target
        becomes: target = StringConcat(field1, field2)
        """
        # Build a series of StringConcat calls
        # Start with first field
        if not stmt.source_fields:
            # If no source, assign empty string
            target_name = self.normalize_name(stmt.target)
            return Assignment(1, 1, target_name, String(1, 1, ""))
        
        # Convert first source field
        result = self.convert_expression(stmt.source_fields[0], variables)
        
        # Chain concatenations for remaining fields
        for i in range(1, len(stmt.source_fields)):
            field_expr = self.convert_expression(stmt.source_fields[i], variables)
            # Create StringConcat function call
            result = FunctionCall(
                1, 1,
                "StringConcat",
                [result, field_expr]
            )
        
        # Assign result to target
        target_name = self.normalize_name(stmt.target)
        assign = Assignment(
            1, 1,
            target_name,
            result
        )
        
        return assign



class AILangASTSerializer:
    
    def __init__(self):
        self.indent_level = 0
        self.indent_str = "    "
    
    def indent(self):
        return self.indent_str * self.indent_level
    
    def serialize(self, ast: Program) -> str:
        lines = []
        
        lines.append("LibraryImport.Cobol")
        lines.append("")
        lines.append("// Generated from COBOL source")
        lines.append("")
        
        # Serialize all declarations (Pools and Functions)
        for decl in ast.declarations:
            
            if isinstance(decl, Pool):
                lines.extend(self.serialize_pool(decl))
            else:
                lines.extend(self.serialize_function(decl))
            lines.append("")
        
        lines.append("RunTask(Main)")
        lines.append("")
        
        return '\n'.join(lines)
    
    def serialize_pool(self, pool) -> List[str]:
        """Serializes a Pool (e.g., FixedPool) AST node."""
        lines = [f"{pool.pool_type}.{pool.name} {{"]
        self.indent_level += 1
        for item in pool.body:
            # item is a ResourceItem
            key = item.key
            # Assuming 'Initialize' is the only attribute for now
            init_attr = item.attributes.get("Initialize")
            if init_attr:
                value_str = self.serialize_expr(init_attr)
                lines.append(self.indent() + f'"{key}": Initialize={value_str}')
        self.indent_level -= 1
        lines.append("}")
        return lines
    
    def serialize_function(self, func) -> List[str]:
        lines = []
        
        if func.__class__.__name__ == 'SubRoutine':
            lines.append(f"SubRoutine.{func.name} {{")
            self.indent_level += 1
            for stmt in func.body:
                stmt_lines = self.serialize_statement(stmt)
                for line in stmt_lines:
                    lines.append(self.indent() + line)
            self.indent_level -= 1
            lines.append("}")
        elif func.name == 'Main' and not func.input_params and not func.output_type:
            lines.append(f"SubRoutine.Main {{")
            self.indent_level += 1
            for stmt in func.body:
                stmt_lines = self.serialize_statement(stmt)
                for line in stmt_lines:
                    lines.append(self.indent() + line)
            self.indent_level -= 1
            lines.append("}")
        else:
            lines.append(f"Function.{func.name} {{")
            if func.input_params:
                for param in func.input_params:
                    lines.append(f"{self.indent_str}Input: {param}")
            if func.output_type:
                lines.append(f"{self.indent_str}Output: {func.output_type}")
            lines.append(f"{self.indent_str}Body: {{")
            self.indent_level += 1
            for stmt in func.body:
                stmt_lines = self.serialize_statement(stmt)
                for line in stmt_lines:
                    lines.append(self.indent() + line)
            self.indent_level -= 1
            lines.append(f"{self.indent_str}}}")
            lines.append("}")
        
        return lines
    
    def serialize_statement(self, stmt: Any) -> List[str]:
        if isinstance(stmt, Assignment):
            # Handle both string targets and Identifier targets with dot notation
            if isinstance(stmt.target, str):
                target_str = stmt.target
            elif isinstance(stmt.target, Identifier):
                target_str = stmt.target.name  # This includes pool.field notation
            else:
                target_str = self.serialize_expr(stmt.target)
            return [f"{target_str} = {self.serialize_expr(stmt.value)}"]
        elif isinstance(stmt, RunTask):
            if stmt.arguments:
                args = ', '.join(f"{k} => {self.serialize_expr(v)}" for k, v in stmt.arguments)
                return [f"RunTask({stmt.task_name}, {args})"]
            else:
                return [f"RunTask({stmt.task_name})"]
        elif isinstance(stmt, FunctionCall):
            func_name = stmt.function if isinstance(stmt.function, str) else self.serialize_expr(stmt.function)
            args = ', '.join(self.serialize_expr(arg) for arg in stmt.arguments)
            return [f"{func_name}({args})"]
        
        elif isinstance(stmt, PrintMessage):
            msg = self.serialize_expr(stmt.message)
            return [f"PrintMessage({msg})"]
        
        elif isinstance(stmt, If):
            lines = [f"IfCondition {self.serialize_expr(stmt.condition)} ThenBlock: {{"]
            self.indent_level += 1
            for then_stmt in stmt.then_body:
                for line in self.serialize_statement(then_stmt):
                    lines.append(self.indent() + line)
            self.indent_level -= 1
            
            if stmt.else_body:
                lines.append(self.indent() + "} ElseBlock: {")
                self.indent_level += 1
                for else_stmt in stmt.else_body:
                    for line in self.serialize_statement(else_stmt):
                        lines.append(self.indent() + line)
                self.indent_level -= 1
            
            lines.append(self.indent() + "}")
            return lines
        
        elif isinstance(stmt, While):
            lines = [f"WhileLoop {self.serialize_expr(stmt.condition)} {{"]
            self.indent_level += 1
            for body_stmt in stmt.body:
                for line in self.serialize_statement(body_stmt):
                    lines.append(self.indent() + line)
            self.indent_level -= 1
            lines.append(self.indent() + "}")
            return lines
        
        elif isinstance(stmt, ReturnValue):
            return [f"ReturnValue({self.serialize_expr(stmt.value)})"]
        
        else:
            print(f"WARNING: Unknown statement type: {type(stmt).__name__}")
            return [f"// Unknown statement type: {type(stmt).__name__}"]
    
    def serialize_expr(self, expr: Any) -> str:
        if isinstance(expr, Identifier):
            return expr.name
        elif isinstance(expr, Number):
            return str(expr.value)
        elif isinstance(expr, String):
            return f'"{expr.value}"'
        elif isinstance(expr, FunctionCall):
            func_name = expr.function if isinstance(expr.function, str) else self.serialize_expr(expr.function)
            args = ', '.join(self.serialize_expr(arg) for arg in expr.arguments)
            return f"{func_name}({args})"
        else:
            print(f"WARNING: Unknown expression type: {type(expr).__name__}, expr={expr}")
            return f"/* UNKNOWN: {type(expr).__name__} */"