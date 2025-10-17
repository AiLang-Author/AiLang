#!/usr/bin/env python3

# Copyright (c) 2025 Sean Collins, 2 Paws Machine and Engineering. All rights reserved.
#
# Licensed under the Sean Collins Software License (SCSL). See the LICENSE file in the root directory of this project
# for the full terms and conditions, including restrictions on forking, corporate use, and permissions for private/teaching purposes.

from __future__ import annotations

"""
COBOL to Ailang AST Converter - Core Conversion Logic
"""
import sys
import os
from typing import List, Optional, Any, Dict

# Add project root to path
project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '../..'))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

from ailang_parser.ailang_ast import (
    Program, Function, If, While, ForEvery, ReturnValue,
    Assignment, FunctionCall, Identifier, Number, String, SubRoutine,
    PrintMessage, RunTask
)
from ailang_parser.ast_modules.ast_pools import Pool, ResourceItem

# Use absolute imports for parser (sibling package)
from cobol_frontend.parser.ast_nodes import (
    COBOLCompilationUnit,
    COBOLProgram,
    COBOLSection,
    COBOLDataDivision,
    COBOLVariableDecl,
    COBOLProcedureDivision,
    COBOLParagraph,
    COBOLDisplay,
    COBOLArraySubscript,
    COBOLCall,
    COBOLMove,
    COBOLCompute,
    COBOLArithmetic,
    COBOLIf,
    COBOLFunctionCall,
    COBOLPerformTimes,
    COBOLPerformUntil,
    COBOLPerformParagraph,
    COBOLPerformVarying,
    COBOLEvaluate,
    COBOLWhenClause,
    COBOLAccept,
    COBOLStopRun,
    COBOLGoback,
    COBOLExit,
    COBOLBinaryOp,
    COBOLUnaryOp,
    COBOLIdentifier,
    COBOLNumberLiteral,
    COBOLStringLiteral,
    COBOLASTNode,
    COBOLStringConcat,
    COBOLUnstring,
    COBOLInspect,
)

from .type_system import TypeSystem
from .expression_converter import ExpressionConverter
from .statement_converter import StatementConverter
from .decimal_support import DecimalSupport
from .debug_emitter import DebugEmitter

class COBOLToAilangMultiProgramConverter:
    def __init__(self, debug=False):
        self.debug = debug
        # These will be reset for each program
        self.variables = {}
        self.paragraphs = {}
        self.program_linkage_params = {}  # Track linkage params per program
        self.program_ws_pools = {}        # Track WORKING-STORAGE pools
        self.condition_names = {}  # Maps condition_name -> condition_info
        self.subscript_parent_map = {}
        # Global accumulator for all subroutines
        self.all_subroutines = []
        self.string_literals = {} 
        self._metadata_header = ""                                  
        self.subscript_parent_map = {}  # Maps child_name -> parent_name for subscripting
        
        
        # Initialize debug emitter (ENABLED by default)
        from .debug_emitter import DebugEmitter
        self.debug_emitter = DebugEmitter(enabled=True, default_level=2)
        
        # Initialize helper systems
        self.type_system = TypeSystem(self)
        self.decimal_support = DecimalSupport(self)
        self.expression_converter = ExpressionConverter(self)
        self.statement_converter = StatementConverter(self)
        
            
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
            ailang_ast = self.convert_compilation_unit(compilation_unit)
        else:
            raise ValueError(f"Expected COBOLCompilationUnit or COBOLProgram, got {type(ast_node)}")
        
        # NEW: Prepend metadata comments
        header = []
        
        for program in ast_node.programs:
            if program.metadata_lines:
                header.append(f"// ========================================")
                header.append(f"// COBOL Program: {program.program_id}")
                header.append(f"// ========================================")
                for line in program.metadata_lines:
                    header.append(f"// {line}")
                header.append(f"// ========================================")
                header.append("")  # Blank line
        
        # Add the header to the Ailang Program AST as a special comment node
        # This is a conceptual step. For now, we'll handle it during serialization.
        # We'll store it on the converter instance to be used by the serializer.
        self._metadata_header = "\n".join(header) if header else ""
        
        return ailang_ast
    
    def convert_split_programs(self, compilation_unit, output_dir):
        """Generate one .ailang file per COBOL program"""
        import os
        os.makedirs(output_dir, exist_ok=True)
        
        generated_files = []
        
        for program in compilation_unit.programs:
            print(f"  Converting {program.program_id}...", file=sys.stderr)
            
            # Convert this single program
            single_unit = COBOLCompilationUnit(programs=[program])
            ailang_ast = self.convert(single_unit)
            
            # Serialize to string  
            serializer = AILangASTSerializer()
            ailang_source = serializer.serialize(ailang_ast)
            
            # Write file
            output_file = os.path.join(output_dir, f"{program.program_id}.ailang")
            with open(output_file, 'w') as f:
                f.write(ailang_source)
            
            generated_files.append(output_file)
            print(f"    ✓ {output_file}", file=sys.stderr)
        
        return generated_files
    
    
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
                        # If it's a group with children (like 01 PARM-1), extract child field names
                        if hasattr(var_decl, 'children') and var_decl.children:
                            for child in var_decl.children:
                                child_name = self.normalize_name(child.name)
                                linkage_params.append(child_name)
                                if self.debug:
                                    print(f"    Linkage field from {var_decl.name}: {child_name}")
                        else:
                            # No children, use the variable itself
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

        # Create Main() that calls all entry programs
        main_body = []
        for program in unit.programs:
            if not program.is_nested:
                program_name = self.normalize_name(program.program_id)
                # Check if this program has linkage parameters (making it a subroutine)
                has_linkage = program_name in self.program_linkage_params
                
                if not has_linkage:
                    # No linkage = entry program, call it from Main
                    main_body.append(RunTask(1, 1, program_name, []))
                elif self.debug:
                    print(f"  Skipping {program_name} in Main (has LINKAGE SECTION)")
        
        main_subroutine = SubRoutine(1, 1, 'Main', main_body)
        
        # Generate FixedPool declarations for programs with linkage parameters
        linkage_pools = []
        for prog_name, params in self.program_linkage_params.items():
            pool_fields = []
            for param in params:
                var_info = None
                if param in self.variables and self.variables[param].get('is_linkage'):
                    var_info = self.variables[param]
                
                # Determine correct initializer based on type
                if var_info:
                    if var_info['type'] == 'Integer':
                        init_value = Number(1, 1, 0)
                    else:  # String or Address
                        init_value = String(1, 1, "")
                else:
                    # Fallback: assume Integer if not found
                    init_value = Number(1, 1, 0)
                
                attributes = {"Initialize": init_value}
                item = ResourceItem(1, 1, key=param, value=None, attributes=attributes)
                pool_fields.append(item)
            pool_name = f"COBOL_{prog_name}_LINKAGE"
            pool = Pool(1, 1, pool_type="FixedPool", name=pool_name, body=pool_fields)
            linkage_pools.append(pool)
        
        # Generate FixedPool declarations for programs with shared variables
        ws_pools = []
        for prog_name, pool_info in self.program_ws_pools.items():
            pool_name = pool_info['name']
            pool_fields = []
            for var_name, var_info in pool_info['variables'].items():
                init_val = self.type_system.get_initial_value_node(var_info)
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
        
        HIGH-LEVEL ORCHESTRATOR - delegates to focused helper methods.
        Each phase is a separate, testable, reusable method.
        
        Returns:
            List of subroutines (program + paragraphs + nested)
        """
        # Phase 0: Reset state for THIS program
        self._reset_program_state(program)
        
        # Phase 1: Collect all variables from DATA DIVISION
        self._collect_all_variables(program)
        
        # Phase 2: Create variable pool and set up scope
        pool_name = self._create_variable_pool(program)
        
        # Phase 3: Register linkage parameters for CALL support
        self._register_linkage_parameters(program)
        
        # Phase 4: Handle copybooks (no PROCEDURE DIVISION)
        if program.procedure_division is None:
            return self._handle_copybook_program(program)
        
        # Phase 5: Collect paragraphs from PROCEDURE DIVISION
        self._collect_paragraphs(program)
        
        # Phase 6: Generate subroutines (program + paragraphs)
        subroutines = self._generate_all_subroutines(program, pool_name)
        
        # Phase 7: Handle nested programs recursively
        subroutines.extend(self._convert_nested_programs(program))
        
        return subroutines
    
    def _reset_program_state(self, program: COBOLProgram):
        """Reset per-program state variables."""
        self.variables = {}
        self.paragraphs = {}
        self.condition_names = {}  # ✅ Reset per program
        
        program_name = self.normalize_name(program.program_id)
        self.current_program_name = program_name
        
        if self.debug:
            print(f"\n{'='*70}")
            print(f"Converting PROGRAM-ID: {program.program_id}")
            print(f"Ailang name: {program_name}")
            print(f"{'='*70}")
            
    def _collect_all_variables(self, program: COBOLProgram):
        """
        Collect variables from ALL sections of DATA DIVISION.
        Delegates to section-specific collectors.
        """
        if not program.data_division:
            return
        
        # Collect from FILE SECTION (FD records)
        self._collect_file_section_variables(program)
        
        # Collect from WORKING-STORAGE SECTION
        self._collect_working_storage_variables(program)
        
        # Collect from LINKAGE SECTION
        self._collect_linkage_section_variables(program)


    def _collect_file_section_variables(self, program: COBOLProgram):
        """Extract FILE SECTION variables from FD records"""
        if not (program.data_division and program.data_division.file_section):
            return
        
        if self.debug:
            print(f"\n  FILE SECTION:")
            print(f"    Found {len(program.data_division.file_section)} file descriptors")
        
        # Iterate through each FD (File Descriptor)
        for fd in program.data_division.file_section:
            if self.debug:
                print(f"    FD {fd.file_name}: {len(fd.records)} records")
            
            # Process each record (01 level) in the FD
            for record in fd.records:
                # Flatten the record structure to get all elementary items
                elementary_items = self.type_system.flatten_variable_declarations([record])
                
                if self.debug:
                    print(f"      Record {record.name}: {len(elementary_items)} elementary items")
                
                # Add each elementary item as a variable
                for decl in elementary_items:
                    self._add_variable_from_declaration(
                        decl, 
                        [record],  # Parent list for REDEFINES lookups
                        is_linkage=False  # FILE SECTION vars are not linkage
                    )
        
        
    def _collect_working_storage_variables(self, program: COBOLProgram):
        """Extract WORKING-STORAGE variables"""
        if not (program.data_division and program.data_division.working_storage):
            return
        
        if self.debug:
            print(f"\n  WORKING-STORAGE SECTION:")
        
        original_ws = program.data_division.working_storage
        
        # ✅ First pass: Collect level 88 condition names
        self._collect_condition_names(original_ws, is_linkage=False)
        
        # Second pass: Collect regular variables (level 88 filtered out)
        elementary_items = self.type_system.flatten_variable_declarations(original_ws)
        
        # Build parent map for subscripting
        self._build_subscript_parent_map(original_ws)
        
        if self.debug:
            print(f"    Found {len(elementary_items)} elementary items")
        
        for decl in elementary_items:
            self._add_variable_from_declaration(decl, original_ws, is_linkage=False)

    def _collect_condition_names(self, declarations: List[COBOLVariableDecl], is_linkage: bool):
        """
        Collect level 88 condition names and map them to their IMMEDIATE parent variables.
        """
        print(f"\n🔍 _collect_condition_names: is_linkage={is_linkage}, got {len(declarations)} declarations")
        
        # 🔍 DEBUG - Show structure of level 88 items
        for decl in declarations:
            if decl.name in ['TABLE-01', 'TABLE_01']:
                print(f"  TABLE-01: Level {decl.level}, children={[f'L{c.level}:{c.name}' for c in (decl.children or [])]}")
            if decl.name in ['DN1']:
                print(f"  DN1: Level {decl.level}, children={[f'L{c.level}:{c.name}' for c in (decl.children or [])]}")
            if decl.level == 88:
                print(f"  ⚠️  PROBLEM: Level 88 '{decl.name}' is a TOP-LEVEL declaration! Should be child of parent.")
        
        def visit(decl: COBOLVariableDecl, parent_name=None, depth=0):
            """Recursively visit declarations, tracking parent variable."""
            current_name = self.normalize_name(decl.name)
            
            # 🔍 DEBUG - show tree structure for TABLE-01 and DN1 trees only
            if 'TABLE' in decl.name or 'DN' in decl.name or decl.level == 88:
                print(f"{'  '*depth}Visit L{decl.level}: {current_name}, parent={parent_name}, pic={decl.pic_clause}, children={len(decl.children) if decl.children else 0}")
            
            # If this is level 88, record it with its parent
            if decl.level == 88:
                print(f"{'  '*depth}🎯 FOUND L88: {current_name}, parent={parent_name}, value={decl.value}")
                if parent_name and decl.value:
                    self.condition_names[current_name] = {
                        'parent': parent_name,
                        'value': decl.value,
                        'is_linkage': is_linkage
                    }
                    if self.debug:
                        print(f"    Condition name: {current_name} "
                            f"(tests {parent_name} = {decl.value})")
                return  # Don't recurse into level 88 children
            
            # For non-88 items, determine what parent to pass to children
            # Only pass current item as parent if it's elementary (has PIC clause)
            # Group items (no PIC) should pass through their parent unchanged
            if decl.children:
                next_parent = current_name if decl.pic_clause else parent_name
                for child in decl.children:
                    visit(child, parent_name=next_parent, depth=depth+1)
        
        for decl in declarations:
            visit(decl, parent_name=None, depth=0)

            
    def _collect_linkage_section_variables(self, program: COBOLProgram):
        """Extract LINKAGE SECTION parameters"""
        if not (program.data_division and 
                hasattr(program.data_division, 'linkage_section') and 
                program.data_division.linkage_section):
            return
        
        if self.debug:
            print(f"\n  LINKAGE SECTION:")
        
        # ✅ First pass: Collect level 88 condition names
        self._collect_condition_names(program.data_division.linkage_section, is_linkage=True)
        
        # Second pass: Collect regular variables
        elementary_items = self.type_system.flatten_variable_declarations(
            program.data_division.linkage_section
        )
        
        # Build parent map
        self._build_subscript_parent_map(program.data_division.linkage_section)
        
        for decl in elementary_items:
            self._add_variable_from_declaration(decl, None, is_linkage=True)
            if self.debug:
                print(f"    {decl.name} (linkage)")
                
                
    def _build_subscript_parent_map(self, declarations: List[COBOLVariableDecl]):
        """
        Build a map of child fields to their subscriptable parent.
        
        When we have:
            03 PARENT OCCURS 10 TIMES.
            05 CHILD-GROUP.
                07 FIELD PIC X.
                88 CONDITION VALUE "A".   ← Skip this (level 88)
        
        And COBOL writes: MOVE X TO CHILD-GROUP(1) or FIELD(1)
        
        The subscript applies to PARENT for all descendants.
        This map stores: {"CHILD-GROUP": "PARENT", "FIELD": "PARENT"}
        
        Level 88 condition names are skipped as they're not real variables.
        """
        def visit(decl: COBOLVariableDecl, parent_with_occurs=None):
            """
            Recursively visit declarations, tracking the nearest parent with OCCURS.
            
            Args:
                decl: Current declaration
                parent_with_occurs: Name of nearest ancestor that has OCCURS, or None
            """
            # ✅ Skip level 88 condition names entirely
            if decl.level == 88:
                return
            
            # Determine new parent context for children
            new_parent = None
            if decl.occurs_count:
                # This node has OCCURS, it becomes the subscriptable parent
                new_parent = self.normalize_name(decl.name)
            else:
                # No OCCURS, inherit parent from above
                new_parent = parent_with_occurs
            
            # ✅ Record mapping for ANY node under a subscriptable parent
            # (not just elementary items with PIC)
            if parent_with_occurs and not decl.occurs_count:
                # This node is a descendant of an OCCURS group
                # Map it to the subscriptable ancestor
                child_name = self.normalize_name(decl.name)
                self.subscript_parent_map[child_name] = parent_with_occurs
                if self.debug:
                    print(f"    Subscript map: {child_name} -> {parent_with_occurs}")
            
            # Recurse into children with the new parent context
            if decl.children:
                for child in decl.children:
                    visit(child, new_parent)
        
        for decl in declarations:
            visit(decl, parent_with_occurs=None)
                

    def _add_variable_from_declaration(self, decl: COBOLVariableDecl,
                                        parent_ws_list=None, is_linkage=False):
        """
        Common logic to add a variable to self.variables.
        Used by both WORKING-STORAGE and LINKAGE collectors.
        """
        var_name = self.normalize_name(decl.name)
        
        # ✅ NEW: Handle group items with OCCURS (arrays of structures)
        # These have no PIC clause but need to be created as arrays
        if not decl.pic_clause and decl.occurs_count:
            if self.debug:
                print(f"    {var_name}: Group array (OCCURS {decl.occurs_count}, no PIC)")
            
            self.variables[var_name] = {
                'type': 'Integer',  # Placeholder type for structured arrays
                'value': '0',
                'occurs': decl.occurs_count,
                'decimal_places': None,
                'storage': 'DISPLAY',
                'is_signed': False,
                'precision': 0,
                'is_edited': False,
                'edit_format': None,
                'pic_clause': None,
                'is_linkage': is_linkage,
                'is_group_array': True  # Mark as group array for special handling
            }
            
            # ✅ Auto-create index variables from INDEXED BY clause
            if hasattr(decl, 'index_names') and decl.index_names:
                for index_name in decl.index_names:
                    idx_var_name = self.normalize_name(index_name)
                    self.variables[idx_var_name] = {
                        'type': 'Integer',
                        'value': '1',
                        'occurs': None,
                        'decimal_places': None,
                        'storage': 'DISPLAY',
                        'is_signed': False,
                        'precision': 0,
                        'is_edited': False,
                        'edit_format': None,
                        'pic_clause': None,
                        'is_linkage': is_linkage,
                        'usage': 'INDEX'
                    }
                    if self.debug:
                        print(f"    {idx_var_name}: Integer (INDEX for array {var_name})")
            
            return  # Early exit for group arrays
        
        # 🔧 NEW: Handle regular group items (no PIC, no OCCURS)
        # These are structural groups like CCVS-H-1 that can still be MOVE'd
        if not decl.pic_clause and not decl.occurs_count:
            # Calculate size from children or use default
            group_size = 120  # Default reasonable size for group items
            
            if self.debug:
                print(f"    {var_name}: String (group item, no PIC, size {group_size})")
            
            self.variables[var_name] = {
                'type': 'String',
                'value': '""',
                'occurs': None,
                'decimal_places': None,
                'storage': 'DISPLAY',
                'is_signed': False,
                'precision': 0,
                'is_edited': False,
                'edit_format': None,
                'pic_clause': f'X({group_size})',  # Synthetic PIC for group
                'is_linkage': is_linkage,
                'is_group': True  # Mark as group item
            }
            return  # Early exit for regular groups
        
        # ✅ Regular processing for elementary items (have PIC clause)
        storage_info = self.type_system.get_storage_info(
            decl.pic_clause,
            decl.usage_type,
            decl.is_signed,
            decl.decimal_places
        )
        
        var_value = decl.value if decl.value else (
            '""' if storage_info['ailang_type'] == 'String' else '0'
        )
        
        # Check for edited format
        is_edited = self.type_system.is_edited_format(decl.pic_clause) if decl.pic_clause else False
        edit_format = None
        if is_edited:
            edit_format = self.type_system.parse_edit_format(decl.pic_clause)
        
        # Build variable info dictionary
        self.variables[var_name] = {
            'type': storage_info['ailang_type'],
            'value': var_value,
            'occurs': decl.occurs_count,
            'decimal_places': decl.decimal_places,
            'storage': storage_info['storage'],
            'is_signed': storage_info['is_signed'],
            'precision': storage_info['precision'],
            'is_edited': is_edited,
            'edit_format': edit_format,
            'pic_clause': decl.pic_clause,
            'is_linkage': is_linkage
        }
        
        # Handle REDEFINES with OCCURS (only for WORKING-STORAGE)
        if not is_linkage and decl.redefines_target and decl.occurs_count and parent_ws_list:
            redefines_values = self.type_system.get_redefines_values(
                decl.redefines_target, parent_ws_list, decl
            )
            if redefines_values:
                self.variables[var_name]['redefines_values'] = redefines_values
                if self.debug:
                    print(f"    {var_name} REDEFINES {decl.redefines_target} "
                        f"with {len(redefines_values)} values")
        
        # ✅ Auto-create index variables from INDEXED BY clause
        if hasattr(decl, 'index_names') and decl.index_names:
            for index_name in decl.index_names:
                idx_var_name = self.normalize_name(index_name)
                self.variables[idx_var_name] = {
                    'type': 'Integer',
                    'value': '1',  # COBOL indexes start at 1
                    'occurs': None,
                    'decimal_places': None,
                    'storage': 'DISPLAY',
                    'is_signed': False,
                    'precision': 0,
                    'is_edited': False,
                    'edit_format': None,
                    'pic_clause': None,
                    'is_linkage': is_linkage,
                    'usage': 'INDEX'
                }
                if self.debug:
                    print(f"    {idx_var_name}: Integer (INDEX for array {var_name})")
        
        # Debug output
        if self.debug and not is_linkage:
            occurs_str = f" OCCURS {decl.occurs_count}" if decl.occurs_count else ""
            decimal_str = f" (V{decl.decimal_places})" if decl.decimal_places else ""
            usage_str = f" {decl.usage_type}" if decl.usage_type else ""
            sign_str = " SIGNED" if decl.is_signed else ""
            edit_str = f" EDITED({edit_format['type']})" if is_edited else ""
            print(f"    {var_name}: {storage_info['ailang_type']}{sign_str}"
                f"{decimal_str}{usage_str}{edit_str}{occurs_str} = {var_value}")
            
    def _create_variable_pool(self, program: COBOLProgram) -> Optional[str]:
        """
        Create a FixedPool for WORKING-STORAGE variables.
        Returns the pool name, or None if no pool needed.
        """
        if not self.variables:
            return None
        
        # Separate WS variables from linkage parameters
        ws_vars_only = {k: v for k, v in self.variables.items() 
                        if not v.get('is_linkage')}
        
        if not ws_vars_only:
            return None
        
        program_name = self.current_program_name
        pool_name = f"COBOL_{program_name}_VARS"
        
        self.program_ws_pools[program_name] = {
            'name': pool_name,
            'variables': ws_vars_only
        }
        self.current_pool_name = pool_name
        
        if self.debug:
            print(f"\n  Created pool: {pool_name} with {len(ws_vars_only)} variables")
        
        return pool_name

    def _register_linkage_parameters(self, program: COBOLProgram):
        """Register linkage parameters for CALL parameter passing."""
        linkage_params = [name for name, info in self.variables.items() 
                        if info.get('is_linkage')]
        
        if linkage_params:
            self.program_linkage_params[self.current_program_name] = linkage_params
            if self.debug:
                print(f"\n  Registered linkage params: {linkage_params}")
                
    def _handle_copybook_program(self, program: COBOLProgram) -> List[SubRoutine]:
        """Handle programs with no PROCEDURE DIVISION (data-only copybooks)."""
        if self.debug:
            print(f"  ℹ Data-only program (copybook) - no procedures generated.")
        return []
    
    
    def _collect_paragraphs(self, program: COBOLProgram):
        """
        Collect all named paragraphs from PROCEDURE DIVISION.
        Handles both:
        - Flat paragraphs (traditional COBOL)
        - Sections with paragraphs (NIST COBOL style)
        Also detect and create stubs for undeclared variables (e.g., from copybooks).
        
        SAFE: Only adds new functionality, doesn't change existing behavior.
        """
        if not program.procedure_division:
            return
        
        # ========================================================================
        # 🔧 NEW: Collect paragraphs from SECTIONS
        # ========================================================================
        if hasattr(program.procedure_division, 'sections') and program.procedure_division.sections:
            if self.debug:
                print(f"\n  SECTIONS ({len(program.procedure_division.sections)}):")
            
            for section in program.procedure_division.sections:
                section_name = self.normalize_name(section.name)
                
                if self.debug:
                    print(f"    {section.name} ({len(section.paragraphs)} paragraphs)")
                
                # Store section itself as a special "paragraph"
                # This allows GO TO SECTION-NAME to work
                self.paragraphs[section_name] = section
                
                # Store each paragraph under the section
                for para in section.paragraphs:
                    if isinstance(para, COBOLParagraph) and para.name:
                        para_name = self.normalize_name(para.name)
                        self.paragraphs[para_name] = para
                        
                        if self.debug:
                            print(f"      - {para.name}")
        
        # ========================================================================
        # ✅ EXISTING: Collect flat paragraphs (no section)
        # ========================================================================
        if not program.procedure_division.paragraphs:
            return
        
        named_paragraphs = [p for p in program.procedure_division.paragraphs 
                            if isinstance(p, COBOLParagraph) and p.name]
        
        # ✅ EXISTING: Debug output for paragraphs
        if self.debug and named_paragraphs:
            print(f"\n  PARAGRAPHS ({len(named_paragraphs)}):")
            for para in named_paragraphs:
                print(f"    - {para.name}")
        
        # ✅ EXISTING: Store paragraphs
        for para in named_paragraphs:
            para_name = self.normalize_name(para.name)
            self.paragraphs[para_name] = para
        
        # ========================================================================
        # 🆕 NEW: Scan for undeclared variables and create stubs
        # This section is OPTIONAL - wrapped in try/except for safety
        # ========================================================================
        try:
            # Scan for undeclared variables across ALL paragraphs
            all_undeclared = set()
            
            # 🔧 NEW: Check paragraphs in sections
            if hasattr(program.procedure_division, 'sections') and program.procedure_division.sections:
                for section in program.procedure_division.sections:
                    for para in section.paragraphs:
                        if para.name:
                            undeclared = self._detect_undeclared_variables_in_paragraph(para, self.variables)
                            all_undeclared.update(undeclared)
            
            # ✅ EXISTING: Check named paragraphs
            for para in named_paragraphs:
                undeclared = self._detect_undeclared_variables_in_paragraph(para, self.variables)
                all_undeclared.update(undeclared)
            
            # ✅ EXISTING: Also check inline (unnamed) paragraphs
            for para in program.procedure_division.paragraphs:
                if not para.name:  # Inline code
                    undeclared = self._detect_undeclared_variables_in_paragraph(para, self.variables)
                    all_undeclared.update(undeclared)
            
            # ✅ EXISTING: Create stub variables for undeclared ones
            if all_undeclared:
                if self.debug:
                    print(f"\n  ⚠️  UNDECLARED VARIABLES ({len(all_undeclared)}) - creating stubs:")
                    print(f"      (likely from COPY statements or external definitions)")
                
                for var_name in sorted(all_undeclared):
                    stub = self._create_stub_variable(var_name)
                    self.variables[var_name] = stub
                    
                    if self.debug:
                        print(f"      + {var_name}: {stub['type']} (stub)")
                
                # Update the pool if one exists
                if self.current_pool_name and self.program_ws_pools.get(self.current_program_name):
                    pool_info = self.program_ws_pools[self.current_program_name]
                    for var_name in all_undeclared:
                        if var_name in self.variables:
                            pool_info['variables'][var_name] = self.variables[var_name]
        
        except Exception as e:
            # If the new stub detection fails, just continue without it
            # This ensures existing functionality still works
            if self.debug:
                print(f"\n  ⚠️  Warning: Stub variable detection failed: {e}")
                print(f"      Continuing without stub generation...")


    def _is_special_register(self, name: str) -> bool:
        """Check if this is a COBOL special register that doesn't need declaration"""
        special_registers = {
            'RETURN_CODE', 'SORT_RETURN', 'NUMBER_OF_CALL_PARAMETERS',
            'TALLY', 'WHEN_COMPILED', 'LENGTH_OF', 'ADDRESS_OF'
        }
        return name.upper() in special_registers


    def _find_undeclared_in_expression(self, expr, variables: Dict) -> set:
        """Recursively find undeclared variables in an expression"""
        undeclared = set()
        
        if isinstance(expr, COBOLIdentifier):
            var_name = self.normalize_name(expr.name)
            if var_name not in variables and not self._is_special_register(var_name):
                undeclared.add(var_name)
        
        elif isinstance(expr, COBOLBinaryOp):
            undeclared.update(self._find_undeclared_in_expression(expr.left, variables))
            undeclared.update(self._find_undeclared_in_expression(expr.right, variables))
        
        elif isinstance(expr, COBOLUnaryOp):
            undeclared.update(self._find_undeclared_in_expression(expr.operand, variables))
        
        return undeclared


    def _find_undeclared_in_statement(self, stmt, variables: Dict) -> set:
        """Recursively find undeclared variables in a statement"""
        undeclared = set()
        
        try:
            # MOVE statement
            if isinstance(stmt, COBOLMove):
                if isinstance(stmt.source, COBOLIdentifier):
                    var_name = self.normalize_name(stmt.source.name)
                    if var_name not in variables and not self._is_special_register(var_name):
                        undeclared.add(var_name)
                
                for target in stmt.targets:
                    if isinstance(target, COBOLIdentifier):
                        var_name = self.normalize_name(target.name)
                        if var_name not in variables and not self._is_special_register(var_name):
                            undeclared.add(var_name)
            
            # DISPLAY statement
            elif isinstance(stmt, COBOLDisplay):
                for item in stmt.expressions:
                    if isinstance(item, COBOLIdentifier):
                        var_name = self.normalize_name(item.name)
                        if var_name not in variables and not self._is_special_register(var_name):
                            undeclared.add(var_name)
            
            # IF statement
            elif isinstance(stmt, COBOLIf):
                if stmt.condition:
                    undeclared.update(self._find_undeclared_in_expression(stmt.condition, variables))
                for then_stmt in stmt.then_statements:
                    undeclared.update(self._find_undeclared_in_statement(then_stmt, variables))
                if stmt.else_statements:
                    for else_stmt in stmt.else_statements:
                        undeclared.update(self._find_undeclared_in_statement(else_stmt, variables))
            
            # COMPUTE statement
            elif isinstance(stmt, COBOLCompute):
                if isinstance(stmt.target, COBOLIdentifier):
                    var_name = self.normalize_name(stmt.target.name)
                    if var_name not in variables and not self._is_special_register(var_name):
                        undeclared.add(var_name)
                if stmt.expression:
                    undeclared.update(self._find_undeclared_in_expression(stmt.expression, variables))
            
            # Add more statement types as needed
            # For now, we handle the most common ones
        
        except Exception as e:
            # If we encounter an unknown statement type, just skip it
            pass
        
        return undeclared


    def _detect_undeclared_variables_in_paragraph(self, para: COBOLParagraph, 
                                                  variables: Dict) -> set:
        """
        Scan a paragraph for undeclared variable references.
        Returns set of undeclared variable names.
        """
        undeclared = set()
        
        for stmt in para.statements:
            undeclared.update(self._find_undeclared_in_statement(stmt, variables))
        
        return undeclared


    def _create_stub_variable(self, var_name: str) -> Dict:
        """
        Create a stub variable declaration for undeclared CCVS copybook variables.
        These are typically String variables used for formatted output.
        """
        return {
            'type': 'String',
            'value': '""',
            'occurs': None,
            'decimal_places': None,
            'storage': None,
            'is_signed': False,
            'precision': None,
            'is_edited': False,
            'edit_format': None,
            'pic_clause': 'X(80)',  # Standard COBOL line width
            'is_linkage': False,
            'is_stub': True  # Mark as auto-generated for debugging
        }
                
    def _generate_all_subroutines(self, program: COBOLProgram, 
                                pool_name: Optional[str]) -> List[SubRoutine]:
        """
        Phase 6: Generate all subroutines for a program.
        
        Generates:
        - Main program subroutine
        - Section subroutines (if any)
        - Paragraph subroutines (flat or under sections)
        
        Returns:
            List of SubRoutine objects
        """
        subroutines = []
        program_name = self.normalize_name(program.program_id)
        self.current_program_name = program_name
        
        # Step 1: Create main program subroutine
        program_subroutine = self.create_program_subroutine(
            program,
            self.variables,
            pool_name
        )
        subroutines.append(program_subroutine)
        
        # Step 2: Generate section subroutines (if any)
        if hasattr(program.procedure_division, 'sections') and program.procedure_division.sections:
            if self.debug:
                print(f"\n  Generating {len(program.procedure_division.sections)} section subroutines")
            
            for section in program.procedure_division.sections:
                # Create subroutine for the section itself
                section_subroutine = self.create_section_subroutine(
                    section,
                    self.variables,
                    program_name,
                    pool_name
                )
                subroutines.append(section_subroutine)
                
                if self.debug:
                    print(f"    - Section: {section.name} → {section_subroutine.name}")
                
                # Create subroutines for paragraphs under this section
                for para in section.paragraphs:
                    if para.name:
                        para_subroutine = self.create_paragraph_subroutine(
                            para,
                            self.variables,
                            program_name,
                            pool_name
                        )
                        subroutines.append(para_subroutine)
                        
                        if self.debug:
                            print(f"      - Paragraph: {para.name} → {para_subroutine.name}")
        
        # Step 3: Generate flat paragraph subroutines (not in sections)
        for para_name, para in self.paragraphs.items():
            # Skip if it's a section (not a paragraph)
            if not isinstance(para, COBOLParagraph):
                if self.debug:
                    print(f"    - Skipping section object: {para_name}")
                continue
            
            # Skip if already processed as part of a section
            if hasattr(program.procedure_division, 'sections') and program.procedure_division.sections:
                in_section = any(
                    para in section.paragraphs 
                    for section in program.procedure_division.sections
                )
                if in_section:
                    if self.debug:
                        print(f"    - Skipping paragraph in section: {para_name}")
                    continue
            
            # Generate paragraph subroutine
            para_subroutine = self.create_paragraph_subroutine(
                para,
                self.variables,
                program_name,
                pool_name
            )
            subroutines.append(para_subroutine)
            
            if self.debug:
                print(f"    - Flat paragraph: {para.name} → {para_subroutine.name}")
        
        if self.debug:
            print(f"\n  Total subroutines generated: {len(subroutines)}")
        
        return subroutines
    
    def create_section_subroutine(self, section, variables: Dict, 
                                parent_program: str, pool_name: Optional[str]) -> SubRoutine:
        """
        Create a subroutine for a COBOL SECTION.
        
        A SECTION subroutine jumps to the first paragraph in the section.
        This allows: GO TO SECTION-NAME
        
        Args:
            section: COBOLSection object
            variables: Variable dictionary
            parent_program: Parent program name
            pool_name: Variable pool name (if any)
        
        Returns:
            SubRoutine that jumps to first paragraph
        """
        section_name = self.normalize_name(section.name)
        full_name = f"{parent_program}_{section_name}"
        
        statements = []
        
        # If section has paragraphs, jump to the first one
        if section.paragraphs and section.paragraphs[0].name:
            first_para_name = self.normalize_name(section.paragraphs[0].name)
            first_para_full = f"{parent_program}_{first_para_name}"
            
            # RunTask to jump to first paragraph
            statements.append(RunTask(1, 1, first_para_full, []))
        
        # Always return 0
        statements.append(ReturnValue(1, 1, Number(1, 1, 0)))
        
        return SubRoutine(1, 1, full_name, statements)


    def _convert_nested_programs(self, program: COBOLProgram) -> List[SubRoutine]:
        """
        Recursively convert nested programs.
        Returns list of all nested subroutines.
        """
        nested_subroutines = []
        
        if not program.contained_programs:
            return nested_subroutines
        
        if self.debug:
            print(f"\n  Processing {len(program.contained_programs)} nested programs")
        
        for nested_program in program.contained_programs:
            if self.debug:
                print(f"\n  {'>'*3} NESTED: {nested_program.program_id}")
            
            # Recursive call - this will reset state automatically
            nested_subs = self.convert_cobol_program(nested_program)
            nested_subroutines.extend(nested_subs)
        
        return nested_subroutines

    
        
    def create_program_subroutine(self, program: COBOLProgram, variables: Dict, pool_name: Optional[str]) -> SubRoutine:
        """
        Create the main subroutine for a COBOL program.
        
        CRITICAL FIX: This subroutine must:
        1. Initialize variables/arrays
        2. Call the PROCEDURE DIVISION entry point (first paragraph or A10-MAIN)
        3. Return
        
        This ensures Main() -> PROGRAM -> PROCEDURE_LOGIC works correctly.
        """
        name = self.normalize_name(program.program_id)
        
        statements = []
        
        # Step 1: Initialize variables (only if not in a pool)
        statements.extend(self.create_variable_initializations(variables, pool_name))
        
        self.current_program_name = name
        
        # Get PROCEDURE DIVISION parameters (for LINKAGE)
        procedure_params = []
        if program.procedure_division and hasattr(program.procedure_division, 'using_params') and program.procedure_division.using_params:
            procedure_params = [self.normalize_name(p) for p in program.procedure_division.using_params]
        
        if self.debug:
            print(f"  Creating main subroutine: {name}")
            if procedure_params:
                print(f"    USING: {procedure_params}")
        
        # ============================================================================
        # CRITICAL FIX: Call the PROCEDURE DIVISION entry point
        # ============================================================================
        
        # Determine the entry paragraph name
        entry_paragraph_name = None
        
        # Strategy 1: Look for sections with paragraphs
        if hasattr(program.procedure_division, 'sections') and program.procedure_division.sections:
            # Find first section with paragraphs
            for section in program.procedure_division.sections:
                if section.paragraphs and section.paragraphs[0].name:
                    # First paragraph in first section is the entry point
                    entry_paragraph_name = self.normalize_name(section.paragraphs[0].name)
                    break
        
        # Strategy 2: Look for flat paragraphs (no sections)
        if not entry_paragraph_name and program.procedure_division.paragraphs:
            for para in program.procedure_division.paragraphs:
                if para.name:
                    entry_paragraph_name = self.normalize_name(para.name)
                    break
        
        # Strategy 3: Look for standard COBOL entry patterns (A10-MAIN, MAIN-LOGIC, etc.)
        if not entry_paragraph_name:
            # Common COBOL entry point names
            common_entry_names = ['A10_MAIN', 'MAIN_LOGIC', 'MAIN_PARA', '0000_MAIN']
            for common_name in common_entry_names:
                if common_name in self.paragraphs:
                    entry_paragraph_name = common_name
                    break
        
        # If we found an entry paragraph, call it
        if entry_paragraph_name:
            full_para_name = f"{name}_{entry_paragraph_name}"
            if self.debug:
                print(f"    Entry point: {full_para_name}")
            statements.append(RunTask(1, 1, full_para_name, []))
        else:
            # No paragraphs - inline the PROCEDURE DIVISION statements directly
            if self.debug:
                print(f"    No named paragraphs - inlining PROCEDURE DIVISION")
            
            if program.procedure_division and program.procedure_division.paragraphs:
                # Find the unnamed paragraph (inline code at top of PROCEDURE DIVISION)
                for para in program.procedure_division.paragraphs:
                    if not para.name and para.statements:
                        # Inline these statements directly
                        for stmt in para.statements:
                            converted = self.statement_converter.convert_statement(stmt, variables)
                            if isinstance(converted, list):
                                statements.extend(converted)
                            elif converted:
                                statements.append(converted)
        
        # Always return 0
        statements.append(ReturnValue(1, 1, Number(1, 1, 0)))
        
        return SubRoutine(1, 1, name, statements)

    def create_paragraph_subroutine(self, para: COBOLParagraph, variables: Dict, parent_program: str, pool_name: Optional[str]) -> SubRoutine:
        """Create a subroutine for a COBOL paragraph"""
        para_name = self.normalize_name(para.name)
        full_name = f"{parent_program}_{para_name}"
        
        statements = []
        
        self.current_program_name = parent_program
        self.current_pool_name = pool_name
        
        for stmt in para.statements:
            converted = self.statement_converter.convert_statement(stmt, variables)
            if isinstance(converted, list):
                statements.extend(converted)
            elif converted is not None:
                statements.append(converted)
        
        if not statements or not isinstance(statements[-1], ReturnValue):
            statements.append(ReturnValue(1, 1, Number(1, 1, 0)))
        
        return SubRoutine(1, 1, full_name, statements)

    def create_variable_initializations(self, variables: Dict, pool_name: Optional[str] = None) -> List:
        """Create Ailang initialization statements for a variable dict"""
        initializations = []
        
        for var_name, var_info in variables.items():
            if var_info.get('is_linkage'):
                continue
            
            occurs_count = var_info.get('occurs')
            if occurs_count and occurs_count > 0:
                array_target = f"{pool_name}.{var_name}" if pool_name else var_name

                initializations.append(
                    Assignment(1, 1, array_target,
                                FunctionCall(1, 1, 'ArrayCreate', [Number(1, 1, occurs_count)]))
                )

                # Initialize array elements with VALUE if provided and non-default
                if var_info['value'] and var_info['value'] not in ['0', '""', 'SPACES']:
                    if self.debug:
                        print(f"    Initializing array {var_name} elements to {var_info['value']}")
                    
                    # Determine the initial value based on type
                    if var_info['type'] == 'String':
                        value_str = var_info['value'].strip('"') if '"' in var_info['value'] else var_info['value']
                        init_val = String(1, 1, value_str)
                    else:
                        init_val = Number(1, 1, int(var_info['value']))
                    
                    # Initialize each element
                    for i in range(occurs_count):
                        initializations.append(
                            FunctionCall(1, 1, 'ArraySet', [
                                Identifier(1, 1, array_target),
                                Number(1, 1, i),
                                init_val
                            ])
                        )

                if 'redefines_values' in var_info:
                    if self.debug:
                        print(f"    Populating redefined array: {var_name} with {len(var_info['redefines_values'])} values")
                    
                    for i, value in enumerate(var_info['redefines_values']):
                        initializations.append(
                            FunctionCall(1, 1, 'ArraySet', [
                                Identifier(1, 1, array_target),
                                Number(1, 1, i),
                                String(1, 1, value.strip('"'))
                            ])
                        )
                continue
            
            if pool_name:
                continue
            
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
    
    

    def normalize_name(self, cobol_name: str) -> str:
        return cobol_name.replace('-', '_')
    
    def make_variable_reference(self, var_name: str, variables: Dict) -> Identifier:
        """Create a variable reference for READING (not assignment)"""
        normalized = self.normalize_name(var_name)
        
        if self.current_pool_name and normalized in variables:
            return Identifier(1, 1, f"{self.current_pool_name}.{normalized}")
        else:
            return Identifier(1, 1, normalized)

    def make_assignment_target(self, var_name: str, variables: Dict) -> str:
        """Create assignment target with pool prefix if needed"""
        if '.' in var_name:
            return var_name
        
        # ✅ CHECK LINKAGE FIRST (even for arrays!)
        if var_name in variables and variables[var_name].get('is_linkage'):
            pool_name = f"COBOL_{self.current_program_name}_LINKAGE"
            return f"{pool_name}.{var_name}"
        
        # Then check WS pool (for both arrays and scalars)
        if self.current_pool_name and var_name in variables:
            return f"{self.current_pool_name}.{var_name}"
        
        # Fallback: bare name (shouldn't happen often)
        return var_name
        
    

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
        
        for decl in ast.declarations:
            if isinstance(decl, Pool):
                lines.extend(self.serialize_pool(decl))
            else:
                lines.extend(self.serialize_function(decl))
            lines.append("")
        
        lines.append("RunTask(Main)")
        lines.append("")
        
        return '\n'.join(lines)
    
    def serialize_with_header(self, ast: Program, header: str) -> str:
        """Serializes the AST and prepends a header string."""
        serialized_program = self.serialize(ast)
        return header + serialized_program if header else serialized_program

    def serialize_pool(self, pool) -> List[str]:
        """Serializes a Pool (e.g., FixedPool) AST node."""
        lines = [f"{pool.pool_type}.{pool.name} {{"]
        self.indent_level += 1
        for item in pool.body:
            key = item.key
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
            if isinstance(stmt.target, str):
                target_str = stmt.target
            elif isinstance(stmt.target, Identifier):
                target_str = stmt.target.name
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
        
        # ✅ ADD THIS: Handle DebugBlock
        elif hasattr(stmt, '__class__') and stmt.__class__.__name__ == 'DebugBlock':
            lines = [f'Debug("{stmt.label}", level={stmt.level}) {{']
            self.indent_level += 1
            for debug_stmt in stmt.body:
                for line in self.serialize_statement(debug_stmt):
                    lines.append(self.indent() + line)
            self.indent_level -= 1
            lines.append(self.indent() + "}")
            return lines
        
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