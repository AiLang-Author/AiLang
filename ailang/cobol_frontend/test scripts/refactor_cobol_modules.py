#!/usr/bin/env python3
"""
Complete COBOL refactoring script that actually extracts and splits the code
"""
import os
import re
import shutil
from pathlib import Path

class CompleteRefactorer:
    def __init__(self, dry_run=False):
        self.dry_run = dry_run
        self.base_dir = Path('.')
        self.parser_dir = self.base_dir / 'parser'
        self.converter_dir = self.base_dir / 'converter'
    
    def read_file_lines(self, filepath):
        """Read file and return lines with line numbers"""
        with open(filepath, 'r', encoding='utf-8') as f:
            return f.readlines()
    
    def extract_header(self, lines):
        """Extract file header (license, imports) before first class definition"""
        header_lines = []
        for line in lines:
            if line.strip().startswith('class ') or line.strip().startswith('@dataclass'):
                break
            header_lines.append(line)
        return ''.join(header_lines)
    
    def extract_dataclasses(self, lines):
        """Extract all @dataclass definitions from parser file"""
        dataclasses = []
        i = 0
        while i < len(lines):
            line = lines[i]
            
            # Found a dataclass
            if line.strip().startswith('@dataclass'):
                dc_lines = [line]
                i += 1
                
                # Get the class line
                while i < len(lines) and not lines[i].strip().startswith('class '):
                    dc_lines.append(lines[i])
                    i += 1
                
                if i < len(lines):
                    dc_lines.append(lines[i])  # class line
                    i += 1
                    
                    # Get the class body until next @dataclass or class
                    while i < len(lines):
                        next_line = lines[i]
                        if (next_line.strip().startswith('@dataclass') or 
                            (next_line.strip().startswith('class ') and not next_line.strip().startswith('class method'))):
                            break
                        dc_lines.append(next_line)
                        i += 1
                
                dataclasses.append(''.join(dc_lines))
            else:
                i += 1
        
        return dataclasses
    
    def find_method_bounds(self, lines, method_name, class_name):
        """Find start and end line numbers for a method"""
        # Find method start
        start_idx = None
        for i, line in enumerate(lines):
            if re.match(rf'    def {re.escape(method_name)}\(', line):
                start_idx = i
                break
        
        if start_idx is None:
            return None, None
        
        # Find method end (next method at same indentation or end of class)
        end_idx = len(lines)
        for i in range(start_idx + 1, len(lines)):
            line = lines[i]
            # Next method at class level
            if re.match(r'    def \w+\(', line):
                end_idx = i
                break
            # Start of new class
            if re.match(r'^class \w+', line):
                end_idx = i
                break
        
        return start_idx, end_idx
    
    def extract_methods(self, lines, method_names, class_name):
        """Extract specified methods from a class"""
        methods_code = {}
        
        for method_name in method_names:
            start, end = self.find_method_bounds(lines, method_name, class_name)
            if start is not None:
                methods_code[method_name] = ''.join(lines[start:end])
        
        return methods_code
    
    def create_parser_ast_nodes(self):
        """Create parser/ast_nodes.py with all dataclasses"""
        print("  Creating parser/ast_nodes.py...")
        
        lines = self.read_file_lines('cobol_parser.py')
        header = self.extract_header(lines)
        dataclasses = self.extract_dataclasses(lines)
        
        content = header + '\n' + '\n\n'.join(dataclasses)
        
        output_file = self.parser_dir / 'ast_nodes.py'
        if not self.dry_run:
            with open(output_file, 'w', encoding='utf-8') as f:
                f.write(content)
            print(f"    ✓ Wrote {len(dataclasses)} dataclasses")
        else:
            print(f"    [DRY-RUN] Would write {len(dataclasses)} dataclasses")
    
    def create_parser_core(self):
        """Create parser/parser_core.py with core parser infrastructure"""
        print("  Creating parser/parser_core.py...")
        
        lines = self.read_file_lines('cobol_parser.py')
        header = self.extract_header(lines)
        
        # Add imports
        header += "\nfrom .ast_nodes import *\n\n"
        
        # Find the COBOLMultiProgramParser class definition
        class_start = None
        for i, line in enumerate(lines):
            if line.strip().startswith('class COBOLMultiProgramParser:'):
                class_start = i
                break
        
        if class_start is None:
            print("    ✗ Could not find COBOLMultiProgramParser class")
            return
        
        # Extract core methods
        core_methods = [
            '__init__', 'push_context', 'pop_context', 'current_context',
            'requires_period', 'current_token', 'peek_token', 'advance',
            'match', 'consume', 'error', 'consume_optional_period',
            'skip_insignificant_tokens', 'parse_all_programs',
            'parse_single_program', 'parse_end_program'
        ]
        
        # Build class with core methods
        class_def = lines[class_start]
        methods_code = self.extract_methods(lines, core_methods, 'COBOLMultiProgramParser')
        
        # Also include static methods
        static_methods = ['is_data_only_program', 'validate_program']
        static_code = self.extract_methods(lines, static_methods, 'COBOLMultiProgramParser')
        methods_code.update(static_code)
        
        content = header + class_def
        for method_name in core_methods + static_methods:
            if method_name in methods_code:
                content += '\n' + methods_code[method_name]
        
        output_file = self.parser_dir / 'parser_core.py'
        if not self.dry_run:
            with open(output_file, 'w', encoding='utf-8') as f:
                f.write(content)
            print(f"    ✓ Wrote {len(methods_code)} methods")
        else:
            print(f"    [DRY-RUN] Would write {len(methods_code)} methods")
    
    def create_parser_modules(self):
        """Create all parser sub-modules"""
        print("  Creating parser sub-modules...")
        
        lines = self.read_file_lines('cobol_parser.py')
        
        modules = {
            'division_parsers.py': [
                'parse_identification_division', 'parse_environment_division',
                'parse_select_statement', 'parse_file_section',
                'parse_data_division', 'parse_working_storage',
                'parse_linkage_section', 'parse_fd_entry',
                'parse_variable_decl', 'parse_pic_clause', 'parse_usage_type',
                'parse_procedure_division'
            ],
            'statement_parsers.py': [
                'parse_go_to', 'parse_paragraph', 'parse_statement',
                'parse_read_statement', 'parse_display', 'parse_accept',
                'parse_move', 'parse_compute', 'parse_add', 'parse_subtract',
                'parse_multiply', 'parse_divide', 'parse_string_statement',
                'parse_unstring_statement', 'parse_inspect',
                'parse_inspect_statement', 'parse_if', 'parse_perform',
                'parse_perform_until', 'parse_perform_varying', 'parse_call',
                'parse_evaluate', 'parse_when_clause', 'parse_stop_run'
            ],
            'expression_parsers.py': [
                'parse_condition', 'parse_or_expression', 'parse_and_expression',
                'parse_not_expression', 'parse_comparison',
                'parse_arithmetic_expression', 'parse_additive',
                'parse_multiplicative', 'parse_primary', 'parse_function_call',
                'parse_expression', 'parse_string'
            ]
        }
        
        for filename, method_names in modules.items():
            header = "from .parser_core import COBOLMultiProgramParser\nfrom .ast_nodes import *\n"
            header += "from cobol_lexer import Token, COBOLTokenType\n\n"
            header += "# Extend the parser with additional methods\n\n"
            
            methods_code = self.extract_methods(lines, method_names, 'COBOLMultiProgramParser')
            
            # Wrap methods as extensions
            content = header
            for method_name in method_names:
                if method_name in methods_code:
                    # Add to class as monkey patch
                    content += f"# Extension: {method_name}\n"
                    method_code = methods_code[method_name]
                    # Change 'def method' to 'def _method' and add assignment
                    content += method_code + '\n'
                    content += f"COBOLMultiProgramParser.{method_name} = {method_name}\n\n"
            
            output_file = self.parser_dir / filename
            if not self.dry_run:
                with open(output_file, 'w', encoding='utf-8') as f:
                    f.write(content)
                print(f"    ✓ {filename}: {len(methods_code)} methods")
            else:
                print(f"    [DRY-RUN] {filename}: {len(methods_code)} methods")
    
    def create_converter_modules(self):
        """Create all converter modules"""
        print("  Creating converter modules...")
        
        lines = self.read_file_lines('cobol_ast_converter.py')
        header = self.extract_header(lines)
        
        modules = {
            'type_system.py': {
                'header': header + '\n# Type system utilities\n\n',
                'methods': [
                    'calculate_pic_size', 'normalize_name', 'infer_type',
                    'get_storage_info', 'is_edited_format', 'parse_edit_format',
                    'create_format_function', 'format_storage_display'
                ],
                'standalone': True  # These can be standalone functions
            },
            'decimal_support.py': {
                'header': header + '\nfrom .type_system import *\n\n# Decimal arithmetic support\n\n',
                'methods': [
                    'is_decimal_operation', 'get_arithmetic_function',
                    'convert_decimal_value', 'get_redefines_values'
                ],
                'standalone': False
            }
        }
        
        for filename, config in modules.items():
            methods_code = self.extract_methods(lines, config['methods'], 'COBOLToAilangMultiProgramConverter')
            
            content = config['header']
            for method_name in config['methods']:
                if method_name in methods_code:
                    content += methods_code[method_name] + '\n\n'
            
            output_file = self.converter_dir / filename
            if not self.dry_run:
                with open(output_file, 'w', encoding='utf-8') as f:
                    f.write(content)
                print(f"    ✓ {filename}: {len(methods_code)} methods")
            else:
                print(f"    [DRY-RUN] {filename}: {len(methods_code)} methods")
    
    def run(self):
        """Execute complete refactoring"""
        print("="*80)
        print("COMPLETE COBOL REFACTORING")
        print("="*80)
        
        if self.dry_run:
            print("\n⚠️  DRY RUN MODE\n")
        
        # Backup
        print("\n1. Creating backups...")
        if not self.dry_run:
            shutil.copy2('cobol_parser.py', 'cobol_parser.py.backup')
            shutil.copy2('cobol_ast_converter.py', 'cobol_ast_converter.py.backup')
            print("  ✓ Backups created")
        
        # Create directories
        print("\n2. Creating directories...")
        if not self.dry_run:
            self.parser_dir.mkdir(exist_ok=True)
            self.converter_dir.mkdir(exist_ok=True)
            print("  ✓ Directories created")
        
        # Create __init__ files
        print("\n3. Creating __init__.py files...")
        if not self.dry_run:
            (self.parser_dir / '__init__.py').write_text(
                'from .parser_core import COBOLMultiProgramParser\n'
                'from .ast_nodes import *\n\n'
                '__all__ = ["COBOLMultiProgramParser"]\n'
            )
            (self.converter_dir / '__init__.py').write_text(
                'from .converter_core import COBOLToAilangMultiProgramConverter, AILangASTSerializer\n'
                '__all__ = ["COBOLToAilangMultiProgramConverter", "AILangASTSerializer"]\n'
            )
            print("  ✓ __init__.py files created")
        
        # Extract code
        print("\n4. Extracting and splitting code...")
        self.create_parser_ast_nodes()
        self.create_parser_core()
        self.create_parser_modules()
        self.create_converter_modules()
        
        print("\n" + "="*80)
        if self.dry_run:
            print("✓ DRY RUN COMPLETE")
        else:
            print("✓ REFACTORING COMPLETE")
            print("\nNext steps:")
            print("  python3 test_refactored_imports.py")
        print("="*80)

if __name__ == '__main__':
    import sys
    dry_run = '--dry-run' in sys.argv
    refactorer = CompleteRefactorer(dry_run=dry_run)
    refactorer.run()