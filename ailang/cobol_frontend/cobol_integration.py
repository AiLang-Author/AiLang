#!/usr/bin/env python3

# Copyright (c) 2025 Sean Collins, 2 Paws Machine and Engineering. All rights reserved.
#
# Licensed under the Sean Collins Software License (SCSL). See the LICENSE file in the root directory of this project
# for the full terms and conditions, including restrictions on forking, corporate use, and permissions for private/teaching purposes.

"""
COBOL Integration Module
Orchestrates the compilation pipeline from COBOL source to native binary.
Now uses the split parser/ and converter/ packages for better maintainability.
"""

import sys
import os
import glob
import argparse
import subprocess
from typing import List, Set, Dict, Optional

# Ensure the parent directory is in the path
project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

# ✅ UPDATED: Import from split packages instead of monolithic files
from cobol_frontend.cobol_lexer import COBOLLexer, LexerError
from cobol_frontend.parser import COBOLMultiProgramParser, ParserError
from cobol_frontend.converter import COBOLToAilangMultiProgramConverter, AILangASTSerializer

# ============================================================================
# Dependency resolver class
# ============================================================================

class COBOLDependencyResolver:
    """Automatically discovers and resolves COBOL program dependencies"""
    
    def __init__(self, search_paths: List[str] = None, debug: bool = False):
        self.search_paths = search_paths or ['.']
        self.debug = debug
        self.resolved_files = {}  # program_name -> file_path
        self.parsed_programs = set()  # Track what we've already parsed
    
    def find_cobol_file(self, program_name: str, relative_to: str = None) -> Optional[str]:
        """
        Find a COBOL file for a given program name.
        
        Tries variations:
            COBLOAN → cobloan.cbl, COBLOAN.cbl, cobloan.cob, COBLOAN.COB
            
        Args:
            program_name: PROGRAM-ID or CALL name (e.g., "COBLOAN")
            relative_to: Directory to search first (where the calling file is)
        
        Returns:
            Full path to .cbl/.cob file, or None if not found
        """
        # Generate filename variations
        variations = [
            f"{program_name.lower()}.cbl",
            f"{program_name.upper()}.cbl",
            f"{program_name.lower()}.cob",
            f"{program_name.upper()}.cob",
            f"{program_name}.cbl",
            f"{program_name}.cob",
        ]
        
        # Build search path list
        search_dirs = []
        
        # 1. Directory of the calling file (highest priority)
        if relative_to:
            search_dirs.append(os.path.dirname(relative_to))
        
        # 2. Additional search paths
        search_dirs.extend(self.search_paths)
        
        # 3. Current directory (if not already included)
        if '.' not in search_dirs:
            search_dirs.append('.')
        
        # Search for the file
        for directory in search_dirs:
            for variation in variations:
                file_path = os.path.join(directory, variation)
                if os.path.exists(file_path):
                    if self.debug:
                        print(f"  ✓ Found dependency: {program_name} → {file_path}")
                    return os.path.abspath(file_path)
        
        if self.debug:
            print(f"  ✗ Could not find file for: {program_name}")
        return None
    
    def extract_calls_from_ast(self, compilation_unit) -> Set[str]:
        """
        Extract all CALL'd program names from a COBOL AST.
        
        Returns:
            Set of program names that are CALL'd
        """
        called_programs = set()
        
        def scan_statements(statements):
            """Recursively scan statements for CALL"""
            for stmt in statements:
                if hasattr(stmt, '__class__'):
                    class_name = stmt.__class__.__name__
                    
                    # Found a CALL statement
                    if class_name == 'COBOLCall':
                        called_programs.add(stmt.program_name)
                    
                    # Recursively check nested statements
                    if hasattr(stmt, 'then_statements') and stmt.then_statements:
                        scan_statements(stmt.then_statements)
                    if hasattr(stmt, 'else_statements') and stmt.else_statements:
                        scan_statements(stmt.else_statements)
                    if hasattr(stmt, 'statements') and stmt.statements:
                        scan_statements(stmt.statements)
                    if hasattr(stmt, 'when_clauses') and stmt.when_clauses:
                        for clause in stmt.when_clauses:
                            if hasattr(clause, 'statements'):
                                scan_statements(clause.statements)
        
        # Scan all programs in the compilation unit
        for program in compilation_unit.programs:
            if program.procedure_division and program.procedure_division.paragraphs:
                for para in program.procedure_division.paragraphs:
                    if para.statements:
                        scan_statements(para.statements)
        
        return called_programs
    
    def resolve_dependencies(self, main_file: str) -> List[str]:
        """
        Starting from main_file, recursively find all dependencies.
        
        Returns:
            List of file paths in dependency order (main file first)
        """
        if self.debug:
            print(f"\n{'='*70}")
            print(f"Resolving dependencies for: {main_file}")
            print(f"{'='*70}")
        
        main_file = os.path.abspath(main_file)
        to_process = [main_file]  # Queue of files to parse
        processed_files = []  # Result list in order
        
        while to_process:
            current_file = to_process.pop(0)
            
            # Skip if already processed
            if current_file in processed_files:
                continue
            
            if self.debug:
                print(f"\nProcessing: {os.path.basename(current_file)}")
            
            # Parse the file to find dependencies
            try:
                with open(current_file, 'r') as f:
                    source = f.read()
                
                # ✅ UPDATED: Use split parser package
                lexer = COBOLLexer(source)
                tokens = lexer.tokenize()
                parser = COBOLMultiProgramParser(tokens, debug=self.debug)
                compilation_unit = parser.parse_all_programs()
                
                # Extract CALL statements
                called_programs = self.extract_calls_from_ast(compilation_unit)
                
                if called_programs and self.debug:
                    print(f"  Found CALL statements: {', '.join(called_programs)}")
                
                # Find files for called programs
                for program_name in called_programs:
                    # Skip if we've already resolved this
                    if program_name in self.resolved_files:
                        continue
                    
                    # Find the file
                    dep_file = self.find_cobol_file(program_name, relative_to=current_file)
                    
                    if dep_file:
                        self.resolved_files[program_name] = dep_file
                        # Add to processing queue
                        if dep_file not in processed_files and dep_file not in to_process:
                            to_process.append(dep_file)
                    else:
                        if self.debug:
                            print(f"  ⚠ Warning: Cannot find file for CALL '{program_name}'")
                        # Not fatal - might be an external library
                
                # Mark as processed
                processed_files.append(current_file)
                
            except Exception as e:
                if self.debug:
                    print(f"  ✗ Error parsing {current_file}: {e}")
                raise
        
        if self.debug:
            print(f"\n{'='*70}")
            print(f"Dependency resolution complete!")
            print(f"Total files: {len(processed_files)}")
            for i, f in enumerate(processed_files, 1):
                print(f"  {i}. {os.path.basename(f)}")
            print(f"{'='*70}\n")
        
        return processed_files

class COBOLIntegratedCompiler:
    """
    Full COBOL to native binary compiler.
    Manages the pipeline:
    1. Lexing (COBOL source -> Tokens)
    2. Parsing (Tokens -> COBOL AST)
    3. Conversion (COBOL AST -> Ailang AST)
    4. Serialization (Ailang AST -> .ailang source file)
    5. Invocation (Call backend compiler on .ailang file)
    """
    
    def __init__(self, debug=False, search_paths=None):
        self.debug = debug
        self.search_paths = search_paths or ['.']
    
    def compile_file(self, input_file: str, output_name: str, 
                 ailang_only: bool = False, auto_resolve: bool = True,
                 split_programs: bool = False): 
        """
        Compile a COBOL file with automatic dependency resolution.
        
        Args:
            input_file: Main COBOL file
            output_name: Output binary name
            ailang_only: Only generate .ailang
            auto_resolve: Automatically find and include CALL'd programs
        """
        if auto_resolve:
            # ✅ AUTO-DISCOVERY: Find all dependencies
            resolver = COBOLDependencyResolver(
                search_paths=self.search_paths, 
                debug=self.debug
            )
            all_files = resolver.resolve_dependencies(input_file)
        else:
            # Manual mode: just compile the single file
            all_files = [input_file]
        
        # Use existing multi-file compilation
        self.compile_files(all_files, output_name, ailang_only, split_programs)  
    
    def compile_files(self, input_files: List[str], output_name: str, 
                    ailang_only: bool = False, split_programs: bool = False):
        """Compile multiple COBOL files"""
        all_source_code = ""
        for file_path in input_files:
            try:
                with open(file_path, 'r', encoding='utf-8') as f:
                    all_source_code += f.read() + "\n"
            except FileNotFoundError:
                print(f"✗ Error: Input file not found: {file_path}", file=sys.stderr)
                raise

        try:
            # Use the combined source code
            source_code = all_source_code

            # --- Stage 1: Lexing ---
            print(f"✓ Stage 1: Lexing {len(input_files)} COBOL source file(s)...", file=sys.stderr)
            lexer = COBOLLexer(source_code)
            tokens = lexer.tokenize()
            
            if self.debug:
                print("--- Tokens (first 20) ---", file=sys.stderr)
                for token in tokens[:20]:
                    print(f"  {token.type.name}: {token.value}", file=sys.stderr)
                if len(tokens) > 20:
                    print(f"  ... and {len(tokens) - 20} more", file=sys.stderr)

            # --- Stage 2: Parsing (UPDATED to use split parser) ---
            print("✓ Stage 2: Parsing tokens into COBOL AST...", file=sys.stderr)
            # ✅ UPDATED: Use parser from split package
            parser = COBOLMultiProgramParser(tokens, debug=self.debug)
            compilation_unit = parser.parse_all_programs()
            
            print(f"    Found {len(compilation_unit.programs)} COBOL program(s):", file=sys.stderr)
            for i, prog in enumerate(compilation_unit.programs):
                var_count = len(prog.data_division.working_storage) if prog.data_division else 0
                para_count = len([p for p in prog.procedure_division.paragraphs if p.name]) if prog.procedure_division else 0
                print(f"      {i+1}. {prog.program_id} ({var_count} variables, {para_count} paragraphs)", file=sys.stderr)
            
            if self.debug:
                print("\n--- COBOL AST Structure ---", file=sys.stderr)
                for i, prog in enumerate(compilation_unit.programs):
                    print(f"  Program: {prog.program_id}", file=sys.stderr)
                    if prog.data_division:
                        print(f"    Variables: {len(prog.data_division.working_storage)}", file=sys.stderr)
                    if prog.procedure_division:
                        print(f"    Paragraphs: {len(prog.procedure_division.paragraphs)}", file=sys.stderr)

            # --- Stage 3: Converting (FIXED to convert entire unit at once) ---
            print("✓ Stage 3: Converting COBOL AST to Ailang AST...", file=sys.stderr)
            
            # ✅ FIXED: Convert entire compilation unit at once
            converter = COBOLToAilangMultiProgramConverter(debug=self.debug)
            
            if split_programs:
                output_dir = output_name if os.path.isdir(output_name) else 'cobol_frontend/converter_output'
                generated_files = converter.convert_split_programs(compilation_unit, output_dir)
                print(f"✓ Generated {len(generated_files)} .ailang files to {output_dir}", file=sys.stderr)
                return generated_files
            
            # Convert entire compilation unit at once
            # This creates ONE Main() that calls all entry programs
            try:
                ailang_ast = converter.convert(compilation_unit)
                print(f"✓ Conversion complete: {len(compilation_unit.programs)} programs converted", file=sys.stderr)
            except Exception as e:
                print(f"✗ Conversion failed: {e}", file=sys.stderr)
                if self.debug:
                    import traceback
                    traceback.print_exc()
                raise
            
            if self.debug:
                print(f"\n--- Ailang AST ---", file=sys.stderr)
                print(f"  Total declarations: {len(ailang_ast.declarations)}", file=sys.stderr)
                for decl in ailang_ast.declarations:
                    if hasattr(decl, 'name'):
                        print(f"    - {decl.name}", file=sys.stderr)

            # --- Stage 4: Serializing ---
            print("✓ Stage 4: Serializing Ailang AST to source...", file=sys.stderr)
            # ✅ UPDATED: Use serializer from split package
            serializer = AILangASTSerializer()
            ailang_source = serializer.serialize(ailang_ast)
            
            # Write .ailang file
            ailang_file = f"{output_name}.ailang"
            with open(ailang_file, 'w', encoding='utf-8') as f:
                f.write(ailang_source)
            
            if self.debug:
                print(f"\n--- Generated Ailang Source (first 50 lines) ---", file=sys.stderr)
                lines = ailang_source.split('\n')
                for i, line in enumerate(lines[:50], 1):
                    print(f"  {i:3}: {line}", file=sys.stderr)
                if len(lines) > 50:
                    print(f"  ... and {len(lines) - 50} more lines", file=sys.stderr)

            if ailang_only:
                print(f"✓ Ailang output saved to: {ailang_file}", file=sys.stderr)
                return

            # --- Stage 5: Backend Compilation ---
            print(f"✓ Stage 5: Invoking Ailang compiler on '{ailang_file}'...", file=sys.stderr)
            
            # Find main.py (Ailang compiler)
            ailang_compiler = os.path.join(project_root, 'main.py')
            if not os.path.exists(ailang_compiler):
                raise FileNotFoundError(f"Ailang compiler not found at: {ailang_compiler}")
            
            # Compile .ailang to native binary
            compile_cmd = [sys.executable, ailang_compiler, ailang_file]
            result = subprocess.run(
                compile_cmd,
                capture_output=True,
                text=True
            )
            
            if result.returncode != 0:
                print("\n✗ Ailang Backend Compiler Failed:", file=sys.stderr)
                print(result.stderr, file=sys.stderr)
                raise RuntimeError("Ailang compilation failed")
            
            # Success!
            executable = f"{output_name}_exec"
            print(f"✓ Compilation successful!", file=sys.stderr)
            print(f"  Executable: {executable}", file=sys.stderr)
            print(f"\nRun with: ./{executable}", file=sys.stderr)
            
            # Keep intermediate files for debugging
            print(f"✓ Ailang output saved to: {ailang_file}", file=sys.stderr)
            return executable

        except (LexerError, ParserError) as e:
            print(f"\n✗ COBOL Frontend Error: {e}", file=sys.stderr)
            raise
        except FileNotFoundError as e:
            print(f"\n✗ File not found: {e}", file=sys.stderr)
            raise
        except Exception as e:
            print(f"\n✗ An unexpected error occurred: {e}", file=sys.stderr)
            if self.debug:
                import traceback
                traceback.print_exc()
            raise

def main():
    """Command line interface for the integrated compiler."""
    parser = argparse.ArgumentParser(
        description="COBOL to Ailang Compiler with Auto-Dependency Resolution",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Auto-resolve dependencies (default)
  python3 cobol_frontend/cobol_integration.py calc.cbl -o calc
  → Automatically finds and includes cobloan.cbl and cobvalu.cbl!
  
  # Manual mode (specify all files)
  python3 cobol_frontend/cobol_integration.py calc.cbl cobloan.cbl -o calc --no-auto
  
  # Add search paths
  python3 cobol_frontend/cobol_integration.py calc.cbl -o calc -I../lib -I../common
  
  # Debug dependency resolution
  python3 cobol_frontend/cobol_integration.py calc.cbl -o calc --debug

Auto-Dependency Resolution:
  The compiler scans COBOL source for CALL statements and automatically
  finds and includes the called programs. For example:
  
    CALL "COBLOAN" → Searches for cobloan.cbl, COBLOAN.cbl, etc.
  
  Search order:
    1. Same directory as the main file
    2. Directories specified with -I/--include-path
    3. Current working directory
        """
    )
    parser.add_argument(
        "input", 
        nargs='+',
        help="Input COBOL file(s)"
    )
    parser.add_argument(
        "-o", "--output",
        help="Output binary name"
    )
    parser.add_argument(
        "--debug",
        action="store_true",
        help="Enable debug output, showing tokens and ASTs at each stage."
    )
    parser.add_argument(
        "--ailang-only",
        action="store_true",
        help="Generate Ailang source file only, don't compile to binary."
    )
    parser.add_argument(
        "--no-auto", 
        action="store_true",
        help="Disable automatic dependency resolution"
    )
    parser.add_argument(
        "-I", "--include-path",
        action="append",
        dest="include_paths",
        help="Add directory to search path for dependencies (can be used multiple times)"
    )
    parser.add_argument(
        "--split-programs",
        action="store_true",
        help="Generate one .ailang file per COBOL program"
    )
    
    args = parser.parse_args()
    
    # Determine output file name
    if not args.output:
        args.output = os.path.splitext(os.path.basename(args.input[0]))[0]
    
    # Build search paths
    search_paths = args.include_paths or ['.']
    
    compiler = COBOLIntegratedCompiler(
        debug=args.debug, 
        search_paths=search_paths
    )
    
    try:
        if len(args.input) == 1 and not args.no_auto:
            # Single file with auto-resolution
            compiler.compile_file(
                args.input[0], 
                args.output, 
                ailang_only=args.ailang_only,
                auto_resolve=True,
                split_programs=args.split_programs 
            )
        else:
            # Multiple files or manual mode
            compiler.compile_files(
                args.input, 
                args.output, 
                ailang_only=args.ailang_only,
                split_programs=args.split_programs
            )
            
        if args.ailang_only:
            print(f"✓ Ailang source generated: {args.output}.ailang")
        else:
            print(f"✓ Compilation successful: {args.output}")
            
    except Exception as e:
        print(f"✗ Error: {e}")
        if args.debug:
            import traceback
            traceback.print_exc()
        sys.exit(1)

if __name__ == "__main__":
    main()