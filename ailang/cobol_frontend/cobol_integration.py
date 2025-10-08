#!/usr/bin/env python3

# Copyright (c) 2025 Sean Collins, 2 Paws Machine and Engineering. All rights reserved.
#
# Licensed under the Sean Collins Software License (SCSL). See the LICENSE file in the root directory of this project
# for the full terms and conditions, including restrictions on forking, corporate use, and permissions for private/teaching purposes.

"""
COBOL Integration Module
Orchestrates the compilation pipeline from COBOL source to native binary.
"""

import sys
import os
import argparse
import subprocess

# Ensure the parent directory is in the path
project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

# Import COBOL frontend stages
from cobol_lexer import COBOLLexer, LexerError
from cobol_parser import COBOLMultiProgramParser, ParserError
from cobol_ast_converter import COBOLToAilangMultiProgramConverter, AILangASTSerializer

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
    
    def __init__(self, debug=False):
        self.debug = debug
    
    def compile_file(self, input_file: str, output_file: str):
        """
        Executes the full compilation pipeline for COBOL file(s).
        Now handles multiple PROGRAM-IDs in one file correctly.
        """
        try:
            with open(input_file, 'r', encoding='utf-8') as f:
                source_code = f.read()

            # --- Stage 1: Lexing ---
            print("✓ Stage 1: Lexing COBOL source...", file=sys.stderr)
            lexer = COBOLLexer(source_code)
            tokens = lexer.tokenize()
            
            if self.debug:
                print("--- Tokens (first 20) ---", file=sys.stderr)
                for token in tokens[:20]:
                    print(f"  {token.type.name}: {token.value}", file=sys.stderr)
                if len(tokens) > 20:
                    print(f"  ... and {len(tokens) - 20} more", file=sys.stderr)

        # --- Stage 2: Parsing (UPDATED for multi-program) ---
            print("✓ Stage 2: Parsing tokens into COBOL AST...", file=sys.stderr)
            parser = COBOLMultiProgramParser(tokens)
            compilation_unit = parser.parse_all_programs()  # Returns COBOLCompilationUnit
            
            print(f"    Found {len(compilation_unit.programs)} COBOL program(s):", file=sys.stderr)
            for i, prog in enumerate(compilation_unit.programs):
                var_count = len(prog.data_division.working_storage) if prog.data_division else 0
                para_count = len([p for p in prog.procedure_division.paragraphs if p.name])
                print(f"      {i+1}. {prog.program_id} ({var_count} variables, {para_count} paragraphs)", file=sys.stderr)
            
            if self.debug:
                print("\n--- COBOL AST Structure ---", file=sys.stderr)
                for i, prog in enumerate(compilation_unit.programs):
                    print(f"  Program: {prog.program_id}", file=sys.stderr)
                    if prog.data_division:
                        print(f"    Variables: {len(prog.data_division.working_storage)}", file=sys.stderr)
                    print(f"    Paragraphs: {len(prog.procedure_division.paragraphs)}", file=sys.stderr)

        # --- Stage 3: Converting (UPDATED to handle compilation unit) ---
            print("✓ Stage 3: Converting COBOL AST to Ailang AST...", file=sys.stderr)
            converter = COBOLToAilangMultiProgramConverter(debug=self.debug)
            ailang_ast = converter.convert(compilation_unit)  # Pass compilation unit
            
            if self.debug:
                print(f"\n--- Ailang AST ---", file=sys.stderr)
                print(f"  Total subroutines: {len(ailang_ast.declarations)}", file=sys.stderr)
                for decl in ailang_ast.declarations:
                    if hasattr(decl, 'name'):
                        print(f"    - {decl.name}", file=sys.stderr)

        # --- Stage 4: Serializing ---
            print("✓ Stage 4: Serializing Ailang AST to source...", file=sys.stderr)
            serializer = AILangASTSerializer()
            ailang_source = serializer.serialize(ailang_ast)
            
            # Write .ailang file
            ailang_file = f"{output_file}.ailang"
            with open(ailang_file, 'w', encoding='utf-8') as f:
                f.write(ailang_source)
            
            if self.debug:
                print(f"\n--- Generated Ailang Source (first 50 lines) ---", file=sys.stderr)
                lines = ailang_source.split('\n')
                for i, line in enumerate(lines[:50], 1):
                    print(f"  {i:3}: {line}", file=sys.stderr)
                if len(lines) > 50:
                    print(f"  ... and {len(lines) - 50} more lines", file=sys.stderr)

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
            executable = f"{output_file}_exec"
            print(f"✓ Compilation successful!", file=sys.stderr)
            print(f"  Executable: {executable}", file=sys.stderr)
            print(f"\nRun with: ./{executable}", file=sys.stderr)
            
            # Cleanup intermediate file unless debugging
            # Keep intermediate files for debugging
            print(f"✓ Ailang output saved to: {ailang_file}", file=sys.stderr)
            # os.remove(ailang_file) # Disabled for debugging
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
        description="COBOL to Ailang Integrated Compiler: Compiles COBOL files to native binaries.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Compile a COBOL program
  python3 cobol_frontend/cobol_integration.py program.cbl -o program
  
  # Compile with debug output
  python3 cobol_frontend/cobol_integration.py program.cbl -o program --debug
  
  # Generate Ailang file only (don't compile to binary)
  python3 cobol_frontend/cobol_integration.py program.cbl --ailang-only

Supported COBOL features:
  - IDENTIFICATION DIVISION (PROGRAM-ID)
  - DATA DIVISION (WORKING-STORAGE SECTION)
  - PROCEDURE DIVISION
  - Statements: DISPLAY, MOVE, COMPUTE, ADD, SUBTRACT, MULTIPLY, DIVIDE
  - Control flow: IF/ELSE/END-IF, PERFORM UNTIL/END-PERFORM, PERFORM paragraph
  - STOP RUN, GOBACK
        """
    )
    parser.add_argument("input", help="Input COBOL file (e.g., 'program.cbl' or 'program.cob')")
    parser.add_argument(
        "-o", "--output",
        help="Output file name for the native binary. Defaults to input file name without extension."
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
    
    args = parser.parse_args()
    
    # Determine output file name
    if not args.output:
        base_name = os.path.splitext(args.input)[0]
        args.output = base_name
    
    # For ailang-only mode, generate the .ailang file
    if args.ailang_only:
        try:
            with open(args.input, 'r', encoding='utf-8') as f:
                source_code = f.read()
            
            print("✓ Stage 1: Lexing COBOL source...")
            lexer = COBOLLexer(source_code)
            tokens = lexer.tokenize()
            
            print("✓ Stage 2: Parsing tokens into COBOL AST...")
            parser_obj = COBOLMultiProgramParser(tokens)
            compilation_unit = parser_obj.parse_all_programs()
            
            print("✓ Stage 3: Converting COBOL AST to Ailang AST...")            
            converter = COBOLToAilangMultiProgramConverter(debug=args.debug)
            ailang_ast = converter.convert(compilation_unit)
            
            print("✓ Stage 4: Serializing Ailang AST to source...")
            serializer = AILangASTSerializer()
            program_body_source = serializer.serialize(ailang_ast)

            # Prepend the string pool to the final source
            string_pool_source = converter.generate_string_pool_decl()
            
            final_source_parts = []
            if string_pool_source:
                final_source_parts.append("// String literal pool for memory safety")
                final_source_parts.append(string_pool_source)
                final_source_parts.append("")
            final_source_parts.append(program_body_source)
            ailang_source = "\n".join(final_source_parts)
            
            output_ailang = args.output if args.output.endswith('.ailang') else args.output + '.ailang'
            with open(output_ailang, 'w', encoding='utf-8') as f:
                f.write(ailang_source)
            
            print(f"\n✓ Generated Ailang source: {output_ailang}")
            
            if args.debug:
                print("\n" + "=" * 60)
                print("Generated Ailang Code:")
                print("=" * 60)
                print(ailang_source)
        
        except Exception as e:
            print(f"\n✗ Error: {e}", file=sys.stderr)
            if args.debug:
                import traceback
                traceback.print_exc()
            sys.exit(1)
    else:
        compiler = COBOLIntegratedCompiler(debug=args.debug)
        compiler.compile_file(args.input, args.output)

if __name__ == "__main__":
    main()