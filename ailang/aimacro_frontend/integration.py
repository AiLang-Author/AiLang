#!/usr/bin/env python3

# Copyright (c) 2025 Sean Collins, 2 Paws Machine and Engineering. All rights reserved.
#
# Licensed under the Sean Collins Software License (SCSL). See the LICENSE file in the root directory of this project
# for the full terms and conditions, including restrictions on forking, corporate use, and permissions for private/teaching purposes.

"""
AIMacro Integration Module
Orchestrates the compilation pipeline from AIMacro source to native binary.
"""

import sys
import os
import argparse
import subprocess

# Ensure the parent directory is in the path for ailang_parser imports
project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

# Stage 1: Lexer
from aimacro_frontend.lexer import AIMacroLexer, LexerError
# Stage 2: Parser
from aimacro_frontend.parser import AIMacroParser, ParserError
# Stage 3: AST Converter
from aimacro_frontend.ast_converter import AIMacroToAILangConverter, AILangASTSerializer

class AIMacroIntegratedCompiler:
    """
    Full AIMacro to native binary compiler.
    Manages the pipeline:
    1. Lexing (AIMacro source -> Tokens)
    2. Parsing (Tokens -> AIMacro AST)
    3. Conversion (AIMacro AST -> AILang AST)
    4. Serialization (AILang AST -> .ailang source file)
    5. Invocation (Call backend compiler on .ailang file)
    """
    
    def __init__(self, debug=False):
        self.debug = debug
    
    def compile_file(self, input_file: str, output_file: str):
        """
        Executes the full compilation pipeline for a given AIMacro file.
        """
        try:
            with open(input_file, 'r', encoding='utf-8') as f:
                source_code = f.read()

            # --- Stage 1: Lexing ---
            print("✅ Stage 1: Lexing AIMacro source...", file=sys.stderr)
            lexer = AIMacroLexer(source_code)
            tokens = lexer.tokenize()

            # --- Stage 2: Parsing ---
            print("✅ Stage 2: Parsing tokens into AIMacro AST...", file=sys.stderr)
            parser = AIMacroParser(tokens)
            aimacro_ast = parser.parse()
            
            # --- Stage 3: AST Conversion ---
            print("✅ Stage 3: Converting AIMacro AST to AILang AST...", file=sys.stderr)
            converter = AIMacroToAILangConverter()
            ailang_ast = converter.convert(aimacro_ast)

            # --- Stage 4: Serialize AILang AST to .ailang source ---
            print("✅ Stage 4: Serializing AILang AST to source...", file=sys.stderr)
            serializer = AILangASTSerializer()
            ailang_source = serializer.serialize(ailang_ast)

            # Create a temporary .ailang file
            temp_ailang_file = os.path.splitext(output_file)[0] + ".ailang"
            with open(temp_ailang_file, 'w', encoding='utf-8') as f:
                f.write(ailang_source)

            if self.debug:
                print("--- Generated AILang Source ---", file=sys.stderr)
                print(ailang_source, file=sys.stderr)
                print("-----------------------------", file=sys.stderr)

            # --- Stage 5: Invoke the AILang backend compiler ---
            print(f"✅ Stage 5: Invoking AILang compiler on '{temp_ailang_file}'...", file=sys.stderr)
            project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
            backend_main_py = os.path.join(project_root, 'main.py')
            
            # Check if the backend compiler exists
            if not os.path.exists(backend_main_py):
                print(f"\n❌ Backend compiler not found at: {backend_main_py}", file=sys.stderr)
                print(f"Please ensure the AILang backend compiler (main.py) is in the project root.", file=sys.stderr)
                sys.exit(1)
            
            temp_ailang_file_abs = os.path.abspath(temp_ailang_file)
            output_file_abs = os.path.abspath(output_file)

            compile_command = [sys.executable, backend_main_py, temp_ailang_file_abs, '-o', output_file_abs]

            result = subprocess.run(compile_command, cwd=project_root, capture_output=True, text=True)

            # Clean up the intermediate file
            if not self.debug:
                os.remove(temp_ailang_file)

            if result.returncode != 0:
                print(f"\n❌ AILang Backend Compiler Failed:", file=sys.stderr)
                sys.stderr.write(result.stderr)
                sys.exit(1)

            print(f"\n🎉 Success! Compiled {input_file} to native binary: {output_file}", file=sys.stderr)

        except (LexerError, ParserError) as e:
            print(f"\n❌ Compilation Failed: {e.message}", file=sys.stderr)
            print(f"   at Line {e.line}, Column {e.column}", file=sys.stderr)
            sys.exit(1)
        except Exception as e:
            print(f"\n❌ An unexpected error occurred: {e}", file=sys.stderr)
            if self.debug:
                import traceback
                traceback.print_exc()
            sys.exit(1)

def main():
    """Command line interface for the integrated compiler."""
    parser = argparse.ArgumentParser(
        description="AIMacro Integrated Compiler: Compiles Python-like AIMacro files to native binaries."
    )
    parser.add_argument("input", help="Input AIMacro file (e.g., 'program.aim')")
    parser.add_argument(
        "-o", "--output",
        help="Output file name for the native binary. Defaults to input file name without extension."
    )
    parser.add_argument(
        "--debug",
        action="store_true",
        help="Enable debug output, showing tokens and ASTs at each stage."
    )
    
    args = parser.parse_args()
    
    # Determine output file name
    if not args.output:
        args.output = os.path.splitext(args.input)[0]
    
    compiler = AIMacroIntegratedCompiler(debug=args.debug)
    compiler.compile_file(args.input, args.output)

if __name__ == "__main__":
    main()