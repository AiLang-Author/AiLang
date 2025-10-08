# Copyright (c) 2025 Sean Collins, 2 Paws Machine and Engineering. All rights reserved.
#
# Licensed under the Sean Collins Software License (SCSL). See the LICENSE file in the root directory of this project
# for the full terms and conditions, including restrictions on forking, corporate use, and permissions for private/teaching purposes.

# compiler.py

# --- FIX: Use relative imports for modules in the same package ---
from .lexer import Lexer
from .parser import Parser
from .ailang_ast import Program, Library, Function, SubRoutine
import os
# --- End FIX ---

class AILANGCompiler:
    """Main compiler class for AILANG"""
    
    def __init__(self, strict_mode: bool = True):
        self.strict_mode = strict_mode
        # This symbol table will be populated and used by the next compilation stage (e.g., code generation).
        self.symbol_table = {}
        self._parsed_files = set()

    def _parse_and_collect(self, filepath: str, project_root: str):
        """Recursively parse a file and its imports, collecting symbols."""
        abs_filepath = os.path.abspath(filepath)
        if abs_filepath in self._parsed_files:
            return
        self._parsed_files.add(abs_filepath)
        
        try:
            with open(abs_filepath, 'r') as f:
                source = f.read()
        except FileNotFoundError:
            print(f"Warning: Could not find file to import: {abs_filepath}")
            return
        
        lexer = Lexer(source, strict_mode=self.strict_mode)
        tokens = lexer.tokenize()
        parser = Parser(tokens, strict_math=self.strict_mode)
        ast = parser.parse()
        
        # Process imports first (depth-first) to build the full symbol dependency tree
        for decl in ast.declarations:
            if isinstance(decl, Library):
                # Convention: Library.XArrays -> Librarys/Library/Library.XArrays.ailang
                lib_filename = f"{decl.name}.ailang"
                lib_path = os.path.join(project_root, 'Librarys', 'Library', lib_filename)
                self._parse_and_collect(lib_path, project_root)
        
        # Then collect symbols from the current file
        for decl in ast.declarations:
            if isinstance(decl, (Function, SubRoutine)):
                if decl.name in self.symbol_table:
                    # This is a common scenario if multiple files import the same library.
                    # We can safely ignore it.
                    pass
                self.symbol_table[decl.name] = decl
    
    def compile(self, source: str) -> Program:
        """Compile AILANG source code to AST. Note: Does not handle file-based imports."""
        lexer = Lexer(source, strict_mode=self.strict_mode)
        tokens = lexer.tokenize()
        parser = Parser(tokens, strict_math=self.strict_mode)
        ast = parser.parse()
        return ast
    
    def compile_file(self, filename: str) -> Program:
        """
        Compile an AILANG file, populating the compiler's symbol_table
        with all functions from the file and its imports.
        """
        # Reset state for a new compilation run
        self.symbol_table.clear()
        self._parsed_files.clear()
        
        # Assume the project root is two levels up from the source file (e.g., from '.../AiLang_v_2.0/redis_server.ailang')
        project_root = os.path.dirname(os.path.dirname(os.path.abspath(filename)))
        
        # Start the recursive parsing and symbol collection
        self._parse_and_collect(filename, project_root)
        
        # Finally, return the AST of the main file. The compiler instance now
        # holds the complete symbol table for the next compilation stage.
        return self.compile_file_without_imports(filename)

    def compile_file_without_imports(self, filename: str) -> Program:
        """Helper to just parse a file without reprocessing imports."""
        with open(filename, 'r') as f: source = f.read()
        return self.compile(source)
