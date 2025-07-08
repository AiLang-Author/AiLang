
# compiler.py
from .lexer import Lexer
from .parser import Parser
from .ailang_ast import Program

class AILANGCompiler:
    """Main compiler class for AILANG"""
    
    def __init__(self, strict_mode: bool = True):
        self.strict_mode = strict_mode
    
    def compile(self, source: str) -> Program:
        """Compile AILANG source code to AST"""
        lexer = Lexer(source, strict_mode=self.strict_mode)
        tokens = lexer.tokenize()
        parser = Parser(tokens, strict_math=self.strict_mode)
        ast = parser.parse()
        return ast
    
    def compile_file(self, filename: str) -> Program:
        """Compile AILANG file"""
        with open(filename, 'r') as f:
            source = f.read()
        return self.compile(source)
