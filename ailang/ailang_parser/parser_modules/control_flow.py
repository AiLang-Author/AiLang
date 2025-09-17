# ailang_parser/compiler_modules/control_flow.py
"""
Compiler module for handling core flow control constructs like If, While, Branch, etc.

Try/Catch logic has been moved to its own module: try_catch.py
"""

class ControlFlowCompiler:
    def __init__(self, compiler):
        self.compiler = compiler
        self.emitter = compiler.emitter

    # Methods like compile_if, compile_while, etc. would go here.