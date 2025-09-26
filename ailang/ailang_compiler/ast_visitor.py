#!/usr/bin/env python3
"""
AST Visitor Base Class for AILANG Compiler
Provides a standard, unified way to traverse the AST.
"""

class ASTVisitor:
    """
    A base class for visitors of the AILang AST.
    Implements the visitor pattern to traverse the AST.
    """
    def visit(self, node):
        """Dispatches to the appropriate handler for the node type."""
        if node is None:
            return
        
        method_name = f'handle_{type(node).__name__}'
        handler = getattr(self, method_name, self.handle_generic)
        return handler(node)
    
    def handle_generic(self, node):
        """Default visitor: recursively visits all children of a node."""
        for attr in dir(node):
            if not attr.startswith('_'):
                value = getattr(node, attr)
                if isinstance(value, list):
                    for item in value:
                        self.visit(item)
                elif hasattr(value, '__dict__'):
                    self.visit(value)