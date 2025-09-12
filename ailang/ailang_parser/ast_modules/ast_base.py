# ailang_parser/ast_modules/ast_base.py
"""Base AST node class"""

from dataclasses import dataclass

@dataclass
class ASTNode:
    line: int
    column: int