# ailang_parser/ast_modules/ast_program.py
"""Program structure AST nodes"""

from dataclasses import dataclass
from typing import List, Dict
from .ast_base import ASTNode

@dataclass
class Program(ASTNode):
    declarations: List[ASTNode]

@dataclass
class Library(ASTNode):
    name: str
    body: List[ASTNode]

@dataclass
class AcronymDefinitions(ASTNode):
    """AST node for acronym definition blocks"""
    definitions: Dict[str, str]  # acronym -> full_name mapping

@dataclass
class Constant(ASTNode):
    name: str
    value: ASTNode