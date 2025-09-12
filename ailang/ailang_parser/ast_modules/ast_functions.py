# ailang_parser/ast_modules/ast_functions.py
"""Function, lambda, combinator and macro AST nodes"""

from dataclasses import dataclass
from typing import List, Dict, Optional, Tuple
from .ast_base import ASTNode

@dataclass
class Function(ASTNode):
    name: str
    input_params: List[Tuple[str, ASTNode]]
    output_type: Optional[ASTNode]
    body: List[ASTNode]

@dataclass
class Lambda(ASTNode):
    params: List[str]
    body: ASTNode

@dataclass
class Combinator(ASTNode):
    name: str
    definition: ASTNode

@dataclass
class MacroBlock(ASTNode):
    name: str
    macros: Dict[str, 'MacroDefinition']

@dataclass
class MacroDefinition(ASTNode):
    name: str
    params: List[str]
    body: ASTNode

@dataclass
class SubRoutine(ASTNode):
    name: str
    body: List[ASTNode]