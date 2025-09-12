# ailang_parser/ast_modules/ast_expressions.py
"""Expression and literal AST nodes"""

from dataclasses import dataclass, field
from typing import List, Optional, Union, Tuple
from .ast_base import ASTNode

@dataclass
class TypeExpression(ASTNode):
    base_type: str
    parameters: List[ASTNode] = field(default_factory=list)
    constraints: Optional[ASTNode] = None

@dataclass
class MathExpression(ASTNode):
    expression: ASTNode

@dataclass
class FunctionCall(ASTNode):
    function: str
    arguments: List[ASTNode]

@dataclass
class Apply(ASTNode):
    function: ASTNode
    arguments: List[ASTNode]

@dataclass
class RunMacro(ASTNode):
    macro_path: str
    arguments: List[ASTNode]

@dataclass
class Identifier(ASTNode):
    name: str

@dataclass
class Number(ASTNode):
    value: Union[int, float]

@dataclass
class String(ASTNode):
    value: str

@dataclass
class Boolean(ASTNode):
    value: bool

@dataclass
class ArrayLiteral(ASTNode):
    elements: List[ASTNode]

@dataclass
class MapLiteral(ASTNode):
    pairs: List[Tuple[ASTNode, ASTNode]]

# Convenience alias
Array = ArrayLiteral  # Some code might use Array instead of ArrayLiteral