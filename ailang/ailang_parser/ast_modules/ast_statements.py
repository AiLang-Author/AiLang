# ailang_parser/ast_modules/ast_statements.py
"""Core statement AST nodes"""

from dataclasses import dataclass
from typing import List, Dict, Optional, Tuple
from .ast_base import ASTNode

@dataclass
class RunTask(ASTNode):
    task_name: str
    arguments: List[Tuple[str, ASTNode]]

@dataclass
class PrintMessage(ASTNode):
    message: ASTNode

@dataclass
class ReturnValue(ASTNode):
    value: ASTNode

@dataclass
class Assignment(ASTNode):
    target: str
    value: ASTNode

@dataclass
class SendMessage(ASTNode):
    target: str
    parameters: Dict[str, ASTNode]

@dataclass
class ReceiveMessage(ASTNode):
    message_type: str
    body: List[ASTNode]

@dataclass
class HaltProgram(ASTNode):
    message: Optional[str] = None

@dataclass
class BreakLoop(ASTNode):
    pass

@dataclass
class ContinueLoop(ASTNode):
    pass