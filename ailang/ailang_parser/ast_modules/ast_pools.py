# Copyright (c) 2025 Sean Collins, 2 Paws Machine and Engineering. All rights reserved.
#
# Licensed under the Sean Collins Software License (SCSL). See the LICENSE file in the root directory of this project
# for the full terms and conditions, including restrictions on forking, corporate use, and permissions for private/teaching purposes.

# ailang_parser/ast_modules/ast_pools.py
"""Pool-related AST nodes"""

from dataclasses import dataclass, field
from typing import List, Dict, Optional
from .ast_base import ASTNode

@dataclass
class Pool(ASTNode):
    pool_type: str
    name: str
    body: List[ASTNode]

@dataclass
class SubPool(ASTNode):
    name: str
    items: Dict[str, 'ResourceItem']

@dataclass
class ResourceItem(ASTNode):
    key: str
    value: Optional[ASTNode]
    attributes: Dict[str, ASTNode] = field(default_factory=dict)