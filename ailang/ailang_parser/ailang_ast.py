# ailang_parser/ailang_ast.py
"""
Main AST module - provides backward compatibility bridge
Imports all AST node classes from the modular structure
"""

# Handle both relative and absolute imports
try:
    # Try relative import first (when imported as part of package)
    from .ast_modules import *
except ImportError:
    # Fall back to absolute import (when imported directly)
    from ailang_parser.ast_modules import *

# Re-export everything for backward compatibility
__all__ = [
    # Base
    'ASTNode',
    
    # Expressions
    'TypeExpression', 'MathExpression', 'FunctionCall', 'Apply', 'RunMacro',
    'Identifier', 'Number', 'String', 'Boolean', 'ArrayLiteral', 'MapLiteral',
    'Array',
    
    # Program
    'Program', 'Library', 'AcronymDefinitions', 'Constant',
    
    # Pools
    'Pool', 'SubPool', 'ResourceItem',
    
    # File I/O
    'FilePool', 'FileHandle', 'FileOperation',
    
    # Statements
    'RunTask', 'PrintMessage', 'ReturnValue', 'Assignment',
    'SendMessage', 'ReceiveMessage', 'HaltProgram',
    'BreakLoop', 'ContinueLoop',
    
    # Control Flow
    'If', 'While', 'ForEvery', 'ChoosePath', 'Try', 'Fork', 'Branch',
    'EveryInterval', 'WithSecurity',
    
    # Functions
    'Function', 'Lambda', 'Combinator', 'MacroBlock', 'MacroDefinition', 'SubRoutine',
    
    # Loops
    'Loop', 'LoopMain', 'LoopActor', 'LoopStart', 'LoopShadow',
    'LoopSend', 'LoopReceive', 'LoopCase', 'LoopReply', 'LoopYield',
    'LoopContinue', 'LoopSpawn', 'LoopJoin', 'LoopGetState',
    'LoopSetPriority', 'LoopGetCurrent', 'LoopSuspend',
    'LoopResume', 'LoopInterrupt',
    
    # Security and Types
    'SecurityContext', 'SecurityLevel', 'ConstrainedType',
    'RecordTypeDefinition', 'RecordType',
    
    # Low-Level
    'PointerOperation', 'Dereference', 'AddressOf', 'SizeOf',
    'MemoryAllocation', 'MemoryDeallocation', 'MemoryOperation',
    'HardwareRegisterAccess', 'PortOperation',
    'InterruptHandler', 'InterruptControl',
    'AtomicOperation', 'MemoryBarrier', 'CacheOperation',
    'InlineAssembly', 'SystemCall', 'PrivilegeLevel',
    'DeviceDriver', 'DeviceRegisterAccess',
    'MMIOOperation', 'DMAOperation',
    'BootloaderCode', 'KernelEntry',
    'TaskSwitch', 'ProcessContext',
    'PointerType', 'LowLevelType',
    
    # Virtual Memory
    'PageTableOperation', 'VirtualMemoryOperation',
    'MemoryMappingOperation', 'TLBOperation', 'MemoryBarrierOperation',
    
    # Debug
    'DebugBlock', 'DebugAssert', 'DebugTrace', 'DebugBreak',
    'DebugMemory', 'DebugPerf', 'DebugInspect'
]