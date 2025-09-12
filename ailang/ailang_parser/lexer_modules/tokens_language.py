# tokens_language.py - Language Feature Token Types
from enum import Enum, auto

class FunctionTokens(Enum):
    # Lambda/Function Keywords
    FUNCTION = auto()
    LAMBDA = auto()
    APPLY = auto()
    COMBINATOR = auto()
    INPUT = auto()
    OUTPUT = auto()
    BODY = auto()
    CURRY = auto()
    UNCURRY = auto()
    COMPOSE = auto()

class MacroTokens(Enum):
    # Macro Keywords
    MACROBLOCK = auto()
    MACRO = auto()
    RUNMACRO = auto()
    EXPANDMACRO = auto()

class SecurityTokens(Enum):
    # Security Keywords
    SECURITYCONTEXT = auto()
    WITHSECURITY = auto()
    ALLOWEDOPERATIONS = auto()
    DENIEDOPERATIONS = auto()
    MEMORYLIMIT = auto()
    CPUQUOTA = auto()
    LEVEL = auto()

class SystemTokens(Enum):
    # System/Hardware Keywords
    HARDWARE = auto()
    SYSCALL = auto()
    INTERRUPT = auto()
    REGISTER = auto()
    MEMORY = auto()
    PHYSICALADDRESS = auto()
    VIRTUALADDRESS = auto()
    FLAGS = auto()

class OrganizationTokens(Enum):
    # Code Organization
    SUBROUTINE = auto()
    LIBRARYIMPORT = auto()
    LOOPMAIN = auto()
    LOOPACTOR = auto()
    LOOPSTART = auto()
    LOOPEND = auto()
    LOOPSHADOW = auto()