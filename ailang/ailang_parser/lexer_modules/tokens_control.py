# tokens_control.py - Control Flow and Debug Token Types
from enum import Enum, auto

class ControlFlowTokens(Enum):
    # Control Flow Keywords
    RUNTASK = auto()
    PRINTMESSAGE = auto()
    RETURNVALUE = auto()
    IFCONDITION = auto()
    THENBLOCK = auto()
    ELSEBLOCK = auto()
    CHOOSEPATH = auto()
    CASEOPTION = auto()
    DEFAULTOPTION = auto()
    WHILELOOP = auto()
    UNTILCONDITION = auto()
    FOREVERY = auto()
    IN = auto()
    TRYBLOCK = auto()
    CATCHERROR = auto()
    FINALLYBLOCK = auto()
    SENDMESSAGE = auto()
    RECEIVEMESSAGE = auto()
    EVERYINTERVAL = auto()
    BREAKLOOP = auto()
    HALTPROGRAM = auto()
    CONTINUELOOP = auto()

    # Branching logic
    FORK = auto()
    BRANCH = auto()
    CASE = auto()
    DEFAULT = auto()
    TRUEPATH = auto()
    FALSEPATH = auto()

class DebugTokens(Enum):
    # Debug Operations
    DEBUG = auto()
    DEBUGASSERT = auto()
    DEBUGTRACE = auto()
    DEBUGBREAK = auto()
    DEBUGMEMORY = auto()
    DEBUGPERF = auto()
    DEBUGINSPECT = auto()
    DEBUGCONTROL = auto()