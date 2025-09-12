# tokens_constants.py - Constants and Values Token Types
from enum import Enum, auto

class ConstantTokens(Enum):
    # Constants/Values
    TRUE = auto()
    FALSE = auto()
    NULL = auto()
    AUTOMATIC = auto()
    UNLIMITED = auto()

    # Mathematical Constants
    CONSTANT = auto()
    PI = auto()
    E = auto()
    PHI = auto()

class UnitTokens(Enum):
    # Units
    BYTES = auto()
    KILOBYTES = auto()
    MEGABYTES = auto()
    GIGABYTES = auto()
    SECONDS = auto()
    MILLISECONDS = auto()
    MICROSECONDS = auto()
    PERCENT = auto()