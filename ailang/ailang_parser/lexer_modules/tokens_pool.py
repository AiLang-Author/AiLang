# tokens_pool.py - Pool Types and Operations Token Types
from enum import Enum, auto

class PoolTokens(Enum):
    # Pool Types
    FIXEDPOOL = auto()
    DYNAMICPOOL = auto()
    TEMPORALPOOL = auto()
    NEURALPOOL = auto()
    KERNELPOOL = auto()
    ACTORPOOL = auto()
    SECURITYPOOL = auto()
    CONSTRAINEDPOOL = auto()
    FILEPOOL = auto()

    # Pool Operations
    SUBPOOL = auto()
    INITIALIZE = auto()
    CANCHANGE = auto()
    CANBENULL = auto()
    RANGE = auto()
    MAXIMUMLENGTH = auto()
    MINIMUMLENGTH = auto()
    ELEMENTTYPE = auto()
    WHERE = auto()

    # Pool management
    POOLRESIZE = auto()
    POOLMOVE = auto()
    POOLCOMPACT = auto()
    POOLALLOCATE = auto()
    HASHCREATE = auto()
    HASHFUNCTION = auto()
    HASHSET = auto()
    HASHGET = auto()
    SOCKETCREATE = auto()
    SOCKETBIND = auto()
    SOCKETLISTEN = auto()
    SOCKETACCEPT = auto()
    SOCKETREAD = auto()
    SOCKETWRITE = auto()
    SOCKETCLOSE = auto()
    POOLFREE = auto()