# tokens_virtual_memory.py - Virtual Memory Token Types
from enum import Enum, auto

class VirtualMemoryTokens(Enum):
    # === VIRTUAL MEMORY TOKENS ===
    PAGETABLE = auto()
    VIRTUALMEMORY = auto()
    MMIO = auto()
    CACHE = auto()
    TLB = auto()
    MEMORYBARRIER = auto()

    # Memory Management Flags
    READONLY = auto()
    READWRITE = auto()
    READEXECUTE = auto()
    READWRITEEXECUTE = auto()
    USERMODE = auto()
    KERNELMODE = auto()
    GLOBAL = auto()
    DIRTY = auto()
    ACCESSED = auto()

    # Cache Types and Levels
    CACHED = auto()
    UNCACHED = auto()
    WRITECOMBINING = auto()
    WRITETHROUGH = auto()
    WRITEBACK = auto()
    L1CACHE = auto()
    L2CACHE = auto()
    L3CACHE = auto()

    # Page Sizes
    PAGESIZE4KB = auto()
    PAGESIZE2MB = auto()
    PAGESIZE1GB = auto()

    # TLB Operations
    INVALIDATE = auto()
    FLUSH = auto()
    FLUSHALL = auto()
    FLUSHGLOBAL = auto()