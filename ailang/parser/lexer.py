# lexer.py - ENHANCED FOR SYSTEMS PROGRAMMING
from enum import Enum, auto
from dataclasses import dataclass
from typing import List, Optional, Union, Any, Tuple
import re

class TokenType(Enum):
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

    # Debug Operations
    DEBUG = auto()
    DEBUGASSERT = auto()
    DEBUGTRACE = auto()
    DEBUGBREAK = auto()
    DEBUGMEMORY = auto()
    DEBUGPERF = auto()
    DEBUGINSPECT = auto()
    DEBUGCONTROL = auto()
    CONTINUELOOP = auto()


    # Branching logic
    FORK = auto()
    BRANCH = auto()
    CASE = auto()
    DEFAULT = auto()
    TRUEPATH = auto()
    FALSEPATH = auto()


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

    #Pool management
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

    # Math Operators (Named)
    ADD = auto()
    SUBTRACT = auto()
    MULTIPLY = auto()
    DIVIDE = auto()
    POWER = auto()
    MODULO = auto()
    SQUAREROOT = auto()
    ABSOLUTEVALUE = auto()

    # Comparison Operators (Named)
    GREATERTHAN = auto()
    LESSTHAN = auto()
    GREATEREQUAL = auto()
    LESSEQUAL = auto()
    EQUALTO = auto()
    NOTEQUAL = auto()

    # Logical Operators (Named)
    AND = auto()
    OR = auto()
    NOT = auto()
    XOR = auto()
    IMPLIES = auto()

    # Bitwise Operators (Named)
    BITWISEAND = auto()
    BITWISEOR = auto()
    BITWISEXOR = auto()
    BITWISENOT = auto()
    LEFTSHIFT = auto()
    RIGHTSHIFT = auto()

    # Interactive Input Functions
    READINPUT = auto()
    READINPUTNUMBER = auto()
    GETUSERCHOICE = auto()
    READKEY = auto()

    # String Comparison Functions
    STRINGEQUALS = auto()
    STRINGCONTAINS = auto()
    STRINGSTARTSWITH = auto()
    STRINGENDSWITH = auto()

    # String Manipulation Functions
    STRINGCONCAT = auto()
    STRINGLENGTH = auto()
    STRINGSUBSTRING = auto()
    STRINGTOUPPER = auto()
    STRINGTOLOWER = auto()
    STRINGTRIM = auto()
    STRINGREPLACE = auto()
    STRINGTOSTRING = auto()
    NUMBERTOSTRING = auto()
    STRINGTONUMBER = auto()
    CHARTOSTRING = auto()  
    
    # === FILE I/O OPERATIONS ===
    OPENFILE = auto()
    CLOSEFILE = auto()
    READFILE = auto()
    WRITEFILE = auto()
    CREATEFILE = auto()
    DELETEFILE = auto()
    READLINE = auto()
    WRITELINE = auto()
    READTEXTFILE = auto()
    WRITETEXTFILE = auto()
    APPENDTEXTFILE = auto()
    READBINARYFILE = auto()
    WRITEBINARYFILE = auto()
    APPENDBINARYFILE = auto()
    FILEEXISTS = auto()
    GETFILESIZE = auto()
    GETFILEDATE = auto()
    SETFILEDATE = auto()
    GETFILEPERMISSIONS = auto()
    SETFILEPERMISSIONS = auto()
    SEEKPOSITION = auto()
    GETPOSITION = auto()
    REWIND = auto()
    COPYFILE = auto()
    MOVEFILE = auto()
    RENAMEFILE = auto()
    FLUSHFILE = auto()
    LOCKFILE = auto()
    UNLOCKFILE = auto()
    CREATEDIRECTORY = auto()
    DELETEDIRECTORY = auto()
    LISTDIRECTORY = auto()
    DIRECTORYEXISTS = auto()
    GETWORKINGDIRECTORY = auto()
    SETWORKINGDIRECTORY = auto()
    BUFFEREDREAD = auto()
    BUFFEREDWRITE = auto()
    SETBUFFERSIZE = auto()
    FLUSHBUFFERS = auto()

    # === NEW: LOW-LEVEL SYSTEMS PROGRAMMING TOKENS ===

    # Memory and Pointer Operations
    POINTER = auto()                    # Pointer type declaration
    DEREFERENCE = auto()               # Dereference pointer to get value
    ADDRESSOF = auto()                 # Get address of variable
    SIZEOF = auto()                    # Get size of type/variable
    ALLOCATE = auto()
    DEALLOCATE = auto()
    MEMORYCOPY = auto()                # Copy memory blocks
    MEMORYSET = auto()                 # Set memory to value
    MEMORYCOMPARE = auto()             # Compare memory blocks
    STOREVALUE = auto()                # Store value to memory address

    # Hardware Register Access
    HARDWAREREGISTER = auto()          # Access CPU registers
    CONTROLREGISTER = auto()           # Access control registers (CR0, CR3, etc.)
    SEGMENTREGISTER = auto()           # Access segment registers (CS, DS, etc.)
    FLAGSREGISTER = auto()             # Access flags register
    MODELSPECIFICREGISTER = auto()     # Access MSRs

    # Port I/O Operations
    PORTREAD = auto()                  # Read from I/O port
    PORTWRITE = auto()                 # Write to I/O port
    PORTREADBYTE = auto()              # Read byte from port
    PORTWRITEBYTE = auto()             # Write byte to port
    PORTREADWORD = auto()              # Read word from port
    PORTWRITEWORD = auto()             # Write word to port
    PORTREADDWORD = auto()             # Read dword from port
    PORTWRITEDWORD = auto()            # Write dword to port

    # Interrupt and Exception Handling
    INTERRUPTHANDLER = auto()          # Define interrupt handler
    EXCEPTIONHANDLER = auto()          # Define exception handler
    ENABLEINTERRUPTS = auto()          # Enable interrupts (STI)
    DISABLEINTERRUPTS = auto()         # Disable interrupts (CLI)
    HALT = auto()                      # Halt processor (HLT)
    WAIT = auto()                      # Wait for interrupt
    TRIGGERSOFTWAREINTERRUPT = auto()  # Software interrupt (INT)
    INTERRUPTVECTOR = auto()           # Interrupt vector table

    # Atomic Operations
    ATOMICREAD = auto()                # Atomic read operation
    ATOMICWRITE = auto()               # Atomic write operation
    ATOMICADD = auto()                 # Atomic add operation
    ATOMICSUBTRACT = auto()            # Atomic subtract operation
    ATOMICCOMPARESWAP = auto()         # Compare and swap
    ATOMICEXCHANGE = auto()            # Atomic exchange
    COMPILERFENCE = auto()             # Compiler fence

    # Cache and Memory Management
    CACHEINVALIDATE = auto()           # Invalidate cache
    CACHEFLUSH = auto()                # Flush cache
    TLBINVALIDATE = auto()             # Invalidate TLB
    TLBFLUSH = auto()                  # Flush TLB
    PHYSICALMEMORY = auto()            # Physical memory access

    # Inline Assembly
    INLINEASSEMBLY = auto()            # Inline assembly block
    ASSEMBLY = auto()                  # Assembly instruction
    VOLATILE = auto()                  # Volatile memory access
    BARRIER = auto()                   # Memory/compiler barrier

    # System Calls and Kernel Operations
    SYSTEMCALL = auto()                # Make system call
    PRIVILEGELEVEL = auto()            # CPU privilege level
    TASKSWITCH = auto()                # Task/context switch
    PROCESSCONTEXT = auto()            # Process context

    # Device Driver Operations
    DEVICEDRIVER = auto()              # Device driver declaration
    DEVICEREGISTER = auto()            # Device register access
    DMAOPERATION = auto()              # DMA operations
    MMIOREAD = auto()                  # Memory-mapped I/O read
    MMIOWRITE = auto()                 # Memory-mapped I/O write
    DEVICEINTERRUPT = auto()           # Device interrupt handler

    # Boot and Initialization
    BOOTLOADER = auto()                # Bootloader code
    KERNELENTRY = auto()               # Kernel entry point
    INITIALIZATION = auto()            # System initialization
    GLOBALCONSTRUCTORS = auto()        # Global constructors
    GLOBALDESTRUCTORS = auto()         # Global destructors


    # === VIRTUAL MEMORY TOKENS ===
    PAGETABLE = auto()              # PageTable operations
    VIRTUALMEMORY = auto()          # VirtualMemory operations
    MMIO = auto()                   # Memory-mapped I/O
    CACHE = auto()                  # Cache operations
    TLB = auto()                    # Translation Lookaside Buffer
    MEMORYBARRIER = auto()          # Memory barriers/fences

    # Memory Management Flags
    READONLY = auto()               # RO protection
    READWRITE = auto()              # RW protection
    READEXECUTE = auto()            # RX protection
    READWRITEEXECUTE = auto()       # RWX protection
    USERMODE = auto()               # User mode access
    KERNELMODE = auto()             # Kernel mode access
    GLOBAL = auto()                 # Global page
    DIRTY = auto()                  # Dirty bit
    ACCESSED = auto()               # Accessed bit

    # Cache Types and Levels
    CACHED = auto()                 # Cached memory
    UNCACHED = auto()               # Uncached memory
    WRITECOMBINING = auto()         # Write combining
    WRITETHROUGH = auto()           # Write through
    WRITEBACK = auto()              # Write back
    L1CACHE = auto()                # L1 cache
    L2CACHE = auto()                # L2 cache
    L3CACHE = auto()                # L3 cache

    # Page Sizes
    PAGESIZE4KB = auto()            # 4KB pages
    PAGESIZE2MB = auto()            # 2MB pages (huge)
    PAGESIZE1GB = auto()            # 1GB pages (gigantic)

    # TLB Operations
    INVALIDATE = auto()             # Invalidate operation
    FLUSH = auto()                  # Flush operation
    FLUSHALL = auto()               # Flush all operation
    FLUSHGLOBAL = auto()            # Flush global operation




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

    # Type Keywords
    INTEGER = auto()
    FLOATINGPOINT = auto()
    TEXT = auto()
    BOOLEAN = auto()
    ADDRESS = auto()
    ARRAY = auto()
    MAP = auto()
    TUPLE = auto()
    RECORD = auto()
    OPTIONALTYPE = auto()
    CONSTRAINEDTYPE = auto()
    ANY = auto()
    VOID = auto()

    # === NEW: Low-Level Type Keywords ===
    BYTE = auto()                      # 8-bit unsigned integer
    WORD = auto()                      # 16-bit unsigned integer
    DWORD = auto()                     # 32-bit unsigned integer
    QWORD = auto()                     # 64-bit unsigned integer
    UINT8 = auto()                     # 8-bit unsigned
    UINT16 = auto()                    # 16-bit unsigned
    UINT32 = auto()                    # 32-bit unsigned
    UINT64 = auto()                    # 64-bit unsigned
    INT8 = auto()                      # 8-bit signed
    INT16 = auto()                     # 16-bit signed
    INT32 = auto()                     # 32-bit signed
    INT64 = auto()                     # 64-bit signed





    # Macro Keywords
    MACROBLOCK = auto()
    MACRO = auto()
    RUNMACRO = auto()
    EXPANDMACRO = auto()

    # Security Keywords
    SECURITYCONTEXT = auto()
    WITHSECURITY = auto()
    ALLOWEDOPERATIONS = auto()
    DENIEDOPERATIONS = auto()
    MEMORYLIMIT = auto()
    CPUQUOTA = auto()
    LEVEL = auto()

    # System/Hardware Keywords
    HARDWARE = auto()
    SYSCALL = auto()
    INTERRUPT = auto()
    REGISTER = auto()
    MEMORY = auto()
    PHYSICALADDRESS = auto()
    VIRTUALADDRESS = auto()
    FLAGS = auto()

    # Code Organization
    SUBROUTINE = auto()
    LIBRARYIMPORT = auto()
    LOOPMAIN = auto()
    LOOPACTOR = auto()
    LOOPSTART = auto()
    LOOPEND = auto()
    LOOPSHADOW = auto()

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

    # Units
    BYTES = auto()
    KILOBYTES = auto()
    MEGABYTES = auto()
    GIGABYTES = auto()
    SECONDS = auto()
    MILLISECONDS = auto()
    MICROSECONDS = auto()
    PERCENT = auto()

    # Delimiters
    DOT = auto()
    LBRACE = auto()
    RBRACE = auto()
    LPAREN = auto()
    RPAREN = auto()
    LBRACKET = auto()
    RBRACKET = auto()
    COMMA = auto()
    COLON = auto()
    SEMICOLON = auto()
    DASH = auto()
    EQUALS = auto()
    ARROW = auto()

    # Literals
    IDENTIFIER = auto()
    NUMBER = auto()
    STRING = auto()

    # Comments
    COMMENT = auto()
    DOC_COMMENT = auto()
    COM_COMMENT = auto()
    TAG_COMMENT = auto()

    # Special
    EOF = auto()
    NEWLINE = auto()

    # Error token
    ERROR = auto()

@dataclass
class Token:
    type: TokenType
    value: Any
    line: int
    column: int
    length: int = 1

class LexerError(Exception):
    def __init__(self, message: str, line: int, column: int):
        self.message = message
        self.line = line
        self.column = column
        super().__init__(f"Lexer error at line {line}, column {column}: {message}")

class Lexer:
    def __init__(self, source: str, strict_mode: bool = True):
        self.source = source
        self.position = 0
        self.line = 1
        self.column = 1
        self.tokens: List[Token] = []
        self.strict_mode = strict_mode
        self.keywords = {
            # Original keywords (preserved)
            'RunTask': TokenType.RUNTASK,
            'PrintMessage': TokenType.PRINTMESSAGE,
            'ReturnValue': TokenType.RETURNVALUE,
            'IfCondition': TokenType.IFCONDITION,
            'ThenBlock': TokenType.THENBLOCK,
            'ElseBlock': TokenType.ELSEBLOCK,
            'ChoosePath': TokenType.CHOOSEPATH,
            'CaseOption': TokenType.CASEOPTION,
            'DefaultOption': TokenType.DEFAULTOPTION,
            'WhileLoop': TokenType.WHILELOOP,
            'UntilCondition': TokenType.UNTILCONDITION,
            'ForEvery': TokenType.FOREVERY,
            'in': TokenType.IN,
            'TryBlock': TokenType.TRYBLOCK,
            'CatchError': TokenType.CATCHERROR,
            'FinallyBlock': TokenType.FINALLYBLOCK,
            'SendMessage': TokenType.SENDMESSAGE,
            'ReceiveMessage': TokenType.RECEIVEMESSAGE,
            'EveryInterval': TokenType.EVERYINTERVAL,
            'BreakLoop': TokenType.BREAKLOOP,
            'HaltProgram': TokenType.HALTPROGRAM,

            'Debug': TokenType.DEBUG,
            'DebugAssert': TokenType.DEBUGASSERT,
            'DebugTrace': TokenType.DEBUGTRACE,
            'DebugBreak': TokenType.DEBUGBREAK,
            'DebugMemory': TokenType.DEBUGMEMORY,
            'DebugPerf': TokenType.DEBUGPERF,
            'DebugInspect': TokenType.DEBUGINSPECT,
            'DebugControl': TokenType.DEBUGCONTROL,
            'ContinueLoop': TokenType.CONTINUELOOP,
            'FixedPool': TokenType.FIXEDPOOL,
            'DynamicPool': TokenType.DYNAMICPOOL,
            'TemporalPool': TokenType.TEMPORALPOOL,
            'NeuralPool': TokenType.NEURALPOOL,
            'KernelPool': TokenType.KERNELPOOL,
            'ActorPool': TokenType.ACTORPOOL,
            'SecurityPool': TokenType.SECURITYPOOL,
            'ConstrainedPool': TokenType.CONSTRAINEDPOOL,
            'PoolResize': TokenType.POOLRESIZE,
            'PoolMove': TokenType.POOLMOVE,
            'PoolCompact': TokenType.POOLCOMPACT,
            'PoolAllocate': TokenType.POOLALLOCATE,
            'PoolFree': TokenType.POOLFREE,
            'FilePool': TokenType.FILEPOOL,
            'SubPool': TokenType.SUBPOOL,
            'Initialize': TokenType.INITIALIZE,
            'CanChange': TokenType.CANCHANGE,
            'CanBeNull': TokenType.CANBENULL,
            'Range': TokenType.RANGE,
            'MaximumLength': TokenType.MAXIMUMLENGTH,
            'MinimumLength': TokenType.MINIMUMLENGTH,
            'ElementType': TokenType.ELEMENTTYPE,
            'Where': TokenType.WHERE,
            'Add': TokenType.ADD,
            'Subtract': TokenType.SUBTRACT,
            'Multiply': TokenType.MULTIPLY,
            'Divide': TokenType.DIVIDE,
            'Power': TokenType.POWER,
            'Modulo': TokenType.MODULO,
            'SquareRoot': TokenType.SQUAREROOT,
            'AbsoluteValue': TokenType.ABSOLUTEVALUE,
            'GreaterThan': TokenType.GREATERTHAN,
            'LessThan': TokenType.LESSTHAN,
            'GreaterEqual': TokenType.GREATEREQUAL,
            'LessEqual': TokenType.LESSEQUAL,
            'EqualTo': TokenType.EQUALTO,
            'NotEqual': TokenType.NOTEQUAL,
            'And': TokenType.AND,
            'Or': TokenType.OR,
            'Not': TokenType.NOT,
            'Xor': TokenType.XOR,
            'Implies': TokenType.IMPLIES,
            'BitwiseAnd': TokenType.BITWISEAND,
            'BitwiseOr': TokenType.BITWISEOR,
            'BitwiseXor': TokenType.BITWISEXOR,
            'BitwiseNot': TokenType.BITWISENOT,
            'LeftShift': TokenType.LEFTSHIFT,
            'RightShift': TokenType.RIGHTSHIFT,
            'Function': TokenType.FUNCTION,
            'Lambda': TokenType.LAMBDA,
            'Apply': TokenType.APPLY,
            'Combinator': TokenType.COMBINATOR,
            'Input': TokenType.INPUT,
            'Output': TokenType.OUTPUT,
            'Body': TokenType.BODY,
            'Curry': TokenType.CURRY,
            'Uncurry': TokenType.UNCURRY,
            'Compose': TokenType.COMPOSE,
            'Integer': TokenType.INTEGER,
            'FloatingPoint': TokenType.FLOATINGPOINT,
            'Text': TokenType.TEXT,
            'Boolean': TokenType.BOOLEAN,
            'Address': TokenType.ADDRESS,
            'Array': TokenType.ARRAY,
            'Map': TokenType.MAP,
            'Tuple': TokenType.TUPLE,
            'Record': TokenType.RECORD,
            'OptionalType': TokenType.OPTIONALTYPE,
            'ConstrainedType': TokenType.CONSTRAINEDTYPE,
            'Any': TokenType.ANY,
            'Void': TokenType.VOID,
            'MacroBlock': TokenType.MACROBLOCK,
            'Macro': TokenType.MACRO,
            'RunMacro': TokenType.RUNMACRO,
            'ExpandMacro': TokenType.EXPANDMACRO,
            'SecurityContext': TokenType.SECURITYCONTEXT,
            'WithSecurity': TokenType.WITHSECURITY,
            'AllowedOperations': TokenType.ALLOWEDOPERATIONS,
            'DeniedOperations': TokenType.DENIEDOPERATIONS,
            'MemoryLimit': TokenType.MEMORYLIMIT,
            'CPUQuota': TokenType.CPUQUOTA,
            'Level': TokenType.LEVEL,
            'Hardware': TokenType.HARDWARE,
            'Syscall': TokenType.SYSCALL,
            'Interrupt': TokenType.INTERRUPT,
            'Register': TokenType.REGISTER,
            'Memory': TokenType.MEMORY,
            'PhysicalAddress': TokenType.PHYSICALADDRESS,
            'VirtualAddress': TokenType.VIRTUALADDRESS,
            'Flags': TokenType.FLAGS,
            'SubRoutine': TokenType.SUBROUTINE,
            'LibraryImport': TokenType.LIBRARYIMPORT,
            'LoopMain': TokenType.LOOPMAIN,
            'LoopActor': TokenType.LOOPACTOR,
            'LoopStart': TokenType.LOOPSTART,
            'LoopEnd': TokenType.LOOPEND,
            'LoopShadow': TokenType.LOOPSHADOW,
            'True': TokenType.TRUE,
            'False': TokenType.FALSE,
            'Null': TokenType.NULL,
            'Automatic': TokenType.AUTOMATIC,
            'Unlimited': TokenType.UNLIMITED,
            'Constant': TokenType.CONSTANT,
            'PI': TokenType.PI,
            'E': TokenType.E,
            'PHI': TokenType.PHI,
            'Bytes': TokenType.BYTES,
            'Kilobytes': TokenType.KILOBYTES,
            'Megabytes': TokenType.MEGABYTES,
            'Gigabytes': TokenType.GIGABYTES,
            'Seconds': TokenType.SECONDS,
            'Milliseconds': TokenType.MILLISECONDS,
            'Microseconds': TokenType.MICROSECONDS,
            'Percent': TokenType.PERCENT,

            #branching logic
            'Fork': TokenType.FORK,
            'Branch': TokenType.BRANCH,
            'Case': TokenType.CASE,
            'Default': TokenType.DEFAULT,
            'TruePath': TokenType.TRUEPATH,
            'FalsePath': TokenType.FALSEPATH,



            # String and I/O functions (preserved)
            'ReadInput': TokenType.READINPUT,
            'ReadInputNumber': TokenType.READINPUTNUMBER,
            'GetUserChoice': TokenType.GETUSERCHOICE,
            'ReadKey': TokenType.READKEY,
            'StringEquals': TokenType.STRINGEQUALS,
            'StringContains': TokenType.STRINGCONTAINS,
            'StringStartsWith': TokenType.STRINGSTARTSWITH,
            'StringEndsWith': TokenType.STRINGENDSWITH,
            'StringConcat': TokenType.STRINGCONCAT,
            'StringLength': TokenType.STRINGLENGTH,
            'StringSubstring': TokenType.STRINGSUBSTRING,
            'StringToUpper': TokenType.STRINGTOUPPER,
            'StringToLower': TokenType.STRINGTOLOWER,
            'StringTrim': TokenType.STRINGTRIM,
            'StringReplace': TokenType.STRINGREPLACE,
            'StringToString': TokenType.STRINGTOSTRING,
            'NumberToString': TokenType.NUMBERTOSTRING,
            'StringToNumber': TokenType.STRINGTONUMBER,
            'CharToString': TokenType.CHARTOSTRING,

            # File I/O (preserved)
            'OpenFile': TokenType.OPENFILE,
            'CloseFile': TokenType.CLOSEFILE,
            'ReadFile': TokenType.READFILE,
            'WriteFile': TokenType.WRITEFILE,
            'CreateFile': TokenType.CREATEFILE,
            'DeleteFile': TokenType.DELETEFILE,
            'ReadLine': TokenType.READLINE,
            'WriteLine': TokenType.WRITELINE,
            'ReadTextFile': TokenType.READTEXTFILE,
            'WriteTextFile': TokenType.WRITETEXTFILE,
            'AppendTextFile': TokenType.APPENDTEXTFILE,
            'ReadBinaryFile': TokenType.READBINARYFILE,
            'WriteBinaryFile': TokenType.WRITEBINARYFILE,
            'AppendBinaryFile': TokenType.APPENDBINARYFILE,
            'FileExists': TokenType.FILEEXISTS,
            'GetFileSize': TokenType.GETFILESIZE,
            'GetFileDate': TokenType.GETFILEDATE,
            'SetFileDate': TokenType.SETFILEDATE,
            'GetFilePermissions': TokenType.GETFILEPERMISSIONS,
            'SetFilePermissions': TokenType.SETFILEPERMISSIONS,
            'SeekPosition': TokenType.SEEKPOSITION,
            'GetPosition': TokenType.GETPOSITION,
            'Rewind': TokenType.REWIND,
            'CopyFile': TokenType.COPYFILE,
            'MoveFile': TokenType.MOVEFILE,
            'RenameFile': TokenType.RENAMEFILE,
            'FlushFile': TokenType.FLUSHFILE,
            'LockFile': TokenType.LOCKFILE,
            'UnlockFile': TokenType.UNLOCKFILE,
            'CreateDirectory': TokenType.CREATEDIRECTORY,
            'DeleteDirectory': TokenType.DELETEDIRECTORY,
            'ListDirectory': TokenType.LISTDIRECTORY,
            'DirectoryExists': TokenType.DIRECTORYEXISTS,
            'GetWorkingDirectory': TokenType.GETWORKINGDIRECTORY,
            'SetWorkingDirectory': TokenType.SETWORKINGDIRECTORY,
            'BufferedRead': TokenType.BUFFEREDREAD,
            'BufferedWrite': TokenType.BUFFEREDWRITE,
            'SetBufferSize': TokenType.SETBUFFERSIZE,
            'FlushBuffers': TokenType.FLUSHBUFFERS,

            # === NEW: Low-Level Systems Programming Keywords ===

            # Memory and Pointer Operations
            'Pointer': TokenType.POINTER,
            'Dereference': TokenType.DEREFERENCE,
            'AddressOf': TokenType.ADDRESSOF,
            'SizeOf': TokenType.SIZEOF,
            'Allocate': TokenType.ALLOCATE,
            'HashCreate': TokenType.HASHCREATE,
            'HashFunction': TokenType.HASHFUNCTION,
            'HashSet': TokenType.HASHSET,
            'HashGet': TokenType.HASHGET,
            'SocketCreate': TokenType.SOCKETCREATE,
            'SocketBind': TokenType.SOCKETBIND,
            'SocketListen': TokenType.SOCKETLISTEN,
            'SocketAccept': TokenType.SOCKETACCEPT,
            'SocketRead': TokenType.SOCKETREAD,
            'SocketWrite': TokenType.SOCKETWRITE,
            'SocketClose': TokenType.SOCKETCLOSE,
            'Deallocate': TokenType.DEALLOCATE,
            'MemoryCopy': TokenType.MEMORYCOPY,
            'MemorySet': TokenType.MEMORYSET,
            'MemoryCompare': TokenType.MEMORYCOMPARE,
            'StoreValue': TokenType.STOREVALUE,

            # Hardware Register Access
            'HardwareRegister': TokenType.HARDWAREREGISTER,
            'ControlRegister': TokenType.CONTROLREGISTER,
            'SegmentRegister': TokenType.SEGMENTREGISTER,
            'FlagsRegister': TokenType.FLAGSREGISTER,
            'ModelSpecificRegister': TokenType.MODELSPECIFICREGISTER,

            # Port I/O Operations
            'PortRead': TokenType.PORTREAD,
            'PortWrite': TokenType.PORTWRITE,
            'PortReadByte': TokenType.PORTREADBYTE,
            'PortWriteByte': TokenType.PORTWRITEBYTE,
            'PortReadWord': TokenType.PORTREADWORD,
            'PortWriteWord': TokenType.PORTWRITEWORD,
            'PortReadDWord': TokenType.PORTREADDWORD,
            'PortWriteDWord': TokenType.PORTWRITEDWORD,

            # Interrupt and Exception Handling
            'InterruptHandler': TokenType.INTERRUPTHANDLER,
            'ExceptionHandler': TokenType.EXCEPTIONHANDLER,
            'EnableInterrupts': TokenType.ENABLEINTERRUPTS,
            'DisableInterrupts': TokenType.DISABLEINTERRUPTS,
            'Halt': TokenType.HALT,
            'Wait': TokenType.WAIT,
            'TriggerSoftwareInterrupt': TokenType.TRIGGERSOFTWAREINTERRUPT,
            'InterruptVector': TokenType.INTERRUPTVECTOR,

            # Atomic Operations
            'AtomicRead': TokenType.ATOMICREAD,
            'AtomicWrite': TokenType.ATOMICWRITE,
            'AtomicAdd': TokenType.ATOMICADD,
            'AtomicSubtract': TokenType.ATOMICSUBTRACT,
            'AtomicCompareSwap': TokenType.ATOMICCOMPARESWAP,
            'AtomicExchange': TokenType.ATOMICEXCHANGE,
            'CompilerFence': TokenType.COMPILERFENCE,

            # Cache and Memory Management
            'CacheInvalidate': TokenType.CACHEINVALIDATE,
            'CacheFlush': TokenType.CACHEFLUSH,
            'TLBInvalidate': TokenType.TLBINVALIDATE,
            'TLBFlush': TokenType.TLBFLUSH,
            'PhysicalMemory': TokenType.PHYSICALMEMORY,

            # Inline Assembly
            'InlineAssembly': TokenType.INLINEASSEMBLY,
            'Assembly': TokenType.ASSEMBLY,
            'Volatile': TokenType.VOLATILE,
            'Barrier': TokenType.BARRIER,

            # System Calls and Kernel Operations
            'SystemCall': TokenType.SYSTEMCALL,
            'PrivilegeLevel': TokenType.PRIVILEGELEVEL,
            'TaskSwitch': TokenType.TASKSWITCH,
            'ProcessContext': TokenType.PROCESSCONTEXT,

            # Device Driver Operations
            'DeviceDriver': TokenType.DEVICEDRIVER,
            'DeviceRegister': TokenType.DEVICEREGISTER,
            'DMAOperation': TokenType.DMAOPERATION,
            'MMIORead': TokenType.MMIOREAD,
            'MMIOWrite': TokenType.MMIOWRITE,
            'DeviceInterrupt': TokenType.DEVICEINTERRUPT,

            # Boot and Initialization
            'Bootloader': TokenType.BOOTLOADER,
            'KernelEntry': TokenType.KERNELENTRY,
            'Initialization': TokenType.INITIALIZATION,
            'GlobalConstructors': TokenType.GLOBALCONSTRUCTORS,
            'GlobalDestructors': TokenType.GLOBALDESTRUCTORS,

            # Low-Level Types
            'Byte': TokenType.BYTE,
            'Word': TokenType.WORD,
            'DWord': TokenType.DWORD,
            'QWord': TokenType.QWORD,
            'UInt8': TokenType.UINT8,
            'UInt16': TokenType.UINT16,
            'UInt32': TokenType.UINT32,
            'UInt64': TokenType.UINT64,
            'Int8': TokenType.INT8,
            'Int16': TokenType.INT16,
            'Int32': TokenType.INT32,
            'Int64': TokenType.INT64,

            # === VIRTUAL MEMORY KEYWORDS ===

            # Main VM Operations
            'PageTable': TokenType.PAGETABLE,
            'VirtualMemory': TokenType.VIRTUALMEMORY,
            'MMIO': TokenType.MMIO,
            'Cache': TokenType.CACHE,
            'TLB': TokenType.TLB,
            'MemoryBarrier': TokenType.MEMORYBARRIER,


            # Memory Protection Flags
            'ReadOnly': TokenType.READONLY,
            'ReadWrite': TokenType.READWRITE,
            'ReadExecute': TokenType.READEXECUTE,
            'ReadWriteExecute': TokenType.READWRITEEXECUTE,
            'RO': TokenType.READONLY,           # Short form
            'RW': TokenType.READWRITE,          # Short form
            'RX': TokenType.READEXECUTE,        # Short form
            'RWX': TokenType.READWRITEEXECUTE,  # Short form
            'UserMode': TokenType.USERMODE,
            'KernelMode': TokenType.KERNELMODE,
            'Global': TokenType.GLOBAL,
            'Dirty': TokenType.DIRTY,
            'Accessed': TokenType.ACCESSED,

            # Cache Types
            'Cached': TokenType.CACHED,
            'Uncached': TokenType.UNCACHED,
            'WriteCombining': TokenType.WRITECOMBINING,
            'WriteThrough': TokenType.WRITETHROUGH,
            'WriteBack': TokenType.WRITEBACK,

            # Cache Levels
            'L1': TokenType.L1CACHE,
            'L2': TokenType.L2CACHE,
            'L3': TokenType.L3CACHE,

            # Page Sizes
            '4KB': TokenType.PAGESIZE4KB,
            '2MB': TokenType.PAGESIZE2MB,
            '1GB': TokenType.PAGESIZE1GB,

            # TLB Operations
            'Invalidate': TokenType.INVALIDATE,
            'Flush': TokenType.FLUSH,
            'FlushAll': TokenType.FLUSHALL,
            'FlushGlobal': TokenType.FLUSHGLOBAL,
        }

        # Allow short identifiers for systems programming (register names, etc.)
        self.allowed_short_identifiers = {
            'in', 'to', 'by', 'of', 'as', 'is', 'on', 'at', 'PI', 'E',
            'GRP', 'HW', 'CFG', 'MEM', 'CPU', 'SYS', 'IO', 'RG', 'VG',
            'PG', 'TG', 'NG', 'KG', 'AG', 'SG', 'CG', 'FG',
            # New: CPU register names
            'EAX', 'EBX', 'ECX', 'EDX', 'ESI', 'EDI', 'ESP', 'EBP',
            'RAX', 'RBX', 'RCX', 'RDX', 'RSI', 'RDI', 'RSP', 'RBP',
            'R8', 'R9', 'R10', 'R11', 'R12', 'R13', 'R14', 'R15',
            'CS', 'DS', 'ES', 'FS', 'GS', 'SS', 'CR0', 'CR1', 'CR2',
            'CR3', 'CR4', 'CR8', 'DR0', 'DR1', 'DR2', 'DR3', 'DR6', 'DR7'
        }

    def error(self, message: str):
        raise LexerError(message, self.line, self.column)

    def warning(self, message: str):
        print(f"Warning at line {self.line}, column {self.column}: {message}")

    def current_char(self) -> Optional[str]:
        if self.position >= len(self.source):
            return None
        return self.source[self.position]

    def peek_char(self, offset: int = 1) -> Optional[str]:
        pos = self.position + offset
        if pos >= len(self.source):
            return None
        return self.source[pos]

    def advance(self):
        if self.position < len(self.source):
            if self.source[self.position] == '\n':
                self.line += 1
                self.column = 1
            else:
                self.column += 1
            self.position += 1

    def skip_whitespace(self):
        while self.current_char() and self.current_char() in ' \t\r':
            self.advance()

    def read_string(self) -> str:
        value = ''
        self.advance()
        while self.current_char() and self.current_char() != '"':
            if self.current_char() == '\\':
                self.advance()
                if self.current_char() is None:
                    self.error("Unterminated string escape")
                escape_char = self.current_char()
                if escape_char == 'n':
                    value += '\n'
                elif escape_char == 't':
                    value += '\t'
                elif escape_char == 'r':
                    value += '\r'
                elif escape_char == '"':
                    value += '"'
                elif escape_char == '\\':
                    value += '\\'
                elif escape_char == '0':
                    value += '\0'
                else:
                    self.warning(f"Unknown escape sequence '\\{escape_char}'")
                    value += escape_char
            else:
                value += self.current_char()
            self.advance()
        if self.current_char() == '"':
            self.advance()
        else:
            self.error("Unterminated string literal")
        return value

    def read_number(self) -> Token:
        """Read a numeric literal and return a NUMBER token with string value."""
        start_pos = self.position
        line_start = self.line
        col_start = self.column
        value = ''

        # Handle hexadecimal literals (e.g., 0xFF)
        if self.current_char() == '0' and self.peek_char() in 'xX':
            value += self.current_char()
            self.advance()
            value += self.current_char()
            self.advance()
            while self.current_char() and self.current_char() in '0123456789ABCDEFabcdef_':
                if self.current_char() != '_':
                    value += self.current_char()
                self.advance()
            if len(value) <= 2:  # Only "0x" or "0X"
                self.error("Invalid hexadecimal literal")
        else:
            # Handle decimal numbers (integers or floats)
            has_dot = False
            while self.current_char() and (self.current_char().isdigit() or self.current_char() in '._'):
                if self.current_char() == '.':
                    if has_dot:
                        break
                    has_dot = True
                    value += '.'
                elif self.current_char() != '_':
                    value += self.current_char()
                self.advance()

            # Handle scientific notation (e.g., 1e-10)
            if self.current_char() and self.current_char() in 'eE':
                value += self.current_char()
                self.advance()
                if self.current_char() and self.current_char() in '+-':
                    value += self.current_char()
                    self.advance()
                while self.current_char() and self.current_char().isdigit():
                    value += self.current_char()
                    self.advance()

            # Validate the number
            if not value or value == '.':
                self.error(f"Invalid number literal: {value}")
            if value.count('.') > 1:
                self.error(f"Invalid number literal: multiple decimal points in {value}")

        return Token(TokenType.NUMBER, value, line_start, col_start, len(value))

    def read_identifier(self) -> str:
        value = ''
        if self.current_char().isalpha() or self.current_char() == '_':
            value += self.current_char()
            self.advance()
        while self.current_char() and (self.current_char().isalnum() or self.current_char() == '_'):
            value += self.current_char()
            self.advance()
        if not value:
            self.error(f"Invalid identifier at line {self.line}, column {self.column}")
        return value


    def read_dotted_identifier(self) -> str:
        parts = [self.read_identifier()]
        while self.current_char() == '.':
            if self.peek_char() and self.peek_char().isalpha():
                self.advance()
                parts.append(self.read_identifier())
            else:
                break
        return '.'.join(parts)

    def read_comment(self) -> Tuple[TokenType, str]:
        if self.peek_char() != '/':
            self.error("Invalid comment start")
        self.advance()
        self.advance()
        comment_type = TokenType.COMMENT
        prefix = ''
        if self.current_char() and self.current_char().isalpha():
            while self.current_char() and self.current_char().isalpha():
                prefix += self.current_char()
                self.advance()
            if prefix == 'DOC':
                comment_type = TokenType.DOC_COMMENT
            elif prefix == 'COM':
                comment_type = TokenType.COM_COMMENT
            elif prefix == 'TAG':
                comment_type = TokenType.TAG_COMMENT
            else:
                self.position -= len(prefix)
                self.column -= len(prefix)
                prefix = ''
        if prefix and self.current_char() == ':':
            self.advance()
        while self.current_char() and self.current_char() in ' \t':
            self.advance()
        value = ''
        if comment_type in (TokenType.DOC_COMMENT, TokenType.COM_COMMENT):
            while self.position + 1 < len(self.source):
                if self.source[self.position:self.position+2] == '//':
                    self.advance()
                    self.advance()
                    break
                value += self.current_char()
                self.advance()
        else:
            while self.current_char() and self.current_char() != '\n':
                value += self.current_char()
                self.advance()
        return comment_type, value.strip()

    def tokenize(self) -> List[Token]:
        self.tokens = []
        while self.position < len(self.source):
            self.skip_whitespace()
            if not self.current_char():
                break
            line_start = self.line
            col_start = self.column
            if self.current_char() == '\n':
                self.tokens.append(Token(TokenType.NEWLINE, '\n', line_start, col_start))
                self.advance()
                continue
            if self.current_char() == '/' and self.peek_char() == '/':
                comment_type, value = self.read_comment()
                self.tokens.append(Token(comment_type, value, line_start, col_start))
                continue
            if self.current_char() == '"':
                value = self.read_string()
                self.tokens.append(Token(TokenType.STRING, value, line_start, col_start))
                continue
            if self.current_char().isdigit() or (self.current_char() == '-' and self.peek_char() and self.peek_char().isdigit()):
                value = ''
                if self.current_char() == '-':
                    value = self.current_char()
                    self.advance()
                value += self.read_number().value
                self.tokens.append(Token(TokenType.NUMBER, value, line_start, col_start, len(value)))
                continue
            two_char = self.source[self.position:self.position+2]
            if two_char == '->':
                self.advance()
                self.advance()
                self.tokens.append(Token(TokenType.ARROW, '->', line_start, col_start))
                continue
            single_char_tokens = {
                '=': TokenType.EQUALS,
                '{': TokenType.LBRACE,
                '}': TokenType.RBRACE,
                '(': TokenType.LPAREN,
                ')': TokenType.RPAREN,
                '[': TokenType.LBRACKET,
                ']': TokenType.RBRACKET,
                ',': TokenType.COMMA,
                ':': TokenType.COLON,
                ';': TokenType.SEMICOLON,
                '.': TokenType.DOT,
                '-': TokenType.DASH,
            }
            if self.current_char() in single_char_tokens:
                if self.current_char() == '.' and self.peek_char() and self.peek_char().isalpha():
                    pass
                else:
                    token_type = single_char_tokens[self.current_char()]
                    value = self.current_char()
                    self.advance()
                    self.tokens.append(Token(token_type, value, line_start, col_start, 1))
                    continue
            if self.current_char().isalpha() or self.current_char() == '_':
                value = self.read_identifier()
                token_type = self.keywords.get(value, TokenType.IDENTIFIER)
                if token_type != TokenType.IDENTIFIER:
                    self.tokens.append(Token(token_type, value, line_start, col_start, len(value)))
                    continue
                if self.current_char() == '.' and self.peek_char() and self.peek_char().isalpha():
                    parts = [value]
                    while self.current_char() == '.' and self.peek_char() and self.peek_char().isalpha():
                        self.advance()  # consume the dot
                        parts.append(self.read_identifier())
                    value = '.'.join(parts)
                self.tokens.append(Token(TokenType.IDENTIFIER, value, line_start, col_start, len(value)))
                continue
            if self.current_char() == '.':
                self.advance()
                self.tokens.append(Token(TokenType.DOT, '.', line_start, col_start, 1))
                continue
            self.error(f"Unknown character '{self.current_char()}'")
        self.tokens.append(Token(TokenType.EOF, None, self.line, self.column))
        return self.tokens