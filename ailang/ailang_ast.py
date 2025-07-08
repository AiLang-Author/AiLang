# ailang_ast.py - ENHANCED FOR SYSTEMS PROGRAMMING
from dataclasses import dataclass, field
from typing import List, Optional, Union, Dict, Tuple

@dataclass
class ASTNode:
    line: int
    column: int

@dataclass
class Program(ASTNode):
    declarations: List[ASTNode]

@dataclass
class Library(ASTNode):
    name: str
    body: List[ASTNode]

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

# === FILE I/O AST NODES ===

@dataclass
class FilePool(ASTNode):
    """Represents a FilePool declaration for managing file handles"""
    name: str
    handles: Dict[str, 'FileHandle']

@dataclass
class FileHandle(ASTNode):
    """Represents a file handle with path and mode"""
    handle_name: str
    file_path: ASTNode  # Usually a String node
    mode: str          # "read", "write", "append", "readwrite", etc.
    options: Dict[str, ASTNode] = field(default_factory=dict)

@dataclass
class FileOperation(ASTNode):
    """Generic file operation node"""
    operation: str              # "open", "read", "write", "close", etc.
    file_argument: ASTNode      # File path or handle
    mode: Optional[str] = None  # File mode for open operations
    data: Optional[ASTNode] = None      # Data for write operations
    position: Optional[ASTNode] = None  # Position for seek operations
    buffer_size: Optional[ASTNode] = None  # Buffer size for buffered operations

# === NEW: LOW-LEVEL SYSTEMS PROGRAMMING AST NODES ===

@dataclass
class PointerOperation(ASTNode):
    """Base class for pointer operations"""
    operation: str  # "dereference", "address_of", "pointer_arithmetic"
    target: ASTNode
    offset: Optional[ASTNode] = None  # For pointer arithmetic

@dataclass
class Dereference(ASTNode):
    """Dereference a pointer to get the value it points to"""
    pointer: ASTNode  # The pointer to dereference
    size_hint: Optional[str] = None  # "byte", "word", "dword", "qword"

@dataclass
class AddressOf(ASTNode):
    """Get the address of a variable"""
    variable: ASTNode  # The variable to get address of

@dataclass
class SizeOf(ASTNode):
    """Get the size of a type or variable"""
    target: ASTNode  # Type or variable to get size of

@dataclass
class MemoryAllocation(ASTNode):
    """Allocate memory"""
    size: ASTNode = None  # Size to allocate
    alignment: Optional[ASTNode] = None  # Memory alignment  # Memory alignment

@dataclass
class MemoryDeallocation(ASTNode):
    """Free allocated memory"""
    pointer: ASTNode  # Pointer to memory to free

@dataclass
class MemoryOperation(ASTNode):
    """Generic memory operation (copy, set, compare)"""
    operation: str  # "copy", "set", "compare"
    destination: ASTNode
    source: Optional[ASTNode] = None
    size: ASTNode = None
    value: Optional[ASTNode] = None  # For memory set operations

@dataclass
class HardwareRegisterAccess(ASTNode):
    """Access CPU/hardware registers"""
    register_type: str  # "general", "control", "segment", "flags", "msr"
    register_name: str  # "RAX", "CR3", "CS", etc.
    operation: str  # "read", "write"
    value: Optional[ASTNode] = None  # For write operations

@dataclass
class PortOperation(ASTNode):
    """I/O port operations"""
    operation: str  # "read", "write"
    port: ASTNode  # Port number
    size: str  # "byte", "word", "dword"
    value: Optional[ASTNode] = None  # For write operations

@dataclass
class InterruptHandler(ASTNode):
    """Define an interrupt or exception handler"""
    handler_type: str  # "interrupt", "exception"
    vector: ASTNode  # Interrupt vector number
    handler_name: str  # Name of the handler function
    body: List[ASTNode]

@dataclass
class InterruptControl(ASTNode):
    """Control interrupt state"""
    operation: str  # "enable", "disable", "trigger"
    interrupt_number: Optional[ASTNode] = None  # For software interrupts

@dataclass
class AtomicOperation(ASTNode):
    """Atomic memory operations"""
    operation: str  # "read", "write", "add", "subtract", "compare_swap", "exchange"
    target: ASTNode  # Memory location
    value: Optional[ASTNode] = None  # Value for operations
    compare_value: Optional[ASTNode] = None  # For compare_swap
    ordering: str = "sequential"  # Memory ordering

@dataclass
class MemoryBarrier(ASTNode):
    """Memory barrier/fence operations"""
    barrier_type: str  # "memory", "compiler", "acquire", "release"

@dataclass
class CacheOperation(ASTNode):
    """Cache management operations"""
    operation: str  # "invalidate", "flush"
    cache_type: str  # "data", "instruction", "tlb"
    address: Optional[ASTNode] = None  # Specific address or None for all

@dataclass
class InlineAssembly(ASTNode):
    """Inline assembly code"""
    assembly_code: str  # Raw assembly instructions
    inputs: List[Tuple[str, ASTNode]] = field(default_factory=list)  # Input constraints
    outputs: List[Tuple[str, ASTNode]] = field(default_factory=list)  # Output constraints
    clobbers: List[str] = field(default_factory=list)  # Clobbered registers
    volatile: bool = False  # Whether assembly has side effects

@dataclass
class SystemCall(ASTNode):
    """Make a system call"""
    call_number: ASTNode  # System call number
    arguments: List[ASTNode] = field(default_factory=list)  # System call arguments

@dataclass
class PrivilegeLevel(ASTNode):
    """Set or check privilege level"""
    operation: str  # "set", "get", "check"
    level: Optional[ASTNode] = None  # Privilege level (0-3)

@dataclass
class DeviceDriver(ASTNode):
    """Device driver declaration"""
    driver_name: str
    device_type: str  # "block", "character", "network", etc.
    operations: Dict[str, ASTNode]  # Driver operation handlers

@dataclass
class DeviceRegisterAccess(ASTNode):
    """Access device registers"""
    operation: str  # "read", "write"
    device: ASTNode  # Device identifier
    register_offset: ASTNode  # Register offset
    value: Optional[ASTNode] = None  # For write operations

@dataclass
class MMIOOperation(ASTNode):
    """Memory-mapped I/O operations"""
    operation: str  # "read", "write"
    address: ASTNode  # Physical/virtual address
    size: str  # "byte", "word", "dword", "qword"
    value: Optional[ASTNode] = None  # For write operations
    volatile: bool = True  # MMIO is typically volatile

@dataclass
class DMAOperation(ASTNode):
    """Direct Memory Access operations"""
    operation: str  # "setup", "start", "stop", "status"
    channel: ASTNode  # DMA channel
    source: Optional[ASTNode] = None  # Source address
    destination: Optional[ASTNode] = None  # Destination address
    size: Optional[ASTNode] = None  # Transfer size

@dataclass
class BootloaderCode(ASTNode):
    """Bootloader-specific code"""
    stage: str  # "stage1", "stage2", "uefi"
    body: List[ASTNode]

@dataclass
class KernelEntry(ASTNode):
    """Kernel entry point"""
    entry_name: str
    parameters: List[Tuple[str, ASTNode]] = field(default_factory=list)
    body: List[ASTNode] = field(default_factory=list)

@dataclass
class PageTableOperation(ASTNode):
    """Page table management"""
    operation: str  # "create", "map", "unmap", "switch"
    virtual_address: Optional[ASTNode] = None
    physical_address: Optional[ASTNode] = None
    page_table: Optional[ASTNode] = None
    flags: Optional[ASTNode] = None  # Page flags (readable, writable, executable, etc.)

@dataclass
class VirtualMemoryOperation(ASTNode):
    """Virtual memory management"""
    operation: str  # "allocate", "free", "protect", "map"
    address: Optional[ASTNode] = None
    size: ASTNode = None
    protection: Optional[ASTNode] = None  # Protection flags

@dataclass
class TaskSwitch(ASTNode):
    """Context/task switching"""
    operation: str  # "save", "restore", "switch"
    context: ASTNode  # Context identifier or structure

@dataclass
class ProcessContext(ASTNode):
    """Process context management"""
    operation: str  # "create", "destroy", "switch"
    process_id: Optional[ASTNode] = None
    context_data: Optional[ASTNode] = None

# === END LOW-LEVEL AST NODES ===

@dataclass
class Loop(ASTNode):
    loop_type: str
    name: str
    body: List[ASTNode]
    end_name: Optional[str] = None

@dataclass
class SubRoutine(ASTNode):
    name: str
    body: List[ASTNode]

@dataclass
class Function(ASTNode):
    name: str
    input_params: List[Tuple[str, ASTNode]]
    output_type: Optional[ASTNode]
    body: List[ASTNode]

@dataclass
class Lambda(ASTNode):
    params: List[str]
    body: ASTNode

@dataclass
class Combinator(ASTNode):
    name: str
    definition: ASTNode

@dataclass
class MacroBlock(ASTNode):
    name: str
    macros: Dict[str, 'MacroDefinition']

@dataclass
class MacroDefinition(ASTNode):
    name: str
    params: List[str]
    body: ASTNode

@dataclass
class SecurityContext(ASTNode):
    name: str
    levels: Dict[str, 'SecurityLevel']

@dataclass
class SecurityLevel(ASTNode):
    name: str
    allowed_operations: List[str]
    denied_operations: List[str]
    memory_limit: Optional[ASTNode]
    cpu_quota: Optional[ASTNode]

@dataclass
class ConstrainedType(ASTNode):
    name: str
    base_type: ASTNode
    constraints: ASTNode

@dataclass
class Constant(ASTNode):
    name: str
    value: ASTNode

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
class If(ASTNode):
    condition: ASTNode
    then_body: List[ASTNode]
    else_body: Optional[List[ASTNode]] = None

@dataclass
class ChoosePath(ASTNode):
    expression: ASTNode
    cases: List[Tuple[str, List[ASTNode]]]
    default: Optional[List[ASTNode]] = None

@dataclass
class While(ASTNode):
    condition: ASTNode
    body: List[ASTNode]

@dataclass
class ForEvery(ASTNode):
    variable: str
    collection: ASTNode
    body: List[ASTNode]

@dataclass
class Try(ASTNode):
    body: List[ASTNode]
    catch_clauses: List[Tuple[str, List[ASTNode]]]
    finally_body: Optional[List[ASTNode]] = None

@dataclass
class SendMessage(ASTNode):
    target: str
    parameters: Dict[str, ASTNode]

@dataclass
class ReceiveMessage(ASTNode):
    message_type: str
    body: List[ASTNode]

@dataclass
class EveryInterval(ASTNode):
    interval_type: str
    interval_value: Union[int, float]
    body: List[ASTNode]

@dataclass
class WithSecurity(ASTNode):
    context: str
    body: List[ASTNode]

@dataclass
class Assignment(ASTNode):
    target: str
    value: ASTNode

@dataclass
class BreakLoop(ASTNode):
    pass

@dataclass
class ContinueLoop(ASTNode):
    pass

@dataclass
class HaltProgram(ASTNode):
    message: Optional[str] = None

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

@dataclass
class TypeExpression(ASTNode):
    base_type: str
    parameters: List[ASTNode] = field(default_factory=list)
    constraints: Optional[ASTNode] = None

@dataclass
class AcronymDefinitions(ASTNode):
    """AST node for acronym definition blocks"""
    definitions: Dict[str, str]  # acronym -> full_name mapping

# === NEW: Low-Level Type Nodes ===

@dataclass
class PointerType(ASTNode):
    """Pointer type declaration"""
    pointed_type: ASTNode  # Type being pointed to
    
@dataclass
class LowLevelType(ASTNode):
    """Low-level primitive types"""
    type_name: str  # "byte", "word", "dword", "qword", "uint8", etc.
    signed: bool = False  # Whether type is signed
    size: int = 1  # Size in bytes (give it a default)  # Whether type is signed
    
# === VIRTUAL MEMORY MANAGEMENT AST NODES ===

@dataclass
class PageTableOperation(ASTNode):
    """Page table operations - core virtual memory management"""
    operation: str  # "create", "map", "unmap", "switch", "get_entry"
    page_table: Optional[ASTNode] = None  # Page table identifier
    virtual_addr: Optional[ASTNode] = None  # Virtual address
    physical_addr: Optional[ASTNode] = None  # Physical address  
    size: Optional[ASTNode] = None  # Size in bytes
    flags: Optional[ASTNode] = None  # Protection flags
    levels: Optional[ASTNode] = None  # Page table levels (4 for x86-64)
    page_size: Optional[ASTNode] = None  # Page size (4KB, 2MB, 1GB)

@dataclass
class VirtualMemoryOperation(ASTNode):
    """Virtual memory allocation and management"""
    operation: str  # "allocate", "free", "protect", "query", "commit"
    address: Optional[ASTNode] = None  # Memory address
    size: ASTNode = None  # Size to allocate/operate on
    alignment: Optional[ASTNode] = None  # Memory alignment requirement
    protection: Optional[ASTNode] = None  # Memory protection flags
    numa_node: Optional[ASTNode] = None  # NUMA node preference
    cache_policy: Optional[ASTNode] = None  # Cache behavior hint

@dataclass
class MemoryMappingOperation(ASTNode):
    """Memory-mapped I/O and device mapping"""
    operation: str  # "map", "unmap", "remap", "sync"
    device_addr: Optional[ASTNode] = None  # Physical device address
    virtual_addr: Optional[ASTNode] = None  # Virtual address to map to
    size: ASTNode = None  # Size of mapping
    access_type: Optional[ASTNode] = None  # "read", "write", "execute"
    cache_type: Optional[ASTNode] = None  # "cached", "uncached", "write_combining"
    device_type: Optional[ASTNode] = None  # Device type hint for optimization

@dataclass
class TLBOperation(ASTNode):
    """Translation Lookaside Buffer operations"""
    operation: str  # "invalidate", "flush", "flush_global", "flush_single"
    address: Optional[ASTNode] = None  # Address to invalidate (for single)
    asid: Optional[ASTNode] = None  # Address space identifier
    global_pages: Optional[ASTNode] = None  # Include global pages

@dataclass
class CacheOperation(ASTNode):
    """Cache management operations"""
    operation: str  # "flush", "invalidate", "prefetch", "flush_range"
    cache_level: Optional[ASTNode] = None  # L1, L2, L3
    cache_type: Optional[ASTNode] = None  # "data", "instruction", "unified"
    address: Optional[ASTNode] = None  # Address for range operations
    size: Optional[ASTNode] = None  # Size for range operations

@dataclass
class MemoryBarrierOperation(ASTNode):
    """Memory barrier and ordering operations"""
    barrier_type: str  # "read", "write", "full", "acquire", "release"
    scope: Optional[ASTNode] = None  # "local", "global", "device"
    ordering: Optional[ASTNode] = None  # Memory ordering semantics    