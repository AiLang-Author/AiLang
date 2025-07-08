# AILANG Syntax Reference Guide
**The World's First Cache-Aware, Systems Programming Language**

## Table of Contents
1. [Program Structure](#program-structure)
2. [Pool System](#pool-system)
3. [Data Types](#data-types)
4. [Control Flow](#control-flow)
5. [Functions and Subroutines](#functions-and-subroutines)
6. [Expressions and Operators](#expressions-and-operators)
7. [String Operations](#string-operations)
8. [File I/O Operations](#file-io-operations)
9. [Systems Programming](#systems-programming)
10. [Virtual Memory Operations](#virtual-memory-operations)
11. [Security and Access Control](#security-and-access-control)
12. [Macros and Metaprogramming](#macros-and-metaprogramming)
13. [Error Handling](#error-handling)
14. [Comments and Documentation](#comments-and-documentation)

---

## Program Structure

### Basic Program
```ailang
// Simple program structure
PrintMessage("Hello, AILANG!")
```

### Program with Declarations
```ailang
// Complete program with declarations
FixedPool Configuration {
    "app_name": Initialize-"My AILANG App"
    "version": Initialize-"1.0"
}

PrintMessage(Configuration.app_name)
PrintMessage(Configuration.version)
```

---

## Pool System

### Pool Types and Declarations

#### Fixed Pool (Static Memory)
```ailang
FixedPool GameData {
    "player_health": Initialize-100, Range-[0, 100], CanChange-True
    "max_level": Initialize-10, CanChange-False
    "difficulty": Initialize-"normal", Range-["easy", "normal", "hard"]
}
```

#### Dynamic Pool (Runtime Memory)
```ailang
DynamicPool UserSessions {
    "active_connections": Initialize-0, CanChange-True
    "session_data": ElementType-Text, CanBeNull-True
}
```

#### Neural Pool (AI/ML Optimized)
```ailang
NeuralPool AIModel {
    "weights": ElementType-FloatingPoint, MaximumLength-1000000
    "learning_rate": Initialize-0.001, Range-[0.0001, 0.1]
    "epochs": Initialize-100, CanChange-True
}
```

#### Kernel Pool (Privileged Memory)
```ailang
KernelPool SystemResources {
    "interrupt_table": Initialize-0x1000, CanChange-False
    "page_directory": Initialize-0x2000, CanChange-True
}
```

### SubPools
```ailang
FixedPool GameState {
    SubPool Player {
        "name": Initialize-"Anonymous", MaximumLength-50
        "score": Initialize-0, CanChange-True
        "level": Initialize-1, Range-[1, 100]
    }
    
    SubPool Environment {
        "map_name": Initialize-"Level_1"
        "weather": Initialize-"sunny"
        "time_of_day": Initialize-"morning"
    }
}
```

### Pool Access
```ailang
// Direct access
player_score = GameState.Player.score
environment_weather = GameState.Environment.weather

// Assignment
GameState.Player.score = Add(GameState.Player.score, 100)
```

---

## Data Types

### Basic Types
```ailang
// Integer types
age = 25                    // Integer (64-bit)
small_num = UInt8(255)      // 8-bit unsigned
big_num = Int64(-9223372036854775808)

// Floating point
price = 19.99               // FloatingPoint (64-bit)
scientific = 2.5e10         // Scientific notation

// Text and Boolean
name = "Alice"              // Text (UTF-8 string)
is_active = True            // Boolean
is_empty = False

// Low-level types
byte_val = Byte(0xFF)       // 8-bit unsigned
word_val = Word(0xFFFF)     // 16-bit unsigned
dword_val = DWord(0xFFFFFFFF) // 32-bit unsigned
qword_val = QWord(0xFFFFFFFFFFFFFFFF) // 64-bit unsigned
```

### Collection Types
```ailang
// Arrays
numbers = [1, 2, 3, 4, 5]
names = ["Alice", "Bob", "Charlie"]
mixed = [1, "hello", True]

// Maps
person = {
    "name": "Alice",
    "age": 30,
    "city": "New York"
}

// Tuples
coordinates = (10, 20, 30)
person_info = ("Alice", 30, True)
```

### Type Expressions in Function Signatures
```ailang
Function TypeExamples {
    Input: (
        simple_int: Integer,
        array_of_text: Array[Text],
        map_data: Map[Text, Integer],
        optional_value: OptionalType[Integer],
        pointer_to_int: Pointer[Integer]
    )
    Output: Boolean
    Body: {
        // Function implementation
        ReturnValue(True)
    }
}
```

---

## Control Flow

### If-Then-Else Statements
```ailang
// Simple if statement
IfCondition GreaterThan(score, 100) ThenBlock {
    PrintMessage("High score!")
}

// If-else statement
IfCondition EqualTo(user_input, "quit") ThenBlock {
    PrintMessage("Goodbye!")
    HaltProgram()
} ElseBlock {
    PrintMessage("Continuing...")
}

// Nested conditions
IfCondition LessThan(health, 20) ThenBlock {
    IfCondition EqualTo(difficulty, "easy") ThenBlock {
        health = Add(health, 10)
        PrintMessage("Health restored!")
    } ElseBlock {
        PrintMessage("Critical health!")
    }
}
```

### While Loops
```ailang
// Basic while loop
counter = 0
WhileLoop LessThan(counter, 10) {
    PrintMessage(NumberToString(counter))
    counter = Add(counter, 1)
}

// Condition with function call
WhileLoop Not(FileExists("config.txt")) {
    PrintMessage("Waiting for config file...")
    // Wait logic here
}
```

### For Each Loops
```ailang
// Iterate over array
fruits = ["apple", "banana", "cherry"]
ForEvery fruit in fruits {
    PrintMessage(StringConcat("I like ", fruit))
}

// Iterate over map keys
scores = {"Alice": 100, "Bob": 85, "Charlie": 92}
ForEvery player in MapKeys(scores) {
    score = MapGet(scores, player)
    PrintMessage(StringConcat(player, ": ", NumberToString(score)))
}
```

### Choose Path (Switch Statement)
```ailang
user_choice = ReadInput("Choose option (1-3): ")

ChoosePath(user_choice) {
    CaseOption "1": PrintMessage("You chose option 1")
    CaseOption "2": PrintMessage("You chose option 2")
    CaseOption "3": PrintMessage("You chose option 3")
    DefaultOption: PrintMessage("Invalid choice")
}
```

### Loop Control
```ailang
// Break and continue
counter = 0
WhileLoop True {
    counter = Add(counter, 1)
    
    IfCondition EqualTo(Modulo(counter, 2), 0) ThenBlock {
        ContinueLoop  // Skip even numbers
    }
    
    IfCondition GreaterThan(counter, 10) ThenBlock {
        BreakLoop     // Exit loop
    }
    
    PrintMessage(NumberToString(counter))
}
```

---

## Functions and Subroutines

### Function Declaration
```ailang
Function.MathUtils.CalculateArea {
    Input: (width: FloatingPoint, height: FloatingPoint)
    Output: FloatingPoint
    Body: {
        area = Multiply(width, height)
        ReturnValue(area)
    }
}
```

### Function with Multiple Parameters
```ailang
Function.StringUtils.FormatMessage {
    Input: (
        template: Text,
        name: Text,
        score: Integer,
        level: Integer
    )
    Output: Text
    Body: {
        result = StringReplace(template, "{name}", name)
        result = StringReplace(result, "{score}", NumberToString(score))
        result = StringReplace(result, "{level}", NumberToString(level))
        ReturnValue(result)
    }
}
```

### Subroutine Declaration
```ailang
SubRoutine.Utilities.PrintHeader {
    PrintMessage("=" * 50)
    PrintMessage("    AILANG Application")
    PrintMessage("    Version 2.0")
    PrintMessage("=" * 50)
}
```

### Function Calls
```ailang
// Simple function call
area = MathUtils.CalculateArea(10.0, 5.0)

// Function call with named parameters  
message = StringUtils.FormatMessage(
    template-"Player {name} scored {score} points on level {level}!",
    name-"Alice",
    score-1500,
    level-3
)

// Subroutine call
Utilities.PrintHeader()
```

### Lambda Functions
```ailang
// Simple lambda
square = Lambda(x) { Multiply(x, x) }
result = Apply(square, 5)  // Result: 25

// Lambda with multiple parameters
add_and_multiply = Lambda(a, b, factor) {
    sum = Add(a, b)
    Multiply(sum, factor)
}
result = Apply(add_and_multiply, 3, 7, 2)  // Result: 20

// Higher-order functions
numbers = [1, 2, 3, 4, 5]
squared_numbers = Map(numbers, Lambda(x) { Multiply(x, x) })
```

### Combinators
```ailang
// Define combinators
Combinator.Compose = Lambda(f, g) { 
    Lambda(x) { Apply(f, Apply(g, x)) } 
}

Combinator.Curry = Lambda(f) {
    Lambda(a) { Lambda(b) { Apply(f, a, b) } }
}

// Use combinators
increment = Lambda(x) { Add(x, 1) }
double = Lambda(x) { Multiply(x, 2) }
increment_then_double = Apply(Compose, double, increment)

result = Apply(increment_then_double, 5)  // Result: 12
```

---

## Expressions and Operators

### Arithmetic Operators
```ailang
// Basic arithmetic
sum = Add(10, 5)           // 15
difference = Subtract(10, 5)  // 5
product = Multiply(10, 5)     // 50
quotient = Divide(10, 5)      // 2
remainder = Modulo(10, 3)     // 1
power = Power(2, 8)           // 256

// Mathematical functions
sqrt_val = SquareRoot(16)     // 4
abs_val = AbsoluteValue(-5)   // 5
```

### Comparison Operators
```ailang
// Comparison operations
is_greater = GreaterThan(10, 5)     // True
is_less = LessThan(10, 5)           // False
is_equal = EqualTo(10, 10)          // True
is_not_equal = NotEqual(10, 5)      // True
is_gte = GreaterEqual(10, 10)       // True
is_lte = LessEqual(5, 10)           // True
```

### Logical Operators
```ailang
// Logical operations
logical_and = And(True, False)      // False
logical_or = Or(True, False)        // True
logical_not = Not(True)             // False
logical_xor = Xor(True, False)      // True
logical_implies = Implies(True, False)  // False
```

### Bitwise Operators
```ailang
// Bitwise operations (for integer types)
bitwise_and = BitwiseAnd(12, 10)    // 8 (1100 & 1010 = 1000)
bitwise_or = BitwiseOr(12, 10)      // 14 (1100 | 1010 = 1110)
bitwise_xor = BitwiseXor(12, 10)    // 6 (1100 ^ 1010 = 0110)
bitwise_not = BitwiseNot(12)        // -13 (~1100 = ...11110011)
left_shift = LeftShift(5, 2)        // 20 (101 << 2 = 10100)
right_shift = RightShift(20, 2)     // 5 (10100 >> 2 = 101)
```

### Complex Expressions
```ailang
// Nested expressions
result = Add(
    Multiply(3, 4),
    Divide(20, 5)
)  // Result: 16

// Parenthesized infix notation
result = (3 Multiply 4) Add (20 Divide 5)  // Result: 16

// Mixed operations
is_valid = And(
    GreaterThan(age, 18),
    LessThan(age, 65)
)

// Function calls in expressions
formatted = StringConcat(
    "Result: ",
    NumberToString(Add(x, y)),
    " (calculated at ",
    GetCurrentTime(),
    ")"
)
```

---

## String Operations

### String Input Functions
```ailang
// Interactive input
name = ReadInput("Enter your name: ")
age = ReadInputNumber("Enter your age: ")
choice = GetUserChoice("Choose (y/n): ")
key = ReadKey("Press any key to continue...")
```

### String Comparison
```ailang
// String comparison
are_equal = StringEquals("hello", "hello")        // True
contains = StringContains("hello world", "world") // True
starts = StringStartsWith("hello", "hell")        // True
ends = StringEndsWith("hello", "llo")             // True
comparison = StringCompare("apple", "banana")     // -1
```

### String Manipulation
```ailang
// String manipulation
greeting = StringConcat("Hello", ", ", "World", "!")  // "Hello, World!"
length = StringLength("Hello")                        // 5
substring = StringSubstring("Hello World", 6, 5)      // "World"
uppercase = StringToUpper("hello")                    // "HELLO"
lowercase = StringToLower("HELLO")                    // "hello"
trimmed = StringTrim("  hello  ")                     // "hello"
replaced = StringReplace("hello world", "world", "AILANG")  // "hello AILANG"
```

### String Conversion
```ailang
// Type conversions
str_from_num = NumberToString(42)         // "42"
num_from_str = StringToNumber("123")      // 123
str_identity = StringToString("hello")    // "hello"

// Advanced formatting
formatted = StringConcat(
    "Player ",
    player_name,
    " has ",
    NumberToString(points),
    " points"
)
```

### String Processing Example
```ailang
// Complete string processing example
SubRoutine.ProcessUserInput {
    user_input = ReadInput("Enter a message: ")
    
    // Validate input
    IfCondition EqualTo(StringLength(user_input), 0) ThenBlock {
        PrintMessage("Empty input!")
        ReturnValue()
    }
    
    // Process input
    cleaned = StringTrim(user_input)
    uppercase_version = StringToUpper(cleaned)
    
    // Output results
    PrintMessage(StringConcat("Original: ", user_input))
    PrintMessage(StringConcat("Cleaned: ", cleaned))
    PrintMessage(StringConcat("Uppercase: ", uppercase_version))
    PrintMessage(StringConcat("Length: ", NumberToString(StringLength(cleaned))))
}
```

---

## File I/O Operations

### Basic File Operations
```ailang
// Simple file operations
WriteTextFile("output.txt", "Hello, File System!")
content = ReadTextFile("input.txt")
file_exists = FileExists("data.txt")

PrintMessage(content)
PrintMessage(NumberToString(file_exists))
```

### Advanced File Operations
```ailang
// File handle operations
file_handle = OpenFile("data.bin", "readwrite")

// Read and write operations
data = ReadFile(file_handle, 1024)  // Read 1KB
WriteFile(file_handle, "New data")

// File positioning
SeekPosition(file_handle, 100)      // Seek to position 100
current_pos = GetPosition(file_handle)

// Cleanup
CloseFile(file_handle)
```

### File Management
```ailang
// File and directory management
CreateFile("newfile.txt")
DeleteFile("oldfile.txt")
CopyFile("source.txt", "backup.txt")
MoveFile("temp.txt", "archive/temp.txt")
RenameFile("old_name.txt", "new_name.txt")

// Directory operations
CreateDirectory("new_folder")
dir_exists = DirectoryExists("my_folder")
files = ListDirectory(".")

// File information
size = GetFileSize("document.pdf")
date = GetFileDate("script.sh")
permissions = GetFilePermissions("executable")
```

### File Processing Example
```ailang
Function.FileProcessor.ProcessLogFile {
    Input: (filename: Text)
    Output: Integer
    Body: {
        // Check if file exists
        IfCondition Not(FileExists(filename)) ThenBlock {
            PrintMessage("File not found!")
            ReturnValue(-1)
        }
        
        // Read and process file
        content = ReadTextFile(filename)
        lines = StringSplit(content, "\n")
        line_count = ArrayLength(lines)
        
        // Create processed output
        header = StringConcat("Processed ", NumberToString(line_count), " lines\n")
        processed_content = StringConcat(header, StringToUpper(content))
        
        // Write output
        output_filename = StringConcat("processed_", filename)
        WriteTextFile(output_filename, processed_content)
        
        PrintMessage(StringConcat("Processed file saved as: ", output_filename))
        ReturnValue(line_count)
    }
}
```

---

## Systems Programming

### Memory Operations
```ailang
// Pointer operations
my_variable = 42
ptr = AddressOf(my_variable)        // Get address
value = Dereference(ptr)            // Get value (42)
byte_value = Dereference(ptr, "byte") // Read as byte

// Size operations
int_size = SizeOf(Integer)          // 8 bytes
var_size = SizeOf(my_variable)      // Size of variable
```

### Memory Management
```ailang
// Memory allocation
buffer = Allocate(1024)             // Allocate 1KB
aligned_buffer = Allocate(1024, 16) // 16-byte aligned

// Memory operations
MemoryCopy(dest_ptr, src_ptr, 1024) // Copy 1KB
MemorySet(buffer, 0, 1024)          // Zero 1KB
result = MemoryCompare(ptr1, ptr2, 100) // Compare 100 bytes

// Cleanup
Deallocate(buffer)
```

### Hardware Access
```ailang
// Port I/O
status = PortRead(0x80, "byte")     // Read byte from port 0x80
PortWrite(0x3F8, 0x41, "byte")      // Write 'A' to serial port

// Register access (kernel mode)
cr3_value = HardwareRegister("CR3", "read")
HardwareRegister("CR0", "write", new_cr0_value)

// Memory-mapped I/O
device_status = MMIORead(0xFEE00000, "dword")
MMIOWrite(0xFEE00000, 0x12345678, "dword")
```

### Atomic Operations
```ailang
// Atomic memory operations
old_value = AtomicRead(shared_counter)
AtomicWrite(shared_counter, new_value)
AtomicAdd(shared_counter, 1)
success = AtomicCompareSwap(shared_counter, expected, new_value)
```

### Interrupt Control
```ailang
// Interrupt management
DisableInterrupts()                 // CLI
// Critical section
EnableInterrupts()                  // STI

// Software interrupts
TriggerSoftwareInterrupt(0x80)      // Linux system call
```

### Inline Assembly
```ailang
// Direct assembly code
InlineAssembly("
    mov rax, 1
    mov rdi, 1
    mov rsi, message
    mov rdx, message_len
    syscall
", 
    inputs-[],
    outputs-[],
    clobbers-["rax", "rdi", "rsi", "rdx"]
)
```

---

## Virtual Memory Operations

### Page Table Operations
```ailang
// Create and manage page tables
page_table = PageTable.Create(levels-4, page_size-"4KB")

// Map virtual to physical memory
PageTable.Map(
    page_table-page_table,
    virtual_addr-0x40000000,
    physical_addr-0x1000000,
    flags-"RW"
)

// Switch page table (changes CR3)
PageTable.Switch(page_table-page_table)

// Unmap memory
PageTable.Unmap(page_table-page_table, virtual_addr-0x40000000)
```

### Virtual Memory Allocation
```ailang
// Allocate virtual memory
virtual_addr = VirtualMemory.Allocate(
    size-65536,
    protection-"RW",
    alignment-"4KB"
)

// Change memory protection
VirtualMemory.Protect(
    address-virtual_addr,
    size-4096,
    protection-"RO"
)

// Free virtual memory
VirtualMemory.Free(address-virtual_addr)
```

### Cache Management
```ailang
// Cache operations
Cache.Flush(level-"L1", address-0x40000000, size-4096)
Cache.Invalidate()                  // Invalidate all caches
Cache.Prefetch(address-0x40000000)  // Prefetch cache line

// Specific cache levels
Cache.Flush(level-"L2")             // Flush L2 cache
Cache.Flush(level-"L3")             // Flush L3 cache
```

### TLB Operations
```ailang
// TLB management
TLB.FlushAll()                      // Flush entire TLB
TLB.Flush(address-0x40000000)       // Flush specific page
TLB.Invalidate(address-0x40000000)  // Invalidate TLB entry
```

### Memory Barriers
```ailang
// Memory ordering
MemoryBarrier.Full()                // Full memory barrier (MFENCE)
MemoryBarrier.Read()                // Load barrier (LFENCE)
MemoryBarrier.Write()               // Store barrier (SFENCE)
```

### Complete VM Example
```ailang
Function.VMManager.SetupVirtualSpace {
    Input: (size: Integer)
    Output: Address
    Body: {
        // Create new page table
        pt = PageTable.Create(levels-4, page_size-"4KB")
        
        // Allocate virtual memory
        vaddr = VirtualMemory.Allocate(
            size-size,
            protection-"RW",
            alignment-"4KB"
        )
        
        // Map to physical memory
        PageTable.Map(
            page_table-pt,
            virtual_addr-vaddr,
            physical_addr-0x1000000,
            flags-"RW"
        )
        
        // Optimize for cache locality
        Cache.Prefetch(address-vaddr)
        
        // Ensure memory visibility
        MemoryBarrier.Full()
        
        ReturnValue(vaddr)
    }
}
```

---

## Security and Access Control

### Security Context Declaration
```ailang
SecurityContext.ApplicationSecurity {
    Level.Public = {
        AllowedOperations: ["Read", "Print"],
        DeniedOperations: ["Write", "Delete", "Execute"],
        MemoryLimit: 1 Megabytes,
        CPUQuota: 10 Percent
    }
    
    Level.Restricted = {
        AllowedOperations: ["Read", "Write"],
        DeniedOperations: ["Delete", "Execute", "Network"],
        MemoryLimit: 10 Megabytes,
        CPUQuota: 25 Percent
    }
    
    Level.Privileged = {
        AllowedOperations: ["Read", "Write", "Delete", "Execute"],
        DeniedOperations: [],
        MemoryLimit: 100 Megabytes,
        CPUQuota: 50 Percent
    }
}
```

### Security Pool
```ailang
SecurityPool.UserData {
    "personal_info": Initialize-"encrypted", CanChange-False
    "session_token": Initialize-Null, CanBeNull-True
    "access_level": Initialize-"public"
}
```

### Secured Execution
```ailang
// Execute with security restrictions
WithSecurity(context-"ApplicationSecurity.Restricted") {
    user_input = ReadInput("Enter data: ")
    
    // This block runs with restricted permissions
    // Cannot delete files or execute programs
    WriteTextFile("user_data.txt", user_input)
    
    // File operations are allowed
    content = ReadTextFile("config.txt")
    PrintMessage(content)
}
```

---

## Macros and Metaprogramming

### Macro Block Declaration
```ailang
MacroBlock.Utilities {
    Macro.Repeat(count, action) = {
        counter = 0
        WhileLoop LessThan(counter, count) {
            ExpandMacro(action)
            counter = Add(counter, 1)
        }
    }
    
    Macro.IfDebug(code) = {
        IfCondition EqualTo(DEBUG_MODE, True) ThenBlock {
            ExpandMacro(code)
        }
    }
    
    Macro.Benchmark(name, code) = {
        start_time = GetCurrentTime()
        ExpandMacro(code)
        end_time = GetCurrentTime()
        duration = Subtract(end_time, start_time)
        PrintMessage(StringConcat("Benchmark ", name, ": ", NumberToString(duration), "ms"))
    }
}
```

### Macro Usage
```ailang
// Use macros
RunMacro.Utilities.Repeat(5, {
    PrintMessage("Hello from macro!")
})

RunMacro.Utilities.IfDebug({
    PrintMessage("Debug: Processing user input")
})

RunMacro.Utilities.Benchmark("SortOperation", {
    sorted_array = QuickSort(input_array)
})
```

### Code Generation Macros
```ailang
MacroBlock.CodeGeneration {
    Macro.GenerateAccessor(type, name) = {
        Function.Get{name} {
            Output: type
            Body: {
                ReturnValue(private_{name})
            }
        }
        
        Function.Set{name} {
            Input: (value: type)
            Body: {
                private_{name} = value
            }
        }
    }
}

// Generate getter/setter methods
RunMacro.CodeGeneration.GenerateAccessor(Integer, "Count")
// Creates GetCount() and SetCount() functions
```

---

## Error Handling

### Try-Catch-Finally
```ailang
// Basic error handling
TryBlock: {
    result = Divide(10, 0)
    PrintMessage(NumberToString(result))
}
CatchError.DivisionByZero {
    PrintMessage("Error: Cannot divide by zero!")
    result = 0
}
```

### Multiple Exception Types
```ailang
// Handle multiple exception types
TryBlock: {
    content = ReadTextFile("config.txt")
    value = StringToNumber(content)
    result = Divide(100, value)
}
CatchError.FileNotFound {
    PrintMessage("Configuration file not found!")
    result = -1
}
CatchError.InvalidInput {
    PrintMessage("Invalid number in configuration!")
    result = -2
}
CatchError.DivisionByZero {
    PrintMessage("Configuration value cannot be zero!")
    result = -3
}
FinallyBlock: {
    PrintMessage("Configuration processing completed")
}
```

### Error Propagation
```ailang
Function.SafeFileProcessor {
    Input: (filename: Text)
    Output: OptionalType[Text]
    Body: {
        TryBlock: {
            IfCondition Not(FileExists(filename)) ThenBlock {
                ReturnValue(Null)
            }
            
            content = ReadTextFile(filename)
            processed = StringToUpper(content)
            ReturnValue(processed)
        }
        CatchError.FileNotFound {
            PrintMessage(StringConcat("File error: ", filename))
            ReturnValue(Null)
        }
    }
}
```

---

## Comments and Documentation

### Comment Types
```ailang
// Single line comment
x = 42  // Inline comment

//DOC: This is a documentation comment
//     It can span multiple lines
//     Used for API documentation //

//COM: Complex comment block
//     For detailed explanations
//     And multi-paragraph documentation //

//TAG: Special tagged comment for tools
```

### Documentation Examples
```ailang
//DOC: Calculate the factorial of a number
//     @param n: The number to calculate factorial for
//     @returns: The factorial of n
//     @throws: InvalidInput if n is negative //
Function.MathUtils.Factorial {
    Input: (n: Integer)
    Output: Integer
    Body: {
        // Validate input
        IfCondition LessThan(n, 0) ThenBlock {
            // Throw error for negative numbers
            ReturnValue(-1)  // Error code
        }
        
        // Base cases
        IfCondition Or(EqualTo(n, 0), EqualTo(n, 1)) ThenBlock {
            ReturnValue(1)
        }
        
        // Recursive calculation
        result = 1
        counter = 2
        WhileLoop LessEqual(counter, n) {
            result = Multiply(result, counter)
            counter = Add(counter, 1)
        }
        
        ReturnValue(result)
    }
}
```

---

## Acronym Definitions

### Acronym Declaration
```ailang
// Define acronyms for cleaner code
AcronymDefinitions {
    GS = "GameState",
    UI = "UserInterface",
    CFG = "Configuration",
    NET = "NetworkManager",
    DB = "Database"
}
```

### Using Acronyms
```ailang
// Use acronyms in pool access
FixedPool GameState {
    "player_health": Initialize-100
    "player_score": Initialize-0
}

FixedPool Configuration {
    "difficulty": Initialize-"normal"
    "sound_enabled": Initialize-True
}

// Access using acronyms
IfCondition LessEqual(GS.player_health, 20) ThenBlock {
    PrintMessage("Low health warning!")
    
    IfCondition StringEquals(CFG.difficulty, "easy") ThenBlock {
        GS.player_health = Add(GS.player_health, 10)
        PrintMessage("Health restored (easy mode)")
    }
}
```

---

## Complete Program Examples

### Simple Calculator
```ailang
// Complete calculator program
FixedPool Calculator {
    "operation": Initialize-"", CanChange-True
    "num1": Initialize-0.0, CanChange-True
    "num2": Initialize-0.0, CanChange-True
    "result": Initialize-0.0, CanChange-True
}

SubRoutine.Calculator.GetInput {
    Calculator.num1 = ReadInputNumber("Enter first number: ")
    Calculator.operation = ReadInput("Enter operation (+, -, *, /): ")
    Calculator.num2 = ReadInputNumber("Enter second number: ")
}

SubRoutine.Calculator.Calculate {
    ChoosePath(Calculator.operation) {
        CaseOption "+": {
            Calculator.result = Add(Calculator.num1, Calculator.num2)
        }
        CaseOption "-": {
            Calculator.result = Subtract(Calculator.num1, Calculator.num2)
        }
        CaseOption "*": {
            Calculator.result = Multiply(Calculator.num1, Calculator.num2)
        }
        CaseOption "/": {
            IfCondition EqualTo(Calculator.num2, 0) ThenBlock {
                PrintMessage("Error: Division by zero!")
                ReturnValue()
            }
            Calculator.result = Divide(Calculator.num1, Calculator.num2)
        }
        DefaultOption: {
            PrintMessage("Error: Invalid operation!")
            ReturnValue()
        }
    }
    
    PrintMessage(StringConcat("Result: ", NumberToString(Calculator.result)))
}

// Main program
Calculator.GetInput()
Calculator.Calculate()
```

### File Processing System
```ailang
// File processing with error handling
AcronymDefinitions {
    FP = "FileProcessor"
}

FixedPool FileProcessor {
    "input_dir": Initialize-"input/", CanChange-False
    "output_dir": Initialize-"output/", CanChange-False
    "processed_count": Initialize-0, CanChange-True
}

Function.FileProcessor.ProcessDirectory {
    Input: (directory: Text)
    Output: Integer
    Body: {
        processed = 0
        
        TryBlock: {
            IfCondition Not(DirectoryExists(directory)) ThenBlock {
                PrintMessage("Directory not found!")
                ReturnValue(0)
            }
            
            files = ListDirectory(directory)
            
            ForEvery filename in files {
                IfCondition StringEndsWith(filename, ".txt") ThenBlock {
                    full_path = StringConcat(directory, filename)
                    
                    TryBlock: {
                        content = ReadTextFile(full_path)
                        processed_content = StringToUpper(content)
                        
                        output_path = StringConcat(FP.output_dir, "processed_", filename)
                        WriteTextFile(output_path, processed_content)
                        
                        processed = Add(processed, 1)
                        PrintMessage(StringConcat("Processed: ", filename))
                    }
                    CatchError.FileNotFound {
                        PrintMessage(StringConcat("Could not read: ", filename))
                    }
                }
            }
        }
        CatchError.InvalidInput {
            PrintMessage("Directory processing error!")
        }
        FinallyBlock: {
            PrintMessage(StringConcat("Processing complete. Files processed: ", NumberToString(processed)))
        }
        
        ReturnValue(processed)
    }
}

// Main execution
CreateDirectory(FP.output_dir)
FP.processed_count = FileProcessor.ProcessDirectory(FP.input_dir)
```

This syntax reference covers all major AILANG language constructs with practical examples. Each section demonstrates the proper syntax and usage patterns for building real-world AILANG applications.