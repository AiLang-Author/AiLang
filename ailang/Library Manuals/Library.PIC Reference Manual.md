# Library.PIC Reference Manual
## Program Interface Console - Runtime Introspection System

**Version:** 1.0 (Early Development)  
**Module:** `Library.PIC.ailang`  
**Type:** Pure AILANG Implementation  
**Dependencies:** None

---

## Table of Contents

1. [Overview](#overview)
2. [Philosophy and Design](#philosophy-and-design)
3. [How PIC Works](#how-pic-works)
4. [Core Concepts](#core-concepts)
5. [API Reference](#api-reference)
6. [Usage Examples](#usage-examples)
7. [Comparison to Other Languages](#comparison-to-other-languages)
8. [Performance Considerations](#performance-considerations)
9. [Limitations and Future Work](#limitations-and-future-work)
10. [Interactive REPL](#interactive-repl)

---

## Overview

**Program Interface Console (PIC)** is AILANG's runtime introspection system. It provides the ability to:

- Register functions and types at runtime
- Look up function metadata by name
- Query type information (size, field count)
- Invoke functions dynamically (planned)
- List all registered functions and types
- Check for function existence

### What PIC Is

PIC is a **bare-bones reflection system** that gives programs the ability to:
1. Know what functions exist
2. Find functions by identifier
3. Get metadata about types
4. Potentially invoke functions dynamically

### What PIC Is NOT

PIC is **not**:
- A postmodern "everything is mutable" system
- A way to redefine language semantics at runtime
- An excuse for sloppy architecture
- A replacement for proper type systems
- Magic that makes bad code work

**Philosophy:** PIC provides *introspection* (examining structure) and *invocation* (calling functions), not *intercession* (changing language behavior). The language semantics remain fixed.

---

## Philosophy and Design

### The Problem with "Reflection"

Most languages conflate several different capabilities under the term "reflection":
1. **Introspection** - Examining program structure
2. **Invocation** - Calling functions by name
3. **Intercession** - Changing language behavior
4. **Metaprogramming** - Code generating code

This leads to languages where everything is negotiable at runtime, making it impossible to reason about what your code actually does.

### The PIC Approach

**PIC separates concerns:**

| Capability | PIC Support | Why |
|------------|------------|-----|
| **Introspection** | ✅ Full | Useful for debugging, tooling, serialization |
| **Invocation** | 🚧 Planned | Useful for plugins, dynamic loading |
| **Intercession** | ❌ No | Language semantics should be fixed |
| **Metaprogramming** | ❌ No | Use macros or code generation instead |

**Core Principle:** You can *examine* and *use* the program structure, but you cannot *change* how the language works.

### Design Goals

1. **Explicit Registration** - Functions don't magically appear in PIC, they must be registered
2. **Compile-Time Safety** - Registration happens at program startup, not arbitrarily
3. **No Magic** - Clear data structures, no hidden behavior
4. **Predictable Performance** - Simple array lookups, no dynamic dispatch overhead
5. **Optional** - Programs that don't need PIC don't pay for it

---

## How PIC Works

### Architecture

PIC maintains a **metadata array** in memory with three registries:

```
Memory Layout:
┌─────────────────────────────────────┐
│ Function Count                [0]   │
│ Type Count                    [1]   │
│ Module Count                  [2]   │
│ (Reserved)               [3-9]      │
├─────────────────────────────────────┤
│ FUNCTION TABLE          [10-2569]   │
│   Each entry: [module, func, id,    │
│                param_count]         │
├─────────────────────────────────────┤
│ TYPE TABLE            [2570-3849]   │
│   Each entry: [name, size, fields]  │
├─────────────────────────────────────┤
│ MODULE TABLE          [3850-...]    │
│   (Reserved for future use)         │
└─────────────────────────────────────┘
```

### Registration Flow

```
1. Program starts
2. PIC.Init() allocates metadata array
3. Each function/type calls PIC.RegisterFunction/RegisterType
4. Metadata stored in tables
5. Later: PIC.FindFunction() looks up by name
6. Returns function ID or address
7. Optionally: PIC.Invoke() calls function
```

### Data Structures

**Function Entry (4 integers):**
```
[0] module_name_hash   - Which module (e.g., 1=Core, 2=Math)
[1] function_name_hash - Function identifier (e.g., 5=Add)
[2] function_id        - Actual address or dispatch ID
[3] param_count        - Number of parameters
```

**Type Entry (3 integers):**
```
[0] type_name_hash     - Type identifier
[1] type_size          - Size in bytes
[2] field_count        - Number of fields
```

### Why This Works

1. **Simple Lookups** - Linear search through array (can be optimized later)
2. **Fixed Size** - Maximum 256 functions, 128 types (configurable)
3. **No Allocation** - Single array, allocated once
4. **Cache Friendly** - Contiguous memory, sequential access
5. **Debuggable** - Can dump entire registry with one function

---

## Core Concepts

### Registration

Functions and types must be **explicitly registered**:

```ailang
// Register a function
PIC.RegisterFunction(
    module_name: 1,      // Module ID
    function_name: 5,    // Function ID
    function_id: 12345,  // Address or dispatch ID
    param_count: 2       // Number of parameters
)

// Register a type
PIC.RegisterType(
    type_name: 10,       // Type ID
    type_size: 24,       // Size in bytes
    field_count: 3       // Number of fields
)
```

### Lookup

Find registered functions/types:

```ailang
// Find a function
func_id = PIC.FindFunction(module: 1, function: 5)
// Returns function_id or -1 if not found

// Check existence
exists = PIC.HasFunction(module: 1, function: 5)
// Returns 1 if exists, 0 otherwise

// Get type size
size = PIC.GetTypeSize(type: 10)
// Returns size in bytes or -1 if not found
```

### Introspection

Examine the registry:

```ailang
// List all registered functions
PIC.ListFunctions()

// List all registered types
PIC.ListTypes()

// Get count
count = PIC.GetFunctionCount()
```

### Invocation (Planned)

Dynamically call functions:

```ailang
// Call a function by name
result = PIC.Invoke(
    module: 1,
    function: 5,
    param1: 42
)
```

**Note:** Current implementation is a mock - real dispatch system coming in future versions.

---

## API Reference

### Initialization

#### PIC.Init

Initialize the PIC system.

**Signature:**
```ailang
Function.PIC.Init {
    Body: { ... }
}
```

**Example:**
```ailang
PIC.Init()
PrintMessage("PIC initialized")
```

**Important:** Must be called before any other PIC functions.

---

### Function Registration

#### PIC.RegisterFunction

Register a function in the PIC registry.

**Signature:**
```ailang
Function.PIC.RegisterFunction {
    Input: module_name: Integer
    Input: function_name: Integer
    Input: function_id: Integer
    Input: param_count: Integer
    Output: Integer  // Index in registry or -1 on error
}
```

**Parameters:**
- `module_name` - Module identifier (user-defined numbering)
- `function_name` - Function identifier (user-defined numbering)
- `function_id` - Function address or dispatch ID
- `param_count` - Number of parameters function accepts

**Returns:** Registry index (0-255) or -1 if table full

**Example:**
```ailang
// Register Add function from Math module
index = PIC.RegisterFunction(
    2,      // Module: Math
    10,     // Function: Add
    4096,   // Address: 0x1000
    2       // Parameters: 2
)

IfCondition GreaterEqual(index, 0) ThenBlock: {
    PrintMessage("Function registered at index ")
    PrintNumber(index)
}
```

---

### Function Lookup

#### PIC.FindFunction

Find a function by module and name.

**Signature:**
```ailang
Function.PIC.FindFunction {
    Input: module_name: Integer
    Input: function_name: Integer
    Output: Integer  // function_id or -1 if not found
}
```

**Example:**
```ailang
func_id = PIC.FindFunction(2, 10)  // Math.Add

IfCondition NotEqual(func_id, -1) ThenBlock: {
    PrintMessage("Found function at: ")
    PrintNumber(func_id)
}
```

**Performance:** O(n) where n = registered functions

---

#### PIC.HasFunction

Check if a function exists.

**Signature:**
```ailang
Function.PIC.HasFunction {
    Input: module_name: Integer
    Input: function_name: Integer
    Output: Integer  // 1 if exists, 0 otherwise
}
```

**Example:**
```ailang
IfCondition PIC.HasFunction(2, 10) ThenBlock: {
    PrintMessage("Math.Add is available")
} ElseBlock: {
    PrintMessage("Math.Add not registered")
}
```

---

#### PIC.GetFunctionCount

Get total number of registered functions.

**Signature:**
```ailang
Function.PIC.GetFunctionCount {
    Output: Integer
}
```

**Example:**
```ailang
count = PIC.GetFunctionCount()
PrintMessage("Total functions: ")
PrintNumber(count)
```

---

### Type Registration

#### PIC.RegisterType

Register a type in the PIC registry.

**Signature:**
```ailang
Function.PIC.RegisterType {
    Input: type_name: Integer
    Input: type_size: Integer
    Input: field_count: Integer
    Output: Integer  // Index in registry or -1 on error
}
```

**Example:**
```ailang
// Register User type
index = PIC.RegisterType(
    100,    // Type: User
    48,     // Size: 48 bytes
    6       // Fields: 6
)
```

---

#### PIC.GetTypeSize

Get the size of a registered type.

**Signature:**
```ailang
Function.PIC.GetTypeSize {
    Input: type_name: Integer
    Output: Integer  // Size in bytes or -1 if not found
}
```

**Example:**
```ailang
size = PIC.GetTypeSize(100)  // User type

IfCondition GreaterThan(size, 0) ThenBlock: {
    buffer = Allocate(size)
    // ... initialize User struct ...
}
```

---

### Introspection

#### PIC.ListFunctions

Print all registered functions to stdout.

**Signature:**
```ailang
Function.PIC.ListFunctions {
    Body: { ... }
}
```

**Output Example:**
```
=== PIC FUNCTION REGISTRY ===
Total functions: 3

  [0] Module:1 Func:5 ID:4096 Params:2
  [1] Module:2 Func:10 ID:8192 Params:2
  [2] Module:2 Func:11 ID:8208 Params:1
============================
```

---

#### PIC.ListTypes

Print all registered types to stdout.

**Signature:**
```ailang
Function.PIC.ListTypes {
    Body: { ... }
}
```

**Output Example:**
```
=== PIC TYPE REGISTRY ===
Total types: 2

  [0] Type:100 Size:48 Fields:6
  [1] Type:101 Size:32 Fields:4
========================
```

---

### Invocation (Experimental)

#### PIC.Invoke

Dynamically invoke a function.

**Signature:**
```ailang
Function.PIC.Invoke {
    Input: module_name: Integer
    Input: function_name: Integer
    Input: param1: Integer
    Output: Integer
}
```

**Status:** 🚧 **Mock implementation** - returns 0, prints debug info

**Example:**
```ailang
// Attempt to invoke Math.Add with parameter 42
result = PIC.Invoke(2, 10, 42)
// Currently prints: "[PIC] (Mock invocation - param1=42)"
```

**Future:** Will use function pointers or dispatch table for real invocation.

---

### Cleanup

#### PIC.Cleanup

Free PIC metadata and cleanup.

**Signature:**
```ailang
Function.PIC.Cleanup {
    Body: { ... }
}
```

**Example:**
```ailang
// Before program exit
PIC.Cleanup()
```

---

## Usage Examples

### Example 1: Simple Registration

```ailang
LibraryImport.PIC

SubRoutine.Main {
    // Initialize PIC
    PIC.Init()
    
    // Register some functions
    PIC.RegisterFunction(1, 1, 1000, 0)   // Core.PrintMessage
    PIC.RegisterFunction(1, 2, 1100, 1)   // Core.PrintNumber
    PIC.RegisterFunction(2, 10, 2000, 2)  // Math.Add
    PIC.RegisterFunction(2, 11, 2020, 2)  // Math.Subtract
    
    // List what we registered
    PIC.ListFunctions()
    
    // Find a specific function
    add_id = PIC.FindFunction(2, 10)
    PrintMessage("Math.Add is at: ")
    PrintNumber(add_id)
    
    // Cleanup
    PIC.Cleanup()
}

RunTask(Main)
```

### Example 2: Plugin System

```ailang
// Plugin loader that checks for required functions
Function.LoadPlugin {
    Input: plugin_name: Address
    Output: Integer
    Body: {
        PrintMessage("Loading plugin: ")
        PrintString(plugin_name)
        
        // Check for required functions
        has_init = PIC.HasFunction(100, 1)    // Plugin.Init
        has_process = PIC.HasFunction(100, 2) // Plugin.Process
        has_cleanup = PIC.HasFunction(100, 3) // Plugin.Cleanup
        
        IfCondition And(has_init, And(has_process, has_cleanup)) ThenBlock: {
            PrintMessage("✓ Plugin has all required functions")
            
            // Could invoke Plugin.Init here
            // init_result = PIC.Invoke(100, 1, 0)
            
            ReturnValue(1)
        } ElseBlock: {
            PrintMessage("✗ Plugin missing required functions")
            ReturnValue(0)
        }
    }
}
```

### Example 3: Dynamic Dispatch Table

```ailang
// Build a dispatch table from registered functions
Function.BuildDispatchTable {
    Output: Address
    Body: {
        func_count = PIC.GetFunctionCount()
        dispatch_table = ArrayCreate(func_count)
        
        PrintMessage("Building dispatch table for ")
        PrintNumber(func_count)
        PrintMessage(" functions")
        
        // In real implementation, would:
        // 1. Iterate through PIC registry
        // 2. Extract function addresses
        // 3. Build lookup table
        // 4. Return table for fast dispatch
        
        ReturnValue(dispatch_table)
    }
}
```

### Example 4: Type Information Query

```ailang
Function.AllocateType {
    Input: type_id: Integer
    Output: Address
    Body: {
        size = PIC.GetTypeSize(type_id)
        
        IfCondition LessThan(size, 0) ThenBlock: {
            PrintMessage("Error: Type not registered")
            ReturnValue(0)
        }
        
        PrintMessage("Allocating ")
        PrintNumber(size)
        PrintMessage(" bytes for type ")
        PrintNumber(type_id)
        
        buffer = Allocate(size)
        ReturnValue(buffer)
    }
}

// Usage:
user_obj = AllocateType(100)  // Auto-sizes for User type
```

### Example 5: Serialization Helper

```ailang
Function.SerializeObject {
    Input: obj: Address
    Input: type_id: Integer
    Output: Address
    Body: {
        // Get type metadata
        size = PIC.GetTypeSize(type_id)
        
        IfCondition LessThan(size, 0) ThenBlock: {
            PrintMessage("Cannot serialize: unknown type")
            ReturnValue(0)
        }
        
        // Could use field_count to iterate fields
        // For now, just serialize raw bytes
        
        buffer = Allocate(size)
        
        // Copy object bytes
        i = 0
        WhileLoop LessThan(i, size) {
            byte = GetByte(obj, i)
            SetByte(buffer, i, byte)
            i = Add(i, 1)
        }
        
        ReturnValue(buffer)
    }
}
```

---

## Comparison to Other Languages

### vs Java Reflection

**Java:**
```java
// Get class at runtime
Class<?> clazz = Class.forName("com.example.MyClass");

// Invoke method
Method method = clazz.getMethod("doSomething", int.class);
Object result = method.invoke(instance, 42);

// Modify fields
Field field = clazz.getDeclaredField("privateField");
field.setAccessible(true);
field.set(instance, newValue);
```

**PIC:**
```ailang
// Register function explicitly at startup
PIC.RegisterFunction(1, 10, 5000, 1)

// Find and invoke
func_id = PIC.FindFunction(1, 10)
result = PIC.Invoke(1, 10, 42)

// Cannot modify fields at runtime
// (Must be done through proper interfaces)
```

**Key Differences:**
- PIC requires **explicit registration** (no automatic discovery)
- PIC has **no field modification** (no breaking encapsulation)
- PIC has **no security manager** needed (less attack surface)
- PIC has **predictable performance** (no class loading overhead)

### vs Python Introspection

**Python:**
```python
# Everything is introspectable by default
import inspect

# Get function signature
sig = inspect.signature(some_function)

# Call by name
getattr(obj, "method_name")(args)

# Modify at runtime
obj.new_method = lambda self: "hello"
```

**PIC:**
```ailang
// Must register to introspect
PIC.RegisterFunction(...)

// Explicit lookup
func_id = PIC.FindFunction(module, name)

// Cannot add methods at runtime
// (Structure is fixed)
```

**Key Differences:**
- Python: Everything introspectable, everything mutable
- PIC: Only registered items introspectable, structure immutable

### vs C# Reflection

**C#:**
```csharp
// Get type
Type type = typeof(MyClass);

// Invoke method
MethodInfo method = type.GetMethod("DoSomething");
object result = method.Invoke(instance, new object[] { 42 });

// Create instance
object instance = Activator.CreateInstance(type);
```

**PIC:**
```ailang
// Register type
PIC.RegisterType(100, 48, 6)

// Get type info
size = PIC.GetTypeSize(100)

// Allocate instance
instance = Allocate(size)

// Invoke (via registration)
result = PIC.Invoke(module, func, 42)
```

**Key Differences:**
- C#: Runtime code generation (emit IL)
- PIC: No code generation (fixed functions only)

### vs Go Reflection

**Go:**
```go
// Get type
t := reflect.TypeOf(obj)

// Get value
v := reflect.ValueOf(obj)

// Call method
method := v.MethodByName("DoSomething")
results := method.Call([]reflect.Value{reflect.ValueOf(42)})
```

**PIC:**
```ailang
// Must register
PIC.RegisterFunction(...)

// Lookup and invoke
result = PIC.Invoke(module, func, 42)
```

**Key Differences:**
- Go: Reflection is slow but comprehensive
- PIC: Reflection is fast but limited
- Go: Type safety maintained
- PIC: Type safety through registration

### Comparison Table

| Feature | Java | Python | C# | Go | **PIC** |
|---------|------|--------|----|----|---------|
| Auto-discovery | ✅ | ✅ | ✅ | ✅ | ❌ (explicit) |
| Invoke by name | ✅ | ✅ | ✅ | ✅ | 🚧 (planned) |
| Modify structure | ⚠️ | ✅ | ⚠️ | ❌ | ❌ |
| Code generation | ✅ | ✅ | ✅ | ❌ | ❌ |
| Performance | Slow | Slow | Medium | Slow | **Fast** |
| Predictability | Low | Low | Medium | Medium | **High** |
| Security concerns | High | High | Medium | Low | **Low** |

---

## Performance Considerations

### Time Complexity

| Operation | Complexity | Notes |
|-----------|-----------|-------|
| `RegisterFunction` | O(1) | Array append |
| `FindFunction` | O(n) | Linear search |
| `HasFunction` | O(n) | Linear search |
| `ListFunctions` | O(n) | Iterate all |
| `GetTypeSize` | O(m) | Linear search (m=types) |

### Memory Usage

**Per Function Entry:** 4 integers = 32 bytes  
**Per Type Entry:** 3 integers = 24 bytes  
**Total Overhead:** ~10KB for full registry (256 functions + 128 types)

### Optimization Opportunities

**Current (v1.0):**
```
Linear search through array
→ O(n) lookups
```

**Future (v2.0):**
```
Hash table with chaining
→ O(1) average case lookups
```

**Future (v3.0):**
```
Perfect hash at compile time
→ O(1) guaranteed lookups
```

### When to Use PIC

✅ **Good uses:**
- Plugin systems (check for functions)
- Debugging/development tools
- Serialization/deserialization
- RPC/IPC systems
- Command dispatch tables

❌ **Bad uses:**
- Hot loops (use direct calls)
- Instead of proper architecture
- As a crutch for bad design
- Performance-critical paths

---

## Limitations and Future Work

### Current Limitations (v1.0)

1. **No Real Invocation** - `Invoke()` is mock only
2. **Integer IDs Only** - No actual string names (hash IDs instead)
3. **Linear Search** - O(n) lookups
4. **Fixed Sizes** - 256 functions, 128 types max
5. **Single Parameter** - Invoke only accepts one param
6. **No Type Information** - Can't query field names/types

### Planned Features (v2.0)

- [ ] **Real Function Invocation** - Function pointer dispatch
- [ ] **Multiple Parameters** - Variable argument invocation
- [ ] **Hash Table** - O(1) lookups
- [ ] **String Names** - Store actual function names
- [ ] **Field Metadata** - Query field names and types
- [ ] **Module System** - Proper module registration

### Future Directions (v3.0+)

- [ ] **Compiler Integration** - Auto-generate registration code
- [ ] **Type Introspection** - Full struct layout info
- [ ] **Dynamic Loading** - Load plugins at runtime
- [ ] **Interface Discovery** - Query what interfaces a type implements
- [ ] **Attribute System** - Attach metadata to functions/types

### Not Planned (Ever)

- ❌ Runtime code modification
- ❌ Changing language semantics
- ❌ "Monkey patching"
- ❌ Redefining built-in behavior
- ❌ Eval-style string execution

---

## Interactive REPL

PIC includes an interactive REPL for exploring the registry at runtime.

### Running the REPL

```bash
ailang Interactive_PIC_REPL.ailang
```

### REPL Commands

| Command | Description |
|---------|-------------|
| `h` | Show help menu |
| `r` | Register a function |
| `t` | Register a type |
| `l` | List all functions |
| `y` | List all types |
| `f` | Find function by module/name |
| `?` | Check if function exists |
| `s` | Get type size |
| `i` | Invoke a function (mock) |
| `c` | Count registered functions |
| `q` | Quit |

### Example REPL Session

```
╔═══════════════════════════════════════════╗
║  PIC INTERACTIVE REFLECTION CONSOLE        ║
║  Program Interface Console v1.0            ║
╚═══════════════════════════════════════════╝

Initializing PIC library...
[PIC] Program Interface Console initialized
✓ Ready! Type 'h' for help

> r
Register Function
  Module ID: 1
  Function ID: 10
  Address: 5000
  Param Count: 2
[PIC] Registered function: Module=1 Function=10 ID=5000 Params=2
✓ Function registered successfully

> l
=== PIC FUNCTION REGISTRY ===
Total functions: 1

  [0] Module:1 Func:10 ID:5000 Params:2
============================

> f
Find Function
  Module ID: 1
  Function ID: 10
[PIC] Found function ID: 5000
✓ Function found at address: 5000

> q
Shutting down...
Goodbye!
```

---

## Best Practices

### 1. Register at Startup

```ailang
// GOOD: Register everything at startup
SubRoutine.Main {
    PIC.Init()
    
    RegisterAllCoreFunctions()
    RegisterAllMathFunctions()
    RegisterAllStringFunctions()
    
    // Now ready to use PIC
    RunApplication()
    
    PIC.Cleanup()
}
```

### 2. Use Consistent ID Schemes

```ailang
// GOOD: Organized module IDs
FixedPool.Modules {
    "CORE": Initialize=1
    "MATH": Initialize=2
    "STRING": Initialize=3
    "IO": Initialize=4
}

// GOOD: Organized function IDs per module
FixedPool.MathFunctions {
    "ADD": Initialize=1
    "SUBTRACT": Initialize=2
    "MULTIPLY": Initialize=3
    "DIVIDE": Initialize=4
}

// Register
PIC.RegisterFunction(Modules.MATH, MathFunctions.ADD, 0x1000, 2)
```

### 3. Check Before Use

```ailang
// GOOD: Verify function exists
IfCondition PIC.HasFunction(module, func) ThenBlock: {
    result = PIC.Invoke(module, func, param)
} ElseBlock: {
    PrintMessage("Function not available")
    UseDefaultBehavior()
}
```

### 4. Don't Overuse

```ailang
// BAD: Using PIC in hot loop
WhileLoop LessThan(i, 1000000) {
    func_id = PIC.FindFunction(1, 5)  // Slow!
    result = PIC.Invoke(1, 5, i)
    i = Add(i, 1)
}

// GOOD: Look up once, cache result
func_id = PIC.FindFunction(1, 5)
WhileLoop LessThan(i, 1000000) {
    result = DirectCall(func_id, i)  // Fast!
    i = Add(i, 1)
}
```

### 5. Document Your ID Scheme

```ailang
// GOOD: Clear documentation
// Module IDs:
//   1 = Core system functions
//   2 = Math operations
//   3 = String utilities
//   100-199 = User plugins
//
// Function IDs (per module):
//   Core.1 = PrintMessage
//   Core.2 = PrintNumber
//   Math.1 = Add
//   Math.2 = Subtract
```

---

## Version History

### Version 1.0 (Current)
- Initial release
- Function registration and lookup
- Type registration and size query
- Basic introspection (list functions/types)
- Mock invocation system
- Interactive REPL

### Planned Version 2.0
- Real function invocation
- Multiple parameter support
- Hash table for O(1) lookups
- String name storage
- Field metadata

---

## See Also

- **AILANG Core Language Manual**: Language fundamentals
- **Library.XArrays Manual**: Understanding the underlying storage
- **AILANG Memory Management Guide**: How registration allocates

---

## Philosophy Notes

### Why "Program Interface Console"?

The term **"reflection"** has been corrupted by postmodern programming languages to mean "everything is negotiable at runtime." This leads to:

1. **Unpredictable behavior** - Code means different things at different times
2. **Performance problems** - Everything is slow because nothing is certain
3. **Security issues** - Reflection allows breaking all encapsulation
4. **Maintenance nightmares** - Cannot reason about what code does

**PIC takes a different approach:**

- **Interface** - You can see what's available (introspection)
- **Console** - You can interact with it (invocation)
- **Program** - The program structure, not language semantics

The name emphasizes that you're working with the *program's structure*, not redefining *language behavior*.

### The Real Purpose

PIC exists for **legitimate use cases**:
- Loading plugins dynamically
- Building debugging tools
- Implementing RPC/IPC systems
- Creating command dispatch tables
- Serialization systems

It does **not** exist for:
- Working around bad architecture
- Making slow, dynamic code
- Breaking type safety
- "Clever" meta-programming tricks

**Use PIC when you need it. Use direct calls when you don't.**

---

## Future: Runtime Code Generation (JIT)

### Not on the Roadmap (Yet)

PIC provides **introspection** and **invocation**. It does not provide **code generation** or **modification**. These are fundamentally different concerns and should remain separate.

### If We Ever Add JIT

If AILANG ever gets a JIT (Just-In-Time) compiler, it would be a **separate system** from PIC:

```ailang
// PIC: Introspection (what exists)
func_id = PIC.FindFunction(module, name)
exists = PIC.HasFunction(module, name)
result = PIC.Invoke(module, name, params)

// JIT: Code generation (hypothetical future)
jit_func = JIT.CompileFunction(bytecode_source)
JIT.OptimizeFunction(func_id, profile_data)
JIT.ReplaceFunction(old_func_id, new_jit_func)
```

### Clean Separation of Concerns

| System | Purpose | Examples |
|--------|---------|----------|
| **PIC** | Examine program structure | List functions, check existence, invoke by name |
| **JIT** | Generate optimized code | Compile hot loops, inline functions, SIMD optimization |
| ~~Reflection~~ | ❌ Don't conflate these! | Most languages mix these, causing confusion |

### Why Separate?

**Bad (most languages):**
```
"Reflection" system that does:
- Introspection (what exists)
- Invocation (call by name)
- Code generation (JIT compilation)
- Structure modification (add methods)
- Access control bypass (private field access)
- Type system bypass (cast anything)
→ Result: Unpredictable, insecure, slow
```

**Good (AILANG approach):**
```
PIC: Introspection + Invocation
  → Predictable, safe, fast

JIT (future): Code generation
  → Explicit, opt-in, bounded
  
Language Semantics: Fixed
  → Can't be modified at runtime
  → Compiler can optimize aggressively
```

### What JIT Would Enable

If properly designed, a JIT system could:

✅ **Optimize hot code paths** - Profile-guided optimization  
✅ **Generate specialized code** - Type-specific implementations  
✅ **Inline aggressively** - Eliminate call overhead  
✅ **Dynamic linking** - Load plugins with native performance  
✅ **Ahead-of-time caching** - Save compiled code between runs

### What JIT Would NOT Enable

❌ **Changing language semantics** - Still can't redefine Add()  
❌ **Runtime type modification** - Struct layouts are fixed  
❌ **Monkey patching** - Can't replace system functions  
❌ **Eval-style execution** - No string-to-code  
❌ **Bypassing safety** - Type system still enforced

### Example: Clean JIT Design

```ailang
// Step 1: Define function in AILANG IR (intermediate representation)
ir_code = [
    "function Add(a, b)",
    "  result = a + b",
    "  return result"
]

// Step 2: Compile to native code
jit_func_id = JIT.Compile(ir_code, JIT.OPT_LEVEL_2)

// Step 3: Register with PIC (they work together)
PIC.RegisterFunction(
    module: JIT_MODULE,
    function: ADD_FUNC,
    function_id: jit_func_id,  // JIT-compiled address
    param_count: 2
)

// Step 4: Use normally through PIC
result = PIC.Invoke(JIT_MODULE, ADD_FUNC, 10, 20)
```

### Design Principles for Future JIT

If we ever implement JIT, it must follow these principles:

1. **Explicit Compilation** - Code generation is explicit, not magical
2. **Bounded Semantics** - Can optimize, not redefine
3. **Verifiable Safety** - JIT output is verified before execution
4. **Predictable Performance** - Clear cost model for JIT operations
5. **Opt-In** - Programs don't pay JIT cost unless they use it
6. **Separate Namespace** - JIT functions in own module, not mixed with static

### Why We Don't Have JIT Now

Several good reasons to wait:

1. **Complexity** - JIT is a massive undertaking
2. **Maturity** - Static compilation needs to be solid first
3. **Use Cases** - Don't have clear need yet
4. **Security** - JIT increases attack surface significantly
5. **Focus** - Better to do static compilation really well

### When We Might Add JIT

Potential triggers for JIT development:

- **Performance ceiling** - Static compilation can't get fast enough
- **Plugin ecosystem** - Need dynamic loading with native speed
- **Scientific computing** - Need runtime specialization for numeric code
- **Clear architecture** - Have proven design that maintains safety

### Bottom Line

**PIC gives you what you need now** (introspection, invocation) without the complexity of JIT. If we ever add runtime code generation, it will be:

- A **separate system** (not part of PIC)
- **Explicitly invoked** (not automatic)
- **Carefully bounded** (can't break language semantics)
- **Properly integrated** (works with PIC, doesn't replace it)

This is **good engineering** - solve today's problems today, think carefully about tomorrow's problems, don't rush to add complexity before it's needed.

---

## License

Copyright (c) 2025 Sean Collins, 2 Paws Machine and Engineering  
Licensed under the Sean Collins Software License (SCSL)

---

## Contributing

PIC is in early development. To contribute:

1. Understand the philosophy (read this manual)
2. Don't propose "magic" features
3. Focus on *introspection* and *invocation*, not *intercession*
4. Keep it simple and predictable
5. Performance matters

**Current priorities:**
- Real function invocation mechanism
- Hash table optimization
- Better parameter handling
- Compiler integration for auto-registration