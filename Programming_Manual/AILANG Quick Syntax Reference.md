# AILANG Quick Reference Guide

**Version:** 2.0  
**Last Updated:** November 2025

---

## Table of Contents

1. [Basic Syntax](#basic-syntax)
2. [Variables & Types](#variables--types)
3. [Operators](#operators)
4. [Control Flow](#control-flow)
5. [Functions](#functions)
6. [Strings](#strings)
7. [Arrays](#arrays)
8. [Memory & Pools](#memory--pools)
9. [File I/O](#file-io)
10. [Common Patterns](#common-patterns)

---

## Basic Syntax

### Comments

```ailang
// Single line comment

//DOC: Documentation comment
//COM: Component comment  
//TAG: Tagged comment
```

### Output

```ailang
PrintMessage("Hello, World!")    // Print text
PrintNumber(42)                  // Print number
PrintString(variable)            // Print string variable
```

### Program Control

```ailang
HaltProgram()                    // Exit with code 0
HaltProgram("Error message")     // Exit with message
```

---

## Variables & Types

### Declaration

```ailang
x = 42                          // Integer
name = "Alice"                  // String
flag = 1                        // Boolean (1 = true, 0 = false)
ptr = Allocate(100)            // Pointer/Address
```

### Types

| Type | Description | Example |
|------|-------------|---------|
| `Integer` | 64-bit signed | `42`, `-100` |
| `Address` | Memory pointer | `0x1000` |
| `String` | Null-terminated text | `"Hello"` |

**Note:** No explicit type declarations - types inferred from usage.

---

## Operators

### Arithmetic

```ailang
Add(a, b)                       // a + b
Subtract(a, b)                  // a - b
Multiply(a, b)                  // a * b
Divide(a, b)                    // a / b
Modulo(a, b)                    // a % b
```

**Example:**
```ailang
sum = Add(10, 20)              // 30
product = Multiply(5, 6)       // 30
remainder = Modulo(10, 3)      // 1
```

### Comparison

```ailang
EqualTo(a, b)                   // a == b (returns 1 or 0)
NotEqual(a, b)                  // a != b
GreaterThan(a, b)               // a > b
LessThan(a, b)                  // a < b
GreaterEqual(a, b)              // a >= b
LessEqual(a, b)                 // a <= b
```

**Returns:** `1` for true, `0` for false

### Logical

```ailang
And(a, b)                       // a && b
Or(a, b)                        // a || b
Not(a)                          // !a
```

**Example:**
```ailang
is_valid = And(GreaterThan(x, 0), LessThan(x, 100))
// True if x is between 0 and 100
```

### Bitwise

```ailang
BitwiseAnd(a, b)                // a & b
BitwiseOr(a, b)                 // a | b
BitwiseXor(a, b)                // a ^ b
BitwiseNot(a)                   // ~a
LeftShift(a, n)                 // a << n
RightShift(a, n)                // a >> n
```

---

## Control Flow

### If-Then-Else

```ailang
IfCondition expression ThenBlock: {
    // statements
} ElseBlock: {
    // statements
}
```

**Example:**
```ailang
IfCondition GreaterThan(age, 18) ThenBlock: {
    PrintMessage("Adult")
} ElseBlock: {
    PrintMessage("Minor")
}
```

### While Loop

```ailang
WhileLoop condition {
    // statements
    BreakLoop         // Exit loop
    ContinueLoop      // Skip to next iteration
}
```

**Example:**
```ailang
i = 0
WhileLoop LessThan(i, 10) {
    PrintNumber(i)
    i = Add(i, 1)
}
```

### For Each

```ailang
ForEvery item in collection {
    // statements
}
```

### Switch/Case

```ailang
ChoosePath variable {
    CaseOption value1: {
        // statements
    }
    CaseOption value2: {
        // statements
    }
    DefaultOption: {
        // statements
    }
}
```

**Example:**
```ailang
ChoosePath command {
    CaseOption "start": {
        StartService()
    }
    CaseOption "stop": {
        StopService()
    }
    DefaultOption: {
        PrintMessage("Unknown command")
    }
}
```

### Branch (Modern Switch)

```ailang
Branch variable {
    Case value1: {
        // statements
    }
    Case value2: {
        // statements
    }
    Default: {
        // statements
    }
}
```

---

## Functions

### Define Function

```ailang
Function.ModuleName.FunctionName {
    Input: param1: Type
    Input: param2: Type
    Output: ReturnType
    Body: {
        // statements
        ReturnValue(result)
    }
}
```

**Example:**
```ailang
Function.Math.Add {
    Input: a: Integer
    Input: b: Integer
    Output: Integer
    Body: {
        result = Add(a, b)
        ReturnValue(result)
    }
}
```

### Call Function

```ailang
result = ModuleName.FunctionName(arg1, arg2)
```

**Example:**
```ailang
sum = Math.Add(10, 20)
PrintNumber(sum)  // 30
```

### Define SubRoutine (No Return Value)

```ailang
SubRoutine.ModuleName.Name {
    Body: {
        // statements
    }
}
```

**Example:**
```ailang
SubRoutine.Utils.PrintHeader {
    Body: {
        PrintMessage("==================")
        PrintMessage("  My Application  ")
        PrintMessage("==================")
    }
}
```

### Call SubRoutine

```ailang
RunTask(Name)
```

**Example:**
```ailang
RunTask(Utils.PrintHeader)
```

---

## Strings

### Core Operations

```ailang
StringLength(str)                    // Get length
StringConcat(str1, str2)             // Concatenate
StringEquals(str1, str2)             // Compare (1/0)
StringCompare(str1, str2)            // Compare (0 if equal)
```

### Manipulation

```ailang
StringSubstring(str, start, end)     // Extract substring
StringCharAt(str, index)             // Get character at index
StringIndexOf(str, substr)           // Find position of substring
StringTrim(str)                      // Remove whitespace
StringReplace(str, old, new)         // Replace first occurrence
StringSplit(str, delimiter)          // Split into array
```

### Case Conversion

```ailang
StringToUpper(str)                   // Convert to uppercase
StringToLower(str)                   // Convert to lowercase
```

### Conversions

```ailang
NumberToString(num)                  // 42 → "42"
StringToNumber(str)                  // "42" → 42
StringFromChar(ascii_code)           // 65 → "A"
```

### Example

```ailang
name = "Alice"
greeting = StringConcat("Hello, ", name)
greeting = StringConcat(greeting, "!")
PrintString(greeting)  // "Hello, Alice!"

upper = StringToUpper(name)  // "ALICE"
```

---

## Arrays

### Core Operations

```ailang
ArrayCreate(size)                    // Create fixed-size array
ArraySet(arr, index, value)          // Set element
ArrayGet(arr, index)                 // Get element
ArrayLength(arr)                     // Get capacity
ArrayDestroy(arr)                    // Free memory
```

### Example

```ailang
// Create array of 5 elements
scores = ArrayCreate(5)

// Set values
ArraySet(scores, 0, 95)
ArraySet(scores, 1, 87)
ArraySet(scores, 2, 92)

// Get value
first = ArrayGet(scores, 0)  // 95

// Get size
size = ArrayLength(scores)   // 5

// Cleanup
ArrayDestroy(scores)
```

### Iteration

```ailang
i = 0
size = ArrayLength(arr)
WhileLoop LessThan(i, size) {
    value = ArrayGet(arr, i)
    PrintNumber(value)
    i = Add(i, 1)
}
```

---

## Memory & Pools

### Fixed Pools

**Declaration:**
```ailang
FixedPool.PoolName {
    "variable": Initialize=value, CanChange=True
    "constant": Initialize=value, CanChange=False
}
```

**Access:**
```ailang
value = PoolName.variable
PoolName.variable = newValue
```

**Example:**
```ailang
FixedPool.Config {
    "MAX_USERS": Initialize=100, CanChange=False
    "current_users": Initialize=0, CanChange=True
}

// Use pool variables
IfCondition LessThan(Config.current_users, Config.MAX_USERS) ThenBlock: {
    Config.current_users = Add(Config.current_users, 1)
    PrintMessage("User added")
}
```

### Manual Memory

```ailang
Allocate(size)                       // Allocate bytes
Deallocate(ptr, size)                // Free memory
StoreValue(addr, value)              // Write to memory
Dereference(addr)                    // Read from memory
GetByte(addr, offset)                // Read byte
SetByte(addr, offset, value)         // Write byte
```

---

## File I/O

### Basic Operations

```ailang
WriteTextFile(filename, content)     // Write text file
ReadTextFile(filename)               // Read text file
FileExists(filename)                 // Check existence (1/0)
```

**Example:**
```ailang
// Write file
WriteTextFile("config.txt", "port=8080")

// Check existence
IfCondition FileExists("config.txt") ThenBlock: {
    content = ReadTextFile("config.txt")
    PrintString(content)
}
```

### Advanced Operations

```ailang
OpenFile(filename, mode)             // Open file
CloseFile(handle)                    // Close file
ReadLine(handle)                     // Read line
WriteLine(handle, text)              // Write line
```

---

## Common Patterns

### Counter Loop

```ailang
i = 0
WhileLoop LessThan(i, 10) {
    PrintNumber(i)
    i = Add(i, 1)
}
```

### String Building

```ailang
result = ""
result = StringConcat(result, "Hello")
result = StringConcat(result, ", ")
result = StringConcat(result, "World")
result = StringConcat(result, "!")
```

### Null/Zero Check

```ailang
IfCondition EqualTo(value, 0) ThenBlock: {
    PrintMessage("Value is null or zero")
}
```

### Range Check

```ailang
is_in_range = And(GreaterEqual(x, 0), LessThan(x, 100))
IfCondition is_in_range ThenBlock: {
    PrintMessage("Valid range")
}
```

### Array Sum

```ailang
Function.ArraySum {
    Input: arr: Address
    Output: Integer
    Body: {
        sum = 0
        i = 0
        size = ArrayLength(arr)
        
        WhileLoop LessThan(i, size) {
            sum = Add(sum, ArrayGet(arr, i))
            i = Add(i, 1)
        }
        
        ReturnValue(sum)
    }
}
```

### Error Handling

```ailang
TryBlock: {
    // Code that might fail
    result = DivideNumbers(x, y)
}
CatchError.DivisionByZero {
    PrintMessage("Cannot divide by zero")
    result = 0
}
FinallyBlock: {
    PrintMessage("Cleanup complete")
}
```

---

## Compilation & Execution

### Basic Compilation

```bash
# Compile AILANG program
python3 main.py program.ailang

# Run compiled program
./program_exec
```

### Debug Mode

```bash
# Compile with debug symbols
python3 main.py -D program.ailang

# Compile with debug level 4
python3 main.py -D4 program.ailang
```

### Profiling

```bash
# Compile with profiling enabled
python3 main.py -P program.ailang
```

### Debug Features (Require -D flag)

```ailang
DebugAssert(condition, "message")    // Runtime assertion
DebugTrace.Entry("label", value)     // Trace function entry
DebugTrace.Exit("label", value)      // Trace function exit
DebugPerf.Start("label")             // Start timer
DebugPerf.End("label")               // End timer
```

---

## Library Imports

```ailang
LibraryImport.XArrays                // Dynamic arrays
LibraryImport.HashMap                // Hash tables
LibraryImport.StringUtils            // String utilities
LibraryImport.PIC                    // Program Interface Console
```

**Example:**
```ailang
LibraryImport.XArrays

arr = XArray.XCreate(10)
XArray.XPush(arr, 42)
value = XArray.XGet(arr, 0)
XArray.XDestroy(arr)
```

---

## Key Differences from Other Languages

| Feature | Other Languages | AILANG |
|---------|----------------|--------|
| Operators | `a + b` | `Add(a, b)` |
| If statement | `if (x > 0)` | `IfCondition GreaterThan(x, 0) ThenBlock:` |
| While loop | `while (i < 10)` | `WhileLoop LessThan(i, 10)` |
| Function call | `obj.method()` | `Module.Function()` |
| Array access | `arr[i]` | `ArrayGet(arr, i)` |
| Comments | `/* ... */` | `// ...` |

---

## Quick Tips

1. **All operators are named functions** - No symbolic operators (`+`, `-`, `*`, etc.)
2. **Explicit control flow** - Use `ThenBlock:`, `ElseBlock:`, not `{` alone
3. **No implicit conversions** - Use `NumberToString()` and `StringToNumber()`
4. **Arrays are fixed-size** - Use `Library.XArrays` for dynamic arrays
5. **Memory must be freed** - Use `ArrayDestroy()`, `Deallocate()`, etc.
6. **Booleans are integers** - `1` = true, `0` = false
7. **String literals are immutable** - Use `StringConcat()` to build strings

---

## Common Mistakes

❌ **Using symbolic operators:**
```ailang
x = a + b  // WRONG
```
✅ **Use named functions:**
```ailang
x = Add(a, b)  // CORRECT
```

❌ **Forgetting ThenBlock:**
```ailang
IfCondition x > 10 {  // WRONG
```
✅ **Use proper syntax:**
```ailang
IfCondition GreaterThan(x, 10) ThenBlock: {  // CORRECT
```

❌ **Using array[index]:**
```ailang
value = arr[0]  // WRONG
```
✅ **Use ArrayGet:**
```ailang
value = ArrayGet(arr, 0)  // CORRECT
```

---

## Need More Help?

- ** Language Manuals **:Thurough syntax reference
- **Library Manuals**: XArrays, HashMap, StringUtils, PIC
- **GitHub Repository**: Examples and tutorials
- BNF Grammar and Design Specifications are on github

---

**Remember:** AILANG uses explicit, named syntax throughout. This makes code more verbose but significantly more readable and searchable.
