# Library.StringUtils Reference Manual

**Version:** 1.0  
**Module:** `Library.StringUtils.ailang`  
**Type:** Pure AILANG Implementation  
**Dependencies:** `XArray` (for `StrSplit`)

---

## Table of Contents

1. [Overview](#overview)
2. [Installation](#installation)
3. [Function Reference](#function-reference)
4. [Memory Management](#memory-management)
5. [Usage Examples](#usage-examples)
6. [Performance Notes](#performance-notes)
7. [Comparison with Core String Functions](#comparison-with-core-string-functions)

---

## Overview

`Library.StringUtils` provides pure AILANG implementations of common string manipulation functions. Unlike the core string primitives (which are implemented in assembly), these functions are written entirely in AILANG, making them:

- **Portable**: Work on any AILANG implementation
- **Transparent**: Source code available for inspection and modification
- **Educational**: Demonstrate AILANG programming patterns
- **Customizable**: Easy to extend or modify for specific needs

### When to Use This Library

Use `Library.StringUtils` when:
- You need string operations not provided by core primitives
- You want portable, pure AILANG implementations
- You're learning AILANG and want readable examples
- You need to customize string behavior

Use core primitives (`StringLength`, `StringConcat`, etc.) when:
- Performance is critical
- You need the most optimized implementations
- You're doing high-frequency operations

---

## Installation

Import the library at the start of your AILANG program:

```ailang
Import("Library.StringUtils")

// Now you can use all StringUtils functions
length = StrLen("Hello")
```

---

## Function Reference

### StrLen

Get the length of a null-terminated string.

**Signature:**
```ailang
Function.StrLen {
    Input: str: Address
    Output: Integer
}
```

**Parameters:**
- `str`: Pointer to null-terminated string

**Returns:** Number of characters (excluding null terminator)

**Example:**
```ailang
text = "Hello, World!"
length = StrLen(text)  // Returns 13

empty = ""
length2 = StrLen(empty)  // Returns 0
```

**Equivalent Core Function:** `StringLength(str)`

---

### StrToNum

Convert a string to an integer.

**Signature:**
```ailang
Function.StrToNum {
    Input: str: Address
    Output: Integer
}
```

**Parameters:**
- `str`: Pointer to string containing digits

**Returns:** Integer value parsed from string

**Behavior:**
- Parses decimal digits only
- Stops at first non-digit character
- Non-digit characters are silently ignored
- Does **not** handle negative numbers with this implementation

**Example:**
```ailang
num1 = StrToNum("12345")      // Returns 12345
num2 = StrToNum("42")          // Returns 42
num3 = StrToNum("100 dollars") // Returns 100 (stops at space)
num4 = StrToNum("abc")         // Returns 0
```

**Equivalent Core Function:** `StringToNumber(str)`

---

### NumToStr

Convert an integer to a string.

**Signature:**
```ailang
Function.NumToStr {
    Input: num: Integer
    Output: Address
}
```

**Parameters:**
- `num`: Integer value to convert

**Returns:** Pointer to newly allocated null-terminated string

**Memory:** ⚠️ **Caller must deallocate:** `Deallocate(result, Add(StringLength(result), 1))`

**Behavior:**
- Handles positive and negative numbers
- Handles zero
- Always null-terminates the result

**Example:**
```ailang
str1 = NumToStr(12345)    // Returns "12345"
str2 = NumToStr(-42)      // Returns "-42"
str3 = NumToStr(0)        // Returns "0"

// Don't forget to free!
Deallocate(str1, Add(StringLength(str1), 1))
Deallocate(str2, Add(StringLength(str2), 1))
Deallocate(str3, Add(StringLength(str3), 1))
```

**Equivalent Core Function:** `NumberToString(num)`

---

### StrFindChar

Find the first occurrence of a character in a string.

**Signature:**
```ailang
Function.StrFindChar {
    Input: str: Address
    Input: target_char: Integer
    Output: Integer
}
```

**Parameters:**
- `str`: Pointer to string to search
- `target_char`: ASCII code of character to find

**Returns:** 
- Position of first occurrence (0-indexed)
- `-1` if character not found

**Example:**
```ailang
text = "Hello, World!"

// Find comma (ASCII 44)
pos1 = StrFindChar(text, 44)     // Returns 5

// Find 'W' (ASCII 87)
pos2 = StrFindChar(text, 87)     // Returns 7

// Find 'Z' (not present)
pos3 = StrFindChar(text, 90)     // Returns -1

// Common pattern: find delimiter
email = "user@example.com"
at_pos = StrFindChar(email, 64)  // Find '@' (ASCII 64)
```

**Note:** For finding substrings (not just single characters), use core `StringIndexOf()`.

---

### StrCompare

Compare two strings lexicographically.

**Signature:**
```ailang
Function.StrCompare {
    Input: str1: Address
    Input: str2: Address
    Output: Integer
}
```

**Parameters:**
- `str1`: First string
- `str2`: Second string

**Returns:**
- `0` if strings are equal
- `1` if strings are different

**Behavior:**
- Case-sensitive comparison
- Compares byte-by-byte
- Returns on first difference

**Example:**
```ailang
result1 = StrCompare("Hello", "Hello")   // Returns 0 (equal)
result2 = StrCompare("Hello", "hello")   // Returns 1 (different case)
result3 = StrCompare("abc", "abd")       // Returns 1 (different)

// Use in conditionals
str1 = "admin"
str2 = GetUsername()
IfCondition EqualTo(StrCompare(str1, str2), 0) ThenBlock: {
    PrintMessage("Access granted")
}
```

**Equivalent Core Function:** `StringEquals(str1, str2)` (but inverted logic: returns 1 for equal)

---

### StrStartsWith

Check if a string starts with a given prefix.

**Signature:**
```ailang
Function.StrStartsWith {
    Input: str: Address
    Input: prefix: Address
    Output: Integer
}
```

**Parameters:**
- `str`: String to check
- `prefix`: Prefix to look for

**Returns:**
- `1` if string starts with prefix
- `0` otherwise

**Example:**
```ailang
url = "https://example.com"

is_https = StrStartsWith(url, "https://")  // Returns 1
is_http = StrStartsWith(url, "http://")    // Returns 0
is_ftp = StrStartsWith(url, "ftp://")      // Returns 0

// Use for routing
IfCondition StrStartsWith(command, "GET ") ThenBlock: {
    HandleGetRequest(command)
}
```

**Note:** No equivalent core function - this is unique to StringUtils.

---

### StrSubstring

Extract a substring from a string.

**Signature:**
```ailang
Function.StrSubstring {
    Input: str: Address
    Input: start: Integer
    Input: length: Integer
    Output: Address
}
```

**Parameters:**
- `str`: Source string
- `start`: Starting position (0-indexed)
- `length`: Number of characters to extract

**Returns:** Pointer to newly allocated substring

**Memory:** ⚠️ **Caller must deallocate:** `Deallocate(result, Add(StringLength(result), 1))`

**Behavior:**
- If source ends before `length` characters, stops at end
- Always null-terminates result
- Allocates exactly `length + 1` bytes

**Example:**
```ailang
text = "Hello, World!"

// Extract "Hello"
sub1 = StrSubstring(text, 0, 5)      // Returns "Hello"

// Extract "World"
sub2 = StrSubstring(text, 7, 5)      // Returns "World"

// Extract to end (might be shorter)
sub3 = StrSubstring(text, 7, 100)    // Returns "World!" (stops at null)

// Clean up
Deallocate(sub1, 6)
Deallocate(sub2, 6)
Deallocate(sub3, Add(StringLength(sub3), 1))
```

**Comparison with Core:**
- Core `StringSubstring(str, start, end)` uses **end position**
- This function uses **length** (more intuitive for some use cases)

---

### StrSplit

Split a string by a delimiter character.

**Signature:**
```ailang
Function.StrSplit {
    Input: str: Address
    Input: delimiter: Address
    Output: Address
}
```

**Parameters:**
- `str`: String to split
- `delimiter`: Single-character delimiter (only first char is used)

**Returns:** XArray containing pointers to substring parts

**Memory:** ⚠️ **Complex cleanup required:**
1. Free each substring: `Deallocate(part, Add(StringLength(part), 1))`
2. Destroy array: `XArray.XDestroy(result)`

**Behavior:**
- Splits on single-character delimiter only
- Empty segments are skipped
- Returns XArray (dynamic array) of string pointers

**Example:**
```ailang
// Parse CSV
csv = "apple,banana,cherry,date"
parts = StrSplit(csv, ",")

count = XArray.XSize(parts)  // Returns 4

// Access parts
fruit1 = XArray.XGet(parts, 0)  // "apple"
fruit2 = XArray.XGet(parts, 1)  // "banana"
fruit3 = XArray.XGet(parts, 2)  // "cherry"
fruit4 = XArray.XGet(parts, 3)  // "date"

// CRITICAL: Clean up properly!
i = 0
WhileLoop LessThan(i, count) {
    part = XArray.XGet(parts, i)
    Deallocate(part, Add(StringLength(part), 1))
    i = Add(i, 1)
}
XArray.XDestroy(parts)
```

**Example: Parse Path**
```ailang
path = "/usr/local/bin/program"
parts = StrSplit(path, "/")

// parts contains: ["usr", "local", "bin", "program"]
// Note: Empty strings between delimiters are skipped

// Process and clean up
count = XArray.XSize(parts)
i = 0
WhileLoop LessThan(i, count) {
    segment = XArray.XGet(parts, i)
    PrintString(segment)
    Deallocate(segment, Add(StringLength(segment), 1))
    i = Add(i, 1)
}
XArray.XDestroy(parts)
```

**Equivalent Core Function:** `StringSplit(str, delimiter)` (handles multi-char delimiters better)

---

## Memory Management

### Critical Rules

Most StringUtils functions allocate memory that **you must free**:

| Function | Allocates? | How to Free |
|----------|-----------|-------------|
| `StrLen` | ❌ No | N/A |
| `StrToNum` | ❌ No | N/A |
| `NumToStr` | ✅ Yes | `Deallocate(result, Add(StringLength(result), 1))` |
| `StrFindChar` | ❌ No | N/A |
| `StrCompare` | ❌ No | N/A |
| `StrStartsWith` | ❌ No | N/A |
| `StrSubstring` | ✅ Yes | `Deallocate(result, Add(StringLength(result), 1))` |
| `StrSplit` | ✅ Yes (multiple) | Free each part, then destroy array |

### Memory Leak Prevention Pattern

```ailang
// Good: Store and clean up
result = NumToStr(42)
PrintString(result)
Deallocate(result, Add(StringLength(result), 1))

// Bad: Memory leak!
PrintString(NumToStr(42))  // No way to free this!
```

### StrSplit Cleanup Pattern

```ailang
parts = StrSplit(text, ",")
count = XArray.XSize(parts)

// Use the parts
i = 0
WhileLoop LessThan(i, count) {
    part = XArray.XGet(parts, i)
    ProcessPart(part)
    i = Add(i, 1)
}

// Clean up: MUST free in reverse order!
// 1. Free each string
i = 0
WhileLoop LessThan(i, count) {
    part = XArray.XGet(parts, i)
    Deallocate(part, Add(StringLength(part), 1))
    i = Add(i, 1)
}

// 2. Destroy the array
XArray.XDestroy(parts)
```

---

## Usage Examples

### Example 1: Parse Query String

```ailang
Function.ParseQueryString {
    Input: query: Address
    Output: Address  // Returns hash table
    Body: {
        result = XSHash.XCreate()
        
        // Split by '&'
        pairs = StrSplit(query, "&")
        pair_count = XArray.XSize(pairs)
        
        i = 0
        WhileLoop LessThan(i, pair_count) {
            pair = XArray.XGet(pairs, i)
            
            // Find '=' position
            eq_pos = StrFindChar(pair, 61)  // '=' = 61
            
            IfCondition GreaterThan(eq_pos, 0) ThenBlock: {
                // Extract key and value
                key = StrSubstring(pair, 0, eq_pos)
                
                pair_len = StrLen(pair)
                value_start = Add(eq_pos, 1)
                value_len = Subtract(pair_len, value_start)
                value = StrSubstring(pair, value_start, value_len)
                
                // Store in hash
                XSHash.XInsert(result, key, value)
                
                // Free temporaries
                Deallocate(key, Add(StrLen(key), 1))
                Deallocate(value, Add(StrLen(value), 1))
            }
            
            // Free the pair
            Deallocate(pair, Add(StrLen(pair), 1))
            i = Add(i, 1)
        }
        
        XArray.XDestroy(pairs)
        ReturnValue(result)
    }
}

// Usage:
query = "name=John&age=30&city=NYC"
params = ParseQueryString(query)

name = XSHash.XLookup(params, "name")   // "John"
age_str = XSHash.XLookup(params, "age") // "30"
age = StrToNum(age_str)                 // 30
```

### Example 2: Word Counter

```ailang
Function.CountWords {
    Input: text: Address
    Output: Integer
    Body: {
        words = StrSplit(text, " ")
        count = XArray.XSize(words)
        
        // Clean up
        i = 0
        WhileLoop LessThan(i, count) {
            word = XArray.XGet(words, i)
            Deallocate(word, Add(StrLen(word), 1))
            i = Add(i, 1)
        }
        XArray.XDestroy(words)
        
        ReturnValue(count)
    }
}

// Usage:
sentence = "The quick brown fox jumps"
word_count = CountWords(sentence)  // Returns 5
```

### Example 3: Path Manipulation

```ailang
Function.GetFilename {
    Input: path: Address
    Output: Address
    Body: {
        // Find last '/'
        path_len = StrLen(path)
        last_slash = -1
        
        i = 0
        WhileLoop LessThan(i, path_len) {
            char = StringCharAt(path, i)
            IfCondition EqualTo(char, 47) ThenBlock: {  // '/' = 47
                last_slash = i
            }
            i = Add(i, 1)
        }
        
        // Extract filename
        IfCondition GreaterThan(last_slash, -1) ThenBlock: {
            start = Add(last_slash, 1)
            remaining = Subtract(path_len, start)
            filename = StrSubstring(path, start, remaining)
            ReturnValue(filename)
        } ElseBlock: {
            // No slash, entire path is filename
            filename = StrSubstring(path, 0, path_len)
            ReturnValue(filename)
        }
    }
}

// Usage:
path1 = "/usr/local/bin/program"
name1 = GetFilename(path1)  // "program"
Deallocate(name1, Add(StrLen(name1), 1))

path2 = "document.txt"
name2 = GetFilename(path2)  // "document.txt"
Deallocate(name2, Add(StrLen(name2), 1))
```

### Example 4: Simple CSV Parser

```ailang
Function.ParseCSVLine {
    Input: line: Address
    Output: Address  // XArray of fields
    Body: {
        fields = StrSplit(line, ",")
        count = XArray.XSize(fields)
        
        // Trim each field
        i = 0
        WhileLoop LessThan(i, count) {
            field = XArray.XGet(fields, i)
            trimmed = StringTrim(field)  // Using core function
            
            // Replace old with trimmed
            XArray.XSet(fields, i, trimmed)
            
            // Free original untrimmed field
            Deallocate(field, Add(StrLen(field), 1))
            
            i = Add(i, 1)
        }
        
        ReturnValue(fields)
    }
}

// Usage:
csv_row = "John Doe, 30, Engineer"
fields = ParseCSVLine(csv_row)

name = XArray.XGet(fields, 0)     // "John Doe"
age_str = XArray.XGet(fields, 1)  // "30"
title = XArray.XGet(fields, 2)    // "Engineer"

// Process...
age = StrToNum(age_str)

// Clean up
count = XArray.XSize(fields)
i = 0
WhileLoop LessThan(i, count) {
    field = XArray.XGet(fields, i)
    Deallocate(field, Add(StrLen(field), 1))
    i = Add(i, 1)
}
XArray.XDestroy(fields)
```

---

## Performance Notes

### Benchmarks vs Core Functions

| Operation | StringUtils | Core | Notes |
|-----------|-------------|------|-------|
| `StrLen` vs `StringLength` | ~3x slower | Baseline | Core uses optimized assembly |
| `StrToNum` vs `StringToNumber` | ~2x slower | Baseline | Core handles more formats |
| `NumToStr` vs `NumberToString` | ~2x slower | Baseline | Core is more efficient |
| `StrCompare` vs `StringEquals` | ~3x slower | Baseline | Core uses fast SIMD |
| `StrSplit` vs `StringSplit` | Similar | Similar | Both allocate similarly |

### Optimization Tips

1. **Cache string lengths:**
   ```ailang
   // Bad: Multiple calls
   WhileLoop LessThan(i, StrLen(text)) {
       // StrLen called every iteration!
   }
   
   // Good: Cache it
   text_len = StrLen(text)
   WhileLoop LessThan(i, text_len) {
       // Much faster!
   }
   ```

2. **Reuse buffers when possible:**
   ```ailang
   // Allocate once, reuse
   buffer = Allocate(1024)
   // ... do work ...
   Deallocate(buffer, 1024)
   ```

3. **Prefer core functions for hot paths:**
   ```ailang
   // Critical performance section - use core
   length = StringLength(text)  // Not StrLen
   ```

---

## Comparison with Core String Functions

### Function Mapping

| StringUtils | Core Equivalent | Key Differences |
|-------------|----------------|-----------------|
| `StrLen` | `StringLength` | Same behavior, slower |
| `StrToNum` | `StringToNumber` | Same behavior, slower |
| `NumToStr` | `NumberToString` | Same behavior, slower |
| `StrCompare` | `StringEquals` | **Inverted logic!** (0=equal vs 1=equal) |
| `StrStartsWith` | *(none)* | Unique to StringUtils |
| `StrSubstring` | `StringSubstring` | **Different params!** (length vs end) |
| `StrSplit` | `StringSplit` | Similar, single-char delimiter only |
| `StrFindChar` | *(none)* | Use `StringIndexOf` for substrings |

### When to Mix Both

You can use both libraries together:

```ailang
Import("Library.StringUtils")

// Use StringUtils for what it's good at
IfCondition StrStartsWith(url, "https://") ThenBlock: {
    // Use core for performance-critical parts
    domain_start = StringIndexOf(url, "://")
    length = StringLength(url)
    domain = StringSubstring(url, Add(domain_start, 3), length)
    
    // Use StringUtils for convenience
    parts = StrSplit(domain, ".")
    // ... process ...
}
```

---

## Version History

### Version 1.0 (Current)
- Initial release
- 8 core functions
- Pure AILANG implementation
- XArray integration for `StrSplit`

---

## Contributing

This library is part of the AILANG standard library. To contribute:

1. Maintain pure AILANG implementation (no assembly)
2. Follow memory management conventions
3. Document all allocations clearly
4. Add usage examples for new functions
5. Update this manual with changes

---

## License

Copyright (c) 2025 Sean Collins, 2 Paws Machine and Engineering  
Licensed under the Sean Collins Software License (SCSL)

---

## See Also

- **Core String Manual**: For assembly-optimized primitives
- **Library.XArrays Manual**: For dynamic array operations
- **Library.XSHash Manual**: For hash table operations
- **AILANG Memory Management Guide**: For Allocate/Deallocate patterns