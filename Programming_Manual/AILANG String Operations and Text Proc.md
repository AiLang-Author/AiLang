# AILANG String Operations and Text Processing Manual

## Table of Contents
1. [Overview](#overview)
2. [Basic String Operations](#basic-string-operations)
3. [String Analysis Functions](#string-analysis-functions)
4. [String Manipulation Functions](#string-manipulation-functions)
5. [String Conversion Operations](#string-conversion-operations)
6. [Buffer and Memory Integration](#buffer-and-memory-integration)
7. [Advanced Text Processing](#advanced-text-processing)
8. [Performance Optimization](#performance-optimization)
9. [String Pooling and Memory Management](#string-pooling-and-memory-management)
10. [Real-World Examples and Patterns](#real-world-examples-and-patterns)

## Overview

AILANG provides a comprehensive string processing system designed for systems programming, text manipulation, and data processing tasks. The language treats strings as null-terminated byte sequences with full Unicode support and efficient memory management.

### String Representation
- **Null-Terminated**: All strings end with a null byte (0x00)
- **UTF-8 Compatible**: Support for Unicode characters via UTF-8 encoding
- **Memory Managed**: Automatic allocation and cleanup for string operations
- **Buffer Integration**: Seamless interaction with raw memory buffers

### Core Design Principles
- **Performance**: Optimized implementations for common operations
- **Safety**: Memory-safe string handling with bounds checking
- **Flexibility**: Both high-level functions and low-level buffer access
- **Interoperability**: Easy conversion between strings, numbers, and raw data

## Basic String Operations

### String Creation

**String Literals:**
```ailang
// Basic string literals
greeting = "Hello, World!"
empty_string = ""
multichar = "ABC123!@#"

// Strings with special characters
newline_string = "Line 1\nLine 2"
tab_string = "Column 1\tColumn 2"
quote_string = "She said \"Hello\""
```

**Character to String Conversion:**
```ailang
result = CharToString(ascii_code)
```

**Examples:**
```ailang
// Convert ASCII codes to strings
letter_a = CharToString(65)        // Returns "A"
letter_z = CharToString(90)        // Returns "Z"
space = CharToString(32)           // Returns " "
newline = CharToString(10)         // Returns "\n"
```

### String Length

**Function Syntax:**
```ailang
length = StringLength(string)
```

**Examples:**
```ailang
// Basic length operations
hello_len = StringLength("Hello")         // Returns 5
empty_len = StringLength("")              // Returns 0
long_len = StringLength("Very long string with many characters")

// Variable string length
message = "Testing"
msg_len = StringLength(message)           // Returns 7

// Length in loops and conditions
text = "Process this text"
IfCondition GreaterThan(StringLength(text), 10) ThenBlock {
    PrintMessage("Long text detected")
}
```

### String Concatenation

**Function Syntax:**
```ailang
result = StringConcat(string1, string2)
```

**Examples:**
```ailang
// Basic concatenation
greeting = StringConcat("Hello", "World")     // Returns "HelloWorld"
spaced = StringConcat("Hello ", "World")      // Returns "Hello World"

// Multiple concatenations
full_name = StringConcat(first_name, StringConcat(" ", last_name))

// Building strings incrementally
message = "Status: "
message = StringConcat(message, "Processing")
message = StringConcat(message, "...")         // "Status: Processing..."

// Empty string concatenation
result1 = StringConcat("", "Test")            // Returns "Test"
result2 = StringConcat("Test", "")            // Returns "Test"
```

**Optimized Pool Concatenation:**
```ailang
// For high-performance scenarios
result = StringConcatPooled(string1, string2)
```

### String Comparison

**Equality Testing:**
```ailang
result = StringEquals(string1, string2)       // Returns 1 if equal, 0 if not
```

**Examples:**
```ailang
// Basic equality
is_same = StringEquals("Hello", "Hello")      // Returns 1
is_diff = StringEquals("Hello", "hello")      // Returns 0 (case sensitive)

// Variable comparison
username = "admin"
is_admin = StringEquals(username, "admin")    // Returns 1

// Comparison in conditions
password = GetPassword()
IfCondition StringEquals(password, "secret123") ThenBlock {
    PrintMessage("Access granted")
} ElseBlock {
    PrintMessage("Access denied")
}

// Empty string comparison
is_empty = StringEquals(input, "")            // Check if string is empty
```

**Advanced Comparison:**
```ailang
result = StringCompare(string1, string2)      // Returns -1, 0, or 1
result = StringContains(haystack, needle)     // Returns 1 if contains
result = StringStartsWith(string, prefix)     // Returns 1 if starts with
result = StringEndsWith(string, suffix)       // Returns 1 if ends with
```

## String Analysis Functions

### Character Access

**Character at Position:**
```ailang
char_code = StringCharAt(string, index)
```

**Examples:**
```ailang
// Access individual characters
text = "Hello"
first_char = StringCharAt(text, 0)            // Returns 72 ('H')
last_char = StringCharAt(text, 4)             // Returns 111 ('o')

// Character analysis in loops
message = "Test123"
i = 0
digit_count = 0
WhileLoop LessThan(i, StringLength(message)) {
    char_code = StringCharAt(message, i)
    IfCondition And(GreaterEqual(char_code, 48), LessEqual(char_code, 57)) ThenBlock {
        digit_count = Add(digit_count, 1)     // Count digits
    }
    i = Add(i, 1)
}
```

### String Search Functions

**Find Substring:**
```ailang
position = StringIndexOf(string, substring)
```

**Examples:**
```ailang
// Find substring position
text = "Hello World Programming"
world_pos = StringIndexOf(text, "World")      // Returns 6
missing_pos = StringIndexOf(text, "Missing")  // Returns -1

// Search-based logic
email = "user@domain.com"
at_pos = StringIndexOf(email, "@")
IfCondition GreaterThan(at_pos, 0) ThenBlock {
    PrintMessage("Valid email format")
}

// Find and extract domain
domain_start = Add(at_pos, 1)
domain = StringSubstring(email, domain_start, StringLength(email))
```

**Contains Check:**
```ailang
has_substring = StringContains(string, substring)
```

**Examples:**
```ailang
// Check for keywords
message = "Error: File not found"
is_error = StringContains(message, "Error")   // Returns 1
is_warning = StringContains(message, "Warning") // Returns 0

// Content filtering
text = "This contains profanity"
needs_filter = StringContains(text, "profanity")
```

## String Manipulation Functions

### Substring Extraction

**Extract by Position and Length:**
```ailang
result = StringSubstring(string, start_index, end_index)
result = StringExtract(buffer, offset, length)
```

**Examples:**
```ailang
// Basic substring extraction
text = "Programming Language"
first_word = StringSubstring(text, 0, 11)     // Returns "Programming"
second_word = StringSubstring(text, 12, 20)   // Returns "Language"

// Extract file extension
filename = "document.pdf"
dot_pos = StringIndexOf(filename, ".")
extension = StringSubstring(filename, Add(dot_pos, 1), StringLength(filename))

// Buffer extraction (for raw data processing)
buffer = CreateTextBuffer()
header = StringExtract(buffer, 0, 16)         // Extract first 16 bytes as string
```

**Extract Until Delimiter:**
```ailang
result = StringExtractUntil(buffer, start_offset, delimiter)
```

**Examples:**
```ailang
// Parse line-by-line
text_buffer = LoadTextFile("data.txt")
line = StringExtractUntil(text_buffer, 0, "\n")

// Parse CSV fields  
csv_line = "name,age,city"
field = StringExtractUntil(csv_line, 0, ",")   // Returns "name"

// Protocol parsing
http_header = "GET /index.html HTTP/1.1\r\n"
method = StringExtractUntil(http_header, 0, " ")  // Returns "GET"
```

### Case Conversion

**Upper and Lower Case:**
```ailang
result = StringToUpper(string)
result = StringToLower(string)
```

**Examples:**
```ailang
// Case normalization
username = "JohnDoe"
normalized = StringToLower(username)          // Returns "johndoe"

// Case-insensitive comparison helper
Function.CaseInsensitiveEquals {
    Input: str1: String, str2: String
    Output: Integer
    Body: {
        upper1 = StringToUpper(str1)
        upper2 = StringToUpper(str2)
        ReturnValue(StringEquals(upper1, upper2))
    }
}

// Text formatting
title = "programming guide"
formatted_title = StringToUpper(title)        // Returns "PROGRAMMING GUIDE"

// Command processing
user_command = "QUIT"
command_lower = StringToLower(user_command)   // Returns "quit"
```

### String Trimming

**Remove Whitespace:**
```ailang
result = StringTrim(string)
```

**Examples:**
```ailang
// Clean user input
user_input = "  Hello World  "
cleaned = StringTrim(user_input)              // Returns "Hello World"

// Process configuration values
config_value = "\tSomeValue\n"
clean_value = StringTrim(config_value)        // Returns "SomeValue"

// Data cleanup
data_field = "   123.45   "
clean_number = StringTrim(data_field)         // Returns "123.45"
```

### String Replacement

**Replace Substring:**
```ailang
result = StringReplace(string, old_substring, new_substring)
```

**Examples:**
```ailang
// Simple replacement
text = "Hello World"
modified = StringReplace(text, "World", "AILANG")  // Returns "Hello AILANG"

// Path normalization
windows_path = "C:\\Users\\Name"
unix_path = StringReplace(windows_path, "\\", "/")  // Returns "C:/Users/Name"

// Template processing
template = "Welcome {USER} to {SYSTEM}"
personalized = StringReplace(template, "{USER}", username)
personalized = StringReplace(personalized, "{SYSTEM}", "AILANG")

// Data sanitization
unsafe_data = "<script>alert('xss')</script>"
safe_data = StringReplace(unsafe_data, "<script>", "&lt;script&gt;")
```

### String Splitting

**Split by Delimiter:**
```ailang
array = StringSplit(string, delimiter)
```

**Examples:**
```ailang
// Parse comma-separated values
csv_row = "apple,banana,cherry"
fruits = StringSplit(csv_row, ",")
// fruits[0] = "apple", fruits[1] = "banana", fruits[2] = "cherry"

// Parse command line arguments
command_line = "program --input file.txt --output result.txt"
args = StringSplit(command_line, " ")

// Split file path
file_path = "/usr/local/bin/program"
path_parts = StringSplit(file_path, "/")
// path_parts = ["", "usr", "local", "bin", "program"]

// Parse key-value pairs
config_line = "setting=value"
pair = StringSplit(config_line, "=")
key = pair[0]     // "setting"
value = pair[1]   // "value"
```

## String Conversion Operations

### Number to String Conversion

**Function Syntax:**
```ailang
string_result = NumberToString(number)
```

**Examples:**
```ailang
// Basic number conversion
age_str = NumberToString(25)                  // Returns "25"
negative_str = NumberToString(-42)            // Returns "-42"
zero_str = NumberToString(0)                  // Returns "0"

// Large number conversion
big_number = 1000000
big_str = NumberToString(big_number)          // Returns "1000000"

// Dynamic string building
counter = 1
message = StringConcat("Item ", NumberToString(counter))  // "Item 1"

// Formatting output
Function.FormatNumber {
    Input: num: Integer, label: String
    Output: String
    Body: {
        num_str = NumberToString(num)
        result = StringConcat(label, ": ")
        result = StringConcat(result, num_str)
        ReturnValue(result)
    }
}

price_text = FormatNumber(299, "Price")       // Returns "Price: 299"
```

### String to Number Conversion

**Function Syntax:**
```ailang
number_result = StringToNumber(string)
```

**Examples:**
```ailang
// Basic string to number
age = StringToNumber("25")                    // Returns 25
negative = StringToNumber("-42")              // Returns -42
zero = StringToNumber("0")                    // Returns 0

// Parse user input
user_input = GetUserInput("Enter age: ")
age = StringToNumber(user_input)

// Configuration parsing
config_value = "1024"
buffer_size = StringToNumber(config_value)

// Arithmetic on string numbers
str1 = "100"
str2 = "50"
num1 = StringToNumber(str1)
num2 = StringToNumber(str2)
sum = Add(num1, num2)
sum_str = NumberToString(sum)                 // "150"

// Validation helper
Function.IsValidNumber {
    Input: str: String
    Output: Integer
    Body: {
        // Try to convert and check if reasonable
        num = StringToNumber(str)
        // Basic validation: if string was "abc", conversion returns 0
        str_back = NumberToString(num)
        is_valid = StringEquals(str, str_back)
        ReturnValue(is_valid)
    }
}
```

## Buffer and Memory Integration

### String-Buffer Interoperability

AILANG strings integrate seamlessly with raw memory buffers for low-level text processing:

**Creating Strings from Buffers:**
```ailang
// Manual string construction
buffer = Allocate(100)
StoreValue(buffer, 72)                        // 'H'
StoreValue(Add(buffer, 1), 101)               // 'e'
StoreValue(Add(buffer, 2), 108)               // 'l'
StoreValue(Add(buffer, 3), 108)               // 'l'
StoreValue(Add(buffer, 4), 111)               // 'o'
StoreValue(Add(buffer, 5), 0)                 // Null terminator

// Buffer can now be used as string
length = StringLength(buffer)                 // Returns 5
```

**Reading String Data:**
```ailang
// Examine string bytes
text = "Hello"
first_byte = StringCharAt(text, 0)            // Returns 72 ('H')
second_byte = StringCharAt(text, 1)           // Returns 101 ('e')

// Process string as byte sequence
Function.CountVowels {
    Input: text: String
    Output: Integer
    Body: {
        count = 0
        i = 0
        length = StringLength(text)
        
        WhileLoop LessThan(i, length) {
            char_code = StringCharAt(text, i)
            
            // Check for vowels (A=65, E=69, I=73, O=79, U=85)
            // and lowercase (a=97, e=101, i=105, o=111, u=117)
            IfCondition Or(Or(Or(Or(EqualTo(char_code, 65), EqualTo(char_code, 69)), 
                                  EqualTo(char_code, 73)), EqualTo(char_code, 79)), 
                          EqualTo(char_code, 85)) ThenBlock {
                count = Add(count, 1)
            }
            IfCondition Or(Or(Or(Or(EqualTo(char_code, 97), EqualTo(char_code, 101)), 
                                  EqualTo(char_code, 105)), EqualTo(char_code, 111)), 
                          EqualTo(char_code, 117)) ThenBlock {
                count = Add(count, 1)
            }
            
            i = Add(i, 1)
        }
        
        ReturnValue(count)
    }
}
```

### Binary Data Processing

**String as Binary Container:**
```ailang
// Process binary data as strings for certain operations
binary_data = CreateBinaryBuffer(256)
data_string = BufferToString(binary_data)
data_length = StringLength(data_string)

// Search for binary patterns
pattern = "\xFF\xFE"  // BOM marker
has_bom = StringContains(data_string, pattern)

// Extract binary sections
header_section = StringExtract(binary_data, 0, 16)
```

## Advanced Text Processing

### Line-by-Line Processing

**Text File Processing:**
```ailang
Function.ProcessTextFile {
    Input: filename: String
    Output: Integer
    Body: {
        buffer = LoadFile(filename)
        offset = 0
        line_count = 0
        
        WhileLoop LessThan(offset, GetBufferSize(buffer)) {
            line = StringExtractUntil(buffer, offset, "\n")
            line_length = StringLength(line)
            
            // Process line
            IfCondition GreaterThan(line_length, 0) ThenBlock {
                ProcessLine(line)
                line_count = Add(line_count, 1)
            }
            
            // Move to next line
            offset = Add(offset, Add(line_length, 1))
        }
        
        ReturnValue(line_count)
    }
}
```

### Protocol Implementation

**HTTP Request Parsing:**
```ailang
Function.ParseHTTPRequest {
    Input: request_data: String
    Output: String
    Body: {
        // Extract method
        space_pos = StringIndexOf(request_data, " ")
        method = StringSubstring(request_data, 0, space_pos)
        
        // Extract path
        path_start = Add(space_pos, 1)
        second_space = StringIndexOf(StringSubstring(request_data, path_start, 
                                    StringLength(request_data)), " ")
        path = StringSubstring(request_data, path_start, Add(path_start, second_space))
        
        // Build response
        response = StringConcat("Method: ", method)
        response = StringConcat(response, ", Path: ")
        response = StringConcat(response, path)
        
        ReturnValue(response)
    }
}
```

**JSON-like Data Parsing:**
```ailang
Function.ExtractJSONValue {
    Input: json_string: String, key: String
    Output: String
    Body: {
        // Find key pattern: "key":
        key_pattern = StringConcat("\"", key)
        key_pattern = StringConcat(key_pattern, "\":")
        
        key_pos = StringIndexOf(json_string, key_pattern)
        IfCondition EqualTo(key_pos, -1) ThenBlock {
            ReturnValue("")  // Key not found
        }
        
        // Find start of value (after colon and optional space)
        value_start = Add(key_pos, StringLength(key_pattern))
        
        // Skip whitespace
        WhileLoop EqualTo(StringCharAt(json_string, value_start), 32) {
            value_start = Add(value_start, 1)
        }
        
        // Extract until comma or end brace
        comma_pos = StringIndexOf(StringSubstring(json_string, value_start,
                                 StringLength(json_string)), ",")
        brace_pos = StringIndexOf(StringSubstring(json_string, value_start,
                                 StringLength(json_string)), "}")
        
        // Use the closer delimiter
        end_pos = comma_pos
        IfCondition And(GreaterThan(brace_pos, 0), LessThan(brace_pos, comma_pos)) ThenBlock {
            end_pos = brace_pos
        }
        
        value = StringSubstring(json_string, value_start, Add(value_start, end_pos))
        value = StringTrim(value)  // Clean up whitespace
        
        ReturnValue(value)
    }
}
```

## Performance Optimization

### String Pooling

**High-Performance String Operations:**
```ailang
// Initialize string pool once
pool = StringPool.Init(65536)  // 64KB pool

// Use pooled operations for repeated concatenations
Function.BuildLongString {
    Input: count: Integer
    Output: String
    Body: {
        result = "Start"
        i = 0
        
        WhileLoop LessThan(i, count) {
            // Use pooled concatenation for better performance
            result = StringConcatPooled(result, "X")
            i = Add(i, 1)
        }
        
        ReturnValue(result)
    }
}

// Regular concatenation vs pooled
regular_result = BuildLongString(1000)      // Uses many allocations
pooled_result = StringPool.BuildString(1000) // Uses single pool
```

### Memory-Efficient Patterns

**Avoid Temporary Strings:**
```ailang
// Less efficient - creates temporary strings
temp1 = StringConcat("Hello", " ")
temp2 = StringConcat(temp1, "World")
temp3 = StringConcat(temp2, "!")
result = temp3

// More efficient - direct chaining
result = StringConcat(StringConcat(StringConcat("Hello", " "), "World"), "!")

// Most efficient - use StringBuilder pattern
Function.StringBuilder {
    Input: parts: Array
    Output: String
    Body: {
        total_length = 0
        i = 0
        
        // Calculate total length
        WhileLoop LessThan(i, ArrayLength(parts)) {
            part = ArrayGet(parts, i)
            total_length = Add(total_length, StringLength(part))
            i = Add(i, 1)
        }
        
        // Allocate once
        buffer = Allocate(Add(total_length, 1))
        offset = 0
        i = 0
        
        // Copy all parts
        WhileLoop LessThan(i, ArrayLength(parts)) {
            part = ArrayGet(parts, i)
            CopyString(buffer, offset, part)
            offset = Add(offset, StringLength(part))
            i = Add(i, 1)
        }
        
        // Null terminate
        StoreValue(Add(buffer, offset), 0)
        
        ReturnValue(buffer)
    }
}
```

## String Pooling and Memory Management

### Pool-Based String Management

**String Pool Operations:**
```ailang
// Initialize a string pool
pool = StringPool.Init(pool_size)

// Pool status and monitoring
used_bytes = StringPool.Status()
remaining = Subtract(pool_size, used_bytes)

// Pool-optimized operations
result = StringConcatPooled(str1, str2)
```

**Custom Pool Management:**
```ailang
Function.ManagedStringPool {
    Body: {
        // Create fixed pool for strings
        FixedPool.StringCache {
            "buffer": Initialize=0
            "next_offset": Initialize=0
            "pool_size": Initialize=65536
        }
        
        // Initialize buffer once
        StringCache.buffer = Allocate(StringCache.pool_size)
        StringCache.next_offset = 0
        
        PrintMessage("String pool initialized")
        ReturnValue(1)
    }
}
```

### Memory Safety Patterns

**Safe String Operations:**
```ailang
Function.SafeStringConcat {
    Input: str1: String, str2: String, max_length: Integer
    Output: String
    Body: {
        len1 = StringLength(str1)
        len2 = StringLength(str2)
        total = Add(len1, len2)
        
        IfCondition GreaterThan(total, max_length) ThenBlock {
            // Truncate to fit
            available = Subtract(max_length, len1)
            truncated = StringSubstring(str2, 0, available)
            ReturnValue(StringConcat(str1, truncated))
        } ElseBlock {
            ReturnValue(StringConcat(str1, str2))
        }
    }
}

// Buffer overflow protection
Function.SafeStringCopy {
    Input: dest_buffer: Address, src_string: String, buffer_size: Integer
    Output: Integer
    Body: {
        src_length = StringLength(src_string)
        max_copy = Subtract(buffer_size, 1)  // Reserve space for null
        
        copy_length = src_length
        IfCondition GreaterThan(src_length, max_copy) ThenBlock {
            copy_length = max_copy
        }
        
        // Perform safe copy
        i = 0
        WhileLoop LessThan(i, copy_length) {
            char_code = StringCharAt(src_string, i)
            StoreValue(Add(dest_buffer, i), char_code)
            i = Add(i, 1)
        }
        
        // Null terminate
        StoreValue(Add(dest_buffer, copy_length), 0)
        
        ReturnValue(copy_length)
    }
}
```

## Real-World Examples and Patterns

### Configuration File Parser

**INI File Processing:**
```ailang
Function.ParseINIFile {
    Input: filename: String
    Output: Integer
    Body: {
        buffer = LoadTextFile(filename)
        offset = 0
        
        WhileLoop LessThan(offset, GetBufferSize(buffer)) {
            line = StringExtractUntil(buffer, offset, "\n")
            line = StringTrim(line)
            
            // Skip empty lines and comments
            IfCondition And(GreaterThan(StringLength(line), 0), 
                           NotEqual(StringCharAt(line, 0), 35)) ThenBlock {  // 35 = '#'
                
                // Check for section header [section]
                IfCondition EqualTo(StringCharAt(line, 0), 91) ThenBlock {  // 91 = '['
                    close_bracket = StringIndexOf(line, "]")
                    section = StringSubstring(line, 1, close_bracket)
                    ProcessSection(section)
                } ElseBlock {
                    // Parse key=value
                    equals_pos = StringIndexOf(line, "=")
                    IfCondition GreaterThan(equals_pos, 0) ThenBlock {
                        key = StringTrim(StringSubstring(line, 0, equals_pos))
                        value = StringTrim(StringSubstring(line, Add(equals_pos, 1), 
                                                          StringLength(line)))
                        ProcessKeyValue(key, value)
                    }
                }
            }
            
            offset = Add(offset, Add(StringLength(line), 1))
        }
        
        ReturnValue(1)
    }
}
```

### Log File Analyzer

**Log Processing System:**
```ailang
Function.AnalyzeLogFile {
    Input: log_file: String
    Output: Integer
    Body: {
        buffer = LoadTextFile(log_file)
        offset = 0
        error_count = 0
        warning_count = 0
        
        WhileLoop LessThan(offset, GetBufferSize(buffer)) {
            line = StringExtractUntil(buffer, offset, "\n")
            
            // Count log levels
            IfCondition StringContains(line, "ERROR") ThenBlock {
                error_count = Add(error_count, 1)
            }
            IfCondition StringContains(line, "WARN") ThenBlock {
                warning_count = Add(warning_count, 1)
            }
            
            // Extract timestamp (assuming ISO format)
            IfCondition GreaterThan(StringLength(line), 19) ThenBlock {
                timestamp = StringSubstring(line, 0, 19)
                IfCondition StringContains(timestamp, "-") ThenBlock {
                    ProcessTimestamp(timestamp)
                }
            }
            
            offset = Add(offset, Add(StringLength(line), 1))
        }
        
        // Report results
        PrintMessage("Errors found: ")
        PrintNumber(error_count)
        PrintMessage("Warnings found: ")
        PrintNumber(warning_count)
        
        ReturnValue(Add(error_count, warning_count))
    }
}
```

### Template Engine

**Simple Template Processor:**
```ailang
Function.ProcessTemplate {
    Input: template: String, variables: Array
    Output: String
    Body: {
        result = template
        i = 0
        
        WhileLoop LessThan(i, ArrayLength(variables)) {
            variable = ArrayGet(variables, i)
            
            // Expect format: ["{{KEY}}", "VALUE"]
            key = ArrayGet(variable, 0)
            value = ArrayGet(variable, 1)
            
            // Replace all occurrences
            result = StringReplace(result, key, value)
            
            i = Add(i, 1)
        }
        
        ReturnValue(result)
    }
}

// Usage example:
template_text = "Hello {{NAME}}, welcome to {{SYSTEM}}!"
variables = ArrayCreate(2)
ArraySet(variables, 0, ArrayFromStrings("{{NAME}}", "John"))
ArraySet(variables, 1, ArrayFromStrings("{{SYSTEM}}", "AILANG"))

personalized = ProcessTemplate(template_text, variables)
// Returns: "Hello John, welcome to AILANG!"
```

## Best Practices and Guidelines

### Performance Guidelines

1. **Use StringConcatPooled for repeated operations**
2. **Avoid creating temporary strings in loops**
3. **Pre-calculate string lengths when possible**
4. **Use StringBuilder pattern for complex string building**
5. **Pool string allocations for high-frequency operations**

### Memory Management

1. **Always null-terminate manually created strings**
2. **Use appropriate buffer sizes for string operations**
3. **Clean up allocated string resources**
4. **Validate string lengths before operations**
5. **Use bounds checking for buffer operations**

### Code Organization

1. **Group related string operations into functions**
2. **Create reusable string processing utilities**
3. **Use descriptive names for string variables**
4. **Document string format expectations**
5. **Handle edge cases (empty strings, null inputs)**

This comprehensive manual provides everything needed to effectively use AILANG's string processing capabilities, from basic operations to advanced text processing patterns. The system is designed for both high-level convenience and low-level performance optimization, making it suitable for systems programming, text processing, and data manipulation tasks.