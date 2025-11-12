# AILANG Arithmetic and Mathematical Operations Manual

## Table of Contents
1. [Overview](#overview)
2. [Basic Arithmetic Operations](#basic-arithmetic-operations)
3. [Infix Notation System](#infix-notation-system)
4. [Unary Negation](#unary-negation)
5. [Comparison Operations](#comparison-operations)
6. [Bitwise Operations](#bitwise-operations)
7. [Logical Operations](#logical-operations)
8. [Advanced Mathematical Functions](#advanced-mathematical-functions)
9. [Floating-Point Operations (Future)](#floating-point-operations-future)
10. [Mixed Notation Programming](#mixed-notation-programming)
11. [Performance Optimization](#performance-optimization)
12. [Common Patterns and Examples](#common-patterns-and-examples)

## Overview

AILANG provides a comprehensive mathematical computation system that supports both traditional function-call syntax and modern infix notation. The language seamlessly integrates symbolic operators with named functions, enabling flexible and readable mathematical expressions.

### Supported Number Types
- **Signed Integers**: 64-bit signed integers (-2^63 to 2^63-1)
- **Automatic Promotion**: Operations automatically handle large numbers
- **Zero Handling**: Proper zero semantics for all operations

### Dual Syntax Support
AILANG uniquely supports both syntaxes simultaneously:
```ailang
// Function call syntax
result = Add(a, b)

// Infix symbol syntax  
result = (a + b)

// Mixed usage
result = Add((a * b), Divide(c, d))
```

## Basic Arithmetic Operations

### Addition

**Function Syntax:**
```ailang
result = Add(operand1, operand2)
```

**Infix Syntax:**
```ailang
result = (operand1 + operand2)
```

**Examples:**
```ailang
// Basic addition
sum = Add(10, 5)          // Returns 15
sum = (10 + 5)            // Returns 15

// Variable addition
a = 100
b = 25
total = Add(a, b)         // Returns 125
total = (a + b)           // Returns 125

// Negative numbers
result = Add(-10, 5)      // Returns -5
result = (-10 + 5)        // Returns -5

// Chain operations
result = Add(Add(1, 2), 3)  // Returns 6
result = ((1 + 2) + 3)      // Returns 6
```

### Subtraction

**Function Syntax:**
```ailang
result = Subtract(minuend, subtrahend)
```

**Infix Syntax:**
```ailang
result = (minuend - subtrahend)
```

**Examples:**
```ailang
// Basic subtraction
diff = Subtract(20, 8)    // Returns 12
diff = (20 - 8)           // Returns 12

// Negative results
result = Subtract(5, 10)  // Returns -5
result = (5 - 10)         // Returns -5

// Double negatives
result = Subtract(-5, -10) // Returns 5
result = (-5 - (-10))     // Returns 5
```

### Multiplication

**Function Syntax:**
```ailang
result = Multiply(factor1, factor2)
```

**Infix Syntax:**
```ailang
result = (factor1 * factor2)
```

**Examples:**
```ailang
// Basic multiplication
product = Multiply(7, 6)   // Returns 42
product = (7 * 6)          // Returns 42

// Sign handling
result = Multiply(-3, 4)   // Returns -12
result = (-3 * 4)          // Returns -12

result = Multiply(-3, -4)  // Returns 12
result = (-3 * -4)         // Returns 12

// Large numbers
big = Multiply(1000000, 1000)  // Returns 1000000000
big = (1000000 * 1000)         // Returns 1000000000
```

### Division

**Function Syntax:**
```ailang
result = Divide(dividend, divisor)
```

**Infix Syntax:**
```ailang
result = (dividend / divisor)
```

**Examples:**
```ailang
// Integer division (truncated)
quotient = Divide(20, 3)   // Returns 6 (truncated)
quotient = (20 / 3)        // Returns 6 (truncated)

// Exact division
result = Divide(100, 4)    // Returns 25
result = (100 / 4)         // Returns 25

// Negative division
result = Divide(-20, 4)    // Returns -5
result = (-20 / 4)         // Returns -5

result = Divide(20, -4)    // Returns -5
result = (20 / -4)         // Returns -5

result = Divide(-20, -4)   // Returns 5
result = (-20 / -4)        // Returns 5
```

**Important Notes:**
- Division by zero results in undefined behavior
- Integer division truncates toward zero
- Sign handling follows standard mathematical rules

### Modulo (Remainder)

**Function Syntax:**
```ailang
result = Modulo(dividend, divisor)
```

**Infix Syntax:**
```ailang
result = (dividend % divisor)
```

**Examples:**
```ailang
// Basic modulo
remainder = Modulo(17, 5)  // Returns 2
remainder = (17 % 5)       // Returns 2

// Zero remainder
result = Modulo(20, 4)     // Returns 0
result = (20 % 4)          // Returns 0

// Negative operands (truncated division)
result = Modulo(-17, 5)    // Returns -2
result = (-17 % 5)         // Returns -2

result = Modulo(17, -5)    // Returns 2
result = (17 % -5)         // Returns 2
```

### Power/Exponentiation

**Function Syntax:**
```ailang
result = Power(base, exponent)
```

**Infix Syntax:**
```ailang
result = (base ^ exponent)
```

**Examples:**
```ailang
// Basic powers
squared = Power(5, 2)      // Returns 25
squared = (5 ^ 2)          // Returns 25

cubed = Power(3, 3)        // Returns 27
cubed = (3 ^ 3)           // Returns 27

// Powers of 2
power2 = Power(2, 8)       // Returns 256
power2 = (2 ^ 8)          // Returns 256

// Power of 1 and 0
identity = Power(42, 1)    // Returns 42
one = Power(42, 0)        // Returns 1
```

## Infix Notation System

AILANG's infix notation requires parentheses for proper precedence and parsing:

### Basic Infix Rules

1. **Parentheses Required**: All infix operations must be enclosed in parentheses
2. **Left-to-Right Evaluation**: Operations evaluate left-to-right within parentheses
3. **Nested Operations**: Parentheses can be nested to any depth
4. **Mixed Syntax**: Infix and function calls can be combined

**Examples:**
```ailang
// Valid infix expressions
result = (a + b)
result = ((a + b) * c)
result = (((a + b) * c) - d)

// Invalid - missing parentheses
// result = a + b           // SYNTAX ERROR
// result = a + b * c       // SYNTAX ERROR
```

### Operator Precedence Through Parentheses

Since AILANG requires explicit parentheses, precedence is controlled by nesting:

```ailang
// Mathematical precedence through explicit grouping
result = ((a * b) + c)     // Multiply first, then add
result = (a * (b + c))     // Add first, then multiply

// Complex expressions
result = (((a + b) * (c - d)) / (e + f))

// Equivalent function call version
result = Divide(Multiply(Add(a, b), Subtract(c, d)), Add(e, f))
```

## Comparison Operations

### Relational Comparisons

**Function Syntax:**
```ailang
result = LessThan(a, b)        // Returns 1 if a < b, 0 otherwise
result = LessEqual(a, b)       // Returns 1 if a <= b, 0 otherwise
result = GreaterThan(a, b)     // Returns 1 if a > b, 0 otherwise
result = GreaterEqual(a, b)    // Returns 1 if a >= b, 0 otherwise
result = EqualTo(a, b)         // Returns 1 if a == b, 0 otherwise
result = NotEqual(a, b)        // Returns 1 if a != b, 0 otherwise
```

**Infix Syntax:**
```ailang
result = (a < b)
result = (a <= b)
result = (a > b)
result = (a >= b)
result = (a == b)
result = (a != b)
```

**Examples:**
```ailang
// Basic comparisons
age = 25
is_adult = GreaterEqual(age, 18)  // Returns 1
is_adult = (age >= 18)            // Returns 1

// Chained comparisons
x = 10
y = 20
z = 30
result = And(LessThan(x, y), LessThan(y, z))  // Returns 1
result = ((x < y) && (y < z))                 // Returns 1

// Equality testing
value = 42
is_answer = EqualTo(value, 42)    // Returns 1
is_answer = (value == 42)         // Returns 1
```

## Bitwise Operations

### Basic Bitwise Operations

**Function Syntax:**
```ailang
result = BitwiseAnd(a, b)      // Bitwise AND
result = BitwiseOr(a, b)       // Bitwise OR
result = BitwiseXor(a, b)      // Bitwise XOR (exclusive OR)
result = BitwiseNot(a)         // Bitwise NOT (complement)
result = LeftShift(a, bits)    // Left shift
result = RightShift(a, bits)   // Right shift
```

**Infix Syntax:**
```ailang
result = (a & b)               // Bitwise AND
result = (a | b)               // Bitwise OR
result = (a << bits)           // Left shift
result = (a >> bits)           // Right shift
result = (~a)                  // Bitwise NOT
```

**Note:** XOR uses the function `BitwiseXor()` as the `^` symbol is reserved for power operations.

**Examples:**
```ailang
// Basic bitwise operations
mask = 15                      // Binary: 1111
value = 255                    // Binary: 11111111

// AND operation - clear upper bits
result = BitwiseAnd(value, mask)  // Returns 15
result = (value & mask)           // Returns 15

// OR operation - set bits
flags = 0
flag1 = 1
flag2 = 4
combined = BitwiseOr(BitwiseOr(flags, flag1), flag2)  // Returns 5
combined = ((flags | flag1) | flag2)                  // Returns 5

// XOR operation - toggle bits
original = 170                 // Binary: 10101010
toggle_mask = 15              // Binary: 00001111
toggled = BitwiseXor(original, toggle_mask)  // Returns 165
// Note: No infix XOR symbol, use function

// NOT operation - complement
value = 0
all_bits = BitwiseNot(value)  // Returns -1 (all bits set)
all_bits = (~value)           // Returns -1 (all bits set)
```

### Bit Shifting Operations

**Examples:**
```ailang
// Left shift - multiply by powers of 2
value = 5                     // Binary: 101
shifted = LeftShift(value, 2) // Returns 20 (Binary: 10100)
shifted = (value << 2)        // Returns 20

// Right shift - divide by powers of 2
value = 20                    // Binary: 10100
shifted = RightShift(value, 2) // Returns 5 (Binary: 101)
shifted = (value >> 2)         // Returns 5

// Generate powers of 2
power2_8 = LeftShift(1, 8)    // Returns 256
power2_8 = (1 << 8)           // Returns 256

power2_16 = LeftShift(1, 16)  // Returns 65536
power2_16 = (1 << 16)         // Returns 65536
```

### Common Bit Manipulation Patterns

**Set a Bit:**
```ailang
// Set bit n in value
bit_pos = 5
value = 0
set_bit = BitwiseOr(value, LeftShift(1, bit_pos))
set_bit = (value | (1 << bit_pos))
```

**Clear a Bit:**
```ailang
// Clear bit n in value
bit_pos = 3
value = 255
mask = BitwiseNot(LeftShift(1, bit_pos))
clear_bit = BitwiseAnd(value, mask)
clear_bit = (value & (~(1 << bit_pos)))
```

**Toggle a Bit:**
```ailang
// Toggle bit n in value
bit_pos = 4
value = 170
toggle_bit = BitwiseXor(value, LeftShift(1, bit_pos))
// Note: Must use BitwiseXor function, no infix symbol
```

**Check if Bit is Set:**
```ailang
// Check if bit n is set
bit_pos = 3
value = 42
is_set = NotEqual(BitwiseAnd(value, LeftShift(1, bit_pos)), 0)
is_set = ((value & (1 << bit_pos)) != 0)
```

**Check if Power of 2:**
```ailang
// Check if number is power of 2: n & (n-1) == 0
test_val = 16
is_pow2 = EqualTo(BitwiseAnd(test_val, Subtract(test_val, 1)), 0)
is_pow2 = ((test_val & (test_val - 1)) == 0)
```

## Logical Operations

### Boolean Logic Functions

**Function Syntax:**
```ailang
result = And(a, b)             // Logical AND
result = Or(a, b)              // Logical OR  
result = Not(a)                // Logical NOT
```

**Infix Syntax:**
```ailang
result = (a && b)              // Logical AND
result = (a || b)              // Logical OR
result = (!a)                  // Logical NOT
```

**Examples:**
```ailang
// Basic logical operations
flag1 = 1
flag2 = 0

// AND - both must be true
both_true = And(flag1, flag2)   // Returns 0
both_true = (flag1 && flag2)    // Returns 0

// OR - either can be true
either_true = Or(flag1, flag2)  // Returns 1
either_true = (flag1 || flag2)  // Returns 1

// NOT - invert boolean
not_flag = Not(flag1)           // Returns 0
not_flag = (!flag1)             // Returns 0

// Complex boolean expressions
x = 5
y = 10
z = 5
result = And(EqualTo(x, z), GreaterThan(y, x))  // Returns 1
result = ((x == z) && (y > x))                   // Returns 1
```

### Short-Circuit Evaluation

AILANG logical operators support short-circuit evaluation in infix form:

```ailang
// Short-circuit AND - if first is false, second not evaluated
result = (false_condition && expensive_operation())

// Short-circuit OR - if first is true, second not evaluated  
result = (true_condition || expensive_operation())
```

## Advanced Mathematical Functions

### Absolute Value

**Function Syntax:**
```ailang
result = Absolute(value)
```

**Examples:**
```ailang
pos = Absolute(42)     // Returns 42
pos = Absolute(-42)    // Returns 42
zero = Absolute(0)     // Returns 0
```

### Minimum and Maximum

**Function Syntax:**
```ailang
result = Min(a, b)     // Returns smaller value
result = Max(a, b)     // Returns larger value
```

**Examples:**
```ailang
smaller = Min(10, 5)   // Returns 5
larger = Max(10, 5)    // Returns 10

// Can be chained
smallest = Min(Min(a, b), c)
largest = Max(Max(a, b), c)
```

### Square Root (if available)

**Function Syntax:**
```ailang
result = SquareRoot(value)
```

## Mixed Notation Programming

### Combining Syntaxes

AILANG allows seamless mixing of function calls and infix notation:

```ailang
// Mix infix with function calls
result = Add((a * b), Divide(c, d))

// Use infix inside function calls
result = Add((x + y), (z - w))

// Complex mixed expressions
result = Multiply(((a + b) * c), Power(d, 2))
result = (((a + b) * c) * (d ^ 2))
```

### Style Guidelines

**Recommendation 1: Use infix for simple operations**
```ailang
// Preferred for readability
total = (price + tax)
area = (length * width)
is_valid = (age >= 18)
```

**Recommendation 2: Use functions for complex operations**
```ailang
// Clearer for complex operations
distance = SquareRoot(Add(Power(x2 - x1, 2), Power(y2 - y1, 2)))

// Or mix styles
distance = SquareRoot(((x2 - x1) ^ 2) + ((y2 - y1) ^ 2))
```

## Performance Optimization

### Efficient Operation Ordering

**Multiplication by Constants:**
```ailang
// Use bit shifting for powers of 2
result = (value << 3)     // 8x faster than (value * 8)
result = (value << 10)    // 1024x faster than (value * 1024)
```

**Division by Constants:**
```ailang
// Use bit shifting for powers of 2
result = (value >> 2)     // 4x faster than (value / 4)
result = (value >> 8)     // 256x faster than (value / 256)
```

**Modulo by Powers of 2:**
```ailang
// Use bitwise AND for powers of 2
remainder = (value & 7)   // Equivalent to (value % 8)
remainder = (value & 15)  // Equivalent to (value % 16)
```

### Memory-Efficient Patterns

**Avoid Temporary Variables:**
```ailang
// Less efficient
temp1 = (a + b)
temp2 = (c * d)
result = (temp1 - temp2)

// More efficient
result = ((a + b) - (c * d))
```

## Common Patterns and Examples

### Mathematical Algorithms

**Quadratic Formula Components:**
```ailang
// For ax² + bx + c = 0
// Calculate discriminant: b² - 4ac
a_coef = 1
b_coef = 5  
c_coef = 6
discriminant = ((b_coef ^ 2) - ((4 * a_coef) * c_coef))
```

**Fibonacci Sequence:**
```ailang
// Calculate next Fibonacci number
fib_n_minus_2 = 8
fib_n_minus_1 = 13
fib_n = (fib_n_minus_2 + fib_n_minus_1)  // Returns 21
```

**Distance Formula:**
```ailang
// Distance between two points
x1 = 0
y1 = 0
x2 = 3
y2 = 4
distance_squared = (((x2 - x1) ^ 2) + ((y2 - y1) ^ 2))  // Returns 25
// distance = SquareRoot(distance_squared)  // Would return 5
```

### Bit Manipulation Algorithms

**Count Set Bits (Brian Kernighan's Algorithm):**
```ailang
value = 42
count = 0
WhileLoop NotEqual(value, 0) {
    value = BitwiseAnd(value, Subtract(value, 1))
    count = Add(count, 1)
}
// count now contains number of set bits
```

**Swap Without Temporary Variable:**
```ailang
// XOR swap (use with caution)
a = 5
b = 10
a = BitwiseXor(a, b)
b = BitwiseXor(a, b)
a = BitwiseXor(a, b)
// a is now 10, b is now 5
```

### Number Theory Functions

**Greatest Common Divisor (Euclidean Algorithm):**
```ailang
gcd_a = 48
gcd_b = 18
WhileLoop NotEqual(gcd_b, 0) {
    temp = gcd_b
    gcd_b = Modulo(gcd_a, gcd_b)
    gcd_a = temp
}
// gcd_a now contains GCD(48, 18) = 6
```

**Check if Prime (Basic):**
```ailang
n = 17
is_prime = 1
i = 2
WhileLoop And(LessThan(i, n), EqualTo(is_prime, 1)) {
    IfCondition EqualTo(Modulo(n, i), 0) ThenBlock {
        is_prime = 0
    }
    i = Add(i, 1)
}
// is_prime is 1 if n is prime, 0 otherwise
```

## Error Conditions and Edge Cases

### Division Errors
- **Division by Zero**: Undefined behavior, program may crash
- **Integer Overflow**: Large results may wrap around

### Bitwise Operation Considerations
- **Right Shift of Negative Numbers**: Implementation uses logical shift (fills with zeros), not arithmetic shift
- **Shift Amounts**: Shifting by more than 63 bits has undefined behavior

### Best Practices
1. **Always validate divisors** before division operations
2. **Use parentheses** to make operator precedence explicit
3. **Test edge cases** like zero, negative numbers, and large values
4. **Use appropriate data types** for expected value ranges
5. **Consider overflow** in multiplication and power operations

This manual provides comprehensive coverage of AILANG's mathematical capabilities, enabling developers to write efficient, readable, and mathematically sound programs using both traditional function syntax and modern infix notation.