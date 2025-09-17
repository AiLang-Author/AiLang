# AILANG Flow Control Programming Manual

## Table of Contents
1. [Overview](#overview)
2. [Conditional Statements](#conditional-statements)
3. [Loop Constructs](#loop-constructs)
4. [Advanced Flow Control](#advanced-flow-control)
5. [Examples and Patterns](#examples-and-patterns)
6. [Error Handling](#error-handling)
7. [Performance Considerations](#performance-considerations)

## Overview

AILANG provides a comprehensive set of flow control constructs that enable structured programming patterns. The language supports traditional control structures with modern enhancements for concurrent and event-driven programming.

### Supported Flow Control Types
- Conditional Execution: IfCondition, Fork, Branch
- Iterative Loops: WhileLoop, ForEvery
- Loop Control: BreakLoop, ContinueLoop
- Pattern Matching: ChoosePath
- Exception Handling: Try/Catch
- Concurrent Constructs: EveryInterval, WithSecurity


### WORKING
- If/Then/Else with all condition types
- WhileLoop with simple and nested patterns
- Boolean literals (True/False)
- Numeric conditions (0=false, non-zero=true)
- Comparison functions in conditions
- State machines and complex patterns

### ISSUES
- Complex And() conditions in WhileLoop may evaluate incorrectly

### NOT IMPLEMENTED
- BreakLoop
- ContinueLoop
- ForLoop
- Switch/Case

**Recommendation**: Use workarounds for break/continue (set loop variable to exit condition)


## Conditional Statements

### Basic If-Then-Else

The IfCondition construct provides standard conditional execution:

IfCondition condition ThenBlock {
    // statements executed if condition is true
} ElseBlock {
    // statements executed if condition is false
}
Condition Types Supported:
- Function Calls: LessThan(x, y), EqualTo(a, b), GreaterThan(i, j)
- Variables: flag, isRunning, status
- Boolean Literals: True, False
- Numeric Values: 0 (false), non-zero (true)

Example:
ailang
age = 25
IfCondition GreaterEqual(age, 18) ThenBlock {
    PrintMessage("Adult")
} ElseBlock {
    PrintMessage("Minor")
}

### Fork Construct

The Fork construct provides binary branching with explicit true/false blocks:

Fork condition {
    true_block: {
        // executed when condition is true
    }
    false_block: {
        // executed when condition is false
    }
}
Example:
ailang
Fork GreaterThan(balance, 0) {
    true_block: {
        PrintMessage("Account has funds")
        status = "active"
    }
    false_block: {
        PrintMessage("Account overdrawn")
        status = "frozen"
    }
}

### Branch Construct (Switch-Like)

The Branch construct provides multi-way conditional execution:

Branch expression {
    case value1: {
        // statements for value1
    }
    case value2: {
        // statements for value2
    }
    default: {
        // default case
    }
}
Example:
ailang
Branch day_of_week {
    case 1: {
        PrintMessage("Monday")
        schedule = "work"
    }
    case 6: {
        PrintMessage("Saturday")
        schedule = "weekend"
    }
    case 7: {
        PrintMessage("Sunday")
        schedule = "weekend"
    }
    default: {
        PrintMessage("Weekday")
        schedule = "work"
    }
}

## Loop Constructs

### While Loops

The primary iteration construct in AILANG is the WhileLoop:

WhileLoop condition {
    // loop body
}
Example - Basic Counting:
ailang
counter = 0
WhileLoop LessThan(counter, 10) {
    PrintNumber(counter)
    counter = Add(counter, 1)
}

Example - Processing with Exit Condition:
```ailang
processing = True
data_count = 0
WhileLoop EqualTo(processing, True) {
    // Process data
    result = ProcessNextItem()
    data_count = Add(data_count, 1)

    // Check exit condition
    IfCondition EqualTo(result, "DONE") ThenBlock {
        processing = False
    }
}
```

### Nested Loops

Loops can be nested for multi-dimensional processing:

row = 0
WhileLoop LessThan(row, 3) {
    col = 0
    WhileLoop LessThan(col, 3) {
        position = Add(Multiply(row, 3), col)
        PrintMessage("Position:")
        PrintNumber(position)
        col = Add(col, 1)
    }
    row = Add(row, 1)
}
### Loop Control Statements

#### BreakLoop
Exits the innermost loop immediately:

i = 0
WhileLoop LessThan(i, 100) {
    IfCondition EqualTo(i, 5) ThenBlock {
        PrintMessage("Breaking at 5")
        BreakLoop
    }
    PrintNumber(i)
    i = Add(i, 1)
}
#### ContinueLoop
Skips the rest of the current iteration and jumps to the loop condition:

i = 0
WhileLoop LessThan(i, 10) {
    i = Add(i, 1)
    IfCondition EqualTo(Modulo(i, 2), 0) ThenBlock {
        ContinueLoop  // Skip even numbers
    }
    PrintNumber(i)  // Only odd numbers printed
}
### ForEvery Loops (Collection Iteration)

For iterating over collections:

ForEvery item in collection {
    // Process each item
    PrintMessage(item)
}
## Advanced Flow Control

### ChoosePath (Pattern Matching)

Advanced multi-case selection:

ChoosePath input_type {
    case "number": {
        result = ProcessNumber(input)
    }
    case "string": {
        result = ProcessString(input)
    }
    case "array": {
        result = ProcessArray(input)
    }
    default: {
        result = "unknown_type"
    }
}
### Try-Catch Exception Handling

Try {
    risky_operation = DivideByZero()
} Catch "division_error" {
    PrintMessage("Division by zero handled")
    result = 0
} Catch "general_error" {
    PrintMessage("General error occurred")
    result = -1
} Finally {
    PrintMessage("Cleanup completed")
}
### Interval-Based Execution

EveryInterval "seconds" 5 {
    PrintMessage("Heartbeat every 5 seconds")
    CheckSystemStatus()
}

EveryInterval "minutes" 1 {
    SaveDataToFile()
}
### Security Context Execution

WithSecurity "admin" {
    // Privileged operations
    ModifySystemSettings()
    AccessSecureData()
}

WithSecurity "user" {
    // Standard operations
    ReadPublicData()
    UpdateUserProfile()
}
## Examples and Patterns

### Pattern 1: Input Validation Loop

valid_input = False
attempts = 0
max_attempts = 3

WhileLoop And(EqualTo(valid_input, False), LessThan(attempts, max_attempts)) {
    input = GetUserInput()
    attempts = Add(attempts, 1)
    
    IfCondition ValidateInput(input) ThenBlock {
        valid_input = True
        PrintMessage("Valid input received")
    } ElseBlock {
        PrintMessage("Invalid input, try again")
    }
}

IfCondition EqualTo(valid_input, False) ThenBlock {
    PrintMessage("Max attempts exceeded")
}
### Pattern 2: State Machine Implementation

state = "start"
running = True

WhileLoop EqualTo(running, True) {
    Branch state {
        case "start": {
            PrintMessage("Initializing")
            state = "processing"
        }
        case "processing": {
            result = ProcessData()
            IfCondition EqualTo(result, "success") ThenBlock {
                state = "complete"
            } ElseBlock {
                state = "error"
            }
        }
        case "complete": {
            PrintMessage("Processing complete")
            running = False
        }
        case "error": {
            PrintMessage("Error occurred")
            running = False
        }
        default: {
            PrintMessage("Unknown state")
            running = False
        }
    }
}
### Pattern 3: Menu System

exit_requested = False

WhileLoop EqualTo(exit_requested, False) {
    PrintMessage("1. View Data")
    PrintMessage("2. Edit Data")
    PrintMessage("3. Exit")
    choice = GetUserChoice()
    
    Branch choice {
        case 1: {
            DisplayData()
        }
        case 2: {
            EditData()
        }
        case 3: {
            exit_requested = True
        }
        default: {
            PrintMessage("Invalid choice")
        }
    }
}
## Error Handling

### Common Flow Control Errors

Infinite Loops: Always ensure loop conditions can become false
Break/Continue Outside Loops: Only use within loop constructs
Unbalanced Blocks: Ensure all ThenBlock/ElseBlock pairs are closed
Condition Type Mismatches: Use appropriate condition types
### Best Practices

Initialize Loop Variables: Always set initial values
Update Loop Variables: Ensure progress toward exit condition
Limit Loop Iterations: Use counters for safety
Handle Edge Cases: Account for empty collections, null values
Use Descriptive Variable Names: is_complete vs flag
## Performance Considerations

### Optimization Tips

Minimize Condition Complexity: Simple conditions execute faster
Avoid Nested Loops: Use single loops with calculated indices when possible
Early Exit Conditions: Use BreakLoop for efficiency
Cache Repeated Calculations: Don't recalculate in loop conditions
### Memory Management

// Good: Clean up in loops
WhileLoop LessThan(i, count) {
    buffer = Allocate(1024)
    ProcessData(buffer)
    Deallocate(buffer, 1024)  // Clean up each iteration
    i = Add(i, 1)
}

// Bad: Memory accumulation
WhileLoop LessThan(i, count) {
    buffer = Allocate(1024)
    ProcessData(buffer)
    // Missing Deallocate - memory leak!
    i = Add(i, 1)
}
## Compiler Implementation Notes

The AILANG flow control system uses label-based jumps for efficient code generation:

Condition Evaluation: Results stored in RAX register (0=false, non-zero=true)
Jump Instructions: Modern x86-64 conditional jumps
Label Management: Automatic label generation and resolution
Stack Management: Proper cleanup for nested constructs
### Condition Compilation

The compiler handles different condition types:

def compile_condition(self, condition):
    if isinstance(condition, FunctionCall):
        # LessThan(x, y) -> function call
        self.compiler.compile_function_call(condition)
    elif isinstance(condition, Identifier):
        # variable -> load value
        self.compiler.compile_expression(condition)
    elif isinstance(condition, Boolean):
        # True/False -> immediate value
        self.asm.emit_mov_rax_imm64(1 if condition.value else 0)
This manual provides comprehensive coverage of AILANG's flow control capabilities, enabling developers to write structured, efficient, and maintainable code using the language's control flow constructs.    ,,,,,,,,,, you've been writing unit tests and documentation today  your up to file io, lmk if you need a example test, were fully documenting ailamg, project knowledge is full of files compiler and more