# AIMacro Core Function Mappings

This document outlines the essential Python language constructs and built-ins that need to be mapped to AILang for the AIMacro project.

## 1. Language Basics & Control Flow

### Function Definitions
```python
# AIMacro/Python
def function_name(param1, param2):
    # body
    return result;
end;
```
**Maps to AILang:**
```ailang
Function.function_name {
    Input: param1: Integer, param2: Integer  
    Output: Integer
    Body: {
        # body
        ReturnValue(result)
    }
}
```

### Control Structures

#### If Statements
```python
# AIMacro
if condition:
    # code
elif other:
    # code  
else:
    # code
end;
```
**Maps to:**
```ailang
IfCondition condition ThenBlock {
    # code
} ElseBlock {
    IfCondition other ThenBlock {
        # code
    } ElseBlock {
        # code
    }
}
```

#### While Loops
```python
# AIMacro  
while condition:
    # code
end;
```
**Maps to:**
```ailang
WhileLoop condition {
    # code
}
```

#### For Loops
```python
# AIMacro
for item in collection:
    # code
end;
```
**Maps to:**
```ailang
ForEvery item in collection {
    # code
}
```

## 2. Python Built-in Functions

### I/O Operations
| Python Function | AIMacro | AILang Mapping |
|----------------|---------|----------------|
| `print(x)` | `print(x);` | `PrintMessage(x)` |
| `input(prompt)` | `input(prompt);` | Custom input function |

### Data Type Conversions  
| Python Function | AIMacro | AILang Mapping |
|----------------|---------|----------------|
| `str(x)` | `str(x);` | `NumberToString(x)` |
| `int(x)` | `int(x);` | `StringToNumber(x)` |
| `float(x)` | `float(x);` | Custom float conversion |
| `bool(x)` | `bool(x);` | `NotEqual(x, 0)` |

### Container Operations
| Python Function | AIMacro | AILang Mapping |
|----------------|---------|----------------|
| `len(container)` | `len(container);` | `ArrayLength(container)` |
| `list()` | `list();` | `Collection.CreateList()` |
| `dict()` | `dict();` | `Map.Create()` |

### Mathematical Operations
| Python Function | AIMacro | AILang Mapping |
|----------------|---------|----------------|
| `abs(x)` | `abs(x);` | `AbsoluteValue(x)` |
| `max(a, b)` | `max(a, b);` | `IfCondition GreaterThan(a,b) ThenBlock {a} ElseBlock {b}` |
| `min(a, b)` | `min(a, b);` | `IfCondition LessThan(a,b) ThenBlock {a} ElseBlock {b}` |
| `sum(items)` | `sum(items);` | Custom sum function |
| `round(x)` | `round(x);` | Custom rounding function |

### Iterator/Sequence Operations
| Python Function | AIMacro | AILang Mapping |
|----------------|---------|----------------|
| `range(n)` | `range(n);` | Custom range generator |
| `enumerate(seq)` | `enumerate(seq);` | Custom enumeration |
| `zip(a, b)` | `zip(a, b);` | Custom zip function |

## 3. Data Structures

### Lists/Arrays
```python
# AIMacro
my_list = [];
my_list.append(item);
item = my_list[index];
```
**Maps to:**
```ailang
my_list = Collection.CreateList()
Collection.Append(my_list, item)
item = Collection.Get(my_list, index)
```

### Dictionaries/Maps
```python
# AIMacro
my_dict = {};
my_dict[key] = value;
value = my_dict[key];
```
**Maps to:**
```ailang
my_dict = Map.Create()
Map.Set(my_dict, key, value)
value = Map.Get(my_dict, key)
```

### Strings
```python
# AIMacro
text = "hello";
length = len(text);
upper_text = text.upper();
```
**Maps to:**
```ailang
text = "hello"
length = StringLength(text)
upper_text = StringToUpperCase(text)
```

## 4. Core Operators

### Arithmetic Operators (Infix Notation)
| Python | AIMacro | Direct Translation | Complex Cases |
|--------|---------|-------------------|---------------|
| `a + b` | `a + b` | `a + b` | `Add(a, b)` for nested |
| `a - b` | `a - b` | `a - b` | `Subtract(a, b)` for nested |
| `a * b` | `a * b` | `a * b` | `Multiply(a, b)` for nested |
| `a / b` | `a / b` | `a / b` | `Divide(a, b)` for nested |
| `a % b` | `a % b` | `a % b` | `Modulo(a, b)` for nested |
| `a ** b` | `a ** b` | `a ** b` | `Power(a, b)` for nested |

### Comparison Operators (Infix Notation)
| Python | AIMacro | Direct Translation | Complex Cases |
|--------|---------|-------------------|---------------|
| `a == b` | `a == b` | `a == b` | `EqualTo(a, b)` for nested |
| `a != b` | `a != b` | `a != b` | `NotEqual(a, b)` for nested |
| `a < b` | `a < b` | `a < b` | `LessThan(a, b)` for nested |
| `a <= b` | `a <= b` | `a <= b` | `LessEqual(a, b)` for nested |
| `a > b` | `a > b` | `a > b` | `GreaterThan(a, b)` for nested |
| `a >= b` | `a >= b` | `a >= b` | `GreaterEqual(a, b)` for nested |

### Logical Operators (Keywords Stay)
| Python | AIMacro | AILang Translation |
|--------|---------|-------------------|
| `a and b` | `a and b` | `And(a, b)` |
| `a or b` | `a or b` | `Or(a, b)` |
| `not a` | `not a` | `Not(a)` |

### Operator Precedence Rules

**Simple Infix (Direct mapping):**
```python
# AIMacro - These translate directly
result = a + b * c;           # Standard precedence
condition = x < y;            # Direct comparison
index = i + 1;                # Simple arithmetic
```

**Complex Expressions (Use AILang functions):**
```python
# AIMacro - For readability and control
result = Add(Multiply(a, b), Divide(c, d));
condition = And(GreaterThan(x, 0), LessThan(x, 100));
complex_calc = Power(Add(a, b), Subtract(c, d));
```

## 5. Memory Management

### Variable Assignment
```python
# AIMacro
x = 42;
name = "Alice";
```
**Maps to:**
```ailang
x = 42
name = "Alice"
```

### Memory Operations (Advanced)
```python
# AIMacro - Advanced memory control
ptr = allocate(1024);
free(ptr);
```
**Maps to:**
```ailang
ptr = Allocate(1024)
Deallocate(ptr, 1024)
```

## 6. Error Handling

### Try-Catch
```python
# AIMacro
try:
    risky_operation();
except Exception as e:
    handle_error(e);
finally:
    cleanup();
end;
```
**Maps to:**
```ailang
TryBlock {
    risky_operation()
} CatchError {
    handle_error()
} FinallyBlock {
    cleanup()
}
```

## 7. Implementation Priority

### Phase 1: Core Language (✅ Target)
1. **Function definitions** (`def`, `return`)
2. **Control flow** (`if`, `while`, `for`)
3. **Variable assignment**
4. **Basic operators** (`+`, `-`, `*`, `/`, `==`, `!=`, etc.)
5. **Print function**

### Phase 2: Data Types & Built-ins
1. **Type conversions** (`str()`, `int()`, `bool()`)
2. **Container creation** (`list()`, `dict()`)
3. **Length function** (`len()`)
4. **Mathematical functions** (`abs()`, `max()`, `min()`)

### Phase 3: Advanced Features
1. **List/dict methods** (`.append()`, `.get()`, etc.)
2. **String methods** (`.upper()`, `.lower()`, `.split()`)
3. **Iterator functions** (`range()`, `enumerate()`, `zip()`)
4. **Error handling** (`try`/`except`)

### Phase 4: Standard Library
1. **File I/O** (`open()`, `read()`, `write()`)
2. **Math module** (`math.sqrt()`, `math.sin()`, etc.)
3. **Regular expressions**
4. **Date/time operations**

## 8. Mapping Strategy

### Parser Integration
```python
# In aimacro_parser.py
def map_python_builtin(self, func_name, args):
    """Map Python built-in to AILang equivalent"""
    builtin_mappings = {
        'print': lambda args: FunctionCall('PrintMessage', args),
        'len': lambda args: FunctionCall('ArrayLength', args),
        'str': lambda args: FunctionCall('NumberToString', args),
        'int': lambda args: FunctionCall('StringToNumber', args),
        # ... more mappings
    }
    
    if func_name in builtin_mappings:
        return builtin_mappings[func_name](args)
    else:
        # Regular function call
        return FunctionCall(func_name, args)
```

### Code Generation
```python
# In ailang compilation phase
def compile_python_builtin(self, node):
    """Generate AILang AST for Python built-ins"""
    if isinstance(node, PythonBuiltin):
        if node.name == 'print':
            return PrintMessage(node.args[0])
        elif node.name == 'len':
            return ArrayLength(node.args[0])
        # ... handle other built-ins
```

## 9. Testing Framework

### Test Structure
```python
# test_builtin_mappings.py
def test_print_mapping():
    aimacro_code = 'print("Hello");'
    expected_ailang = 'PrintMessage("Hello")'
    assert convert_and_compile(aimacro_code) == expected_ailang

def test_len_mapping():
    aimacro_code = 'length = len(my_array);'
    expected_ailang = 'length = ArrayLength(my_array)'
    assert convert_and_compile(aimacro_code) == expected_ailang
```

## 10. Hybrid Operator Strategy

### Design Philosophy
AIMacro uses **hybrid operator mapping** to avoid implicit conversion chaos while maintaining readability:

1. **Simple infix operators** translate directly to AILang infix where available
2. **Complex expressions** use explicit AILang function calls  
3. **Logical operators** always use AILang keywords for clarity
4. **Nested operations** automatically use function notation

### Expression Complexity Examples

**Level 1: Simple Direct Translation**
```python
# AIMacro
x = a + b;
y = c * d;
flag = i < 10;
```
**Compiles to AILang:**
```ailang
x = a + b
y = c * d  
flag = i < 10
```

**Level 2: Mixed Operations (Auto-detect)**
```python
# AIMacro - Parser detects complexity
result = a + b * c + d;  # Precedence clear
```
**Compiles to AILang:**
```ailang
temp1 = b * c
temp2 = a + temp1
result = temp2 + d
```

**Level 3: Explicit Function Style (Developer Choice)**
```python
# AIMacro - Developer chooses explicit style for clarity
result = Add(a, Multiply(b, c));
condition = And(GreaterThan(x, 0), LessThan(x, 100));
```
**Compiles to AILang:**
```ailang
result = Add(a, Multiply(b, c))
condition = And(GreaterThan(x, 0), LessThan(x, 100))
```

### Parser Rules
1. **Single operator expressions** → Direct infix
2. **Multiple same-precedence operators** → Direct infix with temp vars
3. **Mixed precedence** → Function calls or temp variables
4. **Logical combinations** → Always use `And()`, `Or()`, `Not()`
5. **Developer explicit functions** → Pass through unchanged

### Benefits
- ✅ **No implicit chaos**: Clear rules for when functions are used
- ✅ **Python familiarity**: Simple expressions look exactly like Python
- ✅ **AILang power**: Complex logic uses explicit, readable functions
- ✅ **Developer control**: Can choose explicit style anytime
- ✅ **Maintainability**: Easy to understand what maps where