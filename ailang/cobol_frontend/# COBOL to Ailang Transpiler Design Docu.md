# COBOL to Ailang Transpiler Design Document

## Overview

This document outlines the design and implementation of a COBOL to Ailang transpiler, following the same architecture as the existing AIMacro (Python-like) to Ailang transpiler.

## Architecture

```
COBOL Source (.cbl, .cob)
    ↓
Lexer (cobol_lexer.py) → Tokens
    ↓
Parser (cobol_parser.py) → COBOL AST
    ↓
Converter (cobol_ast_converter.py) → Ailang AST
    ↓
Serializer → Ailang Source (.ailang)
    ↓
main.py (Ailang Compiler) → Native Binary
```

## Directory Structure

```
cobol_frontend/
├── __init__.py
├── cobol_lexer.py          # Tokenizes COBOL source
├── cobol_parser.py         # Builds COBOL AST
├── cobol_ast_converter.py  # Converts COBOL AST → Ailang AST
├── cobol_integration.py    # Main compiler pipeline
└── tests/
    ├── test_lexer.py
    ├── test_parser.py
    ├── test_converter.py
    └── samples/
        └── hello.cbl
```

## COBOL Language Features to Support

### Phase 1: Core Features (MVP)
1. **IDENTIFICATION DIVISION** - Program metadata
2. **DATA DIVISION** - Variable declarations
   - WORKING-STORAGE SECTION (local variables)
   - File descriptions (basic)
3. **PROCEDURE DIVISION** - Code execution
   - DISPLAY statement → `PrintMessage()`
   - MOVE statement → Variable assignment
   - COMPUTE → Arithmetic operations
   - IF/ELSE/END-IF → `IfCondition`
   - PERFORM...UNTIL → `WhileLoop`
   - PERFORM paragraph-name → Function calls
4. **Basic Data Types**
   - PIC 9(n) → Integer
   - PIC X(n) → String
   - PIC S9(n) → Signed integer

### Phase 2: Extended Features
1. **PERFORM...TIMES** → Counted loops
2. **PERFORM...VARYING** → For loops
3. **Nested programs** → Multiple functions
4. **EVALUATE** → ChoosePath (switch)
5. **STRING/UNSTRING** → String operations
6. **INSPECT** → String manipulation
7. **CALL** → External function calls

### Phase 3: Advanced Features
1. **FILE-CONTROL** → File I/O
2. **OPEN/READ/WRITE/CLOSE** → File operations
3. **Tables (OCCURS)** → Arrays/Collections
4. **REDEFINES** → Memory overlays
5. **COPY** → Include files

## COBOL to Ailang Mappings

### Program Structure

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO-WORLD.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 MESSAGE PIC X(20) VALUE "Hello, World!".
01 COUNTER PIC 9(4) VALUE 0.

PROCEDURE DIVISION.
    DISPLAY MESSAGE.
    MOVE 42 TO COUNTER.
    DISPLAY COUNTER.
    STOP RUN.
```

**Maps to Ailang:**

```ailang
// Program: HELLO-WORLD
// Generated from COBOL source

Function.Main {
    Input: 
    Output: Integer
    Body: {
        // WORKING-STORAGE SECTION
        MESSAGE = "Hello, World!"
        COUNTER = 0
        
        // PROCEDURE DIVISION
        PrintMessage(MESSAGE)
        COUNTER = 42
        PrintNumber(COUNTER)
        
        ReturnValue(0)
    }
}
```

### Data Type Mappings

| COBOL Declaration | Ailang Equivalent |
|-------------------|-------------------|
| `01 X PIC 9(4).` | `X = 0` (Integer) |
| `01 NAME PIC X(20).` | `NAME = ""` (String) |
| `01 FLAG PIC 9 VALUE 1.` | `FLAG = 1` |
| `01 AMOUNT PIC S9(5).` | `AMOUNT = 0` (Signed) |
| `01 TABLE OCCURS 10 TIMES.` | `TABLE = Collection.CreateList()` |

### Statement Mappings

| COBOL Statement | Ailang Equivalent |
|-----------------|-------------------|
| `DISPLAY "text"` | `PrintMessage("text")` |
| `DISPLAY var` | `PrintNumber(var)` or `PrintMessage(var)` |
| `MOVE X TO Y` | `Y = X` |
| `ADD A TO B` | `B = Add(B, A)` |
| `SUBTRACT A FROM B` | `B = Subtract(B, A)` |
| `MULTIPLY A BY B` | `B = Multiply(A, B)` |
| `DIVIDE A INTO B` | `B = Divide(B, A)` |
| `COMPUTE X = A + B` | `X = Add(A, B)` |
| `IF X = Y` | `IfCondition EqualTo(X, Y) ThenBlock: {` |
| `PERFORM PARA-1` | `RunTask(PARA_1)` or function call |
| `PERFORM UNTIL X > 10` | `WhileLoop LessThan(X, 11) {` |
| `STOP RUN` | `ReturnValue(0)` |

### Control Flow

#### IF-ELSE

```cobol
IF COUNTER > 10
    DISPLAY "High"
ELSE
    DISPLAY "Low"
END-IF.
```

**Maps to:**

```ailang
IfCondition GreaterThan(COUNTER, 10) ThenBlock: {
    PrintMessage("High")
} ElseBlock: {
    PrintMessage("Low")
}
```

#### PERFORM UNTIL (While Loop)

```cobol
PERFORM UNTIL COUNTER = 10
    ADD 1 TO COUNTER
    DISPLAY COUNTER
END-PERFORM.
```

**Maps to:**

```ailang
WhileLoop NotEqual(COUNTER, 10) {
    COUNTER = Add(COUNTER, 1)
    PrintNumber(COUNTER)
}
```

#### PERFORM paragraph (Subroutine)

```cobol
PROCEDURE DIVISION.
MAIN-PARA.
    PERFORM SUB-PARA.
    STOP RUN.

SUB-PARA.
    DISPLAY "In subroutine".
```

**Maps to:**

```ailang
SubRoutine.SUB_PARA {
    PrintMessage("In subroutine")
}

Function.Main {
    Input:
    Output: Integer
    Body: {
        RunTask(SUB_PARA)
        ReturnValue(0)
    }
}
```

## Lexer Design

### Token Types

```python
class COBOLTokenType(Enum):
    # Divisions
    IDENTIFICATION = "IDENTIFICATION"
    DIVISION = "DIVISION"
    PROGRAM_ID = "PROGRAM-ID"
    DATA = "DATA"
    WORKING_STORAGE = "WORKING-STORAGE"
    SECTION = "SECTION"
    PROCEDURE = "PROCEDURE"
    
    # Data declarations
    LEVEL_NUMBER = "LEVEL_NUMBER"  # 01, 02, 77, etc.
    PIC = "PIC"
    VALUE = "VALUE"
    OCCURS = "OCCURS"
    TIMES = "TIMES"
    REDEFINES = "REDEFINES"
    
    # Statements
    DISPLAY = "DISPLAY"
    ACCEPT = "ACCEPT"
    MOVE = "MOVE"
    TO = "TO"
    FROM = "FROM"
    INTO = "INTO"
    COMPUTE = "COMPUTE"
    ADD = "ADD"
    SUBTRACT = "SUBTRACT"
    MULTIPLY = "MULTIPLY"
    DIVIDE = "DIVIDE"
    BY = "BY"
    GIVING = "GIVING"
    
    # Control flow
    IF = "IF"
    THEN = "THEN"
    ELSE = "ELSE"
    END_IF = "END-IF"
    PERFORM = "PERFORM"
    UNTIL = "UNTIL"
    VARYING = "VARYING"
    END_PERFORM = "END-PERFORM"
    EVALUATE = "EVALUATE"
    WHEN = "WHEN"
    END_EVALUATE = "END-EVALUATE"
    
    # Operators
    EQUAL = "EQUAL"
    EQUALS = "="
    GREATER = ">"
    LESS = "<"
    NOT = "NOT"
    AND = "AND"
    OR = "OR"
    
    # Literals
    STRING_LITERAL = "STRING_LITERAL"
    NUMBER_LITERAL = "NUMBER_LITERAL"
    IDENTIFIER = "IDENTIFIER"
    
    # Special
    PERIOD = "."
    COMMA = ","
    LPAREN = "("
    RPAREN = ")"
    STOP_RUN = "STOP"
    GOBACK = "GOBACK"
    
    # Meta
    NEWLINE = "NEWLINE"
    COMMENT = "COMMENT"
    EOF = "EOF"
```

### Lexer Features

1. **Free-form and Fixed-form support**
   - Fixed: Columns 1-6 (sequence), 7 (indicator), 8-72 (code)
   - Free: Modern COBOL without column restrictions

2. **Case insensitivity**
   - COBOL keywords are case-insensitive
   - Normalize to uppercase during lexing

3. **Comment handling**
   - `*` in column 7 (fixed-form)
   - `*>` anywhere (free-form)

## Parser Design

### COBOL AST Node Types

```python
class COBOLASTNode:
    pass

class COBOLProgram(COBOLASTNode):
    def __init__(self, program_id, data_division, procedure_division):
        self.program_id = program_id
        self.data_division = data_division
        self.procedure_division = procedure_division

class COBOLDataDivision(COBOLASTNode):
    def __init__(self, working_storage):
        self.working_storage = working_storage

class COBOLVariableDecl(COBOLASTNode):
    def __init__(self, level, name, pic, value):
        self.level = level
        self.name = name
        self.pic = pic
        self.value = value

class COBOLProcedureDivision(COBOLASTNode):
    def __init__(self, paragraphs):
        self.paragraphs = paragraphs

class COBOLParagraph(COBOLASTNode):
    def __init__(self, name, statements):
        self.name = name
        self.statements = statements

class COBOLDisplay(COBOLASTNode):
    def __init__(self, expression):
        self.expression = expression

class COBOLMove(COBOLASTNode):
    def __init__(self, source, target):
        self.source = source
        self.target = target

class COBOLIf(COBOLASTNode):
    def __init__(self, condition, then_stmts, else_stmts):
        self.condition = condition
        self.then_stmts = then_stmts
        self.else_stmts = else_stmts

class COBOLPerform(COBOLASTNode):
    def __init__(self, paragraph_name, until_condition):
        self.paragraph_name = paragraph_name
        self.until_condition = until_condition

class COBOLCompute(COBOLASTNode):
    def __init__(self, target, expression):
        self.target = target
        self.expression = expression

class COBOLArithmetic(COBOLASTNode):
    def __init__(self, operation, operands, target):
        self.operation = operation  # ADD, SUBTRACT, etc.
        self.operands = operands
        self.target = target
```

## Converter Design

### Type Inference

COBOL's PIC clauses determine data types:

```python
def infer_ailang_type(pic_clause):
    """Convert COBOL PIC to Ailang type"""
    if pic_clause.startswith('9') or pic_clause.startswith('S9'):
        return 'Integer'
    elif pic_clause.startswith('X'):
        return 'String'
    elif pic_clause in ['A', 'a']:
        return 'String'
    else:
        return 'Integer'  # Default

def get_default_value(pic_clause):
    """Get default initialization value"""
    if pic_clause.startswith('9') or pic_clause.startswith('S9'):
        return 0
    elif pic_clause.startswith('X'):
        return '""'
    else:
        return 0
```

### Name Normalization

COBOL uses hyphens in identifiers, Ailang uses underscores:

```python
def normalize_name(cobol_name):
    """Convert COBOL-STYLE-NAME to COBOL_STYLE_NAME"""
    return cobol_name.replace('-', '_').upper()
```

### Expression Conversion

```python
def convert_expression(self, expr):
    """Convert COBOL expression to Ailang"""
    if isinstance(expr, COBOLBinaryOp):
        op_map = {
            '+': 'Add',
            '-': 'Subtract',
            '*': 'Multiply',
            '/': 'Divide',
            '=': 'EqualTo',
            '>': 'GreaterThan',
            '<': 'LessThan'
        }
        func_name = op_map[expr.operator]
        left = self.convert_expression(expr.left)
        right = self.convert_expression(expr.right)
        return FunctionCall(func_name, [left, right])
    elif isinstance(expr, COBOLIdentifier):
        return Identifier(normalize_name(expr.name))
    elif isinstance(expr, COBOLNumberLiteral):
        return Number(expr.value)
    elif isinstance(expr, COBOLStringLiteral):
        return String(expr.value)
```

## Integration Module

Similar to `aimacro_frontend/integration.py`:

```python
class COBOLIntegratedCompiler:
    def __init__(self, debug=False):
        self.debug = debug
    
    def compile_file(self, input_file: str, output_file: str):
        # Stage 1: Lexing
        with open(input_file, 'r') as f:
            source = f.read()
        lexer = COBOLLexer(source)
        tokens = lexer.tokenize()
        
        # Stage 2: Parsing
        parser = COBOLParser(tokens)
        cobol_ast = parser.parse()
        
        # Stage 3: Conversion
        converter = COBOLToAilangConverter()
        ailang_ast = converter.convert(cobol_ast)
        
        # Stage 4: Serialization
        serializer = AILangASTSerializer()
        ailang_source = serializer.serialize(ailang_ast)
        
        # Stage 5: Write .ailang file
        temp_ailang = output_file + '.ailang'
        with open(temp_ailang, 'w') as f:
            f.write(ailang_source)
        
        # Stage 6: Invoke main.py
        subprocess.run([
            sys.executable, 'main.py', 
            temp_ailang, '-o', output_file
        ])
```

## Example Translation

### Input COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATE-SUM.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUM1 PIC 9(4) VALUE 0.
       01 NUM2 PIC 9(4) VALUE 0.
       01 RESULT PIC 9(5) VALUE 0.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           MOVE 100 TO NUM1.
           MOVE 250 TO NUM2.
           COMPUTE RESULT = NUM1 + NUM2.
           DISPLAY "Result: " RESULT.
           IF RESULT > 300
               DISPLAY "Large sum"
           ELSE
               DISPLAY "Small sum"
           END-IF.
           STOP RUN.
```

### Output Ailang

```ailang
// Program: CALCULATE_SUM
// Generated from COBOL source

Function.Main {
    Input:
    Output: Integer
    Body: {
        // WORKING-STORAGE SECTION
        NUM1 = 0
        NUM2 = 0
        RESULT = 0
        
        // PROCEDURE DIVISION - MAIN_LOGIC
        NUM1 = 100
        NUM2 = 250
        RESULT = Add(NUM1, NUM2)
        PrintMessage("Result: ")
        PrintNumber(RESULT)
        
        IfCondition GreaterThan(RESULT, 300) ThenBlock: {
            PrintMessage("Large sum")
        } ElseBlock: {
            PrintMessage("Small sum")
        }
        
        ReturnValue(0)
    }
}
```

## Testing Strategy

### Unit Tests

1. **Lexer Tests** (`test_lexer.py`)
   - Tokenize simple COBOL programs
   - Handle fixed vs free format
   - Parse comments correctly

2. **Parser Tests** (`test_parser.py`)
   - Parse minimal program structure
   - Parse DATA DIVISION declarations
   - Parse PROCEDURE DIVISION statements
   - Handle nested IF statements

3. **Converter Tests** (`test_converter.py`)
   - Convert simple programs
   - Test type inference
   - Verify statement mappings

### Integration Tests

1. Hello World program
2. Arithmetic operations
3. Conditionals
4. Loops
5. Subroutines

## Implementation Phases

### Phase 1: Foundation (Week 1)
- [ ] Basic lexer with core tokens
- [ ] Simple parser for minimal program
- [ ] Basic converter for DISPLAY/MOVE
- [ ] Integration pipeline
- [ ] Hello World test

### Phase 2: Core Features (Week 2)
- [ ] Complete DATA DIVISION support
- [ ] Arithmetic operations (COMPUTE, ADD, etc.)
- [ ] IF/ELSE statements
- [ ] PERFORM UNTIL loops
- [ ] Multiple test programs

### Phase 3: Advanced Features (Week 3)
- [ ] PERFORM paragraph calls
- [ ] Nested structures
- [ ] EVALUATE statements
- [ ] String operations
- [ ] More complex examples

### Phase 4: Polish (Week 4)
- [ ] Error messages
- [ ] Debug output
- [ ] Documentation
- [ ] Full test suite
- [ ] Example programs

## Command-Line Usage

```bash
# Compile COBOL to native binary
python3 cobol_frontend/cobol_integration.py program.cbl -o program

# With debug output
python3 cobol_frontend/cobol_integration.py program.cbl -o program --debug

# Just generate Ailang (no compilation)
python3 cobol_frontend/cobol_integration.py program.cbl --ailang-only
```

## Next Steps

1. Create `cobol_frontend/` directory
2. Implement `cobol_lexer.py` with basic tokenization
3. Implement `cobol_parser.py` with minimal program support
4. Implement `cobol_ast_converter.py` for basic conversions
5. Set up integration pipeline
6. Write first test case (Hello World)
7. Iterate and expand features

## References

- AIMacro transpiler: `aimacro_frontend/`
- Ailang AST nodes: `ailang_parser/ailang_ast.py`
- Ailang compiler: `main.py`
- Ailang syntax reference: Project knowledge documents