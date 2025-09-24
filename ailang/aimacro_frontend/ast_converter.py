# aimacro_frontend/ast_converter.py
"""
AIMacro AST Converter - Converts AIMacro AST nodes to AILang AST nodes
"""

from typing import List, Dict, Any, Optional, Union
import sys
import os

# Add the parent directory (ailang/) to the path so we can import ailang_parser
project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

# Import AILang AST nodes from the correct location
try:
    from ailang_parser.ailang_ast import (
        Program, Function, If, While, ForEvery, ReturnValue, 
        Assignment, FunctionCall, Identifier, Number, String
    )
    # LibraryImport doesn't exist as an AST node - it's handled in parsing
    LibraryImport = None
except ImportError as e:
    print(f"Error: Failed to import from ailang_parser.ailang_ast: {e}", file=sys.stderr)
    print(f"Project root: {project_root}", file=sys.stderr)
    print(f"Python path: {sys.path}", file=sys.stderr)
    sys.exit(1)

class AIMacroASTNode:
    """Base class for AIMacro AST nodes"""
    def __init__(self, line: int, column: int):
        self.line = line
        self.column = column

class AIMacroFunction(AIMacroASTNode):
    def __init__(self, name: str, params: List[tuple], body: List[AIMacroASTNode], 
                 return_type: Optional[str], line: int, column: int):
        super().__init__(line, column)
        self.name = name
        self.params = params
        self.body = body
        self.return_type = return_type

class AIMacroIf(AIMacroASTNode):
    def __init__(self, condition: AIMacroASTNode, then_body: List[AIMacroASTNode], 
                 else_body: Optional[List[AIMacroASTNode]], line: int, column: int):
        super().__init__(line, column)
        self.condition = condition
        self.then_body = then_body
        self.else_body = else_body

class AIMacroWhile(AIMacroASTNode):
    def __init__(self, condition: AIMacroASTNode, body: List[AIMacroASTNode], line: int, column: int):
        super().__init__(line, column)
        self.condition = condition
        self.body = body

class AIMacroFor(AIMacroASTNode):
    def __init__(self, var: str, iterable: AIMacroASTNode, body: List[AIMacroASTNode], line: int, column: int):
        super().__init__(line, column)
        self.var = var
        self.iterable = iterable
        self.body = body

class AIMacroReturn(AIMacroASTNode):
    def __init__(self, value: Optional[AIMacroASTNode], line: int, column: int):
        super().__init__(line, column)
        self.value = value

class AIMacroAssignment(AIMacroASTNode):
    def __init__(self, target: str, value: AIMacroASTNode, line: int, column: int):
        super().__init__(line, column)
        self.target = target
        self.value = value

class AIMacroBinaryOp(AIMacroASTNode):
    def __init__(self, left: AIMacroASTNode, operator: str, right: AIMacroASTNode, line: int, column: int):
        super().__init__(line, column)
        self.left = left
        self.operator = operator
        self.right = right

class AIMacroUnaryOp(AIMacroASTNode):
    def __init__(self, operator: str, operand: AIMacroASTNode, line: int, column: int):
        super().__init__(line, column)
        self.operator = operator
        self.operand = operand

class AIMacroFunctionCall(AIMacroASTNode):
    def __init__(self, name: str, args: List[AIMacroASTNode], line: int, column: int):
        super().__init__(line, column)
        self.name = name
        self.args = args

class AIMacroMethodCall(AIMacroASTNode):
    def __init__(self, object_expr: AIMacroASTNode, method: str, args: List[AIMacroASTNode], line: int, column: int):
        super().__init__(line, column)
        self.object = object_expr
        self.method = method
        self.args = args

class AIMacroIndexAssignment(AIMacroASTNode):
    def __init__(self, expression: AIMacroASTNode, index: AIMacroASTNode, value: AIMacroASTNode, line: int, column: int):
        super().__init__(line, column)
        self.expression = expression
        self.index = index
        self.value = value

class AIMacroIndexAccess(AIMacroASTNode):
    def __init__(self, expression: AIMacroASTNode, index: AIMacroASTNode, line: int, column: int):
        super().__init__(line, column)
        self.expression = expression
        self.index = index

class AIMacroIdentifier(AIMacroASTNode):
    def __init__(self, name: str, line: int, column: int):
        super().__init__(line, column)
        self.name = name

class AIMacroNumber(AIMacroASTNode):
    def __init__(self, value: Union[int, float], line: int, column: int):
        super().__init__(line, column)
        self.value = value

class AIMacroString(AIMacroASTNode):
    def __init__(self, value: str, line: int, column: int):
        super().__init__(line, column)
        self.value = value

class AIMacroBoolean(AIMacroASTNode):
    def __init__(self, value: bool, line: int, column: int):
        super().__init__(line, column)
        self.value = value

class AIMacroList(AIMacroASTNode):
    def __init__(self, elements: List[AIMacroASTNode], line: int, column: int):
        super().__init__(line, column)
        self.elements = elements

class AIMacroDict(AIMacroASTNode):
    def __init__(self, pairs: List[tuple], line: int, column: int):
        super().__init__(line, column)
        self.pairs = pairs

class AIMacroProgram(AIMacroASTNode):
    def __init__(self, statements: List[AIMacroASTNode]):
        super().__init__(1, 1)
        self.statements = statements

class AIMacroToAILangConverter:
    """Converts AIMacro AST nodes to AILang AST nodes"""
    
    def __init__(self):
        # Map Python built-ins to AIMacro library functions
        self.builtin_mappings = {
            'len': 'AIMacro.Len', 
            'str': 'AIMacro.Str',
            'int': 'AIMacro.Int',
            'input': 'AIMacro.Input',
            'bool': 'AIMacro.Bool',
            'abs': 'AIMacro.Abs',
            'max': 'AIMacro.Max',
            'min': 'AIMacro.Min',
            'sum': 'AIMacro.Sum',
            'list': 'AIMacro.List',
            'dict': 'AIMacro.Dict',
            'tuple': 'AIMacro.Tuple',
            'range': 'AIMacro.Range',
            'enumerate': 'AIMacro.Enumerate',
            'zip': 'AIMacro.Zip',
            'open': 'AIMacro.Open',
            'input': 'AIMacro.Input',
        }
        
        # Map operators to AILang function calls
        self.operator_mappings = {
            '+': 'Add',
            '-': 'Subtract',
            '*': 'Multiply',
            '/': 'Divide',
            '%': 'Modulo',
            '**': 'Power',
            '==': 'EqualTo',
            '!=': 'NotEqual',
            '<': 'LessThan',
            '<=': 'LessEqual',  # Changed from 'LessOrEqual'
            '>': 'GreaterThan',
            '>=': 'GreaterEqual',  # Changed from 'GreaterOrEqual'
            'and': 'And',
            'or': 'Or',
            'not': 'Not',
        }
        
        # Track variable types for method resolution
        self.variable_types = {}

    def convert(self, aimacro_ast: AIMacroASTNode) -> Any:
        """Main conversion method - dispatches to specific converters"""
        if aimacro_ast is None:
            return None
            
        converter_method = f'convert_{type(aimacro_ast).__name__}'
        if hasattr(self, converter_method):
            return getattr(self, converter_method)(aimacro_ast)
        else:
            raise ValueError(f"No converter for {type(aimacro_ast).__name__}")

    def convert_AIMacroBinaryOp(self, node: AIMacroBinaryOp) -> Any:
        """Convert binary operation to AILang function call"""
        left = self.convert(node.left)
        right = self.convert(node.right)
        
        mapping = self.operator_mappings.get(node.operator)
        
        if mapping:
            return FunctionCall(line=node.line, column=node.column, function=mapping, arguments=[left, right])
        else:
            raise ValueError(f"Unknown operator: {node.operator}")

    def convert_AIMacroProgram(self, node: AIMacroProgram) -> Program:
        """Convert program without library import (handled at parse time)"""
        declarations = []
        
        # Convert all statements
        for stmt in node.statements:
            converted = self.convert(stmt)
            if converted:
                declarations.append(converted)
        
        return Program(line=node.line, column=node.column, declarations=declarations)
    
    def convert_AIMacroFunction(self, node: AIMacroFunction) -> Function:
        """Convert function definition"""
        input_params = []
        for param_name, param_type in node.params:
            ailang_type = self.convert_type(param_type) if param_type else "Integer"
            input_params.append((param_name, ailang_type))
        
        body = []
        for stmt in node.body:
            converted = self.convert(stmt)
            if converted:
                body.append(converted)
        
        output_type = self.convert_type(node.return_type) if node.return_type else None
        
        return Function(
            line=node.line,
            column=node.column,
            name=node.name,
            input_params=input_params,
            output_type=output_type,
            body=body
        )
    
    def convert_AIMacroIf(self, node: AIMacroIf) -> If:
        """Convert if statement"""
        condition = self.convert(node.condition)
        then_body = [self.convert(stmt) for stmt in node.then_body]
        else_body = [self.convert(stmt) for stmt in node.else_body] if node.else_body else []
        
        return If(line=node.line, column=node.column, condition=condition, then_body=then_body, else_body=else_body)
    
    def convert_AIMacroWhile(self, node: AIMacroWhile) -> While:
        """Convert while loop"""
        condition = self.convert(node.condition)
        body = [self.convert(stmt) for stmt in node.body]
        
        return While(line=node.line, column=node.column, condition=condition, body=body)
    
    def convert_AIMacroFor(self, node: AIMacroFor) -> ForEvery:
        """Convert for loop"""
        var = node.var
        iterable = self.convert(node.iterable)
        body = [self.convert(stmt) for stmt in node.body]
        
        return ForEvery(line=node.line, column=node.column, variable=var, collection=iterable, body=body)
    
    def convert_AIMacroReturn(self, node: AIMacroReturn) -> ReturnValue:
        """Convert return statement"""
        if node.value:
            value = self.convert(node.value)
            return ReturnValue(line=node.line, column=node.column, value=value)
        else:
            default_val = Number(line=node.line, column=node.column, value=0)
            return ReturnValue(line=node.line, column=node.column, value=default_val)
    
    def convert_AIMacroAssignment(self, node: AIMacroAssignment) -> Assignment:
        """Convert assignment statement"""
        value = self.convert(node.value)
        
        # Track variable type for method resolution and print handling
        if isinstance(node.value, AIMacroFunctionCall):
            if node.value.name in ['list', 'AIMacro.List']:
                self.variable_types[node.target] = 'list'
            elif node.value.name in ['dict', 'AIMacro.Dict']:
                self.variable_types[node.target] = 'dict'
            elif node.value.name in ['str', 'AIMacro.Str']:
                self.variable_types[node.target] = 'str'
        elif isinstance(node.value, AIMacroString):
            self.variable_types[node.target] = 'str'
        elif isinstance(node.value, AIMacroNumber):
            self.variable_types[node.target] = 'number'
        elif isinstance(node.value, AIMacroBinaryOp):
            # Arithmetic operations produce numbers
            if node.value.operator in ['+', '-', '*', '/', '%', '**']:
                self.variable_types[node.target] = 'number'
        
        return Assignment(line=node.line, column=node.column, target=node.target, value=value)
    
    def convert_AIMacroUnaryOp(self, node: AIMacroUnaryOp) -> Any:
        """Convert unary operation"""
        operand = self.convert(node.operand)
        
        if node.operator == 'not':
            return FunctionCall(line=node.line, column=node.column, function='Not', arguments=[operand])
        elif node.operator == '-':
            zero = Number(line=node.line, column=node.column, value=0)
            return FunctionCall(line=node.line, column=node.column, function='Subtract', arguments=[zero, operand])
        elif node.operator == '+':
            return operand
        else:
            raise ValueError(f"Unknown unary operator: {node.operator}")
    
    def convert_AIMacroFunctionCall(self, node: AIMacroFunctionCall) -> FunctionCall:
        """Convert function call"""
        args = [self.convert(arg) for arg in node.args]
        
        # Special handling for print function
        if node.name == 'print':
            if len(args) == 1:
                arg = node.args[0]
                
                # Check argument type
                if isinstance(arg, AIMacroNumber):
                    # Literal number - use PrintNumber
                    return FunctionCall(
                        line=node.line,
                        column=node.column,
                        function='PrintNumber',
                        arguments=args
                    )
                elif isinstance(arg, AIMacroString):
                    # String literal - use PrintMessage
                    return FunctionCall(
                        line=node.line,
                        column=node.column,
                        function='PrintMessage',
                        arguments=args
                    )
                elif isinstance(arg, AIMacroIdentifier):
                    # Variable - check its type
                    var_type = self.variable_types.get(arg.name, 'unknown')
                    if var_type == 'str':
                        return FunctionCall(
                            line=node.line,
                            column=node.column,
                            function='PrintMessage',
                            arguments=args
                        )
                    else:
                        # Assume numeric or unknown
                        return FunctionCall(
                            line=node.line,
                            column=node.column,
                            function='PrintNumber',
                            arguments=args
                        )
                else:
                    # Expression result - use PrintNumber
                    return FunctionCall(
                        line=node.line,
                        column=node.column,
                        function='PrintNumber',
                        arguments=args
                    )
            # Multiple args or no args - use PrintMessage
            return FunctionCall(
                line=node.line,
                column=node.column,
                function='PrintMessage',
                arguments=args
            )
        
        # Map other Python built-ins
        if node.name in self.builtin_mappings:
            func_name = self.builtin_mappings[node.name]
            
            if node.name == 'range':
                args = self.process_range_args(args, node.line, node.column)
            
            return FunctionCall(line=node.line, column=node.column, function=func_name, arguments=args)
        else:
            return FunctionCall(line=node.line, column=node.column, function=node.name, arguments=args)
    
    def convert_AIMacroMethodCall(self, node: AIMacroMethodCall) -> FunctionCall:
        """Convert method call to function call"""
        object_expr = self.convert(node.object)
        args = [self.convert(arg) for arg in node.args]
        
        object_type = self.infer_object_type(node.object)
        
        method_mappings = {
            'str': {
                'upper': 'AIMacro.StrUpper',
                'lower': 'AIMacro.StrLower',
                'split': 'AIMacro.StrSplit',
            },
            'list': {
                'append': 'AIMacro.ListAppend',
                'pop': 'AIMacro.ListPop',
                'insert': 'AIMacro.ListInsert',
                'remove': 'AIMacro.ListRemove',
            }
        }
        
        if object_type in method_mappings and node.method in method_mappings[object_type]:
            func_name = method_mappings[object_type][node.method]
            return FunctionCall(line=node.line, column=node.column, function=func_name, arguments=[object_expr] + args)
        else:
            raise ValueError(f"Unknown method: {object_type}.{node.method}")
    
    def convert_AIMacroIndexAssignment(self, node: AIMacroIndexAssignment) -> FunctionCall:
        """Convert index assignment to a function call"""
        expression = self.convert(node.expression)
        index = self.convert(node.index)
        value = self.convert(node.value)
        return FunctionCall(line=node.line, column=node.column, function='AIMacro.Set', arguments=[expression, index, value])

    def convert_AIMacroIndexAccess(self, node: AIMacroIndexAccess) -> FunctionCall:
        """Convert index access to a function call"""
        expression = self.convert(node.expression)
        index = self.convert(node.index)
        return FunctionCall(line=node.line, column=node.column, function='AIMacro.Get', arguments=[expression, index])

    def convert_AIMacroIdentifier(self, node: AIMacroIdentifier) -> Identifier:
        """Convert identifier"""
        return Identifier(line=node.line, column=node.column, name=node.name)
    
    def convert_AIMacroNumber(self, node: AIMacroNumber) -> Number:
        """Convert number literal"""
        return Number(line=node.line, column=node.column, value=node.value)
    
    def convert_AIMacroString(self, node: AIMacroString) -> String:
        """Convert string literal"""
        return String(line=node.line, column=node.column, value=node.value)
    
    def convert_AIMacroBoolean(self, node: AIMacroBoolean) -> Number:
        """Convert boolean to number"""
        return Number(line=node.line, column=node.column, value=1 if node.value else 0)
    
    def convert_AIMacroList(self, node: AIMacroList) -> FunctionCall:
        """Convert list literal"""
        elements = [self.convert(elem) for elem in node.elements]
        return FunctionCall(line=node.line, column=node.column, function='AIMacro.CreateListWithElements', arguments=elements)
    
    def convert_AIMacroDict(self, node: AIMacroDict) -> FunctionCall:
        """Convert dict literal"""
        pairs = []
        for key, value in node.pairs:
            pairs.extend([self.convert(key), self.convert(value)])
        
        return FunctionCall(line=node.line, column=node.column, function='AIMacro.CreateDictWithPairs', arguments=pairs)
    
    def process_range_args(self, args: List[Any], line: int, column: int) -> List[Any]:
        """Process range arguments"""
        if len(args) == 1:
            return [Number(line=line, column=column, value=0), args[0], Number(line=line, column=column, value=1)]
        elif len(args) == 2:
            return [args[0], args[1], Number(line=line, column=column, value=1)]
        else:
            return args[:3]
    
    def convert_type(self, type_hint: Optional[str]) -> str:
        """Convert Python type hint to AILang type"""
        if not type_hint:
            return "Integer"
        
        type_mappings = {
            'int': 'Integer',
            'float': 'FloatingPoint',
            'str': 'Text',
            'bool': 'Boolean',
            'list': 'Array',
            'dict': 'Map',
        }
        
        return type_mappings.get(type_hint, "Any")
    
    def infer_object_type(self, obj: AIMacroASTNode) -> str:
        """Infer the type of an object"""
        if isinstance(obj, AIMacroIdentifier):
            return self.variable_types.get(obj.name, 'unknown')
        elif isinstance(obj, AIMacroString):
            return 'str'
        elif isinstance(obj, AIMacroList):
            return 'list'
        elif isinstance(obj, AIMacroDict):
            return 'dict'
        elif isinstance(obj, AIMacroFunctionCall):
            if obj.name in ['str', 'AIMacro.Str']:
                return 'str'
            elif obj.name in ['list', 'AIMacro.List']:
                return 'list'
            elif obj.name in ['dict', 'AIMacro.Dict']:
                return 'dict'
        
        return 'unknown'

class AILangASTSerializer:
    """Serializes an AILang AST back into AILang source code"""
    
    def __init__(self):
        self.indent_level = 0

    def serialize(self, node: Any) -> str:
        """Public entry point for serialization"""
        if node is None:
            return ""
        method_name = f'serialize_{type(node).__name__}'
        if hasattr(self, method_name):
            return getattr(self, method_name)(node)
        else:
            return f"/* UNKNOWN NODE: {type(node).__name__} */"

    def indent(self) -> str:
        return "    " * self.indent_level

    def serialize_body(self, body: List[Any]) -> str:
        self.indent_level += 1
        serialized_body = "".join([self.serialize(stmt) for stmt in body])
        self.indent_level -= 1
        return serialized_body

    def serialize_Program(self, node: Program) -> str:
        # Add the library import directive at the top
        result = "LibraryImport.AIMacro\n\n"
        
        # Serialize all declarations
        result += "".join([self.serialize(decl) for decl in node.declarations])
        
        # Add RunTask(Main) if we have a main function
        for decl in node.declarations:
            if hasattr(decl, 'name') and decl.name == 'main':
                result += "\nRunTask(Main)\n"  # Capital M
                break
        
        return result

    def serialize_Function(self, node: Function) -> str:
        # If it's main with no params/return, make it a SubRoutine
        if node.name == 'main' and not node.input_params and not node.output_type:
            header = f"SubRoutine.Main {{\n"  # Note: Main with capital M
            self.indent_level += 1
            body = self.serialize_body(node.body)
            self.indent_level -= 1
            footer = "}\n"
            return header + body + footer
        else:
            # Regular function with full syntax
            params = ", ".join([f"{p_name}: {p_type}" for p_name, p_type in node.input_params])
            output_line = f"    Output: {node.output_type}\n" if node.output_type else ""
            
            header = f"Function.{node.name} {{\n"
            if params:
                header += f"    Input: {params}\n"
            header += output_line
            header += "    Body: {\n"
            
            self.indent_level += 1
            body = self.serialize_body(node.body)
            self.indent_level -= 1
            
            footer = "    }\n}\n"
            return header + body + footer

    def serialize_Assignment(self, node: Assignment) -> str:
        value_str = self.serialize_inline(node.value)
        return f"{self.indent()}{node.target} = {value_str}\n"

    def serialize_inline(self, node: Any) -> str:
        """Serialize a node for inline use"""
        if isinstance(node, FunctionCall):
            args = ", ".join([self.serialize_inline(arg) for arg in node.arguments])
            return f"{node.function}({args})"
        elif isinstance(node, Identifier):
            return node.name
        elif isinstance(node, Number):
            return str(node.value)
        elif isinstance(node, String):
            return f'"{node.value.replace('"', '\\"')}"'
        else:
            return str(node)

    def serialize_FunctionCall(self, node: FunctionCall) -> str:
        args = ", ".join([self.serialize_inline(arg) for arg in node.arguments])
        return f"{self.indent()}{node.function}({args})\n"

    def serialize_Identifier(self, node: Identifier) -> str:
        return node.name

    def serialize_Number(self, node: Number) -> str:
        return str(node.value)

    def serialize_String(self, node: String) -> str:
        return f'"{node.value.replace('"', '\\"')}"'

    def serialize_ReturnValue(self, node: ReturnValue) -> str:
        value_str = self.serialize_inline(node.value)
        return f"{self.indent()}ReturnValue({value_str})\n"

    def serialize_If(self, node: If) -> str:
        condition_str = self.serialize_inline(node.condition)
        s = f"{self.indent()}IfCondition {condition_str} ThenBlock: {{\n"
        s += self.serialize_body(node.then_body)
        
        if node.else_body:
            s += f"{self.indent()}}} ElseBlock: {{\n"
            s += self.serialize_body(node.else_body)
            s += f"{self.indent()}}}\n"
        else:
            s += f"{self.indent()}}}\n"
        return s

    def serialize_While(self, node: While) -> str:
        condition_str = self.serialize_inline(node.condition)
        s = f"{self.indent()}WhileLoop {condition_str} {{\n"
        s += self.serialize_body(node.body)
        s += f"{self.indent()}}}\n"
        return s

    def serialize_ForEvery(self, node: ForEvery) -> str:
        collection_str = self.serialize_inline(node.collection)
        s = f"{self.indent()}ForEvery {node.variable} in {collection_str} {{\n"
        s += self.serialize_body(node.body)
        s += f"{self.indent()}}}\n"
        return s