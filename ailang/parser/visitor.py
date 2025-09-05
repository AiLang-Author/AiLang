
# visitor.py
from ailang_ast import ASTNode, Program, Library, Pool, ResourceItem, Loop, SubRoutine, Function, RunTask, PrintMessage, ReturnValue, If, While, ForEvery, Assignment, MathExpression, FunctionCall, Identifier, Number, String, Boolean, ArrayLiteral, TypeExpression, ChoosePath, Try, SendMessage, ReceiveMessage, EveryInterval, WithSecurity, BreakLoop, ContinueLoop, HaltProgram, Lambda, Combinator, MacroBlock, MacroDefinition, SecurityContext, SecurityLevel, ConstrainedType, Constant, Apply, RunMacro, MapLiteral, SubPool

class ASTVisitor:
    """Base class for AST visitors"""
    
    def visit(self, node: ASTNode):
        """Visit a node"""
        method_name = f'visit_{type(node).__name__}'
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)
    
    def generic_visit(self, node: ASTNode):
        """Called if no explicit visitor method exists for a node"""
        raise NotImplementedError(f"No visitor method for {type(node).__name__}")

class ASTPrinter(ASTVisitor):
    """Pretty print AST for debugging"""
    
    def __init__(self, indent_size: int = 2):
        self.indent_size = indent_size
        self.indent_level = 0
    
    def indent(self) -> str:
        return ' ' * (self.indent_level * self.indent_size)
    
    def visit_Program(self, node: Program) -> str:
        result = "Program:\n"
        self.indent_level += 1
        for decl in node.declarations:
            result += self.indent() + self.visit(decl) + "\n"
        self.indent_level -= 1
        return result
    
    def visit_Library(self, node: Library) -> str:
        result = f"LibraryImport.{node.name}:\n"
        self.indent_level += 1
        for item in node.body:
            result += self.indent() + self.visit(item) + "\n"
        self.indent_level -= 1
        return result
    
    def visit_Pool(self, node: Pool) -> str:
        result = f"{node.pool_type}.{node.name}:\n"
        self.indent_level += 1
        for item in node.body:
            result += self.indent() + self.visit(item) + "\n"
        self.indent_level -= 1
        return result
    
    def visit_SubPool(self, node: SubPool) -> str:
        result = f"SubPool.{node.name}:\n"
        self.indent_level += 1
        for key, item in node.items.items():
            result += self.indent() + self.visit(item) + "\n"
        self.indent_level -= 1
        return result
    
    def visit_ResourceItem(self, node: ResourceItem) -> str:
        attrs = ', '.join(f"{k}-{self.visit(v)}" for k, v in node.attributes.items())
        value = self.visit(node.value) if node.value else "None"
        return f'"{node.key}": {value} {attrs}'
    
    def visit_Loop(self, node: Loop) -> str:
        result = f"{node.loop_type}.{node.name}:\n"
        self.indent_level += 1
        for stmt in node.body:
            result += self.indent() + self.visit(stmt) + "\n"
        self.indent_level -= 1
        if node.end_name:
            result += self.indent() + f"LoopEnd.{node.end_name}\n"
        return result
    
    def visit_SubRoutine(self, node: SubRoutine) -> str:
        result = f"SubRoutine.{node.name}:\n"
        self.indent_level += 1
        for stmt in node.body:
            result += self.indent() + self.visit(stmt) + "\n"
        self.indent_level -= 1
        return result
    
    def visit_Function(self, node: Function) -> str:
        params = ', '.join(f"{name}: {self.visit(type)}" for name, type in node.input_params)
        result = f"Function.{node.name}({params})"
        if node.output_type:
            result += f" -> {self.visit(node.output_type)}"
        result += ":\n"
        self.indent_level += 1
        for stmt in node.body:
            result += self.indent() + self.visit(stmt) + "\n"
        self.indent_level -= 1
        return result
    
    def visit_Lambda(self, node: Lambda) -> str:
        params = ', '.join(node.params)
        return f"Lambda({params}) {{ {self.visit(node.body)} }}"
    
    def visit_Combinator(self, node: Combinator) -> str:
        return f"Combinator.{node.name} = {self.visit(node.definition)}"
    
    def visit_MacroBlock(self, node: MacroBlock) -> str:
        result = f"MacroBlock.{node.name}:\n"
        self.indent_level += 1
        for macro in node.macros.values():
            result += self.indent() + self.visit(macro) + "\n"
        self.indent_level -= 1
        return result
    
    def visit_MacroDefinition(self, node: MacroDefinition) -> str:
        params = ', '.join(node.params)
        return f"Macro.{node.name}({params}) = {self.visit(node.body)}"
    
    def visit_SecurityContext(self, node: SecurityContext) -> str:
        result = f"SecurityContext.{node.name}:\n"
        self.indent_level += 1
        for level in node.levels.values():
            result += self.indent() + self.visit(level) + "\n"
        self.indent_level -= 1
        return result
    
    def visit_SecurityLevel(self, node: SecurityLevel) -> str:
        result = f"Level.{node.name}:\n"
        self.indent_level += 1
        result += self.indent() + f"Allowed: {node.allowed_operations}\n"
        result += self.indent() + f"Denied: {node.denied_operations}\n"
        if node.memory_limit:
            result += self.indent() + f"MemoryLimit: {self.visit(node.memory_limit)}\n"
        if node.cpu_quota:
            result += self.indent() + f"CPUQuota: {self.visit(node.cpu_quota)}\n"
        self.indent_level -= 1
        return result
    
    def visit_ConstrainedType(self, node: ConstrainedType) -> str:
        return f"ConstrainedType.{node.name} = {self.visit(node.base_type)} Where {{ {self.visit(node.constraints)} }}"
    
    def visit_Constant(self, node: Constant) -> str:
        return f"Constant.{node.name} = {self.visit(node.value)}"
    
    def visit_RunTask(self, node: RunTask) -> str:
        args = ', '.join(f"{name}-{self.visit(value)}" for name, value in node.arguments)
        return f"RunTask.{node.task_name}({args})"
    
    def visit_PrintMessage(self, node: PrintMessage) -> str:
        return f"PrintMessage({self.visit(node.message)})"
    
    def visit_ReturnValue(self, node: ReturnValue) -> str:
        return f"ReturnValue({self.visit(node.value)})"
    
    def visit_If(self, node: If) -> str:
        result = f"IfCondition {self.visit(node.condition)} ThenBlock {{\n"
        self.indent_level += 1
        for stmt in node.then_body:
            result += self.indent() + self.visit(stmt) + "\n"
        self.indent_level -= 1
        result += self.indent() + "}"
        if node.else_body:
            result += " ElseBlock {\n"
            self.indent_level += 1
            for stmt in node.else_body:
                result += self.indent() + self.visit(stmt) + "\n"
            self.indent_level -= 1
            result += self.indent() + "}"
        return result
    
    def visit_ChoosePath(self, node: ChoosePath) -> str:
        result = f"ChoosePath({self.visit(node.expression)}) {{\n"
        self.indent_level += 1
        for case_value, case_body in node.cases:
            result += self.indent() + f"CaseOption \"{case_value}\":\n"
            self.indent_level += 1
            for stmt in case_body:
                result += self.indent() + self.visit(stmt) + "\n"
            self.indent_level -= 1
        if node.default:
            result += self.indent() + "DefaultOption:\n"
            self.indent_level += 1
            for stmt in node.default:
                result += self.indent() + self.visit(stmt) + "\n"
            self.indent_level -= 1
        self.indent_level -= 1
        result += self.indent() + "}"
        return result
    
    def visit_While(self, node: While) -> str:
        result = f"WhileLoop {self.visit(node.condition)} {{\n"
        self.indent_level += 1
        for stmt in node.body:
            result += self.indent() + self.visit(stmt) + "\n"
        self.indent_level -= 1
        result += self.indent() + "}"
        return result
    
    def visit_ForEvery(self, node: ForEvery) -> str:
        result = f"ForEvery {node.variable} in {self.visit(node.collection)} {{\n"
        self.indent_level += 1
        for stmt in node.body:
            result += self.indent() + self.visit(stmt) + "\n"
        self.indent_level -= 1
        result += self.indent() + "}"
        return result
    
    def visit_Try(self, node: Try) -> str:
        result = "TryBlock:\n"
        self.indent_level += 1
        for stmt in node.body:
            result += self.indent() + self.visit(stmt) + "\n"
        self.indent_level -= 1
        for error_type, catch_body in node.catch_clauses:
            result += self.indent() + f"CatchError.{error_type}:\n"
            self.indent_level += 1
            for stmt in catch_body:
                result += self.indent() + self.visit(stmt) + "\n"
            self.indent_level -= 1
        if node.finally_body:
            result += self.indent() + "FinallyBlock:\n"
            self.indent_level += 1
            for stmt in node.finally_body:
                result += self.indent() + self.visit(stmt) + "\n"
            self.indent_level -= 1
        return result
    
    def visit_SendMessage(self, node: SendMessage) -> str:
        params = ', '.join(f"{k}-{self.visit(v)}" for k, v in node.parameters.items())
        return f"SendMessage.{node.target}({params})"
    
    def visit_ReceiveMessage(self, node: ReceiveMessage) -> str:
        result = f"ReceiveMessage.{node.message_type} {{\n"
        self.indent_level += 1
        for stmt in node.body:
            result += self.indent() + self.visit(stmt) + "\n"
        self.indent_level -= 1
        result += self.indent() + "}"
        return result
    
    def visit_EveryInterval(self, node: EveryInterval) -> str:
        result = f"EveryInterval {node.interval_type}-{node.interval_value} {{\n"
        self.indent_level += 1
        for stmt in node.body:
            result += self.indent() + self.visit(stmt) + "\n"
        self.indent_level -= 1
        result += self.indent() + "}"
        return result
    
    def visit_WithSecurity(self, node: WithSecurity) -> str:
        result = f"WithSecurity({node.context}) {{\n"
        self.indent_level += 1
        for stmt in node.body:
            result += self.indent() + self.visit(stmt) + "\n"
        self.indent_level -= 1
        result += self.indent() + "}"
        return result
    
    def visit_Assignment(self, node: Assignment) -> str:
        return f"{node.target} = {self.visit(node.value)}"
    
    def visit_BreakLoop(self, node: BreakLoop) -> str:
        return "BreakLoop"
    
    def visit_ContinueLoop(self, node: ContinueLoop) -> str:
        return "ContinueLoop"
    
    def visit_HaltProgram(self, node: HaltProgram) -> str:
        return f"HaltProgram({node.message or ''})"
    
    def visit_MathExpression(self, node: MathExpression) -> str:
        return f"({self.visit(node.expression)})"
    
    def visit_FunctionCall(self, node: FunctionCall) -> str:
        args = ', '.join(self.visit(arg) for arg in node.arguments)
        return f"{node.function}({args})"
    
    def visit_Apply(self, node: Apply) -> str:
        args = ', '.join(self.visit(arg) for arg in node.arguments)
        return f"Apply({self.visit(node.function)}, {args})"
    
    def visit_RunMacro(self, node: RunMacro) -> str:
        args = ', '.join(self.visit(arg) for arg in node.arguments)
        return f"RunMacro.{node.macro_path}({args})"
    
    def visit_Identifier(self, node: Identifier) -> str:
        return node.name
    
    def visit_Number(self, node: Number) -> str:
        return str(node.value)
    
    def visit_String(self, node: String) -> str:
        return f'"{node.value}"'
    
    def visit_Boolean(self, node: Boolean) -> str:
        return 'True' if node.value else 'False'
    
    def visit_ArrayLiteral(self, node: ArrayLiteral) -> str:
        elements = ', '.join(self.visit(elem) for elem in node.elements)
        return f"[{elements}]"
    
    def visit_MapLiteral(self, node: MapLiteral) -> str:
        pairs = ', '.join(f"{self.visit(k)}: {self.visit(v)}" for k, v in node.pairs)
        return f"{{{pairs}}}"
    
    def visit_TypeExpression(self, node: TypeExpression) -> str:
        if node.parameters:
            params = ', '.join(self.visit(p) for p in node.parameters)
            return f"{node.base_type}[{params}]"
        return node.base_type
    def visit_AcronymDefinitions(self, node: AcronymDefinitions) -> str:
        """Pretty print acronym definitions"""
        result = "AcronymDefinitions {\n"
        self.indent_level += 1
        
        for acronym, full_name in node.definitions.items():
            result += self.indent() + f"{acronym} = {full_name}\n"
        
        self.indent_level -= 1
        result += self.indent() + "}"
        return result


    def visit_RecordTypeDefinition(self, node) -> str:
        return f"{node.name} = {self.visit(node.record_type)}"
