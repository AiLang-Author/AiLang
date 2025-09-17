class ScopeManager:
    """Minimal scope manager for parameter resolution"""
    def __init__(self, compiler):
        self.compiler = compiler
        self.function_params = {}  # func_name -> {param_name -> offset}
        self.current_function = None
    
    def push_function(self, name):
        self.current_function = name
        if name not in self.function_params:
            self.function_params[name] = {}
        print(f"DEBUG ScopeManager: Pushed function '{name}', current={self.current_function}")
    
    def pop(self):
        print(f"DEBUG ScopeManager: Popping function '{self.current_function}'")
        self.current_function = None
    
    def add_parameter(self, name, offset):
        if self.current_function:
            self.function_params[self.current_function][name] = offset
            print(f"DEBUG ScopeManager: Added param '{name}' with offset {offset} to function '{self.current_function}'")
            print(f"DEBUG ScopeManager: Current params for {self.current_function}: {self.function_params[self.current_function]}")
    
    def resolve(self, name):
        # 1. Check for local parameters first
        if self.current_function:
            if name in self.function_params[self.current_function]:
                offset = self.function_params[self.current_function][name]
                print(f"DEBUG ScopeManager: Found '{name}' as local param at offset {offset}")
                return ('param', offset)

        # 2. If not found, check for global variables
        if name in self.compiler.variables:
            offset = self.compiler.variables[name]
            print(f"DEBUG ScopeManager: Found '{name}' as registered global variable at offset {offset}")
            return ('global', offset)

        # 3. Fallback: Scan the top-level AST for the global variable declaration.
        # This is a robust way to handle forward references if the pre-pass fails or is incomplete.
        if self.compiler.ast:
            for decl in self.compiler.ast.declarations:
                # Check for an Assignment node at the top level that matches the variable name
                if type(decl).__name__ == 'Assignment' and hasattr(decl, 'target') and decl.target == name:
                    # Found it. Register it now if it's not already there.
                    if name not in self.compiler.variables:
                        self.compiler.stack_size += 8
                        self.compiler.variables[name] = self.compiler.stack_size
                        print(f"DEBUG ScopeManager: JIT registration of global '{name}' at offset {self.compiler.variables[name]}")
                        return ('global', self.compiler.variables[name])

        print(f"DEBUG ScopeManager: '{name}' not found in local or global scope")
        return (None, None)