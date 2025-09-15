#parse_expressions.py
"""Parser methods for handling expressions"""

from typing import List, Tuple
from ..lexer import TokenType
from ..ailang_ast import *

class ParserExpressionsMixin:
    """Mixin for expression parsing methods"""
    
    def parse_expression(self) -> ASTNode:
        # Skip newlines at the start of expressions
        self.skip_newlines()
        return self.parse_strict_expression()

    def parse_strict_expression(self) -> ASTNode:
        self.skip_newlines()
        if self.match(TokenType.LPAREN):
            return self.parse_parenthesized_expression()
        if self.match(TokenType.ADD, TokenType.MULTIPLY, TokenType.DIVIDE, TokenType.SUBTRACT,
            TokenType.POWER, TokenType.MODULO, TokenType.SQUAREROOT, TokenType.GREATERTHAN, TokenType.LESSTHAN,
            TokenType.EQUALTO, TokenType.NOTEQUAL, TokenType.GREATEREQUAL, TokenType.LESSEQUAL,
            TokenType.AND, TokenType.OR, TokenType.NOT,
                # ADD BITWISE OPERATIONS (with correct names):
                TokenType.BITWISEAND, TokenType.BITWISEOR, TokenType.BITWISEXOR,
                TokenType.BITWISENOT, TokenType.LEFTSHIFT, TokenType.RIGHTSHIFT,
                # String and I/O functions
                TokenType.READINPUT, TokenType.READINPUTNUMBER, TokenType.GETUSERCHOICE,
                TokenType.STRINGEQUALS, TokenType.STRINGCONTAINS, TokenType.STRINGSTARTSWITH,
                TokenType.STRINGENDSWITH, TokenType.STRINGCONCAT, TokenType.STRINGSUBSTRING,
                TokenType.STRINGLENGTH, TokenType.STRINGTONUMBER, TokenType.NUMBERTOSTRING,
                TokenType.STRINGTOUPPER, TokenType.STRINGTOLOWER, TokenType.CHARTOSTRING, TokenType.STRINGINDEXOF,
                TokenType.STRINGSPLIT,
                TokenType.STRINGTRIM, TokenType.STRINGREPLACE, TokenType.STRINGTOSTRING,
                TokenType.STRINGEXTRACT, TokenType.STRINGCHARAT, TokenType.STRINGEXTRACTUNTIL,
                TokenType.WRITETEXTFILE, TokenType.READTEXTFILE, TokenType.FILEEXISTS):
            return self.parse_math_function()
   
        # === NEW: Low-Level Function Parsing ===
        elif self.match(TokenType.DEREFERENCE, TokenType.ADDRESSOF, TokenType.SIZEOF,
                    TokenType.ALLOCATE, TokenType.DEALLOCATE, TokenType.MEMORYCOPY,
                    TokenType.PORTREAD, TokenType.PORTWRITE, TokenType.HARDWAREREGISTER,
                    TokenType.ATOMICREAD, TokenType.ATOMICWRITE, TokenType.MMIOREAD, TokenType.MMIOWRITE, TokenType.STOREVALUE,
                    # ADD ATOMIC OPERATIONS HERE
                    TokenType.ATOMICADD, TokenType.ATOMICSUBTRACT, TokenType.ATOMICCOMPARESWAP, TokenType.ATOMICEXCHANGE):
            return self.parse_lowlevel_function()
        # === NEW: Virtual Memory Expression Parsing ===
        elif self.match(TokenType.PAGETABLE, TokenType.VIRTUALMEMORY, TokenType.CACHE, 
                    TokenType.TLB, TokenType.MEMORYBARRIER):
            return self.parse_vm_operation()
        # === NEW: Pool Memory Operations Parsing ===
        elif self.match(TokenType.POOLRESIZE, TokenType.POOLMOVE, TokenType.POOLCOMPACT,
                    TokenType.POOLALLOCATE, TokenType.POOLFREE):
            return self.parse_pool_operation()
        return self.parse_primary()

    def parse_parenthesized_expression(self) -> ASTNode:
        start_token = self.consume(TokenType.LPAREN)
        self.skip_newlines()
        
        # Parse the inner expression recursively
        expr = self.parse_expression()
        self.skip_newlines()
        
        # Check for infix notation (e.g., "(2 Multiply 3)" or "(2 * 3)")
        if isinstance(expr, (Number, Identifier, FunctionCall)):
            self.skip_newlines()
            
            # First check for NAMED operators (existing code)
            if self.match(TokenType.ADD, TokenType.MULTIPLY, TokenType.DIVIDE, TokenType.SUBTRACT, 
                        TokenType.POWER, TokenType.MODULO,
                        TokenType.GREATERTHAN, TokenType.LESSTHAN,
                        TokenType.EQUALTO, TokenType.NOTEQUAL, TokenType.GREATEREQUAL, 
                        TokenType.LESSEQUAL, TokenType.AND, TokenType.OR, TokenType.NOT):
                op_token = self.current_token
                op_name = op_token.value
                
                # Special handling for unary NOT
                if op_token.type == TokenType.NOT:
                    self.advance()
                    self.skip_newlines()
                    operand = self.parse_expression()
                    self.skip_newlines()
                    self.consume(TokenType.RPAREN)
                    return FunctionCall(function='Not', arguments=[operand],
                                    line=start_token.line, column=start_token.column)
                
                # Binary operators
                self.advance()
                self.skip_newlines()
                second_operand = self.parse_expression()
                self.skip_newlines()
                self.consume(TokenType.RPAREN)
                return FunctionCall(function=op_name, arguments=[expr, second_operand],
                                line=start_token.line, column=start_token.column)
            
            #  Check for SYMBOL operators (only valid in parentheses!)
            elif self.match(TokenType.PLUS_SIGN, TokenType.STAR_SIGN, TokenType.SLASH_SIGN,
                TokenType.DASH, TokenType.PERCENT_SIGN, TokenType.CARET_SIGN,
                TokenType.GREATER_SIGN, TokenType.LESS_SIGN, TokenType.BANG_SIGN,
                TokenType.AMPERSAND_SIGN, TokenType.PIPE_SIGN,
                TokenType.EQUALTO, TokenType.NOTEQUAL,  
                TokenType.GREATEREQUAL, TokenType.LESSEQUAL,  
                TokenType.AND_AND, TokenType.PIPE_PIPE,
                TokenType.LESS_LESS, TokenType.GREATER_GREATER):
                
                symbol_token = self.current_token
                
                # Map symbols to operator function names
                symbol_map = {
                TokenType.PLUS_SIGN: 'Add',
                TokenType.DASH: 'Subtract',
                TokenType.STAR_SIGN: 'Multiply',
                TokenType.SLASH_SIGN: 'Divide',
                TokenType.PERCENT_SIGN: 'Modulo',
                TokenType.CARET_SIGN: 'Power',
                TokenType.GREATER_SIGN: 'GreaterThan',
                TokenType.LESS_SIGN: 'LessThan',
                TokenType.EQUALTO: 'EqualTo',           # Changed from EQUAL_EQUAL
                TokenType.NOTEQUAL: 'NotEqual',         # Changed from BANG_EQUAL
                TokenType.GREATEREQUAL: 'GreaterEqual', # Changed from GREATER_EQUAL_SIGN
                TokenType.LESSEQUAL: 'LessEqual',       # Changed from LESS_EQUAL_SIGN
                TokenType.BANG_SIGN: 'Not',
                TokenType.AMPERSAND_SIGN: 'BitwiseAnd',
                TokenType.PIPE_SIGN: 'BitwiseOr',
                TokenType.AND_AND: 'And',
                TokenType.PIPE_PIPE: 'Or',
                TokenType.LESS_LESS: 'LeftShift',
                TokenType.GREATER_GREATER: 'RightShift',
            }
                
                op_name = symbol_map.get(symbol_token.type)
                if not op_name:
                    self.error(f"Unknown operator symbol: {symbol_token.value}")
                
                # Handle unary ! (Not)
                if symbol_token.type == TokenType.BANG_SIGN:
                    self.advance()
                    self.skip_newlines()
                    operand = self.parse_expression()
                    self.skip_newlines()
                    self.consume(TokenType.RPAREN)
                    return FunctionCall(function='Not', arguments=[operand],
                                    line=start_token.line, column=start_token.column)
                
                # Binary operators
                self.advance()
                self.skip_newlines()
                second_operand = self.parse_expression()
                self.skip_newlines()
                self.consume(TokenType.RPAREN)
                return FunctionCall(function=op_name, arguments=[expr, second_operand],
                                line=start_token.line, column=start_token.column)
        
        self.skip_newlines()
        self.consume(TokenType.RPAREN)
        return expr
    
    
    def parse_math_function(self) -> ASTNode:
        op_token = self.current_token
        op_name = op_token.value
        self.advance()
        self.consume(TokenType.LPAREN)
        self.skip_newlines()
        args = []
        while not self.match(TokenType.RPAREN):
            args.append(self.parse_expression())
            if self.match(TokenType.COMMA):
                self.consume(TokenType.COMMA)
                self.skip_newlines()
            elif not self.match(TokenType.RPAREN):
                self.skip_newlines()
        self.consume(TokenType.RPAREN)
        return FunctionCall(function=op_name, arguments=args,
                           line=op_token.line, column=op_token.column)

    def parse_primary(self) -> ASTNode:
        self.skip_newlines()
        
        # Handle prefix ! operator (unary NOT)
        if self.match(TokenType.BANG_SIGN):
            token = self.current_token
            self.advance()
            operand = self.parse_primary()  # Recursively parse what comes after !
            return FunctionCall(function='Not', arguments=[operand],
                            line=token.line if token else 0, 
                            column=token.column if token else 0)
            
        if self.match(TokenType.TILDE_SIGN):
            token = self.current_token
            self.advance()
            operand = self.parse_primary()
            return FunctionCall(function='BitwiseNot', arguments=[operand],
                            line=token.line if token else 0, 
                            column=token.column if token else 0)
        
        if self.match(TokenType.NUMBER):
            token = self.current_token
            self.advance()
            return Number(value=token.value, line=token.line, column=token.column)
        
        elif self.match(TokenType.STRING):
            token = self.current_token
            self.advance()
            return String(value=token.value, line=token.line, column=token.column)
        
        elif self.match(TokenType.TRUE):
            token = self.current_token
            self.advance()
            return Boolean(value=True, line=token.line, column=token.column)
        
        elif self.match(TokenType.FALSE):
            token = self.current_token
            self.advance()
            return Boolean(value=False, line=token.line, column=token.column)
        
        elif self.match(TokenType.NULL):
            token = self.current_token
            self.advance()
            return Number(value=0, line=token.line, column=token.column)
        
        elif self.match(TokenType.LAMBDA):
            return self.parse_lambda()
        
        elif self.match(TokenType.APPLY):
            return self.parse_apply()
        
        elif self.match(TokenType.RUNTASK):
            return self.parse_runtask()
        
        elif self.match(TokenType.RUNMACRO):
            return self.parse_runmacro()
        
        elif self.match(TokenType.IDENTIFIER, TokenType.FIXEDPOOL, TokenType.DYNAMICPOOL,
                       TokenType.TEMPORALPOOL, TokenType.NEURALPOOL, TokenType.KERNELPOOL,
                       TokenType.ACTORPOOL, TokenType.SECURITYPOOL, TokenType.CONSTRAINEDPOOL,
                       TokenType.FILEPOOL):
            return self.parse_identifier()
        
        elif self.match(TokenType.LPAREN):
            return self.parse_parenthesized_expression()
        
        elif self.match(TokenType.LBRACKET):
            return self.parse_array_literal()
        
        elif self.match(TokenType.LBRACE):
        # Map literals not implemented yet
            return None
        
        elif self.match(TokenType.PI):
            token = self.current_token
            self.advance()
            return Number(value=3.14159265358979323846, line=token.line, column=token.column)
        
        elif self.match(TokenType.E):
            token = self.current_token
            self.advance()
            return Number(value=2.71828182845904523536, line=token.line, column=token.column)
        
        elif self.match(TokenType.PHI):
            token = self.current_token
            self.advance()
            return Number(value=1.61803398874989484820, line=token.line, column=token.column)
        
        elif self.match(TokenType.BYTES, TokenType.KILOBYTES, TokenType.MEGABYTES,
                    TokenType.GIGABYTES, TokenType.SECONDS, TokenType.MILLISECONDS,
                    TokenType.MICROSECONDS, TokenType.PERCENT):
            return self.parse_unit()
        
        elif self.match(TokenType.BYTE, TokenType.WORD, TokenType.DWORD, TokenType.QWORD):
            return self.parse_memory_size_cast()
        
        # Hash operations
        elif self.match(TokenType.HASHCREATE):
            line = self.current_token.line
            column = self.current_token.column
            self.advance()
            args = []
            if self.match(TokenType.LPAREN):
                self.advance()
                if not self.check(TokenType.RPAREN):
                    args.append(self.parse_expression())
                    while self.match(TokenType.COMMA):
                        self.advance()
                        args.append(self.parse_expression())
                self.consume(TokenType.RPAREN, "Expected ')'")
            return FunctionCall(line, column, "HashCreate", args)
        
        elif self.match(TokenType.HASHFUNCTION):
            line = self.current_token.line
            column = self.current_token.column
            self.advance()
            args = []
            if self.match(TokenType.LPAREN):
                self.advance()
                if not self.check(TokenType.RPAREN):
                    args.append(self.parse_expression())
                    while self.match(TokenType.COMMA):
                        self.advance()
                        args.append(self.parse_expression())
                self.consume(TokenType.RPAREN, "Expected ')'")
            return FunctionCall(line, column, "HashFunction", args)
        
        elif self.match(TokenType.HASHSET):
            line = self.current_token.line
            column = self.current_token.column
            self.advance()
            args = []
            if self.match(TokenType.LPAREN):
                self.advance()
                if not self.check(TokenType.RPAREN):
                    args.append(self.parse_expression())
                    while self.match(TokenType.COMMA):
                        self.advance()
                        args.append(self.parse_expression())
                self.consume(TokenType.RPAREN, "Expected ')'")
            return FunctionCall(line, column, "HashSet", args)
        
        elif self.match(TokenType.HASHGET):
            line = self.current_token.line
            column = self.current_token.column
            self.advance()
            args = []
            if self.match(TokenType.LPAREN):
                self.advance()
                if not self.check(TokenType.RPAREN):
                    args.append(self.parse_expression())
                    while self.match(TokenType.COMMA):
                        self.advance()
                        args.append(self.parse_expression())
                self.consume(TokenType.RPAREN, "Expected ')'")
            return FunctionCall(line, column, "HashGet", args)
        
        # Socket operations
        elif self.match(TokenType.SOCKETCREATE):
            line = self.current_token.line
            column = self.current_token.column
            self.advance()
            args = []
            if self.match(TokenType.LPAREN):
                self.advance()
                if not self.check(TokenType.RPAREN):
                    args.append(self.parse_expression())
                    while self.match(TokenType.COMMA):
                        self.advance()
                        args.append(self.parse_expression())
                self.consume(TokenType.RPAREN, "Expected ')'")
            return FunctionCall(line, column, "SocketCreate", args)
        
        elif self.match(TokenType.SOCKETBIND):
            line = self.current_token.line
            column = self.current_token.column
            self.advance()
            args = []
            if self.match(TokenType.LPAREN):
                self.advance()
                if not self.check(TokenType.RPAREN):
                    args.append(self.parse_expression())
                    while self.match(TokenType.COMMA):
                        self.advance()
                        args.append(self.parse_expression())
                self.consume(TokenType.RPAREN, "Expected ')'")
            return FunctionCall(line, column, "SocketBind", args)
        
        elif self.match(TokenType.SOCKETLISTEN):
            line = self.current_token.line
            column = self.current_token.column
            self.advance()
            args = []
            if self.match(TokenType.LPAREN):
                self.advance()
                if not self.check(TokenType.RPAREN):
                    args.append(self.parse_expression())
                    while self.match(TokenType.COMMA):
                        self.advance()
                        args.append(self.parse_expression())
                self.consume(TokenType.RPAREN, "Expected ')'")
            return FunctionCall(line, column, "SocketListen", args)
        
        elif self.match(TokenType.SOCKETACCEPT):
            line = self.current_token.line
            column = self.current_token.column
            self.advance()
            args = []
            if self.match(TokenType.LPAREN):
                self.advance()
                if not self.check(TokenType.RPAREN):
                    args.append(self.parse_expression())
                    while self.match(TokenType.COMMA):
                        self.advance()
                        args.append(self.parse_expression())
                self.consume(TokenType.RPAREN, "Expected ')'")
            return FunctionCall(line, column, "SocketAccept", args)
        
        elif self.match(TokenType.SOCKETREAD):
            line = self.current_token.line
            column = self.current_token.column
            self.advance()
            args = []
            if self.match(TokenType.LPAREN):
                self.advance()
                if not self.check(TokenType.RPAREN):
                    args.append(self.parse_expression())
                    while self.match(TokenType.COMMA):
                        self.advance()
                        args.append(self.parse_expression())
                self.consume(TokenType.RPAREN, "Expected ')'")
            return FunctionCall(line, column, "SocketRead", args)
        
        elif self.match(TokenType.SOCKETWRITE):
            line = self.current_token.line
            column = self.current_token.column
            self.advance()
            args = []
            if self.match(TokenType.LPAREN):
                self.advance()
                if not self.check(TokenType.RPAREN):
                    args.append(self.parse_expression())
                    while self.match(TokenType.COMMA):
                        self.advance()
                        args.append(self.parse_expression())
                self.consume(TokenType.RPAREN, "Expected ')'")
            return FunctionCall(line, column, "SocketWrite", args)
        
        elif self.match(TokenType.SOCKETCLOSE):
            line = self.current_token.line
            column = self.current_token.column
            self.advance()
            args = []
            if self.match(TokenType.LPAREN):
                self.advance()
                if not self.check(TokenType.RPAREN):
                    args.append(self.parse_expression())
                    while self.match(TokenType.COMMA):
                        self.advance()
                        args.append(self.parse_expression())
                self.consume(TokenType.RPAREN, "Expected ')'")
            return FunctionCall(line, column, "SocketClose", args)
        
        elif self.match(TokenType.SOCKETCONNECT):
            line = self.current_token.line
            column = self.current_token.column
            self.advance()
            args = []
            if self.match(TokenType.LPAREN):
                self.advance()
                if not self.check(TokenType.RPAREN):
                    args.append(self.parse_expression())
                    while self.match(TokenType.COMMA):
                        self.advance()
                        args.append(self.parse_expression())
                self.consume(TokenType.RPAREN, "Expected ')'")
            return FunctionCall(line, column, "SocketConnect", args)
        
        elif self.match(TokenType.SOCKETSETOPTION):
            line = self.current_token.line
            column = self.current_token.column
            self.advance()
            args = []
            if self.match(TokenType.LPAREN):
                self.advance()
                if not self.check(TokenType.RPAREN):
                    args.append(self.parse_expression())
                    while self.match(TokenType.COMMA):
                        self.advance()
                        args.append(self.parse_expression())
                self.consume(TokenType.RPAREN, "Expected ')'")
            return FunctionCall(line, column, "SocketSetOption", args)
        
        else:
            self.error(f"Unexpected token in expression: {self.current_token.value if self.current_token else 'EOF'}")

    def parse_apply(self) -> Apply:
        start_token = self.consume(TokenType.APPLY)
        self.consume(TokenType.LPAREN)
        self.skip_newlines()
        function = self.parse_expression()
        arguments = []
        while self.match(TokenType.COMMA):
            self.consume(TokenType.COMMA)
            self.skip_newlines()
            arguments.append(self.parse_expression())
        self.skip_newlines()
        self.consume(TokenType.RPAREN)
        return Apply(function=function, arguments=arguments,
                    line=start_token.line, column=start_token.column)

    def parse_runmacro(self) -> RunMacro:
        start_token = self.consume(TokenType.RUNMACRO)
        self.consume(TokenType.DOT)
        macro_path = self.parse_dotted_name()
        self.consume(TokenType.LPAREN)
        self.skip_newlines()
        arguments = []
        while not self.match(TokenType.RPAREN):
            arguments.append(self.parse_expression())
            if self.match(TokenType.COMMA):
                self.consume(TokenType.COMMA)
                self.skip_newlines()
            elif not self.match(TokenType.RPAREN):
                self.skip_newlines()
        self.consume(TokenType.RPAREN)
        return RunMacro(macro_path=macro_path, arguments=arguments,
                        line=start_token.line, column=start_token.column)

    def parse_identifier(self) -> ASTNode:
        """Parse an identifier, which might be a variable or function call"""
        start_token = self.current_token
        name = self.parse_qualified_name()
        
        # Check if this is a function call
        if self.match(TokenType.LPAREN):
            self.consume(TokenType.LPAREN)
            arguments = []
            
            if not self.check(TokenType.RPAREN):
                arguments.append(self.parse_expression())
                while self.match(TokenType.COMMA):
                    self.consume(TokenType.COMMA)
                    arguments.append(self.parse_expression())
            
            self.consume(TokenType.RPAREN)
            return FunctionCall(
                function=name,
                arguments=arguments,
                line=start_token.line,
                column=start_token.column
            )
        # Just a variable reference
        return Identifier(
            name=name,
            line=start_token.line,
            column=start_token.column)

    def parse_array_literal(self):
        """Parse array literal [...]"""
        start_token = self.consume(TokenType.LBRACKET)
        elements = []
        self.skip_newlines()
        while not self.match(TokenType.RBRACKET):
            elements.append(self.parse_expression())
            self.skip_newlines()
            if self.match(TokenType.COMMA):
                self.consume(TokenType.COMMA)
                self.skip_newlines()
            elif not self.match(TokenType.RBRACKET):
                self.error("Expected ',' or ']' in array literal")
        self.consume(TokenType.RBRACKET)
        return Array(elements=elements, line=start_token.line, column=start_token.column)

    def parse_unit(self):
        """Parse units like Bytes, Seconds, etc."""
        token = self.current_token
        unit_type = token.value
        self.advance()
        
        # Some units may be followed by a value
        value = None
        if self.match(TokenType.LPAREN):
            self.consume(TokenType.LPAREN)
            value = self.parse_expression()
            self.consume(TokenType.RPAREN)
        
        return FunctionCall(
            function=unit_type,
            arguments=[value] if value else [],
            line=token.line,
            column=token.column
        )

    def parse_memory_size_cast(self):
        """Parse memory size cast operations"""
        token = self.current_token
        cast_type = token.value
        self.advance()
        
        # Memory casts expect a value
        if self.match(TokenType.LPAREN):
            self.consume(TokenType.LPAREN)
            value = self.parse_expression()
            self.consume(TokenType.RPAREN)
            return FunctionCall(
                function=f"Cast{cast_type}",
                arguments=[value],
                line=token.line,
                column=token.column
            )
        else:
            self.error(f"Expected '(' after {cast_type}")

    def parse_record_type(self):
        """Parse Record type definitions"""
        start_token = self.consume(TokenType.RECORD)
        self.consume(TokenType.DOT)
        name = self.consume(TokenType.IDENTIFIER).value
        self.consume(TokenType.LBRACE)
        
        fields = {}
        self.skip_newlines()
        while not self.match(TokenType.RBRACE):
            field_name = self.consume(TokenType.IDENTIFIER).value
            self.consume(TokenType.COLON)
            field_type = self.parse_type()
            fields[field_name] = field_type
            
            if self.match(TokenType.COMMA):
                self.consume(TokenType.COMMA)
                self.skip_newlines()
            else:
                self.skip_newlines()
        
        self.consume(TokenType.RBRACE)
        return RecordType(
            name=name,
            fields=fields,
            line=start_token.line,
            column=start_token.column
        )