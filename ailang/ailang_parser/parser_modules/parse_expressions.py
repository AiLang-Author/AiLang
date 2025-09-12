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
                TokenType.POWER, TokenType.SQUAREROOT, TokenType.GREATERTHAN, TokenType.LESSTHAN,
                TokenType.EQUALTO, TokenType.NOTEQUAL, TokenType.GREATEREQUAL, TokenType.LESSEQUAL,
                TokenType.AND, TokenType.OR, TokenType.NOT,
                # ADD BITWISE OPERATIONS (with correct names):
                TokenType.BITWISEAND, TokenType.BITWISEOR, TokenType.BITWISEXOR,
                TokenType.BITWISENOT, TokenType.LEFTSHIFT, TokenType.RIGHTSHIFT,
                # Continue with existing:
                TokenType.READINPUT, TokenType.READINPUTNUMBER, TokenType.GETUSERCHOICE,
                TokenType.STRINGEQUALS, TokenType.STRINGCONTAINS, TokenType.STRINGCONCAT,TokenType.STRINGSUBSTRING,
                TokenType.STRINGLENGTH, TokenType.STRINGTONUMBER, TokenType.NUMBERTOSTRING,
                TokenType.STRINGTOUPPER, TokenType.STRINGTOLOWER, TokenType.CHARTOSTRING,  # ADD THESE
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
        
        # Check for infix notation (e.g., "(2 Multiply 3)")
        if isinstance(expr, (Number, Identifier, FunctionCall)):
            self.skip_newlines()
            if self.match(TokenType.ADD, TokenType.MULTIPLY, TokenType.DIVIDE, TokenType.SUBTRACT, 
                         TokenType.POWER, TokenType.GREATERTHAN, TokenType.LESSTHAN,
                         TokenType.EQUALTO, TokenType.NOTEQUAL, TokenType.GREATEREQUAL, 
                         TokenType.LESSEQUAL, TokenType.AND, TokenType.OR):
                op_token = self.current_token
                op_name = op_token.value
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
            return Identifier(name='Null', line=token.line, column=token.column)
        
        elif self.match(TokenType.LAMBDA):
            return self.parse_lambda()
        
        elif self.match(TokenType.APPLY):
            return self.parse_apply()
        
        elif self.match(TokenType.RUNTASK):
            return self.parse_runtask()
        
        elif self.match(TokenType.RUNMACRO):
            return self.parse_runmacro()
        
        elif self.match(TokenType.RECORD):
            return self.parse_record_type()
        
        elif self.match(TokenType.IDENTIFIER):
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
        identifier = self.consume(TokenType.IDENTIFIER)
        
        # Check if this is a function call
        if self.match(TokenType.LPAREN):
            self.consume(TokenType.LPAREN)
            arguments = []
            
            if not self.match(TokenType.RPAREN):
                arguments.append(self.parse_expression())
                while self.match(TokenType.COMMA):
                    self.consume(TokenType.COMMA)
                    arguments.append(self.parse_expression())
            
            self.consume(TokenType.RPAREN)
            return FunctionCall(
                function=identifier.value,
                arguments=arguments,
                line=identifier.line,
                column=identifier.column
            )
        # Handle BitwiseAnd and BitwiseOr as function calls
        elif identifier.value in ['BitwiseAnd', 'BitwiseOr']:
            if self.match(TokenType.LPAREN):
                self.consume(TokenType.LPAREN)
                arguments = []
                
                if not self.match(TokenType.RPAREN):
                    arguments.append(self.parse_expression())
                    while self.match(TokenType.COMMA):
                        self.consume(TokenType.COMMA)
                        arguments.append(self.parse_expression())
                
                self.consume(TokenType.RPAREN)
                return FunctionCall(
                    function=identifier.value,
                    arguments=arguments,
                    line=identifier.line,
                    column=identifier.column
                )
        # Just a variable reference
        return Identifier(
            name=identifier.value,
            line=identifier.line,
            column=identifier.column)

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