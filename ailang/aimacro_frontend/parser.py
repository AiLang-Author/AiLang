#!/usr/bin/env python3
"""
AIMacro Parser
Consumes tokens from the lexer and builds an AIMacro-specific AST.
This AST is then passed to the ast_converter for translation to the AILang AST.
"""

from typing import List, Optional, Tuple

from aimacro_frontend.lexer import AIMacroLexer, Token, TokenType
# The parser instantiates these classes, but doesn't need to know their internal structure.
# Python's dynamic nature allows us to import them at runtime from the converter module.
from aimacro_frontend.ast_converter import (
    AIMacroASTNode, AIMacroFunction, AIMacroIf, AIMacroWhile, AIMacroFor,
    AIMacroReturn, AIMacroAssignment, AIMacroBinaryOp, AIMacroUnaryOp, AIMacroMethodCall,
    AIMacroIndexAccess, AIMacroIndexAssignment, AIMacroFunctionCall, AIMacroIdentifier, AIMacroNumber,
    AIMacroString, AIMacroBoolean, AIMacroList, AIMacroDict, AIMacroProgram
)

class ParserError(Exception):
    def __init__(self, message: str, line: int, column: int):
        self.message = message
        self.line = line
        self.column = column
        super().__init__(f"Parser error at line {line}, column {column}: {message}")

class AIMacroParser:
    """
    Parses a stream of tokens from the AIMacroLexer and builds an
    AIMacro-specific Abstract Syntax Tree (AST).
    """
    
    def __init__(self, tokens: List[Token]):
        self.tokens = tokens
        self.position = 0
    
    def error(self, message: str):
        token = self.current_token()
        raise ParserError(message, token.line, token.column)
    
    def advance(self):
        self.position += 1
    
    def current_token(self) -> Token:
        if self.position < len(self.tokens):
            return self.tokens[self.position]
        return self.tokens[-1] # Should be EOF
    
    def peek_token(self) -> Token:
        if self.position + 1 < len(self.tokens):
            return self.tokens[self.position + 1]
        return self.tokens[-1]

    def match(self, *types: TokenType) -> bool:
        return self.current_token().type in types

    def consume(self, token_type: TokenType, message: Optional[str] = None) -> Token:
        token = self.current_token()
        if token.type != token_type:
            msg = message or f"Expected {token_type.name}, but got {token.type.name}"
            self.error(msg)
        self.advance()
        return token

    def skip_insignificant_tokens(self):
        """Skips over insignificant tokens like newlines, indentation, and comments."""
        while self.match(TokenType.NEWLINE, TokenType.INDENT, TokenType.DEDENT, TokenType.COMMENT):
            self.advance()

    def parse(self) -> AIMacroProgram:
        declarations = []
        self.skip_insignificant_tokens()
        while not self.match(TokenType.EOF):
            # If we skipped all the way to the end, break.
            if self.match(TokenType.EOF):
                break

            if self.match(TokenType.DEF):
                declarations.append(self.parse_function_def())
            else:
                declarations.append(self.parse_statement())

            # Consume an optional semicolon as a statement terminator
            if self.match(TokenType.SEMICOLON):
                self.advance()

            self.skip_insignificant_tokens()
        return AIMacroProgram(declarations)

    def parse_function_def(self) -> AIMacroFunction:
        def_token = self.consume(TokenType.DEF)
        name = self.consume(TokenType.IDENTIFIER).value
        self.consume(TokenType.LPAREN)
        params = []
        while not self.match(TokenType.RPAREN):
            param_name = self.consume(TokenType.IDENTIFIER).value
            param_type = None
            if self.match(TokenType.COLON):
                self.advance()
                param_type = self.consume(TokenType.IDENTIFIER).value
            params.append((param_name, param_type))
            if self.match(TokenType.COMMA):
                self.advance()
        self.consume(TokenType.RPAREN)
        return_type = None
        if self.match(TokenType.ARROW):
            self.advance()
            return_type = self.consume(TokenType.IDENTIFIER).value
        self.consume(TokenType.COLON)
        self.skip_insignificant_tokens()
        body = self.parse_block()
        return AIMacroFunction(name, params, body, return_type, def_token.line, def_token.column)

    def parse_statement(self) -> AIMacroASTNode:
        if self.match(TokenType.IF):
            return self.parse_if_stmt()
        elif self.match(TokenType.WHILE):
            return self.parse_while_stmt()
        elif self.match(TokenType.FOR):
            return self.parse_for_stmt()
        elif self.match(TokenType.RETURN):
            return self.parse_return_stmt()
        else:
            return self.parse_assignment_or_expr_stmt()

    def parse_if_stmt(self) -> AIMacroIf:
        if self.match(TokenType.IF):
            if_token = self.consume(TokenType.IF)
        elif self.match(TokenType.ELIF):
            if_token = self.consume(TokenType.ELIF)
        else:
            self.error("Expected IF or ELIF token")
        condition = self.parse_expression()
        self.consume(TokenType.COLON)
        self.skip_insignificant_tokens()
        then_body = self.parse_block()
        else_body = None
        if self.match(TokenType.ELIF):
            else_body = [self.parse_if_stmt()]
        elif self.match(TokenType.ELSE):
            self.advance()
            self.consume(TokenType.COLON)
            self.skip_insignificant_tokens()
            else_body = self.parse_block()
        return AIMacroIf(condition, then_body, else_body, if_token.line, if_token.column)

    def parse_while_stmt(self) -> AIMacroWhile:
        while_token = self.consume(TokenType.WHILE)
        condition = self.parse_expression()
        self.consume(TokenType.COLON)
        self.skip_insignificant_tokens()
        body = self.parse_block()
        return AIMacroWhile(condition, body, while_token.line, while_token.column)

    def parse_for_stmt(self) -> AIMacroFor:
        for_token = self.consume(TokenType.FOR)
        var = self.consume(TokenType.IDENTIFIER).value
        self.consume(TokenType.IN)
        iterable = self.parse_expression()
        self.consume(TokenType.COLON)
        self.skip_insignificant_tokens()
        body = self.parse_block()
        return AIMacroFor(var, iterable, body, for_token.line, for_token.column)

    def parse_return_stmt(self) -> AIMacroReturn:
        return_token = self.consume(TokenType.RETURN)
        value = None
        if not self.match(TokenType.SEMICOLON, TokenType.NEWLINE, TokenType.END):
            value = self.parse_expression()
        return AIMacroReturn(value, return_token.line, return_token.column)

    def parse_block(self) -> List[AIMacroASTNode]:
        statements = []
        self.skip_insignificant_tokens() # Skip any initial noise after the ':'
        while not self.match(TokenType.END, TokenType.ELIF, TokenType.ELSE, TokenType.EOF):
            statements.append(self.parse_statement())

            # Consume an optional semicolon as a statement terminator
            if self.match(TokenType.SEMICOLON):
                self.advance()

            self.skip_insignificant_tokens() # Skip noise before the next statement

        if self.match(TokenType.END):
            self.consume(TokenType.END)
            if self.match(TokenType.SEMICOLON):
                self.advance()
        return statements

    def parse_assignment_or_expr_stmt(self) -> AIMacroASTNode:
        expr = self.parse_expression()
        if self.match(TokenType.ASSIGN):
            assign_token = self.current_token()
            self.advance()
            value = self.parse_expression()

            if isinstance(expr, AIMacroIdentifier):
                return AIMacroAssignment(expr.name, value, assign_token.line, assign_token.column)
            elif isinstance(expr, AIMacroIndexAccess):
                return AIMacroIndexAssignment(expr.expression, expr.index, value, assign_token.line, assign_token.column)
            else:
                self.error("Invalid assignment target")
        return expr

    def parse_expression(self) -> AIMacroASTNode:
        return self.parse_or()

    def parse_or(self) -> AIMacroASTNode:
        left = self.parse_and()
        while self.match(TokenType.OR):
            op_token = self.consume(TokenType.OR)
            right = self.parse_and()
            left = AIMacroBinaryOp(left, 'or', right, op_token.line, op_token.column)
        return left

    def parse_and(self) -> AIMacroASTNode:
        left = self.parse_equality()
        while self.match(TokenType.AND):
            op_token = self.consume(TokenType.AND)
            right = self.parse_equality()
            left = AIMacroBinaryOp(left, 'and', right, op_token.line, op_token.column)
        return left

    def parse_equality(self) -> AIMacroASTNode:
        left = self.parse_comparison()
        while self.match(TokenType.EQUAL, TokenType.NOT_EQUAL):
            op_token = self.current_token()
            self.advance()
            right = self.parse_comparison()
            op_map = {TokenType.EQUAL: '==', TokenType.NOT_EQUAL: '!='}
            left = AIMacroBinaryOp(left, op_map[op_token.type], right, op_token.line, op_token.column)
        return left

    def parse_comparison(self) -> AIMacroASTNode:
        left = self.parse_addition()
        while self.match(TokenType.LESS, TokenType.LESS_EQUAL, TokenType.GREATER, TokenType.GREATER_EQUAL):
            op_token = self.current_token()
            self.advance()
            right = self.parse_addition()
            op_map = {TokenType.LESS: '<', TokenType.LESS_EQUAL: '<=', TokenType.GREATER: '>', TokenType.GREATER_EQUAL: '>='}
            left = AIMacroBinaryOp(left, op_map[op_token.type], right, op_token.line, op_token.column)
        return left

    def parse_addition(self) -> AIMacroASTNode:
        left = self.parse_multiplication()
        while self.match(TokenType.PLUS, TokenType.MINUS):
            op_token = self.current_token()
            self.advance()
            right = self.parse_multiplication()
            op_map = {TokenType.PLUS: '+', TokenType.MINUS: '-'}
            left = AIMacroBinaryOp(left, op_map[op_token.type], right, op_token.line, op_token.column)
        return left

    def parse_multiplication(self) -> AIMacroASTNode:
        left = self.parse_power()
        while self.match(TokenType.MULTIPLY, TokenType.DIVIDE, TokenType.MODULO):
            op_token = self.current_token()
            self.advance()
            right = self.parse_power()
            op_map = {TokenType.MULTIPLY: '*', TokenType.DIVIDE: '/', TokenType.MODULO: '%'}
            left = AIMacroBinaryOp(left, op_map[op_token.type], right, op_token.line, op_token.column)
        return left

    def parse_power(self) -> AIMacroASTNode:
        left = self.parse_unary()
        if self.match(TokenType.POWER):
            op_token = self.consume(TokenType.POWER)
            right = self.parse_power()
            left = AIMacroBinaryOp(left, '**', right, op_token.line, op_token.column)
        return left

    def parse_unary(self) -> AIMacroASTNode:
        if self.match(TokenType.NOT):
            op_token = self.consume(TokenType.NOT)
            return AIMacroUnaryOp('not', self.parse_unary(), op_token.line, op_token.column)
        elif self.match(TokenType.MINUS):
            op_token = self.consume(TokenType.MINUS)
            return AIMacroUnaryOp('-', self.parse_unary(), op_token.line, op_token.column)
        return self.parse_primary()

    def parse_primary(self) -> AIMacroASTNode:
        # This function will now parse a "base" and then check for chained calls.
        
        # Step 1: Parse the base expression.
        token = self.current_token()
        if self.match(TokenType.NUMBER):
            self.advance()
            expr = AIMacroNumber(token.value, token.line, token.column)
        elif self.match(TokenType.STRING):
            self.advance()
            expr = AIMacroString(token.value, token.line, token.column)
        elif self.match(TokenType.TRUE):
            self.advance()
            expr = AIMacroBoolean(True, token.line, token.column)
        elif self.match(TokenType.FALSE):
            self.advance()
            expr = AIMacroBoolean(False, token.line, token.column)
        elif self.match(TokenType.IDENTIFIER):
            # Just parse the identifier. Function calls are handled in the loop below.
            self.advance()
            expr = AIMacroIdentifier(token.value, token.line, token.column)
        elif self.match(TokenType.LPAREN):
            self.advance()
            expr = self.parse_expression()
            self.consume(TokenType.RPAREN)
        elif self.match(TokenType.LBRACKET):
            expr = self.parse_list_literal()
        elif self.match(TokenType.LBRACE):
            expr = self.parse_dict_literal()
        else:
            self.error(f"Unexpected token: {token.type.name}")

        # Step 2: Loop to handle chained calls (function calls, method calls).
        while True:
            if self.match(TokenType.LPAREN):
                # This is a function call, e.g., my_func(arg)
                if not isinstance(expr, AIMacroIdentifier):
                    self.error("Expression is not callable")
                
                # Create a fake token to pass to the existing function call parser
                name_token = Token(TokenType.IDENTIFIER, expr.name, expr.line, expr.column, len(expr.name))
                expr = self.parse_function_call(name_token)

            elif self.match(TokenType.DOT):
                # This is a method call, e.g., my_obj.method(arg)
                dot_token = self.consume(TokenType.DOT)
                method_name_token = self.consume(TokenType.IDENTIFIER)
                
                self.consume(TokenType.LPAREN, "Expected '(' for a method call")
                args = []
                if not self.match(TokenType.RPAREN):
                    while True:
                        args.append(self.parse_expression())
                        if self.match(TokenType.RPAREN):
                            break
                        self.consume(TokenType.COMMA)
                self.consume(TokenType.RPAREN)
                
                expr = AIMacroMethodCall(expr, method_name_token.value, args, dot_token.line, dot_token.column)
            
            elif self.match(TokenType.LBRACKET):
                # This is an index access, e.g., my_list[0]
                lbracket_token = self.consume(TokenType.LBRACKET)
                index_expr = self.parse_expression()
                self.consume(TokenType.RBRACKET)
                expr = AIMacroIndexAccess(expr, index_expr, lbracket_token.line, lbracket_token.column)

            else:
                # No more chained calls, break the loop.
                break
                
        return expr

    def parse_function_call(self, name_token: Token) -> AIMacroFunctionCall:
        self.consume(TokenType.LPAREN)
        args = []
        while not self.match(TokenType.RPAREN):
            args.append(self.parse_expression())
            if self.match(TokenType.COMMA):
                self.advance()
        self.consume(TokenType.RPAREN)
        return AIMacroFunctionCall(name_token.value, args, name_token.line, name_token.column)

    def parse_list_literal(self) -> AIMacroList:
        lbracket_token = self.consume(TokenType.LBRACKET)
        elements = []
        while not self.match(TokenType.RBRACKET):
            elements.append(self.parse_expression())
            if self.match(TokenType.COMMA):
                self.advance()
        self.consume(TokenType.RBRACKET)
        return AIMacroList(elements, lbracket_token.line, lbracket_token.column)

    def parse_dict_literal(self) -> AIMacroDict:
        lbrace_token = self.consume(TokenType.LBRACE)
        pairs = []
        if not self.match(TokenType.RBRACE):
            while True:
                key = self.parse_expression()
                self.consume(TokenType.COLON)
                value = self.parse_expression()
                pairs.append((key, value))
                if self.match(TokenType.RBRACE):
                    break
                self.consume(TokenType.COMMA)
        self.consume(TokenType.RBRACE)
        return AIMacroDict(pairs, lbrace_token.line, lbrace_token.column)