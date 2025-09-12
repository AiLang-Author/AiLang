# parser.py - ENHANCED FOR SYSTEMS PROGRAMMING
from typing import List, Optional, Tuple, Dict
from .lexer import TokenType, Token, LexerError
from .ailang_ast import *

class ParseError(Exception):
    def __init__(self, message: str, token: Optional[Token] = None):
        self.message = message
        self.token = token
        if token:
            super().__init__(f"Parse error at line {token.line}, column {token.column}: {message}")
        else:
            super().__init__(f"Parse error: {message}")

class Parser:
    def __init__(self, tokens: List[Token], strict_math: bool = True):
        self.tokens = tokens
        self.position = 0
        self.current_token = self.tokens[0] if tokens else None
        self.previous_token = None
        self.strict_math = strict_math
        self.context_stack: List[str] = []

    def push_context(self, context: str):
        self.context_stack.append(context)

    def pop_context(self):
        if self.context_stack:
            self.context_stack.pop()

    def get_context(self) -> str:
        return " > ".join(self.context_stack) if self.context_stack else "top level"

    def error(self, message: str):
        context = self.get_context()
        raise ParseError(f"In {context}: {message}", self.current_token)

    def advance(self):
        if self.position < len(self.tokens) - 1:
            self.position += 1
            self.previous_token = self.current_token
            self.current_token = self.tokens[self.position]

    def peek(self, offset: int = 1) -> Optional[Token]:
        pos = self.position + offset
        if pos < len(self.tokens):
            return self.tokens[pos]
        return None

    def match(self, *token_types: TokenType) -> bool:
        return self.current_token and self.current_token.type in token_types


    def check(self, token_type: TokenType) -> bool:
        """Check if current token matches type without consuming it"""
        return self.current_token and self.current_token.type == token_type
    def match_sequence(self, *token_types: TokenType) -> bool:
        for i, token_type in enumerate(token_types):
            token = self.peek(i) if i > 0 else self.current_token
            if not token or token.type != token_type:
                return False
        return True

    def consume(self, token_type: TokenType, message: str = "") -> Token:
        if not self.current_token:
            self.error(f"Expected {token_type.name} but reached end of file. {message}")
        if self.current_token.type != token_type:
            self.error(f"Expected {token_type.name}, got {self.current_token.type.name}. {message}")
        token = self.current_token
        self.advance()
        return token

    def skip_newlines(self):
        while self.match(TokenType.NEWLINE):
            self.advance()

    def parse(self) -> Program:
        self.push_context("program")
        declarations = []
        self.skip_newlines()
        while not self.match(TokenType.EOF):
            self.skip_newlines()
            if self.match(TokenType.EOF):
                break
            if self.match(TokenType.COMMENT, TokenType.DOC_COMMENT, TokenType.COM_COMMENT, TokenType.TAG_COMMENT):
                self.advance()
                continue
            decl = self.parse_declaration()
            if decl:
                declarations.append(decl)
            self.skip_newlines()
        self.pop_context()
        return Program(declarations=declarations, line=1, column=1)

    def parse_declaration(self) -> Optional[ASTNode]:
        if self.match(TokenType.LIBRARYIMPORT):
            return self.parse_library()
        elif self.match(TokenType.IDENTIFIER) and self.current_token.value == "AcronymDefinitions":
            return self.parse_acronym_definitions()
        elif self.match(TokenType.FIXEDPOOL, TokenType.DYNAMICPOOL, TokenType.TEMPORALPOOL,
                       TokenType.NEURALPOOL, TokenType.KERNELPOOL, TokenType.ACTORPOOL,
                       TokenType.SECURITYPOOL, TokenType.CONSTRAINEDPOOL, TokenType.FILEPOOL):
            return self.parse_pool()
        elif self.match(TokenType.LOOPMAIN, TokenType.LOOPACTOR, TokenType.LOOPSTART,
                       TokenType.LOOPSHADOW):
            return self.parse_loop()
        elif self.match(TokenType.SUBROUTINE):
            return self.parse_subroutine()
        elif self.match(TokenType.FUNCTION):
            return self.parse_function()
        elif self.match(TokenType.COMBINATOR):
            return self.parse_combinator()
        elif self.match(TokenType.MACROBLOCK):
            return self.parse_macro_block()
        elif self.match(TokenType.SECURITYCONTEXT):
            return self.parse_security_context()
        elif self.match(TokenType.CONSTRAINEDTYPE):
            return self.parse_constrained_type()
        elif self.match(TokenType.CONSTANT):
            return self.parse_constant()
        # === NEW: Low-Level Declaration Parsing ===
        elif self.match(TokenType.INTERRUPTHANDLER):
            return self.parse_interrupt_handler()
        elif self.match(TokenType.DEVICEDRIVER):
            return self.parse_device_driver()
        elif self.match(TokenType.BOOTLOADER):
            return self.parse_bootloader_code()
        elif self.match(TokenType.KERNELENTRY):
            return self.parse_kernel_entry()
        else:
            stmt = self.parse_statement()
            if stmt:
                return stmt
            if self.current_token:
                self.error(f"Unexpected token '{self.current_token.value}' at top level")
            return None

    def parse_library(self) -> Library:
        self.push_context("library")
        start_token = self.consume(TokenType.LIBRARYIMPORT)
        self.consume(TokenType.DOT)
        name = self.parse_dotted_name()
        
        body = []  # Default to an empty body for simple imports
        self.skip_newlines()

        # --- NEW: Check for an optional body ---
        if self.match(TokenType.LBRACE):
            self.consume(TokenType.LBRACE)
            self.skip_newlines()
            
            while not self.match(TokenType.RBRACE):
                # This is your original logic for parsing the body's contents
                self.skip_newlines()
                if self.match(TokenType.EOF):
                    self.error("Unclosed library body, reached end of file.")
                
                # You can add more declaration types here as needed
                if self.match(TokenType.LIBRARYIMPORT):
                    body.append(self.parse_library())
                elif self.match(TokenType.FUNCTION):
                    body.append(self.parse_function())
                elif self.match(TokenType.CONSTANT):
                    body.append(self.parse_constant())
                else:
                    self.advance() # Move past unexpected tokens to avoid getting stuck
                
                self.skip_newlines()

            self.consume(TokenType.RBRACE)
        # --- END NEW LOGIC ---

        self.pop_context()
        return Library(name=name, body=body, line=start_token.line, column=start_token.column)

    def parse_dotted_name(self) -> str:
        parts = [self.consume(TokenType.IDENTIFIER).value]
        while self.match(TokenType.DOT) and self.peek() and self.peek().type == TokenType.IDENTIFIER:
            self.consume(TokenType.DOT)
            parts.append(self.consume(TokenType.IDENTIFIER).value)
        return '.'.join(parts)

    def parse_pool(self) -> Pool:
        pool_type_token = self.current_token
        pool_type = pool_type_token.value
        self.advance() # Consumes the pool type (e.g., DynamicPool)
        self.push_context(f"{pool_type}")

        # --- FIX: Consume the DOT token between the type and the name ---
        self.consume(TokenType.DOT)
        # --- END OF FIX ---
        
        name = self.consume(TokenType.IDENTIFIER).value # Now correctly finds the name
        self.skip_newlines()
        self.consume(TokenType.LBRACE)
        self.skip_newlines()
        body = []
        while not self.match(TokenType.RBRACE):
            self.skip_newlines()
            if self.match(TokenType.STRING):
                item = self.parse_resource_item()
                body.append(item)
            else:
                if not self.match(TokenType.RBRACE):
                    self.error(f"Expected string key for resource item, got {self.current_token.type.name}")
            self.skip_newlines()
        self.consume(TokenType.RBRACE)
        self.pop_context()
        return Pool(pool_type=pool_type, name=name, body=body,
                    line=pool_type_token.line, column=pool_type_token.column)

    def parse_subpool(self) -> SubPool:
        start_token = self.consume(TokenType.SUBPOOL)
        self.consume(TokenType.DOT)
        name = self.consume(TokenType.IDENTIFIER).value
        self.push_context(f"SubPool.{name}")
        self.skip_newlines()
        self.consume(TokenType.LBRACE)
        self.skip_newlines()
        items = {}
        while not self.match(TokenType.RBRACE):
            self.skip_newlines()
            if self.match(TokenType.STRING):
                item = self.parse_resource_item()
                items[item.key] = item
            else:
                self.advance()
            self.skip_newlines()
        self.consume(TokenType.RBRACE)
        self.pop_context()
        return SubPool(name=name, items=items, line=start_token.line, column=start_token.column)

    def parse_resource_item(self) -> ResourceItem:
        key = self.consume(TokenType.STRING).value
        self.consume(TokenType.COLON)
        value = None
        attributes = {}
        
        # Handle first attribute - can be Initialize OR ElementType
        if self.match(TokenType.INITIALIZE):
            self.consume(TokenType.INITIALIZE)
            self.consume(TokenType.EQUALS)
            value = self.parse_primary()
        elif self.match(TokenType.ELEMENTTYPE):
            self.consume(TokenType.ELEMENTTYPE)
            self.consume(TokenType.DASH)
            attributes['ElementType'] = self.parse_type()  # Use parse_type() for type names
        
        # Handle comma-separated additional attributes
        while self.match(TokenType.COMMA):
            self.consume(TokenType.COMMA)
            self.skip_newlines()
            
            if self.match(TokenType.CANCHANGE):
                self.consume(TokenType.CANCHANGE)
                self.consume(TokenType.DASH)
                attributes['CanChange'] = self.parse_primary()
            elif self.match(TokenType.CANBENULL):
                self.consume(TokenType.CANBENULL)
                self.consume(TokenType.DASH)
                attributes['CanBeNull'] = self.parse_primary()
            elif self.match(TokenType.RANGE):
                self.consume(TokenType.RANGE)
                self.consume(TokenType.DASH)
                attributes['Range'] = self.parse_array_literal()
            elif self.match(TokenType.MAXIMUMLENGTH):
                self.consume(TokenType.MAXIMUMLENGTH)
                self.consume(TokenType.DASH)
                attributes['MaximumLength'] = self.parse_primary()
            elif self.match(TokenType.MINIMUMLENGTH):
                self.consume(TokenType.MINIMUMLENGTH)
                self.consume(TokenType.DASH)
                attributes['MinimumLength'] = self.parse_primary()
            elif self.match(TokenType.ELEMENTTYPE):
                # ElementType can also appear as additional attribute
                self.consume(TokenType.ELEMENTTYPE)
                self.consume(TokenType.DASH)
                attributes['ElementType'] = self.parse_type()  # Use parse_type() here too
            else:
                # Generic attribute handling
                if self.match(TokenType.IDENTIFIER):
                    attr_name = self.consume(TokenType.IDENTIFIER).value
                    self.consume(TokenType.DASH)
                    attributes[attr_name] = self.parse_expression()
                else:
                    break
        
        return ResourceItem(key=key, value=value, attributes=attributes,
                            line=self.current_token.line, column=self.current_token.column)

    # === NEW: Low-Level Parsing Methods ===

    def parse_interrupt_handler(self) -> InterruptHandler:
        """Parse interrupt handler declaration"""
        start_token = self.consume(TokenType.INTERRUPTHANDLER)
        self.consume(TokenType.DOT)
        handler_name = self.consume(TokenType.IDENTIFIER).value
        self.consume(TokenType.LPAREN)
        
        # Parse interrupt vector
        vector = self.parse_expression()
        self.consume(TokenType.RPAREN)
        
        self.skip_newlines()
        self.consume(TokenType.LBRACE)
        self.skip_newlines()
        
        # Parse handler body
        body = []
        while not self.match(TokenType.RBRACE):
            stmt = self.parse_statement()
            if stmt:
                body.append(stmt)
            self.skip_newlines()
        
        self.consume(TokenType.RBRACE)
        return InterruptHandler(
            handler_type="interrupt",
            vector=vector,
            handler_name=handler_name,
            body=body,
            line=start_token.line,
            column=start_token.column
        )

    def parse_device_driver(self) -> DeviceDriver:
        """Parse device driver declaration"""
        start_token = self.consume(TokenType.DEVICEDRIVER)
        self.consume(TokenType.DOT)
        driver_name = self.consume(TokenType.IDENTIFIER).value
        
        # Parse device type
        self.consume(TokenType.COLON)
        device_type = self.consume(TokenType.IDENTIFIER).value
        
        self.skip_newlines()
        self.consume(TokenType.LBRACE)
        self.skip_newlines()
        
        # Parse driver operations
        operations = {}
        while not self.match(TokenType.RBRACE):
            self.skip_newlines()
            if self.match(TokenType.IDENTIFIER):
                op_name = self.consume(TokenType.IDENTIFIER).value
                self.consume(TokenType.COLON)
                operations[op_name] = self.parse_expression()
            else:
                self.advance()
            self.skip_newlines()
        
        self.consume(TokenType.RBRACE)
        return DeviceDriver(
            driver_name=driver_name,
            device_type=device_type,
            operations=operations,
            line=start_token.line,
            column=start_token.column
        )

    def parse_bootloader_code(self) -> BootloaderCode:
        """Parse bootloader code block"""
        start_token = self.consume(TokenType.BOOTLOADER)
        self.consume(TokenType.DOT)
        stage = self.consume(TokenType.IDENTIFIER).value
        
        self.skip_newlines()
        self.consume(TokenType.LBRACE)
        self.skip_newlines()
        
        body = []
        while not self.match(TokenType.RBRACE):
            stmt = self.parse_statement()
            if stmt:
                body.append(stmt)
            self.skip_newlines()
        
        self.consume(TokenType.RBRACE)
        return BootloaderCode(
            stage=stage,
            body=body,
            line=start_token.line,
            column=start_token.column
        )

    def parse_kernel_entry(self) -> KernelEntry:
        """Parse kernel entry point"""
        start_token = self.consume(TokenType.KERNELENTRY)
        self.consume(TokenType.DOT)
        entry_name = self.consume(TokenType.IDENTIFIER).value
        
        # Optional parameters
        parameters = []
        if self.match(TokenType.LPAREN):
            self.consume(TokenType.LPAREN)
            while not self.match(TokenType.RPAREN):
                param_name = self.consume(TokenType.IDENTIFIER).value
                self.consume(TokenType.COLON)
                param_type = self.parse_type()
                parameters.append((param_name, param_type))
                if self.match(TokenType.COMMA):
                    self.consume(TokenType.COMMA)
                    self.skip_newlines()
            self.consume(TokenType.RPAREN)
        
        self.skip_newlines()
        self.consume(TokenType.LBRACE)
        self.skip_newlines()
        
        body = []
        while not self.match(TokenType.RBRACE):
            stmt = self.parse_statement()
            if stmt:
                body.append(stmt)
            self.skip_newlines()
        
        self.consume(TokenType.RBRACE)
        return KernelEntry(
            entry_name=entry_name,
            parameters=parameters,
            body=body,
            line=start_token.line,
            column=start_token.column
        )

    def parse_loop(self) -> ASTNode:  # Note: return type is now ASTNode, not Loop
        loop_type_token = self.current_token
        loop_type = loop_type_token.value
        self.advance()
        self.push_context(f"{loop_type}")
        self.consume(TokenType.DOT)
        name = self.consume(TokenType.IDENTIFIER).value
        self.skip_newlines()
        self.consume(TokenType.LBRACE)
        self.skip_newlines()
        body = []
        while not self.match(TokenType.RBRACE):
            stmt = self.parse_statement()
            if stmt:
                body.append(stmt)
            self.skip_newlines()
        self.consume(TokenType.RBRACE)
        self.skip_newlines()
        
        self.pop_context()
        
        # Create the correct AST node type based on loop_type
        if loop_type == 'LoopActor':
            return LoopActor(name=name, body=body, 
                            line=loop_type_token.line, column=loop_type_token.column)
        elif loop_type == 'LoopMain':
            return LoopMain(name=name, body=body,
                        line=loop_type_token.line, column=loop_type_token.column)
        elif loop_type == 'LoopStart':
            return LoopStart(name=name, body=body,
                            line=loop_type_token.line, column=loop_type_token.column)
        elif loop_type == 'LoopShadow':
            return LoopShadow(name=name, body=body,
                            line=loop_type_token.line, column=loop_type_token.column)
        else:
            # Fallback for any other loop types
            end_name = None
            if self.match(TokenType.LOOPEND):
                self.consume(TokenType.LOOPEND)
                self.consume(TokenType.DOT)
                end_name = self.consume(TokenType.IDENTIFIER).value
            return Loop(loop_type=loop_type, name=name, body=body, end_name=end_name,
                    line=loop_type_token.line, column=loop_type_token.column)

    def parse_subroutine(self) -> SubRoutine:
        start_token = self.consume(TokenType.SUBROUTINE)
        self.consume(TokenType.DOT)
        name = self.parse_dotted_name()
        self.push_context(f"SubRoutine.{name}")
        self.skip_newlines()
        self.consume(TokenType.LBRACE)
        self.skip_newlines()
        body = []
        while not self.match(TokenType.RBRACE):
            stmt = self.parse_statement()
            if stmt:
                body.append(stmt)
            self.skip_newlines()
        self.consume(TokenType.RBRACE)
        self.pop_context()
        return SubRoutine(name=name, body=body, line=start_token.line, column=start_token.column)

    def parse_function(self) -> Function:
        start_token = self.consume(TokenType.FUNCTION)
        self.consume(TokenType.DOT)
        name = self.parse_dotted_name()
        self.push_context(f"Function.{name}")
        self.skip_newlines()
        self.consume(TokenType.LBRACE)
        self.skip_newlines()
        input_params = []
        output_type = None
        body = []
        while not self.match(TokenType.RBRACE):
            self.skip_newlines()
            if self.match(TokenType.INPUT):
                self.consume(TokenType.INPUT)
                self.consume(TokenType.COLON)
                if self.match(TokenType.LPAREN):
                    self.consume(TokenType.LPAREN)
                    while not self.match(TokenType.RPAREN):
                        param_name = self.consume(TokenType.IDENTIFIER).value
                        self.consume(TokenType.COLON)
                        param_type = self.parse_type()
                        input_params.append((param_name, param_type))
                        if self.match(TokenType.COMMA):
                            self.consume(TokenType.COMMA)
                            self.skip_newlines()
                    self.consume(TokenType.RPAREN)
                else:
                    param_name = self.consume(TokenType.IDENTIFIER).value
                    self.consume(TokenType.COLON)
                    param_type = self.parse_type()
                    input_params.append((param_name, param_type))
            elif self.match(TokenType.OUTPUT):
                self.consume(TokenType.OUTPUT)
                self.consume(TokenType.COLON)
                output_type = self.parse_type()
            elif self.match(TokenType.BODY):
                self.consume(TokenType.BODY)
                self.consume(TokenType.COLON)
                self.skip_newlines()
                self.consume(TokenType.LBRACE)
                self.skip_newlines()
                while not self.match(TokenType.RBRACE):
                    stmt = self.parse_statement()
                    if stmt:
                        body.append(stmt)
                    self.skip_newlines()
                self.consume(TokenType.RBRACE)
            else:
                self.advance()
            self.skip_newlines()
        self.consume(TokenType.RBRACE)
        self.pop_context()
        return Function(name=name, input_params=input_params, output_type=output_type,
                        body=body, line=start_token.line, column=start_token.column)

    def parse_lambda(self) -> Lambda:
        start_token = self.consume(TokenType.LAMBDA)
        self.consume(TokenType.LPAREN)
        params = []
        while not self.match(TokenType.RPAREN):
            params.append(self.consume(TokenType.IDENTIFIER).value)
            if self.match(TokenType.COMMA):
                self.consume(TokenType.COMMA)
                self.skip_newlines()
        self.consume(TokenType.RPAREN)
        self.skip_newlines()
        self.consume(TokenType.LBRACE)
        self.skip_newlines()
        body = self.parse_expression()
        self.skip_newlines()
        self.consume(TokenType.RBRACE)
        return Lambda(params=params, body=body, line=start_token.line, column=start_token.column)

    def parse_combinator(self) -> Combinator:
        start_token = self.consume(TokenType.COMBINATOR)
        self.consume(TokenType.DOT)
        name = self.consume(TokenType.IDENTIFIER).value
        self.consume(TokenType.EQUALS)
        self.skip_newlines()
        definition = self.parse_expression()
        return Combinator(name=name, definition=definition,
                         line=start_token.line, column=start_token.column)

    def parse_macro_block(self) -> MacroBlock:
        start_token = self.consume(TokenType.MACROBLOCK)
        self.consume(TokenType.DOT)
        name = self.parse_dotted_name()
        self.push_context(f"MacroBlock.{name}")
        self.skip_newlines()
        self.consume(TokenType.LBRACE)
        self.skip_newlines()
        macros = {}
        while not self.match(TokenType.RBRACE):
            self.skip_newlines()
            if self.match(TokenType.MACRO):
                macro = self.parse_macro_definition()
                macros[macro.name] = macro
            else:
                self.advance()
            self.skip_newlines()
        self.consume(TokenType.RBRACE)
        self.pop_context()
        return MacroBlock(name=name, macros=macros,
                         line=start_token.line, column=start_token.column)

    def parse_macro_definition(self) -> MacroDefinition:
        start_token = self.consume(TokenType.MACRO)
        self.consume(TokenType.DOT)
        name = self.consume(TokenType.IDENTIFIER).value
        self.consume(TokenType.LPAREN)
        params = []
        while not self.match(TokenType.RPAREN):
            params.append(self.consume(TokenType.IDENTIFIER).value)
            if self.match(TokenType.COMMA):
                self.consume(TokenType.COMMA)
                self.skip_newlines()
        self.consume(TokenType.RPAREN)
        self.consume(TokenType.EQUALS)
        self.skip_newlines()
        body = self.parse_expression()
        return MacroDefinition(name=name, params=params, body=body,
                             line=start_token.line, column=start_token.column)

    def parse_security_context(self) -> SecurityContext:
        start_token = self.consume(TokenType.SECURITYCONTEXT)
        self.consume(TokenType.DOT)
        name = self.consume(TokenType.IDENTIFIER).value
        self.push_context(f"SecurityContext.{name}")
        self.skip_newlines()
        self.consume(TokenType.LBRACE)
        self.skip_newlines()
        levels = {}
        while not self.match(TokenType.RBRACE):
            self.skip_newlines()
            if self.match(TokenType.LEVEL):
                level = self.parse_security_level()
                levels[level.name] = level
            else:
                self.advance()
            self.skip_newlines()
        self.consume(TokenType.RBRACE)
        self.pop_context()
        return SecurityContext(name=name, levels=levels,
                             line=start_token.line, column=start_token.column)

    def parse_security_level(self) -> SecurityLevel:
        self.consume(TokenType.LEVEL)
        self.consume(TokenType.DOT)
        name = self.consume(TokenType.IDENTIFIER).value
        self.consume(TokenType.EQUALS)
        self.consume(TokenType.LBRACE)
        self.skip_newlines()
        allowed_operations = []
        denied_operations = []
        memory_limit = None
        cpu_quota = None
        while not self.match(TokenType.RBRACE):
            self.skip_newlines()
            if self.match(TokenType.ALLOWEDOPERATIONS):
                self.consume(TokenType.ALLOWEDOPERATIONS)
                self.consume(TokenType.COLON)
                allowed_operations = self.parse_string_array()
            elif self.match(TokenType.DENIEDOPERATIONS):
                self.consume(TokenType.DENIEDOPERATIONS)
                self.consume(TokenType.COLON)
                denied_operations = self.parse_string_array()
            elif self.match(TokenType.MEMORYLIMIT):
                self.consume(TokenType.MEMORYLIMIT)
                self.consume(TokenType.COLON)
                memory_limit = self.parse_expression()
            elif self.match(TokenType.CPUQUOTA):
                self.consume(TokenType.CPUQUOTA)
                self.consume(TokenType.COLON)
                cpu_quota = self.parse_expression()
            else:
                self.advance()
            if self.match(TokenType.COMMA):
                self.consume(TokenType.COMMA)
            self.skip_newlines()
        self.consume(TokenType.RBRACE)
        return SecurityLevel(name=name, allowed_operations=allowed_operations,
                             denied_operations=denied_operations,
                             memory_limit=memory_limit, cpu_quota=cpu_quota,
                             line=self.current_token.line, column=self.current_token.column)

    def parse_constrained_type(self) -> ConstrainedType:
        start_token = self.consume(TokenType.CONSTRAINEDTYPE)
        self.consume(TokenType.DOT)
        name = self.consume(TokenType.IDENTIFIER).value
        self.consume(TokenType.EQUALS)
        base_type = self.parse_type()
        self.consume(TokenType.WHERE)
        self.consume(TokenType.LBRACE)
        constraints = self.parse_expression()
        self.consume(TokenType.RBRACE)
        return ConstrainedType(name=name, base_type=base_type, constraints=constraints,
                               line=start_token.line, column=start_token.column)

    def parse_constant(self) -> Constant:
        start_token = self.consume(TokenType.CONSTANT)
        self.consume(TokenType.DOT)
        name = self.consume(TokenType.IDENTIFIER).value
        self.consume(TokenType.EQUALS)
        value = self.parse_expression()
        return Constant(name=name, value=value,
                        line=start_token.line, column=start_token.column)

    def parse_statement(self) -> Optional[ASTNode]:
        self.skip_newlines()
        if self.match(TokenType.COMMENT, TokenType.DOC_COMMENT, TokenType.COM_COMMENT, TokenType.TAG_COMMENT):
            self.advance()
            return None
        if self.match(TokenType.RUNTASK):
            return self.parse_runtask()
        elif self.match(TokenType.PRINTMESSAGE):
            return self.parse_printmessage()
        # Check for Debug statements
        elif self.current_token and hasattr(self.current_token, "value") and self.current_token.value == "Debug":
            return self.parse_debug_block()
        elif self.current_token and hasattr(self.current_token, "value") and self.current_token.value == "DebugAssert":
            return self.parse_debug_assert()
        elif self.match(TokenType.DEBUGPERF):
            return self.parse_debug_perf()    
        elif self.match(TokenType.RETURNVALUE):
            return self.parse_returnvalue()
        elif self.match(TokenType.IFCONDITION):
            return self.parse_if()
        elif self.match(TokenType.CHOOSEPATH):
            return self.parse_choosepath()
        elif self.match(TokenType.WHILELOOP):
            return self.parse_while()
        elif self.match(TokenType.FOREVERY):
            return self.parse_forevery()
        elif self.match(TokenType.FORK):
            return self.parse_fork()
        elif self.match(TokenType.BRANCH):
            return self.parse_branch()
        elif self.match(TokenType.TRYBLOCK):
            return self.parse_try()
        elif self.match(TokenType.SENDMESSAGE):
            return self.parse_sendmessage()
        elif self.match(TokenType.RECEIVEMESSAGE):
            return self.parse_receivemessage()
        elif self.match(TokenType.EVERYINTERVAL):
            return self.parse_everyinterval()
        elif self.match(TokenType.WITHSECURITY):
            return self.parse_withsecurity()
        elif self.match(TokenType.BREAKLOOP):
            self.advance()
            return BreakLoop(line=self.current_token.line, column=self.current_token.column)
        elif self.match(TokenType.CONTINUELOOP):
            self.advance()
            return ContinueLoop(line=self.current_token.line, column=self.current_token.column)
        elif self.match(TokenType.HALTPROGRAM):
            return self.parse_haltprogram()
        # === NEW: Low-Level Statement Parsing ===
        elif self.match(TokenType.ENABLEINTERRUPTS):
            return self.parse_interrupt_control()
        elif self.match(TokenType.DISABLEINTERRUPTS):
            return self.parse_interrupt_control()
        elif self.match(TokenType.INLINEASSEMBLY):
            return self.parse_inline_assembly()
        elif self.match(TokenType.SYSTEMCALL):
            return self.parse_system_call()
        # === NEW: Virtual Memory Statement Parsing ===
        elif self.match(TokenType.PAGETABLE, TokenType.VIRTUALMEMORY, TokenType.CACHE, 
                    TokenType.TLB, TokenType.MEMORYBARRIER):
            return self.parse_vm_operation()
        elif self.match(TokenType.IDENTIFIER):
            if self.peek() and self.peek().type == TokenType.EQUALS:
                return self.parse_assignment()
            else:
                expr = self.parse_expression()
                return expr
        else:
            expr = self.parse_expression()
            if expr:
                return expr
            if self.current_token and self.current_token.type != TokenType.EOF:
                self.advance()
            return None

    # === NEW: Low-Level Statement Parsing Methods ===

    def parse_interrupt_control(self) -> InterruptControl:
        """Parse interrupt control statements"""
        start_token = self.current_token
        operation = "enable" if start_token.type == TokenType.ENABLEINTERRUPTS else "disable"
        self.advance()
        
        return InterruptControl(
            operation=operation,
            line=start_token.line,
            column=start_token.column
        )

    def parse_inline_assembly(self) -> InlineAssembly:
        """Parse inline assembly blocks"""
        start_token = self.consume(TokenType.INLINEASSEMBLY)
        self.consume(TokenType.LPAREN)
        
        # Parse assembly code string
        assembly_code = self.consume(TokenType.STRING).value
        
        # Optional inputs, outputs, clobbers
        inputs = []
        outputs = []
        clobbers = []
        volatile = False
        
        while self.match(TokenType.COMMA):
            self.consume(TokenType.COMMA)
            self.skip_newlines()
            
            if self.match(TokenType.IDENTIFIER):
                param_name = self.consume(TokenType.IDENTIFIER).value
                self.consume(TokenType.COLON)
                
                if param_name == "inputs":
                    inputs = self.parse_assembly_constraints()
                elif param_name == "outputs":
                    outputs = self.parse_assembly_constraints()
                elif param_name == "clobbers":
                    clobbers = self.parse_string_array()
                elif param_name == "volatile":
                    volatile = self.parse_expression().value if hasattr(self.parse_expression(), 'value') else True
        
        self.consume(TokenType.RPAREN)
        return InlineAssembly(
            assembly_code=assembly_code,
            inputs=inputs,
            outputs=outputs,
            clobbers=clobbers,
            volatile=volatile,
            line=start_token.line,
            column=start_token.column
        )

    def parse_assembly_constraints(self) -> List[Tuple[str, ASTNode]]:
        """Parse assembly input/output constraints"""
        constraints = []
        self.consume(TokenType.LBRACKET)
        
        while not self.match(TokenType.RBRACKET):
            constraint = self.consume(TokenType.STRING).value
            self.consume(TokenType.COLON)
            value = self.parse_expression()
            constraints.append((constraint, value))
            
            if self.match(TokenType.COMMA):
                self.consume(TokenType.COMMA)
                self.skip_newlines()
        
        self.consume(TokenType.RBRACKET)
        return constraints

    def parse_system_call(self) -> SystemCall:
        """Parse system call statements"""
        start_token = self.consume(TokenType.SYSTEMCALL)
        self.consume(TokenType.LPAREN)
        
        call_number = self.parse_expression()
        arguments = []
        
        while self.match(TokenType.COMMA):
            self.consume(TokenType.COMMA)
            self.skip_newlines()
            arguments.append(self.parse_expression())
        
        self.consume(TokenType.RPAREN)
        return SystemCall(
            call_number=call_number,
            arguments=arguments,
            line=start_token.line,
            column=start_token.column
        )

    def parse_runtask(self) -> RunTask:
        """Parse RunTask with function call syntax for consistency"""
        start_token = self.consume(TokenType.RUNTASK)
        self.consume(TokenType.LPAREN)  # Change from DOT to LPAREN
        
        # Get task name as string literal
        if self.match(TokenType.STRING):
            task_name = self.consume(TokenType.STRING).value
        else:
            self.error("RunTask requires a task name string")
        
        # No additional arguments for now - just close paren
        self.consume(TokenType.RPAREN)
        
        return RunTask(
            task_name=task_name, 
            arguments=[],
            line=start_token.line, 
            column=start_token.column
        )

    def parse_printmessage(self) -> PrintMessage:
        start_token = self.consume(TokenType.PRINTMESSAGE)
        self.consume(TokenType.LPAREN)
        message = self.parse_expression()
        self.consume(TokenType.RPAREN)
        return PrintMessage(message=message, line=start_token.line, column=start_token.column)

    def parse_returnvalue(self) -> ReturnValue:
        start_token = self.consume(TokenType.RETURNVALUE)
        self.consume(TokenType.LPAREN)
        value = self.parse_expression()
        self.consume(TokenType.RPAREN)
        return ReturnValue(value=value, line=start_token.line, column=start_token.column)

    def parse_if(self) -> If:
        start_token = self.consume(TokenType.IFCONDITION)
        condition = self.parse_expression()
        self.consume(TokenType.THENBLOCK)
        self.skip_newlines()
        self.consume(TokenType.LBRACE)
        self.skip_newlines()
        self.push_context("IfCondition.ThenBlock")
        then_body = []
        while not self.match(TokenType.RBRACE):
            stmt = self.parse_statement()
            if stmt:
                then_body.append(stmt)
            self.skip_newlines()
        self.consume(TokenType.RBRACE)
        self.pop_context()
        self.skip_newlines()
        else_body = None
        if self.match(TokenType.ELSEBLOCK):
            self.consume(TokenType.ELSEBLOCK)
            self.skip_newlines()
            self.consume(TokenType.LBRACE)
            self.skip_newlines()
            self.push_context("IfCondition.ElseBlock")
            else_body = []
            while not self.match(TokenType.RBRACE):
                stmt = self.parse_statement()
                if stmt:
                    else_body.append(stmt)
                self.skip_newlines()
            self.consume(TokenType.RBRACE)
            self.pop_context()
        return If(condition=condition, then_body=then_body, else_body=else_body,
                  line=start_token.line, column=start_token.column)

    def parse_choosepath(self) -> ChoosePath:
        start_token = self.consume(TokenType.CHOOSEPATH)
        self.consume(TokenType.LPAREN)
        expression = self.parse_expression()
        self.consume(TokenType.RPAREN)
        self.skip_newlines()
        self.consume(TokenType.LBRACE)
        self.skip_newlines()
        self.push_context("ChoosePath")
        cases = []
        default = None
        while not self.match(TokenType.RBRACE):
            if self.match(TokenType.CASEOPTION):
                self.consume(TokenType.CASEOPTION)
                case_value = self.consume(TokenType.STRING).value
                self.consume(TokenType.COLON)
                case_body = []
                stmt = self.parse_statement()
                if stmt:
                    case_body.append(stmt)
                cases.append((case_value, case_body))
            elif self.match(TokenType.DEFAULTOPTION):
                self.consume(TokenType.DEFAULTOPTION)
                self.consume(TokenType.COLON)
                default = []
                stmt = self.parse_statement()
                if stmt:
                    default.append(stmt)
            else:
                self.advance()
            self.skip_newlines()
        self.consume(TokenType.RBRACE)
        self.pop_context()
        return ChoosePath(expression=expression, cases=cases, default=default,
                         line=start_token.line, column=start_token.column)

    def parse_while(self) -> While:
        start_token = self.consume(TokenType.WHILELOOP)
        condition = self.parse_expression()
        self.skip_newlines()
        self.consume(TokenType.LBRACE)
        self.skip_newlines()
        self.push_context("WhileLoop")
        body = []
        while not self.match(TokenType.RBRACE):
            stmt = self.parse_statement()
            if stmt:
                body.append(stmt)
            self.skip_newlines()
        self.consume(TokenType.RBRACE)
        self.pop_context()
        return While(condition=condition, body=body,
                     line=start_token.line, column=start_token.column)

    def parse_forevery(self) -> ForEvery:
        start_token = self.consume(TokenType.FOREVERY)
        variable = self.consume(TokenType.IDENTIFIER).value
        self.consume(TokenType.IN)
        collection = self.parse_expression()
        self.skip_newlines()
        self.consume(TokenType.LBRACE)
        self.skip_newlines()
        self.push_context(f"ForEvery({variable})")
        body = []
        while not self.match(TokenType.RBRACE):
            stmt = self.parse_statement()
            if stmt:
                body.append(stmt)
            self.skip_newlines()
        self.consume(TokenType.RBRACE)
        self.pop_context()
        return ForEvery(variable=variable, collection=collection, body=body,
                        line=start_token.line, column=start_token.column)
        
        
        
    def parse_fork(self) -> Fork:
        """Parse Fork construct: Fork condition { true_block } { false_block }"""
        start_token = self.consume(TokenType.FORK)
        self.push_context("Fork")
        
        # Parse condition expression
        condition = self.parse_expression()
        
        self.skip_newlines()
        
        # Parse true block
        self.consume(TokenType.LBRACE)
        self.skip_newlines()
        self.push_context("Fork.TrueBlock")
        true_block = []
        while not self.match(TokenType.RBRACE):
            stmt = self.parse_statement()
            if stmt:
                true_block.append(stmt)
            self.skip_newlines()
        self.consume(TokenType.RBRACE)
        self.pop_context()
        
        self.skip_newlines()
        
        # Parse false block (required - both blocks must be present)
        false_block = []
        if self.match(TokenType.LBRACE):
            self.consume(TokenType.LBRACE)
            self.skip_newlines()
            self.push_context("Fork.FalseBlock")
            while not self.match(TokenType.RBRACE):
                stmt = self.parse_statement()
                if stmt:
                    false_block.append(stmt)
                self.skip_newlines()
            self.consume(TokenType.RBRACE)
            self.pop_context()
        else:
            # Fork requires both blocks
            self.error("Fork requires both true and false blocks")
        
        self.pop_context()
        return Fork(condition=condition, true_block=true_block, false_block=false_block,
                    line=start_token.line, column=start_token.column)

    def parse_branch(self) -> Branch:
        """Parse Branch construct: Branch expression { Case value { block } ... Default { block } }"""
        start_token = self.consume(TokenType.BRANCH)
        self.push_context("Branch")
        
        # Parse expression to branch on
        expression = self.parse_expression()
        
        self.skip_newlines()
        self.consume(TokenType.LBRACE)
        self.skip_newlines()
        
        cases = []
        default = None
        
        while not self.match(TokenType.RBRACE):
            self.skip_newlines()
            
            if self.match(TokenType.CASE):
                self.consume(TokenType.CASE)
                
                # Parse case value (can be number, string, or identifier)
                if self.match(TokenType.NUMBER):
                    case_value = Number(value=self.current_token.value, 
                                    line=self.current_token.line, 
                                    column=self.current_token.column)
                    self.advance()
                elif self.match(TokenType.STRING):
                    case_value = String(value=self.current_token.value,
                                    line=self.current_token.line,
                                    column=self.current_token.column)
                    self.advance()
                elif self.match(TokenType.IDENTIFIER):
                    case_value = Identifier(name=self.current_token.value,
                                        line=self.current_token.line,
                                        column=self.current_token.column)
                    self.advance()
                else:
                    self.error("Case value must be a number, string, or identifier")
                
                self.skip_newlines()
                self.consume(TokenType.LBRACE)
                self.skip_newlines()
                
                self.push_context(f"Branch.Case({case_value})")
                case_body = []
                while not self.match(TokenType.RBRACE):
                    stmt = self.parse_statement()
                    if stmt:
                        case_body.append(stmt)
                    self.skip_newlines()
                self.consume(TokenType.RBRACE)
                self.pop_context()
                
                cases.append((case_value, case_body))
                
            elif self.match(TokenType.DEFAULT):
                self.consume(TokenType.DEFAULT)
                self.skip_newlines()
                self.consume(TokenType.LBRACE)
                self.skip_newlines()
                
                self.push_context("Branch.Default")
                default = []
                while not self.match(TokenType.RBRACE):
                    stmt = self.parse_statement()
                    if stmt:
                        default.append(stmt)
                    self.skip_newlines()
                self.consume(TokenType.RBRACE)
                self.pop_context()
            
            else:
                self.skip_newlines()
                if not self.match(TokenType.RBRACE):
                    self.error(f"Expected Case or Default in Branch, got {self.current_token.type.name if self.current_token else 'EOF'}")
        
        self.consume(TokenType.RBRACE)
        self.pop_context()
        
        return Branch(expression=expression, cases=cases, default=default,
                    line=start_token.line, column=start_token.column)    

    def parse_try(self) -> Try:
        start_token = self.consume(TokenType.TRYBLOCK)
        self.consume(TokenType.COLON)
        self.skip_newlines()
        self.consume(TokenType.LBRACE)
        self.skip_newlines()
        self.push_context("TryBlock")
        body = []
        while not self.match(TokenType.RBRACE):
            stmt = self.parse_statement()
            if stmt:
                body.append(stmt)
            self.skip_newlines()
        self.consume(TokenType.RBRACE)
        self.pop_context()
        self.skip_newlines()
        catch_clauses = []
        while self.match(TokenType.CATCHERROR):
            self.consume(TokenType.CATCHERROR)
            self.consume(TokenType.DOT)
            error_type = self.consume(TokenType.IDENTIFIER).value
            self.skip_newlines()
            self.consume(TokenType.LBRACE)
            self.skip_newlines()
            self.push_context(f"CatchError.{error_type}")
            catch_body = []
            while not self.match(TokenType.RBRACE):
                stmt = self.parse_statement()
                if stmt:
                    catch_body.append(stmt)
                self.skip_newlines()
            self.consume(TokenType.RBRACE)
            self.pop_context()
            self.skip_newlines()
            catch_clauses.append((error_type, catch_body))
        finally_body = None
        if self.match(TokenType.FINALLYBLOCK):
            self.consume(TokenType.FINALLYBLOCK)
            self.consume(TokenType.COLON)
            self.skip_newlines()
            self.consume(TokenType.LBRACE)
            self.skip_newlines()
            self.push_context("FinallyBlock")
            finally_body = []
            while not self.match(TokenType.RBRACE):
                stmt = self.parse_statement()
                if stmt:
                    finally_body.append(stmt)
                self.skip_newlines()
            self.consume(TokenType.RBRACE)
            self.pop_context()
        return Try(body=body, catch_clauses=catch_clauses, finally_body=finally_body,
                  line=start_token.line, column=start_token.column)

    def parse_sendmessage(self) -> SendMessage:
        start_token = self.consume(TokenType.SENDMESSAGE)
        self.consume(TokenType.DOT)
        target = self.consume(TokenType.IDENTIFIER).value
        parameters = {}
        if self.match(TokenType.LPAREN):
            self.consume(TokenType.LPAREN)
            self.skip_newlines()
            while not self.match(TokenType.RPAREN):
                self.skip_newlines()
                if self.match(TokenType.RPAREN):
                    break
                param_name = self.consume(TokenType.IDENTIFIER).value
                self.consume(TokenType.DASH)
                param_value = self.parse_expression()
                parameters[param_name] = param_value
                if self.match(TokenType.COMMA):
                    self.consume(TokenType.COMMA)
                self.skip_newlines()
            self.consume(TokenType.RPAREN)
        return SendMessage(target=target, parameters=parameters,
                         line=start_token.line, column=start_token.column)

    def parse_receivemessage(self) -> ReceiveMessage:
        start_token = self.consume(TokenType.RECEIVEMESSAGE)
        self.consume(TokenType.DOT)
        message_type = self.consume(TokenType.IDENTIFIER).value
        self.skip_newlines()
        self.consume(TokenType.LBRACE)
        self.skip_newlines()
        self.push_context(f"ReceiveMessage.{message_type}")
        body = []
        while not self.match(TokenType.RBRACE):
            stmt = self.parse_statement()
            if stmt:
                body.append(stmt)
            self.skip_newlines()
        self.consume(TokenType.RBRACE)
        self.pop_context()
        return ReceiveMessage(message_type=message_type, body=body,
                            line=start_token.line, column=start_token.column)

    def parse_everyinterval(self) -> EveryInterval:
        start_token = self.consume(TokenType.EVERYINTERVAL)
        interval_type = self.consume(TokenType.IDENTIFIER).value
        self.consume(TokenType.DASH)
        interval_value = self.consume(TokenType.NUMBER).value
        self.skip_newlines()
        self.consume(TokenType.LBRACE)
        self.skip_newlines()
        self.push_context(f"EveryInterval({interval_type}-{interval_value})")
        body = []
        while not self.match(TokenType.RBRACE):
            stmt = self.parse_statement()
            if stmt:
                body.append(stmt)
            self.skip_newlines()
        self.consume(TokenType.RBRACE)
        self.pop_context()
        return EveryInterval(interval_type=interval_type, interval_value=interval_value,
                           body=body, line=start_token.line, column=start_token.column)

    def parse_withsecurity(self) -> WithSecurity:
        start_token = self.consume(TokenType.WITHSECURITY)
        self.consume(TokenType.LPAREN)
        self.consume(TokenType.IDENTIFIER)
        self.consume(TokenType.DASH)
        context = self.consume(TokenType.STRING).value
        self.consume(TokenType.RPAREN)
        self.skip_newlines()
        self.consume(TokenType.LBRACE)
        self.skip_newlines()
        self.push_context(f"WithSecurity({context})")
        body = []
        while not self.match(TokenType.RBRACE):
            stmt = self.parse_statement()
            if stmt:
                body.append(stmt)
            self.skip_newlines()
        self.consume(TokenType.RBRACE)
        self.pop_context()
        return WithSecurity(context=context, body=body,
                          line=start_token.line, column=start_token.column)

    def parse_haltprogram(self) -> HaltProgram:
        start_token = self.consume(TokenType.HALTPROGRAM)
        message = None
        if self.match(TokenType.LPAREN):
            self.consume(TokenType.LPAREN)
            if self.match(TokenType.STRING):
                message = self.consume(TokenType.STRING).value
            self.consume(TokenType.RPAREN)
        return HaltProgram(message=message, line=start_token.line, column=start_token.column)

    def parse_assignment(self) -> Assignment:
        target = self.consume(TokenType.IDENTIFIER).value
        self.consume(TokenType.EQUALS)
        value = self.parse_expression()
        return Assignment(target=target, value=value,
                          line=self.current_token.line, column=self.current_token.column)

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

    # === NEW: Low-Level Function Parsing ===

    def parse_lowlevel_function(self) -> ASTNode:
        """Parse low-level system functions"""
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
        
        # Create specialized AST nodes for certain operations
        if op_name == "Dereference":
            return Dereference(
                pointer=args[0] if args else None,
                size_hint=args[1].value if len(args) > 1 and hasattr(args[1], 'value') else None,
                line=op_token.line,
                column=op_token.column
            )
        elif op_name == "AddressOf":
            return AddressOf(
                variable=args[0] if args else None,
                line=op_token.line,
                column=op_token.column
            )
        elif op_name == "SizeOf":
            return SizeOf(
                target=args[0] if args else None,
                line=op_token.line,
                column=op_token.column
            )
        elif op_name == "StoreValue":
            # StoreValue doesn't need a special AST node, but we keep it as FunctionCall
            # This ensures all arguments are passed through correctly
            return FunctionCall(
                function="StoreValue",
                arguments=args,
                line=op_token.line,
                column=op_token.column
            )                      
        elif op_name in ["PortRead", "PortWrite"]:
            return PortOperation(
                operation="read" if op_name == "PortRead" else "write",
                port=args[0] if args else None,
                size=args[1].value if len(args) > 1 and hasattr(args[1], 'value') else "byte",
                value=args[2] if len(args) > 2 else None,
                line=op_token.line,
                column=op_token.column
            )
        else:
            # Generic function call for other low-level operations
            return FunctionCall(function=op_name, arguments=args,
                               line=op_token.line, column=op_token.column)

    def parse_pool_operation(self) -> FunctionCall:
        """Parse pool memory operations like PoolResize, PoolMove, etc."""
        op_token = self.current_token
        operation = op_token.value  # Will be "PoolResize", "PoolMove", etc.
        self.advance()
        
        self.consume(TokenType.LPAREN, f"Expected '(' after {operation}")
        
        arguments = []
        self.skip_newlines()
        
        # Parse arguments
        while not self.match(TokenType.RPAREN):
            arguments.append(self.parse_expression())
            self.skip_newlines()
            
            if not self.match(TokenType.RPAREN):
                self.consume(TokenType.COMMA, f"Expected ',' or ')' in {operation} arguments")
                self.skip_newlines()
        
        self.consume(TokenType.RPAREN, f"Expected ')' after {operation} arguments")
        
        return FunctionCall(
            function=operation,
            arguments=arguments,
            line=op_token.line,
            column=op_token.column
        )


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


    def parse_type(self):
        """Parse type annotations"""
        # Simple implementation - just consume the type name
        if self.current_token.type == TokenType.IDENTIFIER:
            type_name = self.current_token.value
            self.advance()
            return type_name
        return "Any"  # Default type


    def parse_lowlevel_type(self) -> LowLevelType:
        """Parse low-level type literals"""
        token = self.current_token
        type_name = token.value
        self.advance()
        
        # Map type names to sizes and signedness
        type_info = {
            'Byte': (1, False), 'Word': (2, False), 'DWord': (4, False), 'QWord': (8, False),
            'UInt8': (1, False), 'UInt16': (2, False), 'UInt32': (4, False), 'UInt64': (8, False),
            'Int8': (1, True), 'Int16': (2, True), 'Int32': (4, True), 'Int64': (8, True)
        }
        
        size, signed = type_info.get(type_name, (1, False))
        
        return LowLevelType(
            type_name=type_name,
            size=size,
            signed=signed,
            line=token.line,
            column=token.column
        )

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

    def parse_debug_block(self):
        """Parse Debug("label", level=N) { ... }"""
        self.advance()  # consume 'Debug'
        
        # Default values
        label = "debug"
        level = 1
        
        if self.match(TokenType.LPAREN):
            self.consume(TokenType.LPAREN)
            
            # Get label if it's a string
            if self.match(TokenType.STRING):
                label = self.current_token.value
                self.advance()
                
                # Check for level parameter
                if self.match(TokenType.COMMA):
                    self.advance()
                    # Skip 'level' identifier if present
                    if self.current_token and self.current_token.value == 'level':
                        self.advance()
                        if self.match(TokenType.EQUALS):
                            self.advance()
                    if self.match(TokenType.NUMBER):
                        level = int(self.current_token.value)
                        self.advance()
            
            self.consume(TokenType.RPAREN)
        
        # Parse the block
        body = []
        if self.match(TokenType.LBRACE):
            self.consume(TokenType.LBRACE)
            self.skip_newlines()
            
            while not self.match(TokenType.RBRACE) and self.current_token:
                stmt = self.parse_statement()
                if stmt:
                    body.append(stmt)
                self.skip_newlines()
            
            self.consume(TokenType.RBRACE)
        
        return DebugBlock(label=label, level=level, body=body,
                         line=self.current_token.line if self.current_token else 0,
                         column=self.current_token.column if self.current_token else 0)
    
    def parse_debug_assert(self):
        """Parse DebugAssert(condition, "message")"""
        self.advance()  # consume 'DebugAssert'
        
        self.consume(TokenType.LPAREN)
        
        # Parse condition
        condition = self.parse_expression()
        
        # Get message
        message = "Assertion failed"
        if self.match(TokenType.COMMA):
            self.advance()
            if self.match(TokenType.STRING):
                message = self.current_token.value
                self.advance()
        
        self.consume(TokenType.RPAREN)
        
        return DebugAssert(condition=condition, message=message,
                          line=self.current_token.line if self.current_token else 0,
                          column=self.current_token.column if self.current_token else 0)
        
    def parse_debug_perf(self):
        """Parse DebugPerf.Start/End/Mark operations"""
        self.consume(TokenType.DEBUGPERF)  # Consume 'DebugPerf' token (not IDENTIFIER)
        self.consume(TokenType.DOT)
        
        operation = self.consume(TokenType.IDENTIFIER).value  # Start, End, Mark, etc.
        
        # Parse the label in parentheses
        label = None
        if self.match(TokenType.LPAREN):
            self.consume(TokenType.LPAREN)
            if self.match(TokenType.STRING):
                label = self.consume(TokenType.STRING).value
            self.consume(TokenType.RPAREN)
        
        # Handle block syntax for Start (optional)
        body = []
        if operation == "Start" and self.match(TokenType.LBRACE):
            self.consume(TokenType.LBRACE)
            self.skip_newlines()
            while not self.match(TokenType.RBRACE):
                stmt = self.parse_statement()
                if stmt:
                    body.append(stmt)
                self.skip_newlines()
            self.consume(TokenType.RBRACE)
        
        # Create a FunctionCall node that debug_ops can handle
        from ailang_parser.ailang_ast import FunctionCall
        return FunctionCall(
            function=f"DebugPerf_{operation}",
            arguments=[label] if label else [],
            line=self.current_token.line if self.current_token else 0,
            column=self.current_token.column if self.current_token else 0
        )
        