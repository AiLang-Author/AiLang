# Copyright (c) 2025 Sean Collins, 2 Paws Machine and Engineering. All rights reserved.
#
# Licensed under the Sean Collins Software License (SCSL). See the LICENSE file in the root directory of this project
# for the full terms and conditions, including restrictions on forking, corporate use, and permissions for private/teaching purposes.

import unittest
import sys
import os

# Add the project root to the path to allow absolute imports
project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
sys.path.insert(0, project_root)

from aimacro_frontend.lexer import AIMacroLexer
from aimacro_frontend.parser import AIMacroParser
from aimacro_frontend.ast_converter import AIMacroToAILangConverter

# Import the target AILang AST nodes for verification.
# This will now import the REAL AILang AST nodes.
from ailang_parser.ailang_ast import (
    Program, Assignment, Number, String, Identifier, FunctionCall, If, ReturnValue, ForEvery,
    While, Function
)

class TestASTConverter(unittest.TestCase):
    """
    Unit tests for the AIMacroToAILangConverter.
    Ensures that the AIMacro AST is correctly converted into the final AILang AST.
    """

    def _convert_helper(self, code: str):
        """Helper function to run the full lexer -> parser -> converter pipeline."""
        lexer = AIMacroLexer(code)
        tokens = lexer.tokenize()
        parser = AIMacroParser(tokens)
        aimacro_ast = parser.parse()
        
        converter = AIMacroToAILangConverter()
        ailang_ast = converter.convert(aimacro_ast)
        return ailang_ast

    def test_convert_assignment(self):
        """Tests conversion of a simple assignment."""
        code = "x = 100;"
        ast = self._convert_helper(code)
        
        self.assertIsInstance(ast, Program)
        self.assertEqual(len(ast.declarations), 1)

        stmt = ast.declarations[0]
        self.assertIsInstance(stmt, Assignment)
        self.assertEqual(stmt.target, 'x')
        self.assertIsInstance(stmt.value, Number)
        self.assertEqual(stmt.value.value, 100)

    def test_convert_binary_op_infix(self):
        """Tests conversion of a simple infix binary operation to a function call."""
        code = "y = 5 + 10;"
        ast = self._convert_helper(code)
        stmt = ast.declarations[0]
        self.assertIsInstance(stmt, Assignment)

        func_call = stmt.value
        self.assertIsInstance(func_call, FunctionCall)
        self.assertEqual(func_call.function, 'Add')
        self.assertEqual(len(func_call.arguments), 2)
        self.assertIsInstance(func_call.arguments[0], Number)
        self.assertEqual(func_call.arguments[0].value, 5)
        self.assertIsInstance(func_call.arguments[1], Number)
        self.assertEqual(func_call.arguments[1].value, 10)

    def test_convert_binary_op_function(self):
        """Tests conversion of a logical operator to a function call."""
        code = "z = a and b;"
        ast = self._convert_helper(code)
        stmt = ast.declarations[0]
        self.assertIsInstance(stmt, Assignment)

        func_call = stmt.value
        self.assertIsInstance(func_call, FunctionCall)
        self.assertEqual(func_call.function, 'And')
        self.assertEqual(len(func_call.arguments), 2)
        self.assertIsInstance(func_call.arguments[0], Identifier)
        self.assertEqual(func_call.arguments[0].name, 'a')

    def test_convert_builtin_function_call(self):
        """Tests conversion of a Python built-in to an AIMacro library call."""
        code = 'print("hello");'
        ast = self._convert_helper(code)
        stmt = ast.declarations[0]
        self.assertIsInstance(stmt, FunctionCall)
        self.assertEqual(stmt.function, 'AIMacro.SmartPrint')
        self.assertIsInstance(stmt.arguments[0], String)
        self.assertEqual(stmt.arguments[0].value, "hello")

    def test_convert_if_statement(self):
        """Tests conversion of an if statement."""
        code = """
if x > 0:
    return 1
end;
"""
        ast = self._convert_helper(code)
        stmt = ast.declarations[0]
        self.assertIsInstance(stmt, If)
        self.assertIsInstance(stmt.condition, FunctionCall)
        self.assertEqual(stmt.condition.function, 'GreaterThan')
        self.assertEqual(len(stmt.then_body), 1)
        self.assertIsInstance(stmt.then_body[0], ReturnValue)
        self.assertEqual(stmt.else_body, [])

    def test_convert_while_loop(self):
        """Tests conversion of a while loop."""
        code = """
while i < 10:
    i = i + 1
end;
"""
        ast = self._convert_helper(code)
        stmt = ast.declarations[0]
        self.assertIsInstance(stmt, While)
        self.assertIsInstance(stmt.condition, FunctionCall)
        self.assertEqual(stmt.condition.function, 'LessThan')
        self.assertEqual(len(stmt.body), 1)
        self.assertIsInstance(stmt.body[0], Assignment)

    def test_convert_function_definition(self):
        """Tests conversion of a function definition with type hints."""
        code = """
def my_func(a: int) -> int:
    return a
end;
"""
        ast = self._convert_helper(code)
        stmt = ast.declarations[0]
        self.assertIsInstance(stmt, Function)
        self.assertEqual(stmt.name, 'my_func')
        self.assertEqual(len(stmt.input_params), 1)
        self.assertEqual(stmt.input_params[0], ('a', 'Integer'))
        self.assertEqual(stmt.output_type, 'Integer')
        self.assertEqual(len(stmt.body), 1)
        self.assertIsInstance(stmt.body[0], ReturnValue)

    def test_convert_method_call(self):
        """Tests conversion of a method call to an AIMacro library call."""
        code = """
my_list = list()
my_list.append(42)
"""
        ast = self._convert_helper(code)
        # The program will have two statements: the assignment and the method call.
        # Note: The converter also adds a library import, so we check the last statement.
        self.assertGreaterEqual(len(ast.declarations), 2)

        # Check the second statement, which should be the converted method call.
        stmt = ast.declarations[-1] # The method call is the last statement
        self.assertIsInstance(stmt, FunctionCall)
        self.assertEqual(stmt.function, 'AIMacro.ListAppend')
        
        # The first argument should be the object itself
        self.assertEqual(len(stmt.arguments), 2)
        self.assertIsInstance(stmt.arguments[0], Identifier)
        self.assertEqual(stmt.arguments[0].name, 'my_list')

        # The second argument should be the method's argument
        self.assertIsInstance(stmt.arguments[1], Number)
        self.assertEqual(stmt.arguments[1].value, 42)

    def test_convert_if_elif_else(self):
        """Tests conversion of a full if/elif/else chain."""
        code = """
if x == 1:
    y = 1
elif x == 2:
    y = 2
else:
    y = 3
end;
"""
        ast = self._convert_helper(code)
        if_stmt = ast.declarations[0]
        self.assertIsInstance(if_stmt, If)
        # The 'else' block should contain the nested 'elif' as another 'If' statement.
        self.assertIsInstance(if_stmt.else_body[0], If)
        # The 'else' block of the nested 'elif' should contain the final 'else' body.
        self.assertIsNotNone(if_stmt.else_body[0].else_body)
        self.assertEqual(len(if_stmt.else_body[0].else_body), 1)

    def test_convert_list_literal(self):
        """Tests conversion of a list literal e.g., [1, 2, 3]."""
        code = "my_list = [1, 2];"
        ast = self._convert_helper(code)
        assign_stmt = ast.declarations[0]
        self.assertIsInstance(assign_stmt, Assignment)

        func_call = assign_stmt.value
        self.assertIsInstance(func_call, FunctionCall)
        self.assertEqual(func_call.function, 'AIMacro.CreateListWithElements')
        self.assertEqual(len(func_call.arguments), 2)
        self.assertIsInstance(func_call.arguments[0], Number)
        self.assertEqual(func_call.arguments[0].value, 1)

    def test_convert_for_loop(self):
        """Tests conversion of a for loop."""
        code = """
for i in my_list:
    print(i)
end;
"""
        ast = self._convert_helper(code)
        stmt = ast.declarations[0]
        self.assertIsInstance(stmt, ForEvery)
        self.assertEqual(stmt.variable, 'i') # The loop variable
        self.assertIsInstance(stmt.collection, Identifier) # The collection being iterated
        self.assertEqual(stmt.collection.name, 'my_list')
        self.assertEqual(len(stmt.body), 1)
        self.assertIsInstance(stmt.body[0], FunctionCall)

    def test_convert_dict_literal(self):
        """Tests conversion of a dictionary literal e.g., {'a': 1}."""
        code = "my_dict = {'a': 1};"
        ast = self._convert_helper(code)
        assign_stmt = ast.declarations[0]
        self.assertIsInstance(assign_stmt, Assignment)

        func_call = assign_stmt.value
        self.assertIsInstance(func_call, FunctionCall)
        self.assertEqual(func_call.function, 'AIMacro.CreateDictWithPairs')
        # It should have two arguments: the key and the value
        self.assertEqual(len(func_call.arguments), 2)
        self.assertIsInstance(func_call.arguments[0], String)
        self.assertEqual(func_call.arguments[0].value, 'a')
        self.assertIsInstance(func_call.arguments[1], Number)
        self.assertEqual(func_call.arguments[1].value, 1)

    def test_convert_index_access(self):
        """Tests conversion of an index access e.g., my_list[0]."""
        code = "x = my_list[0];"
        ast = self._convert_helper(code)
        assign_stmt = ast.declarations[0]
        self.assertIsInstance(assign_stmt, Assignment)

        func_call = assign_stmt.value
        self.assertIsInstance(func_call, FunctionCall)
        self.assertEqual(func_call.function, 'AIMacro.Get')
        self.assertEqual(len(func_call.arguments), 2)

        # Check the object being indexed
        self.assertIsInstance(func_call.arguments[0], Identifier)
        self.assertEqual(func_call.arguments[0].name, 'my_list')

        # Check the index
        self.assertIsInstance(func_call.arguments[1], Number)
        self.assertEqual(func_call.arguments[1].value, 0)

    def test_convert_multiline_string(self):
        """Tests conversion of a multi-line string literal."""
        code = '''my_string = """hello
world"""'''
        ast = self._convert_helper(code)
        assign_stmt = ast.declarations[0]
        self.assertIsInstance(assign_stmt, Assignment)
        self.assertIsInstance(assign_stmt.value, String)
        self.assertEqual(assign_stmt.value.value, "hello\nworld")

    def test_convert_index_assignment(self):
        """Tests conversion of an assignment to an index e.g., my_list[0] = 5."""
        code = "my_list[0] = 5;"
        ast = self._convert_helper(code)
        # The result should be a FunctionCall to AIMacro.Set
        stmt = ast.declarations[0]
        self.assertIsInstance(stmt, FunctionCall)
        self.assertEqual(stmt.function, 'AIMacro.Set')
        self.assertEqual(len(stmt.arguments), 3)

        # Check the arguments: object, index, value
        self.assertIsInstance(stmt.arguments[0], Identifier)
        self.assertEqual(stmt.arguments[0].name, 'my_list')
        self.assertIsInstance(stmt.arguments[1], Number)
        self.assertEqual(stmt.arguments[1].value, 0)
        self.assertIsInstance(stmt.arguments[2], Number)
        self.assertEqual(stmt.arguments[2].value, 5)


if __name__ == '__main__':
    unittest.main()