import unittest
import sys
import os

# Add the parent directory to the path to allow sibling imports
project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
sys.path.insert(0, project_root)

from aimacro_frontend.lexer import AIMacroLexer
from aimacro_frontend.parser import AIMacroParser, ParserError
from aimacro_frontend.ast_converter import (
    AIMacroProgram, AIMacroAssignment, AIMacroNumber, AIMacroIdentifier,
    AIMacroBinaryOp, AIMacroFunctionCall, AIMacroMethodCall, AIMacroIf, AIMacroWhile, AIMacroFor, AIMacroIndexAccess,
    AIMacroFunction, AIMacroReturn, AIMacroString
)

class TestAIMacroParser(unittest.TestCase):
    """
    Unit tests for the AIMacroParser.
    Ensures that the token stream from the lexer is correctly converted
    into an AIMacro-specific AST.
    """

    def _parse_helper(self, code: str) -> AIMacroProgram:
        """Helper function to run the lexer and parser on a code snippet."""
        lexer = AIMacroLexer(code)
        tokens = lexer.tokenize()
        parser = AIMacroParser(tokens)
        return parser.parse()

    def test_simple_assignment(self):
        """Tests parsing of a simple variable assignment."""
        code = "x = 42;"
        ast = self._parse_helper(code)
        self.assertIsInstance(ast, AIMacroProgram)
        self.assertEqual(len(ast.statements), 1)

        stmt = ast.statements[0]
        self.assertIsInstance(stmt, AIMacroAssignment)
        self.assertEqual(stmt.target, 'x')
        self.assertIsInstance(stmt.value, AIMacroNumber)
        self.assertEqual(stmt.value.value, 42)

    def test_binary_operation(self):
        """Tests parsing of an expression with a binary operator."""
        code = "y = 10 + 5;"
        ast = self._parse_helper(code)
        stmt = ast.statements[0]
        self.assertIsInstance(stmt, AIMacroAssignment)

        bin_op = stmt.value
        self.assertIsInstance(bin_op, AIMacroBinaryOp)
        self.assertEqual(bin_op.operator, '+')
        self.assertIsInstance(bin_op.left, AIMacroNumber)
        self.assertEqual(bin_op.left.value, 10)
        self.assertIsInstance(bin_op.right, AIMacroNumber)
        self.assertEqual(bin_op.right.value, 5)

    def test_function_call(self):
        """Tests parsing of a function call."""
        code = 'print("hello");'
        ast = self._parse_helper(code)
        stmt = ast.statements[0]
        self.assertIsInstance(stmt, AIMacroFunctionCall)
        self.assertEqual(stmt.name, 'print')
        self.assertEqual(len(stmt.args), 1)
        self.assertIsInstance(stmt.args[0], AIMacroString)
        self.assertEqual(stmt.args[0].value, "hello")

    def test_if_statement(self):
        """Tests parsing of a simple if statement."""
        code = """
if x > 0:
    y = 1
end;
"""
        ast = self._parse_helper(code)
        stmt = ast.statements[0]
        self.assertIsInstance(stmt, AIMacroIf)
        self.assertIsInstance(stmt.condition, AIMacroBinaryOp)
        self.assertEqual(len(stmt.then_body), 1)
        self.assertIsInstance(stmt.then_body[0], AIMacroAssignment)
        self.assertIsNone(stmt.else_body)

    def test_if_else_statement(self):
        """Tests parsing of an if-else statement."""
        code = """
if x == 0:
    y = 1
else:
    y = 2
end;
"""
        ast = self._parse_helper(code)
        stmt = ast.statements[0]
        self.assertIsInstance(stmt, AIMacroIf)
        self.assertEqual(len(stmt.then_body), 1)
        self.assertIsNotNone(stmt.else_body)
        self.assertEqual(len(stmt.else_body), 1)
        self.assertIsInstance(stmt.else_body[0], AIMacroAssignment)

    def test_while_loop(self):
        """Tests parsing of a while loop."""
        code = """
while i < 10:
    i = i + 1
end;
"""
        ast = self._parse_helper(code)
        stmt = ast.statements[0]
        self.assertIsInstance(stmt, AIMacroWhile)
        self.assertIsInstance(stmt.condition, AIMacroBinaryOp)
        self.assertEqual(len(stmt.body), 1)
        self.assertIsInstance(stmt.body[0], AIMacroAssignment)

    def test_function_definition(self):
        """Tests parsing of a function definition."""
        code = """
def my_func(a, b):
    return a + b
end;
"""
        ast = self._parse_helper(code)
        stmt = ast.statements[0]
        self.assertIsInstance(stmt, AIMacroFunction)
        self.assertEqual(stmt.name, 'my_func')
        self.assertEqual(len(stmt.params), 2)
        self.assertEqual(stmt.params[0], ('a', None))
        self.assertEqual(stmt.params[1], ('b', None))
        self.assertEqual(len(stmt.body), 1)
        self.assertIsInstance(stmt.body[0], AIMacroReturn)

    def test_method_call(self):
        """Tests parsing of a method call like 'my_list.append(1)'."""
        code = "my_list.append(1);"
        ast = self._parse_helper(code)
        stmt = ast.statements[0]

        self.assertIsInstance(stmt, AIMacroMethodCall)
        self.assertEqual(stmt.method, 'append')

        # Check the object being called on
        self.assertIsInstance(stmt.object, AIMacroIdentifier)
        self.assertEqual(stmt.object.name, 'my_list')

        # Check the arguments
        self.assertEqual(len(stmt.args), 1)
        self.assertIsInstance(stmt.args[0], AIMacroNumber)
        self.assertEqual(stmt.args[0].value, 1)

    def test_for_loop(self):
        """Tests parsing of a for loop."""
        code = """
for item in my_collection:
    print(item)
end;
"""
        ast = self._parse_helper(code)
        stmt = ast.statements[0]

        self.assertIsInstance(stmt, AIMacroFor)
        self.assertEqual(stmt.var, 'item')
        self.assertIsInstance(stmt.iterable, AIMacroIdentifier)
        self.assertEqual(stmt.iterable.name, 'my_collection')
        self.assertEqual(len(stmt.body), 1)
        self.assertIsInstance(stmt.body[0], AIMacroFunctionCall)

    def test_multiline_string(self):
        """Tests parsing of a multi-line string literal."""
        code = '''
my_string = """hello
world"""
'''
        ast = self._parse_helper(code)
        stmt = ast.statements[0]
        self.assertIsInstance(stmt, AIMacroAssignment)
        self.assertIsInstance(stmt.value, AIMacroString)
        self.assertEqual(stmt.value.value, "hello\nworld")

    def test_index_access(self):
        """Tests parsing of an index access like 'my_list[0]'."""
        code = "x = my_list[0];"
        ast = self._parse_helper(code)
        stmt = ast.statements[0]
        self.assertIsInstance(stmt, AIMacroAssignment)
        
        index_node = stmt.value
        self.assertIsInstance(index_node, AIMacroIndexAccess)
        self.assertIsInstance(index_node.expression, AIMacroIdentifier)
        self.assertEqual(index_node.expression.name, 'my_list')
        self.assertIsInstance(index_node.index, AIMacroNumber)
        self.assertEqual(index_node.index.value, 0)


if __name__ == '__main__':
    unittest.main()