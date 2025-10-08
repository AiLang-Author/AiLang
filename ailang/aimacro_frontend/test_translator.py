# Copyright (c) 2025 Sean Collins, 2 Paws Machine and Engineering. All rights reserved.
#
# Licensed under the Sean Collins Software License (SCSL). See the LICENSE file in the root directory of this project
# for the full terms and conditions, including restrictions on forking, corporate use, and permissions for private/teaching purposes.

import unittest
import os
import sys

# Add the parent directory to the path to allow sibling imports
project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
sys.path.insert(0, project_root)

from aimacro_frontend.aimacro_translator import translate_to_ailang

class TestAilangTranslator(unittest.TestCase):
    """
    Unit tests for the AIMacro to AILang translator.
    """

    def assertTranslation(self, python_code, expected_ailang_code):
        """Helper to compare translation results, ignoring leading/trailing whitespace."""
        actual = translate_to_ailang(python_code.strip())
        expected = expected_ailang_code.strip()
        self.assertEqual(actual, expected, f"\n--- Python Input ---\n{python_code.strip()}\n--- Expected AILang ---\n{expected}\n--- Got AILang ---\n{actual}")

    def test_variable_assignment(self):
        """Tests translation of number and string assignments."""
        py_code = """
x = 123
s = "hello world"
"""
        ai_code = """
x = 123
s = "hello world"
"""
        self.assertTranslation(py_code, ai_code)

    def test_simple_binary_operation(self):
        """Tests translation of a simple arithmetic operation."""
        py_code = "result = 10 + 5"
        ai_code = "result = Add(10, 5)"
        self.assertTranslation(py_code, ai_code)

    def test_multiple_binary_operations(self):
        """Tests translation of multiple arithmetic operations."""
        py_code = """
a = 100
b = 50
c = a - b
d = c * 2
"""
        ai_code = """
a = 100
b = 50
c = Subtract(a, b)
d = Multiply(c, 2)
"""
        self.assertTranslation(py_code, ai_code)

    def test_builtin_function_call(self):
        """Tests translation of a mapped built-in function like print()."""
        py_code = 'print("Hello, AILang!")'
        ai_code = 'AIMacro.Print("Hello, AILang!")'
        self.assertTranslation(py_code, ai_code)

    def test_function_call_with_variable(self):
        """Tests function calls with variables as arguments."""
        py_code = """
message = "test"
print(message)
"""
        ai_code = """
message = "test"
AIMacro.Print(message)
"""
        self.assertTranslation(py_code, ai_code)

    def test_complex_expression(self):
        """Tests a more complex expression combining operations and functions."""
        py_code = "result = max(10, 20 + 5)"
        ai_code = "result = AIMacro.Max(10, Add(20, 5))"
        self.assertTranslation(py_code, ai_code)

    def test_full_program_translation(self):
        """Tests the example program from the translator script."""
        py_code = """
x = 10
y = 20
z = x + y
print("The result is:")
print(z)
max_val = max(z, 150)
print("The maximum value is:")
print(max_val)
"""
        ai_code = """
x = 10
y = 20
z = Add(x, y)
AIMacro.Print("The result is:")
AIMacro.Print(z)
max_val = AIMacro.Max(z, 150)
AIMacro.Print("The maximum value is:")
AIMacro.Print(max_val)
"""
        self.assertTranslation(py_code, ai_code)

    def test_unsupported_operator(self):
        """Tests that unsupported operators are handled gracefully."""
        py_code = "x = 10 % 2"
        # The translator is designed to return an error comment
        actual = translate_to_ailang(py_code)
        self.assertTrue(actual.startswith("// Translation Error:"), "Should return a translation error comment")


if __name__ == '__main__':
    unittest.main()