# Copyright (c) 2025 Sean Collins, 2 Paws Machine and Engineering. All rights reserved.
#
# Licensed under the Sean Collins Software License (SCSL). See the LICENSE file in the root directory of this project
# for the full terms and conditions, including restrictions on forking, corporate use, and permissions for private/teaching purposes.

import unittest
import os
import sys
import subprocess

# Add the project root to the path to allow absolute imports
project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
sys.path.insert(0, project_root)

class TestIntegration(unittest.TestCase):
    """
    End-to-end integration tests for the AIMacro compiler.
    Compiles an AIMacro file and runs the resulting binary to check its output.
    """

    def setUp(self):
        """Set up paths for the test."""
        self.frontend_dir = os.path.dirname(os.path.abspath(__file__))
        self.tests_dir = os.path.join(self.frontend_dir, 'tests')
        self.input_file = os.path.join(self.tests_dir, 'hello.aim')
        self.output_file = os.path.join(self.tests_dir, 'hello_exec')

        os.makedirs(self.tests_dir, exist_ok=True)

        # Create the test source file
        hello_aim_content = """
# A simple "Hello, World!" program in AIMacro
def main():
    message = "Hello from AIMacro!"
    x = 10 + 32
    print(message)
    print("The answer is:")
    print(x)
end;
"""
        with open(self.input_file, 'w', encoding='utf-8') as f:
            f.write(hello_aim_content)

    def tearDown(self):
        """Clean up generated files after the test."""
        if os.path.exists(self.input_file):
            os.remove(self.input_file)
        if os.path.exists(self.output_file):
            os.remove(self.output_file)

    def test_compile_and_run_hello(self):
        """
        Tests the full compilation and execution of a simple 'hello.aim' program.
        """
        # 1. Run the AIMacro compiler as a module to ensure correct package context.
        #    This allows relative imports within the frontend to work correctly.
        compile_command = [sys.executable, '-m', 'aimacro_frontend.integration', self.input_file, '-o', self.output_file]
        
        # We need to run this from the project root for imports to work
        compile_result = subprocess.run(compile_command, cwd=project_root, capture_output=True, text=True)
        
        self.assertEqual(compile_result.returncode, 0, f"Compiler failed with error:\n{compile_result.stderr}")
        self.assertTrue(os.path.exists(self.output_file), "Compiler did not produce an output file.")

        # 2. Make the output file executable (for Linux/WSL)
        if sys.platform != "win32":
            os.chmod(self.output_file, 0o755)

        # 3. Run the compiled executable and capture its output
        # On Windows, we might need to specify the full path.
        # On WSL/Linux, we run it from the tests directory.
        exec_path = self.output_file
        exec_cwd = self.tests_dir if sys.platform != "win32" else None
        if sys.platform != "win32":
            exec_path = f"./{os.path.basename(self.output_file)}"

        run_result = subprocess.run(exec_path, capture_output=True, text=True, shell=True, cwd=exec_cwd)
        
        self.assertEqual(run_result.returncode, 0, f"Compiled executable failed with error:\n{run_result.stderr}")

        # 4. Verify the output
        expected_output = "Hello from AIMacro!\nThe answer is:\n42\n"
        self.assertEqual(run_result.stdout.replace('\r\n', '\n'), expected_output)

if __name__ == '__main__':
    unittest.main()