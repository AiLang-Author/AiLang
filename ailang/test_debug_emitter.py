#!/usr/bin/env python3
"""
Test script to see what debug_emitter is actually returning
"""

import sys
import os

# Add project root to path
sys.path.insert(0, os.path.abspath('.'))

from cobol_frontend.converter.debug_emitter import DebugEmitter
from ailang_parser.ailang_ast import Assignment, Identifier, Number

# Create debug emitter
debug = DebugEmitter(enabled=True, default_level=2)

# Create a simple assignment
assignment = Assignment(1, 1, "TEST_VAR", Number(1, 1, 42))

print("=" * 70)
print("Testing debug.wrap_move()")
print("=" * 70)

# Call wrap_move like the converter does
result = debug.wrap_move(assignment, source="42", target="TEST_VAR")

print(f"\nInput assignment type: {type(assignment).__name__}")
print(f"Result type: {type(result)}")
print(f"Result value: {result}")

if isinstance(result, list):
    print(f"\nResult is a list with {len(result)} items:")
    for i, item in enumerate(result):
        print(f"  [{i}] type={type(item).__name__}, value={item}")
else:
    print(f"\nResult is a single item: {result}")

print("\n" + "=" * 70)
print("Testing debug.wrap_display()")
print("=" * 70)

from ailang_parser.ailang_ast import FunctionCall, String

# Create a simple print statement
print_stmt = FunctionCall(1, 1, 'PrintMessage', [String(1, 1, "Hello")])

result2 = debug.wrap_display(print_stmt, items="Hello")

print(f"\nInput print_stmt type: {type(print_stmt).__name__}")
print(f"Result type: {type(result2)}")
print(f"Result value: {result2}")

if isinstance(result2, list):
    print(f"\nResult is a list with {len(result2)} items:")
    for i, item in enumerate(result2):
        print(f"  [{i}] type={type(item).__name__}, value={item}")
else:
    print(f"\nResult is a single item: {result2}")

print("\n" + "=" * 70)
print("Testing debug.flatten()")
print("=" * 70)

# Test flattening
test_list = [result, assignment]
flattened = debug.flatten(test_list)

print(f"\nInput list: {test_list}")
print(f"Flattened result type: {type(flattened)}")
print(f"Flattened length: {len(flattened)}")
print(f"\nFlattened items:")
for i, item in enumerate(flattened):
    print(f"  [{i}] type={type(item).__name__}")

print("\n" + "=" * 70)
print("SUMMARY")
print("=" * 70)
print("\nWhat the converter expects:")
print("  - convert_move() returns: Assignment OR [Assignment, Assignment, ...]")
print("  - convert_display() returns: [FunctionCall]")
print("\nWhat we're actually getting:")
print(f"  - wrap_move() returns: {type(result)}")
print(f"  - wrap_display() returns: {type(result2)}")
print(f"  - After flatten: list with {len(flattened)} items of types:", 
      [type(x).__name__ for x in flattened])