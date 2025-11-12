#!/bin/bash

# Quick one-liners to find the issue:

# 1. Test if compiler runs at all
echo "=== Test 1: Basic import ==="
python3 -c "import ailang_parser.compiler; print('OK')" 2>&1

# 2. Test AST node structure
echo -e "\n=== Test 2: Check FunctionCall signature ==="
python3 -c "
from ailang_parser.ailang_ast import FunctionCall, Identifier
import inspect
sig = inspect.signature(FunctionCall)
print('FunctionCall expects:', sig)
# Try creating with string
try:
    fc = FunctionCall(function='test', arguments=[], line=1, column=1)
    print('✓ String function works')
except Exception as e:
    print('✗ String function failed:', e[:50])
# Try creating with AST node  
try:
    ident = Identifier(name='test', line=1, column=1)
    fc = FunctionCall(function=ident, arguments=[], line=1, column=1)
    print('✓ AST node function works')
except Exception as e:
    print('✗ AST node function failed:', str(e)[:50])
" 2>&1

# 3. Test Assignment signature
echo -e "\n=== Test 3: Check Assignment signature ==="
python3 -c "
from ailang_parser.ailang_ast import Assignment, Identifier
import inspect
sig = inspect.signature(Assignment)
print('Assignment expects:', sig)
# Try with string
try:
    a = Assignment(target='x', value=Identifier(name='y', line=1, column=1), line=1, column=1)
    print('✓ String target works')
except Exception as e:
    print('✗ String target failed:', str(e)[:50])
" 2>&1

# 4. Check if parse_strict_expression exists
echo -e "\n=== Test 4: Check parser methods ==="
python3 -c "
from ailang_parser.parser import Parser
from ailang_parser.lexer import Lexer
l = Lexer('x')
p = Parser(l)
print('Has parse_expression:', hasattr(p, 'parse_expression'))
print('Has parse_strict_expression:', hasattr(p, 'parse_strict_expression'))  
print('Has parse_primary:', hasattr(p, 'parse_primary'))
print('Has parse_postfix_expression:', hasattr(p, 'parse_postfix_expression'))
" 2>&1

# 5. Try simplest parse
echo -e "\n=== Test 5: Parse number ==="
python3 -c "
from ailang_parser.parser import Parser
from ailang_parser.lexer import Lexer
try:
    l = Lexer('42')
    p = Parser(l)
    ast = p.parse()
    print('✓ Parsed OK')
except Exception as e:
    print('✗ Failed:', e)
    import traceback
    traceback.print_tb(e.__traceback__, limit=3)
" 2>&1