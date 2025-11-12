#!/bin/bash

echo "Searching for unpacking operations in statement_converter.py..."

# Find all lines with tuple unpacking (a, b = ...)
grep -n "^[[:space:]]*[a-zA-Z_][a-zA-Z0-9_]*,[[:space:]]*[a-zA-Z_][a-zA-Z0-9_]*[[:space:]]*=" cobol_frontend/converter/statement_converter.py

echo ""
echo "Searching for unpacking in convert_perform methods..."
grep -B 5 -A 5 "convert_perform" cobol_frontend/converter/statement_converter.py | grep -E "^\s*[a-zA-Z_]+,\s*[a-zA-Z_]+\s*="

echo ""
echo "Looking for paragraph_name, thru usage..."
grep -n "paragraph_name.*thru" cobol_frontend/converter/statement_converter.py