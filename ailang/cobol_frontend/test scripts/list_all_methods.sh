#!/bin/bash
# List all methods in cobol_parser.py and cobol_ast_converter.py

echo "================================================================================"
echo "PARSER METHODS (cobol_parser.py)"
echo "================================================================================"
grep -n "^    def " cobol_parser.py | head -100

echo ""
echo "================================================================================"
echo "CONVERTER METHODS (cobol_ast_converter.py)"
echo "================================================================================"
grep -n "^    def " cobol_ast_converter.py | head -100

echo ""
echo "================================================================================"
echo "METHOD COUNTS"
echo "================================================================================"
echo "Parser methods:    $(grep -c '^    def ' cobol_parser.py)"
echo "Converter methods: $(grep -c '^    def ' cobol_ast_converter.py)"