#!/usr/bin/env python3
"""
READ Statement Diagnostic Tool
Traces READ statement conversion to find why they produce no output
"""

import sys
import os

# Add parent directory to path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

from cobol_frontend.cobol_lexer import COBOLLexer
from cobol_frontend.parser import COBOLMultiProgramParser
from cobol_frontend.converter import COBOLToAilangMultiProgramConverter
from cobol_frontend.parser.ast_nodes import COBOLRead, COBOLASTNode

print("="*80)
print("READ STATEMENT DIAGNOSTIC TOOL")
print("="*80)

# Parse BANKING.cbl
filename = 'cobol_frontend/tests/BANKING.cbl'
print(f"\n[1] Reading: {filename}")

with open(filename, 'r') as f:
    source = f.read()

# Tokenize
print("[2] Tokenizing...")
lexer = COBOLLexer(source)
tokens = lexer.tokenize()
print(f"    ✓ {len(tokens)} tokens")

# Parse
print("[3] Parsing...")
parser = COBOLMultiProgramParser(tokens)
ast = parser.parse_all_programs()
print(f"    ✓ {len(ast.programs)} program(s)")

# Find READ statements
print("\n[4] Finding READ statements...")
read_stmts = []

def find_reads(node):
    if isinstance(node, COBOLRead):
        read_stmts.append(node)
    
    for attr_name in dir(node):
        if attr_name.startswith('_'):
            continue
        try:
            attr = getattr(node, attr_name)
            if isinstance(attr, list):
                for item in attr:
                    if isinstance(item, COBOLASTNode):
                        find_reads(item)
            elif isinstance(attr, COBOLASTNode):
                find_reads(attr)
        except:
            pass

find_reads(ast)
print(f"    ✓ Found {len(read_stmts)} READ statement(s)")

if len(read_stmts) == 0:
    print("\n❌ No READ statements found!")
    sys.exit(1)

for i, stmt in enumerate(read_stmts[:3], 1):  # Show first 3
    print(f"\n    READ #{i} (line {stmt.line}):")
    print(f"      File: {stmt.filename}")
    print(f"      Record: {getattr(stmt, 'record_name', None)}")
    print(f"      Into: {getattr(stmt, 'into_variable', None)}")
    at_end = getattr(stmt, 'at_end_statements', None) or []
    inv_key = getattr(stmt, 'invalid_key_statements', None) or []
    print(f"      AT END: {len(at_end)} stmts")
    print(f"      INVALID KEY: {len(inv_key)} stmts")

# Create converter
print("\n[5] Creating converter with JCL backend...")
converter = COBOLToAilangMultiProgramConverter(debug=False, io_backend_type='jcl')

# Instrument convert_read
print("[6] Instrumenting convert_read()...\n")

original_convert_read = converter.io_backend.convert_read
call_count = [0]  # Use list to capture in closure

def traced_convert_read(file_name, record_name, into_identifier,
                        at_end_stmts, not_at_end_stmts,
                        invalid_key_stmts, not_invalid_key_stmts, variables):
    call_count[0] += 1
    
    print(f"\n{'='*80}")
    print(f"convert_read() CALL #{call_count[0]}")
    print(f"{'='*80}")
    print(f"Parameters:")
    print(f"  file_name          : {file_name}")
    print(f"  record_name        : {record_name}")
    print(f"  into_identifier    : {into_identifier}")
    print(f"  at_end_stmts       : {len(at_end_stmts)}")
    print(f"  invalid_key_stmts  : {len(invalid_key_stmts)}")
    
    # File type detection
    normalized = converter.normalize_name(file_name)
    is_request = converter.io_backend._is_request_file(file_name)
    is_response = converter.io_backend._is_response_file(file_name)
    is_indexed = converter.io_backend._is_indexed_file(file_name)
    
    print(f"\nFile Type Detection:")
    print(f"  Normalized name    : '{normalized}'")
    print(f"  _is_request_file   : {is_request}")
    print(f"  _is_response_file  : {is_response}")
    print(f"  _is_indexed_file   : {is_indexed}")
    
    # Metadata lookup
    metadata = converter.io_backend.file_metadata.get(normalized, {})
    print(f"\nFile Metadata:")
    if metadata:
        print(f"  ✓ Found metadata:")
        print(f"    organization     : {metadata.get('organization')}")
        print(f"    access_mode      : {metadata.get('access_mode')}")
        print(f"    record_key       : {metadata.get('record_key')}")
        print(f"    table_name       : {metadata.get('table_name')}")
    else:
        print(f"  ✗ NO METADATA!")
        print(f"    Available files: {list(converter.io_backend.file_metadata.keys())}")
    
    # Record fields
    if record_name:
        rec_norm = converter.normalize_name(record_name)
        fields = converter.io_backend._get_record_fields(rec_norm)
        print(f"\nRecord Fields:")
        print(f"  Record (normalized): '{rec_norm}'")
        if fields:
            print(f"  ✓ {len(fields)} fields:")
            for fname, is_num in fields[:3]:
                print(f"    - {fname} (numeric={is_num})")
        else:
            print(f"  ✗ NO FIELDS FOUND")
    
    # Call original
    print(f"\n⚙️  Calling original convert_read()...")
    result = original_convert_read(file_name, record_name, into_identifier,
                                   at_end_stmts, not_at_end_stmts,
                                   invalid_key_stmts, not_invalid_key_stmts,
                                   variables)
    
    print(f"\n📤 RETURNED: {len(result)} statement(s)")
    if result:
        for i, stmt in enumerate(result[:5]):
            print(f"  [{i}] {type(stmt).__name__}")
    else:
        print(f"  ❌ EMPTY LIST - THIS IS THE BUG!")
    
    return result

converter.io_backend.convert_read = traced_convert_read

# Convert
print("\n[7] Running conversion...")
print("="*80 + "\n")

try:
    ailang_ast = converter.convert_compilation_unit(ast)
    print("\n" + "="*80)
    print("✅ Conversion completed")
except Exception as e:
    print("\n" + "="*80)
    print(f"❌ Conversion failed: {e}")
    import traceback
    traceback.print_exc()

# Summary
print("\n" + "="*80)
print("SUMMARY")
print("="*80)
print(f"READ statements found : {len(read_stmts)}")
print(f"convert_read() calls  : {call_count[0]}")

if call_count[0] == 0:
    print("\n❌ convert_read() was NEVER CALLED!")
    print("   Problem is in statement_converter.py")
elif call_count[0] < len(read_stmts):
    print(f"\n⚠️  Only {call_count[0]}/{len(read_stmts)} READ statements converted")
    
print("="*80)