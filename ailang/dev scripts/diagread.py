#!/usr/bin/env python3
"""
Diagnostic: Why is READ ACCOUNT-FILE not being converted?

Corrected for Ailang/ailang/ directory structure.
Run this from: Ailang/ailang/ directory
"""

import sys
import os

# Verify we're in the right directory
if not os.path.exists('cobol_frontend'):
    print("=" * 70)
    print("ERROR: Run this script from the Ailang/ailang/ directory!")
    print("=" * 70)
    print(f"Current directory: {os.getcwd()}")
    print("\nExpected structure:")
    print("  Ailang/ailang/")
    print("    ├── cobol_frontend/")
    print("    │   ├── converter/")
    print("    │   │   ├── converter_core.py")
    print("    │   │   ├── io_jcl.py")
    print("    │   │   └── ...")
    print("    └── ...")
    print("\nUsage:")
    print("  cd Ailang/ailang")
    print("  python3 diagnose_read_issue.py")
    sys.exit(1)

# Check 1: Does io_jcl.py have the SQL backend code?
print("=" * 70)
print("CHECK 1: Does io_jcl.py have SQL backend?")
print("=" * 70)

io_jcl_path = "cobol_frontend/converter/io_jcl.py"
if os.path.exists(io_jcl_path):
    with open(io_jcl_path, 'r') as f:
        content = f.read()
    
    has_convert_file_control = "def convert_file_control" in content
    has_is_indexed_file = "def _is_indexed_file" in content
    has_convert_read_sql = "def _convert_read_sql" in content
    has_file_metadata = "self.file_metadata" in content
    
    print(f"  ✓ Has convert_file_control(): {has_convert_file_control}")
    print(f"  ✓ Has _is_indexed_file(): {has_is_indexed_file}")
    print(f"  ✓ Has _convert_read_sql(): {has_convert_read_sql}")
    print(f"  ✓ Has file_metadata dict: {has_file_metadata}")
    
    if not all([has_convert_file_control, has_is_indexed_file, has_convert_read_sql, has_file_metadata]):
        print("\n  ❌ ERROR: io_jcl.py is missing SQL backend code!")
        print("  ACTION: Replace with io_jcl_with_sql_FIXED.py")
    else:
        print("\n  ✅ io_jcl.py has SQL backend code")
        
        # Check line count to distinguish old vs new
        lines = len(content.split('\n'))
        print(f"  📊 File size: {lines} lines")
        if lines < 200:
            print("  ⚠️  File seems small for full SQL backend (expected 600+ lines)")
        else:
            print("  ✅ File size looks correct for SQL backend")
else:
    print(f"  ❌ ERROR: {io_jcl_path} not found!")
    print(f"  Current directory: {os.getcwd()}")

# Check 2: Does converter_core.py call convert_file_control()?
print("\n" + "=" * 70)
print("CHECK 2: Does converter_core.py process SELECT statements?")
print("=" * 70)

converter_path = "cobol_frontend/converter/converter_core.py"
if os.path.exists(converter_path):
    with open(converter_path, 'r') as f:
        content = f.read()
    
    # Look for the SELECT processing code
    has_select_processing = "program.data_division.select_statements" in content
    has_backend_call = "self.io_backend.convert_file_control" in content
    
    print(f"  ✓ Checks select_statements: {has_select_processing}")
    print(f"  ✓ Calls io_backend.convert_file_control(): {has_backend_call}")
    
    if not (has_select_processing and has_backend_call):
        print("\n  ❌ ERROR: converter_core.py is NOT processing SELECT statements!")
        print("  ACTION: Apply SELECT statement patch (APPLY_SELECT_PATCH_EXACT.md)")
        print("\n  Quick fix: Add this code after line ~344 (after _reset_program_state):")
        print("""
        # Phase 0.5: Process SELECT statements
        if program.data_division and program.data_division.select_statements:
            for select_stmt in program.data_division.select_statements:
                self.io_backend.convert_file_control(select_stmt)
        """)
    else:
        print("\n  ✅ converter_core.py processes SELECT statements")
        
        # Find the exact location
        lines = content.split('\n')
        for i, line in enumerate(lines):
            if 'io_backend.convert_file_control' in line:
                print(f"  📍 Found at line {i+1}: {line.strip()}")
                break
else:
    print(f"  ❌ ERROR: {converter_path} not found!")

# Check 3: Look at actual output
print("\n" + "=" * 70)
print("CHECK 3: What's actually in BANKING.ailang?")
print("=" * 70)

# Try multiple possible locations
banking_paths = [
    "BANKING.ailang",
    "../BANKING.ailang",
    "cobol_frontend/BANKING.ailang",
    "cobol_frontend/tests/BANKING.ailang"
]

banking_content = None
banking_found = None

for path in banking_paths:
    if os.path.exists(path):
        with open(path, 'r') as f:
            banking_content = f.read()
        banking_found = path
        break

if banking_content:
    print(f"  📂 Found: {banking_found}\n")
    
    has_pg_query = "PG_Query" in banking_content
    has_db_conn = "db_conn" in banking_content
    has_xarray = "XArray.XGet" in banking_content
    has_hashmap = "HashMap.HGetSimple" in banking_content
    has_file_open = "FileOpen" in banking_content
    has_account_fd = "ACCOUNT_FILE_FD = 1" in banking_content
    
    print(f"  SQL Operations:")
    print(f"    - PG_Query found: {has_pg_query} {' ✅' if has_pg_query else ' ❌'}")
    print(f"    - db_conn found: {has_db_conn} {' ✅' if has_db_conn else ' ❌'}")
    print(f"    - XArray.XGet found: {has_xarray} {' ✅' if has_xarray else ' ❌'}")
    print(f"    - HashMap.HGetSimple found: {has_hashmap} {' ✅' if has_hashmap else ' ❌'}")
    print(f"\n  File Operations:")
    print(f"    - FileOpen found: {has_file_open} {' ⚠️ (should not be used for indexed files)' if has_file_open else ' ✅'}")
    print(f"    - ACCOUNT_FILE_FD = 1: {has_account_fd} {' ✅ (OPEN working)' if has_account_fd else ' ❌'}")
    
    if has_pg_query:
        print("\n  ✅ SQL operations ARE being generated!")
        print("     Backend is routing correctly")
    else:
        print("\n  ❌ SQL operations NOT being generated")
        print("     Backend is not routing to SQL handlers")
        
        if has_file_open:
            print("\n  ⚠️  Using FileOpen instead of SQL")
            print("     This means _is_indexed_file() is returning False")
            print("     Which means file_metadata is empty or incorrect")
else:
    print(f"  ⚠️  BANKING.ailang not found in any of these locations:")
    for path in banking_paths:
        print(f"    - {path}")

# Summary
print("\n" + "=" * 70)
print("DIAGNOSTIC SUMMARY")
print("=" * 70)

# Determine the issue
check1_pass = os.path.exists(io_jcl_path)
check2_pass = os.path.exists(converter_path) and "self.io_backend.convert_file_control" in open(converter_path).read()
check3_pass = banking_content and "PG_Query" in banking_content if banking_content else False

if check1_pass and check2_pass and check3_pass:
    print("  ✅ ALL CHECKS PASSED - SQL backend is working!")
    print("     If there are still issues, they're in field mapping or query generation")
elif check1_pass and check2_pass and not check3_pass:
    print("  ⚠️  Backend code is in place but SQL not being generated")
    print("     Possible causes:")
    print("     1. SELECT metadata not being extracted by parser")
    print("     2. File type detection logic failing")
    print("     3. Debug needed: Re-transpile with --debug flag")
elif check1_pass and not check2_pass:
    print("  ❌ CRITICAL: SELECT statement patch NOT applied")
    print("     This is the blocker! Backend can't get file metadata.")
    print("\n  FIX:")
    print("     1. Edit: cobol_frontend/converter/converter_core.py")
    print("     2. Find: self._reset_program_state(program)")
    print("     3. Add after it:")
    print("        if program.data_division and program.data_division.select_statements:")
    print("            for select_stmt in program.data_division.select_statements:")
    print("                self.io_backend.convert_file_control(select_stmt)")
elif not check1_pass:
    print("  ❌ CRITICAL: SQL-enhanced io_jcl.py not found or not in place")
    print("     Expected: cobol_frontend/converter/io_jcl.py (600+ lines)")
    print("\n  FIX:")
    print("     Copy io_jcl_with_sql_FIXED.py to cobol_frontend/converter/io_jcl.py")

print("\n" + "=" * 70)
print("NEXT STEPS")
print("=" * 70)
print("""
1. Fix any failed checks above
2. Re-transpile with debug:
   cd cobol_frontend
   python3 cobol_integration.py tests/BANKING.cbl --io-backend jcl --ailang-only -o ../BANKING.ailang --debug

3. Look for these in the debug output:
   - "Processing N SELECT statement(s)"
   - "SELECT ACCOUNT-FILE ASSIGN TO ACCOUNTS"
   - "ORGANIZATION: INDEXED"
   - "JCL: Registered file ACCOUNT_FILE -> table accounts"

4. Check BANKING.ailang for:
   grep "PG_Query" BANKING.ailang
   # Should find SQL queries!
""")