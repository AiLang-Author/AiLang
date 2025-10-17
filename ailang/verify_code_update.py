#!/usr/bin/env python3
"""
Verify that _parse_occurs_clause has been updated with ASCENDING KEY support
"""

import sys
import os
project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
sys.path.insert(0, project_root)

print("="*70)
print("Code Verification - Checking _parse_occurs_clause")
print("="*70)

# Read the expression_parsers.py file
expr_parsers_path = os.path.join(project_root, 'ailang','cobol_frontend', 'parser', 'expression_parsers.py')

with open(expr_parsers_path, 'r') as f:
    content = f.read()

print("\n[1] Checking for _parse_occurs_clause function...")
if 'def _parse_occurs_clause(self):' in content:
    print("✓ Function exists")
else:
    print("✗ Function NOT found!")
    sys.exit(1)

print("\n[2] Checking for ASCENDING KEY support...")
checks = {
    'ascending_keys': "'ascending_keys': []" in content or '"ascending_keys": []' in content,
    'descending_keys': "'descending_keys': []" in content or '"descending_keys": []' in content,
    'ASCENDING detection': "token_val == 'ASCENDING'" in content or 'token_val == "ASCENDING"' in content,
    'DESCENDING detection': "token_val == 'DESCENDING'" in content or 'token_val == "DESCENDING"' in content,
    'Debug ASCENDING': '[PARSE_OCCURS] Found ASCENDING KEY' in content,
    'Debug DESCENDING': '[PARSE_OCCURS] Found DESCENDING KEY' in content,
}

all_passed = True
for check_name, passed in checks.items():
    status = "✓" if passed else "✗"
    print(f"  {status} {check_name}")
    if not passed:
        all_passed = False

print("\n[3] Checking return dict structure...")
# Look for the return statement
if 'return {' in content:
    # Find the return statement in _parse_occurs_clause
    func_start = content.find('def _parse_occurs_clause(self):')
    if func_start != -1:
        # Find the return statement after function start
        func_content = content[func_start:func_start+5000]  # Get next 5000 chars
        
        if "'ascending_keys':" in func_content or '"ascending_keys":' in func_content:
            print("✓ Return dict includes ascending_keys")
        else:
            print("✗ Return dict MISSING ascending_keys")
            all_passed = False
        
        if "'descending_keys':" in func_content or '"descending_keys":' in func_content:
            print("✓ Return dict includes descending_keys")
        else:
            print("✗ Return dict MISSING descending_keys")
            all_passed = False

print("\n" + "="*70)
if all_passed:
    print("✓ ALL CHECKS PASSED - Function has been updated correctly")
    print("\nThe function should now parse ASCENDING KEY and DESCENDING KEY.")
    print("If parsing still fails, the issue is elsewhere in the code.")
else:
    print("✗ SOME CHECKS FAILED - Function needs to be updated")
    print("\nPlease replace _parse_occurs_clause() in:")
    print(f"  {expr_parsers_path}")
    print("\nWith the version from the artifact that includes:")
    print("  - ascending_keys list")
    print("  - descending_keys list")
    print("  - Loop to parse ASCENDING KEY clauses")
    print("  - Loop to parse DESCENDING KEY clauses")

print("="*70)