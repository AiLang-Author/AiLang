#!/usr/bin/env python3
# debug_import_system.py - Find the import/alias resolution bug

import os
import json
import re
import sys

def find_import_files():
    """Find all import-related files in the compiler"""
    print("=== Finding Import-Related Files ===")
    import_files = []
    
    # Look for import-related Python files
    for root, dirs, files in os.walk("."):
        for file in files:
            if any(keyword in file.lower() for keyword in ["import", "resolve", "alias", "preprocess"]):
                if file.endswith(".py"):
                    filepath = os.path.join(root, file)
                    import_files.append(filepath)
                    print(f"  Found: {filepath}")
    
    return import_files

def check_resolved_files():
    """Check for resolved import files"""
    print("\n=== Checking Resolved Files ===")
    
    # Look for any resolved or temporary files
    for file in os.listdir("."):
        if "resolved" in file or "NSUHTDNJ" in file or file.endswith(".tmp"):
            print(f"  Found resolved file: {file}")
            
            # Check first few lines
            try:
                with open(file, 'r') as f:
                    lines = f.readlines()[:10]
                    print(f"    First few lines of {file}:")
                    for line in lines:
                        if "NSUHTDNJ" in line or "Import" in line:
                            print(f"      {line.strip()}")
            except:
                print(f"    Could not read {file}")

def analyze_import_resolution():
    """Analyze how imports are being resolved"""
    print("\n=== Analyzing Import Resolution ===")
    
    # Check if there's a mapping file
    mapping_files = ["import_mappings.json", "alias_mappings.json", ".import_cache"]
    for mf in mapping_files:
        if os.path.exists(mf):
            print(f"  Found mapping file: {mf}")
            try:
                with open(mf, 'r') as f:
                    data = json.load(f) if mf.endswith('.json') else f.read()
                    print(f"    Content: {data}")
            except:
                pass
    
    # Look for the specific error pattern
    print("\n=== Analyzing Error Pattern ===")
    print("  Error: ValueError: Unknown function: NSUHTDNJ_XArray.XCreate")
    print("  This means:")
    print("    1. Import created alias 'NSUHTDNJ_XArray' for XArray")
    print("    2. Code is calling NSUHTDNJ_XArray.XCreate")
    print("    3. But compiler doesn't know NSUHTDNJ_XArray exists")
    print("\n  Possible causes:")
    print("    - Alias mapping not passed to compiler")
    print("    - Preprocessor not completing resolution")
    print("    - Module.Function syntax not handled for aliases")

def test_simple_import():
    """Create a minimal test case"""
    print("\n=== Creating Minimal Test Case ===")
    
    # Create test library
    with open("TestLib.ailang", "w") as f:
        f.write("""Module TestModule {
    Function.TestFunc {
        Input: x:
        Body: {
            ReturnValue(x)
        }
    }
}""")
    
    # Create test program
    with open("test_import_bug.ailang", "w") as f:
        f.write("""Import TestModule from "./TestLib.ailang"

Program TestImportBug {
    Main: {
        // This should work
        result = TestModule.TestFunc(42)
        PrintNumber(result)
        ReturnValue(0)
    }
}""")
    
    print("  Created TestLib.ailang and test_import_bug.ailang")
    print("  Try: python3 main.py test_import_bug.ailang")
    print("  If this fails with 'Unknown function: XXX_TestModule.TestFunc'")
    print("  then we've confirmed the bug pattern")

def find_compiler_flow():
    """Trace the compiler flow for imports"""
    print("\n=== Compiler Flow for Imports ===")
    
    # Look for main.py
    if os.path.exists("main.py"):
        with open("main.py", "r") as f:
            content = f.read()
            
        # Find import-related calls
        import_calls = re.findall(r'.*import.*|.*resolve.*|.*preprocess.*', content, re.IGNORECASE)
        if import_calls:
            print("  In main.py:")
            for call in import_calls[:5]:
                print(f"    {call.strip()}")
    
    # Check if import resolution happens before compilation
    print("\n  Typical flow should be:")
    print("    1. Parse imports")
    print("    2. Resolve imports (generate aliases)")
    print("    3. Save alias mappings")
    print("    4. Preprocess source (replace imports)")
    print("    5. Pass mappings to compiler")
    print("    6. Compile with alias knowledge")
    print("\n  The bug is likely in step 3, 5, or 6")

def suggest_fixes():
    """Suggest specific fixes"""
    print("\n=== Suggested Fixes ===")
    print()
    print("1. Quick Debug - Add print statements to trace:")
    print("   In import resolver:")
    print("     print(f'Creating alias {alias} for {module}')")
    print("   In compiler:")
    print("     print(f'Looking for function: {function_name}')")
    print("     print(f'Known aliases: {self.aliases}')")
    print()
    print("2. Check if alias mappings are passed to compiler:")
    print("   Look for something like:")
    print("     compiler = Compiler(alias_mappings=...)")
    print("   Or:")
    print("     compiler.set_aliases(mappings)")
    print()
    print("3. Fix Module.Function resolution for aliases:")
    print("   In compile_function_call, before 'Unknown function' error:")
    print("     # Check if it's an aliased module call")
    print("     if '.' in function_name:")
    print("         module, func = function_name.split('.', 1)")
    print("         if module in self.aliases:")
    print("             resolved = f'{self.aliases[module]}.{func}'")
    print("             # Try looking up resolved name")

def main():
    print("=" * 60)
    print("  AILANG Import System Debugger")
    print("=" * 60)
    print()
    
    find_import_files()
    check_resolved_files()
    analyze_import_resolution()
    test_simple_import()
    find_compiler_flow()
    suggest_fixes()
    
    print("\n" + "=" * 60)
    print("  Next Steps:")
    print("=" * 60)
    print("1. Run: python3 main.py test_import_bug.ailang")
    print("2. See if it gives the same 'Unknown function' error")
    print("3. Add debug prints to trace where aliases get lost")
    print("4. The fix is probably a one-line change!")
    print()
    print("This bug is EXACTLY why building Redis was brilliant -")
    print("it's complex enough to expose these compiler issues!")

if __name__ == "__main__":
    main()