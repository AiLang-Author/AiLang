#!/usr/bin/env python3
# quick_fix.py - Apply the import alias fix automatically

import os
import re

def fix_main_py():
    """Fix main.py to capture and use alias mappings"""
    print("Fixing main.py...")
    
    with open("main.py", "r") as f:
        lines = f.readlines()
    
    modified = False
    new_lines = []
    
    for i, line in enumerate(lines):
        # Fix the enhanced_load_source line
        if "source_code = enhanced_load_source(" in line and ", alias_mappings" not in line:
            # Change to capture both values
            new_line = line.replace(
                "source_code = enhanced_load_source(",
                "source_code, alias_mappings = enhanced_load_source("
            )
            new_lines.append(new_line)
            print(f"  ✓ Fixed line {i+1}: capturing alias_mappings")
            modified = True
        elif "compiler = AILANGToX64Compiler(" in line:
            # Add this line, then add alias mapping setter
            new_lines.append(line)
            # Add the alias mapping assignment
            indent = len(line) - len(line.lstrip())
            new_lines.append(" " * indent + "# Pass alias mappings to compiler\n")
            new_lines.append(" " * indent + "compiler.alias_mappings = alias_mappings if 'alias_mappings' in locals() else {}\n")
            print(f"  ✓ Added alias_mappings to compiler at line {i+1}")
            modified = True
        else:
            new_lines.append(line)
    
    if modified:
        # Backup original
        os.rename("main.py", "main.py.backup")
        
        # Write fixed version
        with open("main.py", "w") as f:
            f.writelines(new_lines)
        
        print("  ✓ main.py fixed and backed up as main.py.backup")
    else:
        print("  ⚠️ Could not find lines to fix in main.py")
    
    return modified

def fix_compiler():
    """Fix ailang_compiler.py to handle alias mappings"""
    compiler_path = "ailang_compiler/ailang_compiler.py"
    
    print(f"Fixing {compiler_path}...")
    
    with open(compiler_path, "r") as f:
        content = f.read()
    
    # Find compile_function_call method
    pattern = r'(def compile_function_call\(self, node\):.*?)(\n\s+)(.*?raise ValueError\(f["\']Unknown function:)'
    
    def replacement(match):
        method_def = match.group(1)
        indent = match.group(2)
        rest = match.group(3)
        
        # Insert alias handling code
        alias_code = f'''{method_def}
{indent}# Handle aliased module functions
{indent}function_name = node.function
{indent}if hasattr(self, 'alias_mappings') and self.alias_mappings:
{indent}    if '.' in function_name:
{indent}        # Check all aliases
{indent}        for alias, original in self.alias_mappings.items():
{indent}            if alias in function_name:
{indent}                # Replace alias with original module name
{indent}                node.function = function_name.replace(alias, original)
{indent}                break
{indent}
{indent}{rest}'''
        return alias_code
    
    # Check if we can find the pattern
    if re.search(pattern, content, re.DOTALL):
        # Backup original
        os.rename(compiler_path, compiler_path + ".backup")
        
        # Apply the fix
        new_content = re.sub(pattern, replacement, content, count=1, flags=re.DOTALL)
        
        with open(compiler_path, "w") as f:
            f.write(new_content)
        
        print(f"  ✓ {compiler_path} fixed and backed up")
        return True
    else:
        print(f"  ⚠️ Could not find compile_function_call pattern")
        print("  Manual fix needed - add this at the start of compile_function_call:")
        print("""
        # Handle aliased module functions
        function_name = node.function
        if hasattr(self, 'alias_mappings') and self.alias_mappings:
            if '.' in function_name:
                for alias, original in self.alias_mappings.items():
                    if alias in function_name:
                        node.function = function_name.replace(alias, original)
                        break
        """)
        return False

def test_fix():
    """Test if the fix works"""
    print("\nTesting the fix...")
    
    # Create simple test
    with open("test_fix.ailang", "w") as f:
        f.write("""Import XArrays from "./Library.XArrays.ailang"

Program TestFix {
    Main: {
        arr = XArrays.XArray.XCreate(10)
        PrintMessage("Import fix works!")
        ReturnValue(0)
    }
}""")
    
    # Try to compile
    import subprocess
    result = subprocess.run(
        ["python3", "main.py", "test_fix.ailang"],
        capture_output=True,
        text=True
    )
    
    if "Unknown function" not in result.stderr and result.returncode == 0:
        print("  ✅ FIX WORKS! Test compiled successfully!")
        return True
    elif "Unknown function" in result.stderr:
        print("  ❌ Still getting 'Unknown function' error")
        print("  Error:", result.stderr.split('\n')[-2])
        return False
    else:
        print("  ⚠️ Different error occurred:")
        print(result.stderr[-200:] if result.stderr else result.stdout[-200:])
        return False

def main():
    print("=" * 60)
    print("  Applying Import Alias Fix")
    print("=" * 60)
    print()
    
    # Apply fixes
    main_fixed = fix_main_py()
    compiler_fixed = fix_compiler()
    
    print()
    
    if main_fixed and compiler_fixed:
        # Test the fix
        if test_fix():
            print("\n" + "=" * 60)
            print("  ✅ SUCCESS! Import bug is fixed!")
            print("=" * 60)
            print("\nNow try compiling your Redis server:")
            print("  python3 main.py redis_server.ailang")
        else:
            print("\n⚠️ Fix was applied but test failed.")
            print("Check the manual fix instructions above.")
    else:
        print("\n⚠️ Some fixes need manual application.")
        print("See instructions above.")
    
    print("\nTo restore backups if needed:")
    print("  mv main.py.backup main.py")
    print("  mv ailang_compiler/ailang_compiler.py.backup ailang_compiler/ailang_compiler.py")

if __name__ == "__main__":
    main()