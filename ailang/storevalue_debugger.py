#!/usr/bin/env python3
"""
Correct diagnostic tool for AILANG that uses the proper entry point
"""

import subprocess
import sys
import os
from pathlib import Path
import re

class CorrectDiagnostic:
    def __init__(self):
        self.ailang_dir = Path("/mnt/c/Users/Sean/Documents/Ailang/ailang")
        self.findings = []
        
    def create_minimal_test(self):
        """Create a minimal test case for pool variables"""
        test_code = '''// Minimal pool variable test - checking store/retrieve
FixedPool.RedisData {
    "store_ptr": Initialize=0
}

SubRoutine.Main {
    PrintMessage("Testing pool variable...\\n")
    
    // Store a value
    PrintMessage("Storing value 0xDEADBEEF...\\n")
    RedisData.store_ptr = 0xDEADBEEF
    
    // Read it back
    value = RedisData.store_ptr
    PrintMessage("Retrieved value: ")
    PrintNumber(value)
    PrintMessage("\\n")
    
    // Verify it worked
    IfCondition EqualTo(value, 0xDEADBEEF) ThenBlock: {
        PrintMessage("SUCCESS: Pool variable works!\\n")
    } ElseBlock: {
        PrintMessage("FAILURE: Got wrong value!\\n")
    }
}

RunTask(Main)
'''
        test_file = self.ailang_dir / "pool_test.ailang"
        test_file.write_text(test_code)
        return test_file
        
    def compile_with_main_py(self, source_file):
        """Compile using the correct main.py entry point"""
        print(f"\n[COMPILE] Using main.py to compile {source_file.name}...")
        
        cmd = [
            "python3",
            "main.py",
            str(source_file)
        ]
        
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            cwd=self.ailang_dir
        )
        
        # Save output
        output_log = Path("/tmp/compile_output.log")
        with open(output_log, 'w') as f:
            f.write("=== STDOUT ===\n")
            f.write(result.stdout)
            f.write("\n=== STDERR ===\n")
            f.write(result.stderr)
            
        print(f"[COMPILE] Output saved to {output_log}")
        
        # Parse for key information
        self.parse_compilation_output(result.stdout + result.stderr)
        
        return result.returncode == 0
        
    def parse_compilation_output(self, output):
        """Extract key information from compilation output"""
        patterns = {
            'pool_discovery': r'Discovered pool var.*?(\w+\.\w+).*?index (\d+)',
            'pool_allocation': r'Pool table allocated.*?R15',
            'variable_offset': r'(\w+\.\w+).*?offset (-?\d+)',
            'stack_size': r'Stack size.*?(\d+)',
            'store_instruction': r'MOV \[.*?\], .*?(store_ptr|pool)',
            'phase_order': r'Phase (\d+):.*',
        }
        
        for key, pattern in patterns.items():
            matches = re.findall(pattern, output, re.IGNORECASE)
            if matches:
                self.findings.append((key, matches))
                
    def analyze_binary(self, binary_path):
        """Analyze the generated binary"""
        if not binary_path.exists():
            print(f"[ERROR] Binary not found: {binary_path}")
            return False
            
        print(f"\n[ANALYZE] Examining binary {binary_path}...")
        
        # Run the binary
        print("[RUN] Executing binary...")
        result = subprocess.run(
            str(binary_path),
            capture_output=True,
            text=True,
            timeout=2
        )
        
        print("Output:")
        print(result.stdout)
        if result.stderr:
            print("Errors:")
            print(result.stderr)
            
        # Check exit code
        if result.returncode != 0:
            print(f"[WARNING] Binary exited with code {result.returncode}")
            
            # Try with GDB to see where it crashes
            self.gdb_analysis(binary_path)
            
        return result.returncode == 0
        
    def gdb_analysis(self, binary_path):
        """Quick GDB analysis of crash"""
        print("\n[GDB] Analyzing crash...")
        
        gdb_commands = """
run
bt
info registers rdi rsi rax rbx
x/10i $rip-20
quit
"""
        
        gdb_script = Path("/tmp/gdb_script.txt")
        gdb_script.write_text(gdb_commands)
        
        result = subprocess.run(
            ["gdb", "-batch", "-x", str(gdb_script), str(binary_path)],
            capture_output=True,
            text=True
        )
        
        # Extract key info
        if "SIGSEGV" in result.stdout:
            print("[GDB] Segmentation fault detected!")
            
            # Find the faulting instruction
            rip_match = re.search(r'rip\s+0x([0-9a-f]+)', result.stdout)
            if rip_match:
                print(f"[GDB] Fault at: 0x{rip_match.group(1)}")
                
            # Check register values
            if "rdi" in result.stdout:
                rdi_match = re.search(r'rdi\s+0x([0-9a-f]+)', result.stdout)
                if rdi_match and rdi_match.group(1) == "0":
                    print("[GDB] NULL pointer in RDI!")
                    
    def check_compiler_phases(self):
        """Check if compiler phases are correct"""
        print("\n[CHECK] Verifying compiler phase ordering...")
        
        # Check memory_manager.py for pool table allocation
        mem_manager = self.ailang_dir / "ailang_compiler" / "modules" / "memory_manager.py"
        if mem_manager.exists():
            content = mem_manager.read_text()
            
            # Find compile_program method
            if "def compile_program" in content:
                # Extract the method
                method_start = content.index("def compile_program")
                method_lines = content[method_start:method_start+2000].split('\n')[:50]
                
                # Check order of operations
                pool_table_line = -1
                pool_discover_line = -1
                
                for i, line in enumerate(method_lines):
                    if "allocate_pool_table" in line:
                        pool_table_line = i
                    if "discover_pool_variables" in line:
                        pool_discover_line = i
                        
                if pool_table_line > 0 and pool_discover_line > 0:
                    if pool_table_line > pool_discover_line:
                        print("[ERROR] Pool table allocated AFTER discovery!")
                        self.findings.append(('phase_error', 'Pool table allocated after variable discovery'))
                    else:
                        print("[OK] Pool table allocated before discovery")
                else:
                    print("[WARNING] Could not determine pool allocation order")
                    
    def generate_report(self):
        """Generate diagnostic report"""
        print("\n" + "="*60)
        print("DIAGNOSTIC REPORT")
        print("="*60)
        
        if self.findings:
            for key, data in self.findings:
                print(f"\n[{key.upper()}]")
                if isinstance(data, list):
                    for item in data:
                        print(f"  - {item}")
                else:
                    print(f"  {data}")
        else:
            print("No specific findings captured")
            
        print("\n[RECOMMENDATIONS]")
        
        # Check for common issues
        has_pool_discovery = any(k == 'pool_discovery' for k, _ in self.findings)
        has_pool_allocation = any(k == 'pool_allocation' for k, _ in self.findings)
        
        if not has_pool_discovery:
            print("  ❌ Pool variables not being discovered")
            print("     -> Check memory_manager.discover_pool_variables()")
            
        if not has_pool_allocation:
            print("  ❌ Pool table not being allocated")
            print("     -> Check if allocate_pool_table() is called")
            
        phase_error = any(k == 'phase_error' for k, _ in self.findings)
        if phase_error:
            print("  ❌ Compilation phases are in wrong order")
            print("     -> Pool table must be allocated BEFORE variable discovery")
            
    def run_full_diagnostic(self):
        """Run complete diagnostic"""
        print("="*60)
        print("AILANG POOL VARIABLE DIAGNOSTIC")
        print("="*60)
        
        # Create test
        test_file = self.create_minimal_test()
        print(f"[TEST] Created: {test_file}")
        
        # Compile
        success = self.compile_with_main_py(test_file)
        
        if success:
            print("[COMPILE] Success!")
            
            # Check for binary
            binary_path = test_file.with_name(test_file.stem + "_exec")
            if not binary_path.exists():
                binary_path = test_file.with_suffix("")
                
            if binary_path.exists():
                self.analyze_binary(binary_path)
            else:
                print("[ERROR] No binary generated!")
        else:
            print("[COMPILE] Failed!")
            
        # Check compiler structure
        self.check_compiler_phases()
        
        # Generate report
        self.generate_report()
        
        print("\n[COMPLETE] Check /tmp/compile_output.log for full details")

if __name__ == "__main__":
    diagnostic = CorrectDiagnostic()
    diagnostic.run_full_diagnostic()