#!/usr/bin/env python3
"""
Check if FixedPool/Acronym changes broke function calling convention
"""

import subprocess
import sys

def check_last_cobol_move(binary):
    """Check what the last COBOL MOVE did before crash"""
    
    gdb_script = """
# Set breakpoint on the debug print before C10-PROCESS-MONITOR
# We know it prints "[COBOL] PERFORM C10-PROCESS-MONITOR"

# Can't set breakpoint on string, so let's run and catch signals
catch signal SIGSEGV
commands
    silent
    
    printf "\\n=== ANALYZING STACK CORRUPTION ===\\n"
    
    # The function prologue looks correct:
    # 0x4133f2:  push   %%rbp
    # 0x4133f3:  mov    %%rsp,%%rbp
    # 0x4133f6:  push   %%rbx
    # ... saves registers ...
    # 0x4133fd:  sub    $0x70,%%rsp
    
    # So after prologue, stack should be:
    # [RBP+8] = return address
    # [RBP+0] = saved RBP
    # [RBP-8] = first parameter (RDI)
    # [RBP-16] = second parameter (RSI)
    
    printf "\\nChecking if stack is properly aligned...\\n"
    printf "RSP: 0x%%lx\\n", $rsp
    printf "RBP: 0x%%lx\\n", $rbp
    printf "RBP - RSP = %%d (should be 144 for 0x70 + saved regs)\\n", $rbp - $rsp
    
    printf "\\nChecking return address at [RBP+8]:\\n"
    x/gx $rbp+8
    
    printf "\\nBUT WAIT - return address should be at [RSP] right after CALL!\\n"
    printf "At function entry, [RSP] = return address\\n"
    printf "Let's check what's at [RBP+8] vs current [RSP]:\\n"
    
    printf "\\nStack dump from RSP:\\n"
    x/20gx $rsp
    
    printf "\\nStack dump from RBP:\\n"
    x/20gx $rbp-16
    
    printf "\\n=== THE PROBLEM ===\\n"
    printf "The return address is 0x1 (garbage)\\n"
    printf "This means either:\\n"
    printf "  1. The CALL instruction didn't push a return address\\n"
    printf "  2. The function prologue corrupted the stack\\n"
    printf "  3. A previous function didn't clean up the stack\\n"
    printf "\\n"
    
    printf "Let's check the code right before this function:\\n"
    x/50i 0x4133f2-200
    
    continue
end

run

quit
"""
    
    with open('/tmp/check_stack.gdb', 'w') as f:
        f.write(gdb_script)
    
    cmd = ['gdb', '-batch', '-x', '/tmp/check_stack.gdb', binary]
    result = subprocess.run(cmd, capture_output=True, text=True, timeout=10)
    return result.stdout

def check_fixedpool_changes():
    """Check what changed in FixedPool/Acronym code"""
    
    print("\n" + "="*60)
    print("CHECKING RECENT COMPILER CHANGES")
    print("="*60)
    
    print("""
The crash shows:
- Return address = 0x1 (should be a valid code address like 0x4xxxxx)
- This is STACK CORRUPTION

Recent changes to check:
1. FixedPool compilation - does it corrupt RSP/RBP?
2. Acronym resolution - does it mess up stack offsets?
3. Function prologue generation - is it computing wrong offsets?

Key question: Did you change how function calls/returns work?

Specifically check in these files:
- user_functions.py - function prologue/epilogue
- memory_manager.py - variable offset calculation
- pool_manager.py - FixedPool access code

Look for:
- Incorrect RSP adjustment (sub/add wrong amount)
- RBP setup issues (mov rsp, rbp vs mov rbp, rsp)
- Stack offset bugs (using wrong offsets for variables)
- Missing stack cleanup after function calls
""")

def find_bad_instruction():
    """Try to find which instruction corrupted the stack"""
    
    print("\n" + "="*60)
    print("THEORY: STACK CORRUPTION BEFORE FUNCTION CALL")
    print("="*60)
    
    print("""
The function at 0x4133f2 has a CORRECT prologue:
  push %rbp           # Save old RBP
  mov %rsp, %rbp      # Set up new frame pointer
  push %rbx           # Save callee-saved registers
  ... etc ...
  sub $0x70, %rsp     # Allocate locals (112 bytes)

But the return address is 0x1, which means BEFORE this function was
called, something already corrupted the stack.

The sequence would be:
  1. Some code executes (possibly FixedPool access with Acronym)
  2. It corrupts RSP somehow
  3. CALL instruction pushes return address to corrupted stack location  
  4. Function entry sees garbage return address
  5. When function tries to dereference RDI (which is 0), it crashes

The question is: What code runs between the last COBOL MOVE and the
crash that could corrupt RSP?

From the output:
  [COBOL] MOVE  WZ_LINE_CT          <-- Last working MOVE
  [COBOL] ACCEPT WA-DATE            <-- This works
  >>> ENTER: Cobol.FormatDateString <-- This works
  <<< EXIT: Cobol.FormatDateString  <-- This works
  [COBOL] PERFORM C10-PROCESS-MONITOR <-- Crash happens HERE

So the corruption happens either:
  a) During FormatDateString return (bad epilogue?)
  b) During the PERFORM (calling C10-PROCESS-MONITOR)
  c) In C10-PROCESS-MONITOR's first instruction

Check the generated assembly for:
  - FormatDateString epilogue
  - C10-PROCESS-MONITOR prologue
  - The CALL/JMP to C10-PROCESS-MONITOR
""")

def main():
    if len(sys.argv) < 2:
        binary = "EXEC85_exec"
    else:
        binary = sys.argv[1]
    
    print("="*60)
    print("DIAGNOSING STACK CORRUPTION")
    print("="*60)
    
    output = check_last_cobol_move(binary)
    print(output)
    
    check_fixedpool_changes()
    find_bad_instruction()
    
    print("\n" + "="*60)
    print("ACTION ITEMS")
    print("="*60)
    print("""
1. CHECK USER_FUNCTIONS.PY:
   Look at function prologue/epilogue generation
   Make sure RSP/RBP manipulation is correct
   
2. CHECK IF FIXEDPOOL ACCESS CHANGES RSP:
   FixedPool access should NOT modify RSP
   But recent Acronym changes might have broken this
   
3. GREP FOR RECENT CHANGES:
   git diff HEAD~5 -- ailang_compiler/modules/
   Look for any RSP/RBP manipulation changes
   
4. ADD STACK CANARY:
   Before any FixedPool/Acronym access, save RSP
   After access, verify RSP unchanged
   
5. BISECT THE COMMIT:
   Find which commit broke it by testing old versions
""")

if __name__ == "__main__":
    main()