# Bug Report: AILang Compiler Scope Manager Failure on Stack Arguments

## Summary

The AILang backend compiler fails with an `Undefined variable` error when compiling a function that has more than six integer/pointer arguments. This is caused by a bug in the compiler's implementation of the x86-64 calling convention, where it successfully processes arguments passed in registers but fails to correctly read arguments passed on the stack.

## Error Message

```
ValueError: Undefined variable: 'v_vitality'
```

## Root Cause Analysis

The detailed debug log from the AILang compiler reveals the following:

1.  **Register Arguments Succeed:** The compiler correctly identifies and allocates stack space for the first six parameters of a function (e.g., `v_level` through `v_dexterity`), which correspond to the `RDI`, `RSI`, `RDX`, `RCX`, `R8`, and `R9` registers.

2.  **Stack Arguments Fail:** The compilation fails precisely when attempting to access the **7th parameter** (`v_vitality`). According to the x86-64 ABI, this argument should be passed on the stack.

3.  **Scope Manager Log:** The debug output `DEBUG ScopeManager: 'v_vitality' not found in local or global scope` confirms that the compiler's scope management system is unaware of this 7th parameter, indicating it was never correctly read from the stack frame.

This is a critical bug in the backend compiler's function prologue generation and argument handling logic.

## Minimal Reproducible Example

The following AILang code will trigger the bug:

```ailang
Function.TestSevenParams {
    Input: p1: Integer, p2: Integer, p3: Integer, p4: Integer, p5: Integer, p6: Integer, p7: Integer
    Body: {
        # This call will fail because the compiler cannot find p7 on the stack.
        PrintNumber(p7)
    }
}
```

## Suggested Workaround (Frontend)

A temporary workaround, implemented in the AIMacro frontend, is to refactor functions with more than six parameters to accept a single list/array argument. This avoids triggering the backend bug by ensuring all data is passed through a single pointer in a register.