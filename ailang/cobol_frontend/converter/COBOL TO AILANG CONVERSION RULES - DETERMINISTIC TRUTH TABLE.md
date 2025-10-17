=============================================================================
COBOL TO AILANG CONVERSION RULES - DETERMINISTIC TRUTH TABLE
=============================================================================

VERSION: 1.0
PURPOSE: Define exact, deterministic rules for converting COBOL to AiLang
         to ensure correct program structure and execution flow.

=============================================================================
RULE 1: PROGRAM SUBROUTINE GENERATION
=============================================================================

INPUT CONDITIONS:
  - program.program_id
  - Has LINKAGE SECTION? (Y/N)
  - Has PROCEDURE DIVISION? (Y/N)
  - Has named paragraphs? (Y/N)
  - Is nested program? (Y/N)

OUTPUT STRUCTURE:

┌─────────────────────────────────────────────────────────────────────┐
│ SubRoutine.PROGRAM_NAME {                                           │
│   // 1. Initialize pool/arrays                                      │
│   POOL_NAME.var1 = ArrayCreate(N)                                   │
│   POOL_NAME.var2 = "initial"                                        │
│                                                                      │
│   // 2. Call PROCEDURE DIVISION entry point                         │
│   RunTask(PROGRAM_NAME_ENTRY_PARAGRAPH)                             │
│                                                                      │
│   // 3. Return                                                       │
│   ReturnValue(0)                                                     │
│ }                                                                    │
└─────────────────────────────────────────────────────────────────────┘

ENTRY PARAGRAPH DETERMINATION (Priority Order):
  1. First paragraph in first SECTION (if sections exist)
  2. First named paragraph (if flat structure)
  3. Common patterns: A10-MAIN, MAIN-LOGIC, 0000-MAIN
  4. If no named paragraphs: inline statements directly

=============================================================================
RULE 2: MAIN SUBROUTINE GENERATION
=============================================================================

CONDITION TABLE:

┌──────────────┬────────────┬───────────────┬─────────────────────────┐
│ Program Type │ Has LINKAGE│ Is Nested     │ Main() Action           │
├──────────────┼────────────┼───────────────┼─────────────────────────┤
│ Entry Point  │ NO         │ NO            │ RunTask(PROGRAM)        │
│ Library      │ YES        │ NO            │ (skip - not callable)   │
│ Nested       │ Either     │ YES           │ (skip - called by parent│
│ Multiple     │ Mixed      │ NO            │ RunTask each non-LINKAGE│
└──────────────┴────────────┴───────────────┴─────────────────────────┘

ALGORITHM:

```python
main_body = []
for program in compilation_unit.programs:
    if program.is_nested:
        continue  # Nested programs called by parent
    
    program_name = normalize(program.program_id)
    has_linkage = program_name in linkage_params
    
    if not has_linkage:
        main_body.append(RunTask(program_name))

if main_body:
    return SubRoutine('Main', main_body)
else:
    # All programs have LINKAGE - this is a library
    # Create empty Main that does nothing
    return SubRoutine('Main', [ReturnValue(0)])
```

=============================================================================
RULE 3: PARAGRAPH SUBROUTINE GENERATION
=============================================================================

For each named paragraph in PROCEDURE DIVISION:

┌─────────────────────────────────────────────────────────────────────┐
│ SubRoutine.PROGRAM_NAME_PARAGRAPH_NAME {                            │
│   // Convert all statements in paragraph                            │
│   [converted statements]                                             │
│                                                                      │
│   ReturnValue(0)                                                     │
│ }                                                                    │
└─────────────────────────────────────────────────────────────────────┘

NAMING CONVENTION:
  - Parent program: EXEC85
  - Paragraph: A10-MAIN
  - Generated: EXEC85_A10_MAIN

SECTION HANDLING:
  - Sections become subroutines that call their first paragraph
  - Example: SECTION-NAME → RunTask(PROGRAM_SECTION_FIRST_PARA)

=============================================================================
RULE 4: POOL GENERATION
=============================================================================

CONDITION: Program has WORKING-STORAGE or LINKAGE variables

OUTPUT:

┌─────────────────────────────────────────────────────────────────────┐
│ FixedPool.COBOL_PROGRAM_VARS {                                      │
│   "VAR1": Initialize=value, CanChange=True                          │
│   "VAR2": Initialize=0, CanChange=True                              │
│   "CONST1": Initialize="X", CanChange=False                         │
│ }                                                                    │
│                                                                      │
│ SubRoutine.PROGRAM {                                                 │
│   // Initialize arrays (not in pool)                                │
│   COBOL_PROGRAM_VARS.ARRAY1 = ArrayCreate(50)                       │
│                                                                      │
│   // Call entry point                                                │
│   RunTask(PROGRAM_ENTRY)                                             │
│   ReturnValue(0)                                                     │
│ }                                                                    │
└─────────────────────────────────────────────────────────────────────┘

=============================================================================
RULE 5: LINKAGE SECTION HANDLING
=============================================================================

CONDITION: Program has LINKAGE SECTION

Creates special LINKAGE pool:

┌─────────────────────────────────────────────────────────────────────┐
│ FixedPool.COBOL_PROGRAM_LINKAGE {                                   │
│   "PARAM1": Initialize="", CanChange=True                           │
│   "PARAM2": Initialize=0, CanChange=True                            │
│ }                                                                    │
│                                                                      │
│ // Calling convention:                                               │
│ // 1. Caller sets LINKAGE pool values                               │
│ COBOL_SUBPROG_LINKAGE.PARAM1 = local_var1                           │
│ COBOL_SUBPROG_LINKAGE.PARAM2 = local_var2                           │
│                                                                      │
│ // 2. Caller invokes subroutine                                     │
│ RunTask(SUBPROG)                                                     │
│                                                                      │
│ // 3. Caller reads results back                                     │
│ result = COBOL_SUBPROG_LINKAGE.PARAM2                               │
└─────────────────────────────────────────────────────────────────────┘

Main() EXCLUDES programs with LINKAGE SECTION - they're libraries.

=============================================================================
RULE 6: EXECUTION FLOW
=============================================================================

CORRECT CALL CHAIN:

1. AiLang Runtime starts
   ↓
2. Calls: RunTask(Main)
   ↓
3. Main calls: RunTask(PROGRAM1)
   ↓
4. PROGRAM1 subroutine:
   - Initializes pool/arrays
   - Calls: RunTask(PROGRAM1_A10_MAIN)  ← ENTRY PARAGRAPH
   ↓
5. PROGRAM1_A10_MAIN:
   - Executes PROCEDURE DIVISION logic
   - Calls other paragraphs via RunTask(PROGRAM1_B20_CALC)
   - Returns
   ↓
6. Back to PROGRAM1 subroutine
   - Returns to Main
   ↓
7. Main continues or exits

=============================================================================
RULE 7: DEBUGGING TRUTH TABLE
=============================================================================

Debug blocks are wrapped around statements based on type:

┌──────────────────┬────────────┬─────────────────────────────────────┐
│ Statement Type   │ Debug Level│ Generated Code                      │
├──────────────────┼────────────┼─────────────────────────────────────┤
│ ACCEPT           │ 2          │ Debug("cobol.trace", 2) {           │
│                  │            │   PrintMessage("[COBOL] ACCEPT...") │
│                  │            │ }                                    │
│                  │            │ [assignment statement]              │
├──────────────────┼────────────┼─────────────────────────────────────┤
│ PERFORM          │ 3          │ Debug("cobol.trace", 3) {           │
│                  │            │   PrintMessage("[COBOL] PERFORM")   │
│                  │            │ }                                    │
│                  │            │ RunTask(PARAGRAPH)                  │
├──────────────────┼────────────┼─────────────────────────────────────┤
│ IF               │ 3          │ Debug("cobol.trace", 3) {           │
│                  │            │   PrintMessage("[COBOL] IF")        │
│                  │            │ }                                    │
│                  │            │ IfCondition ... ThenBlock { }       │
├──────────────────┼────────────┼─────────────────────────────────────┤
│ CALL             │ 2          │ Debug("cobol.trace", 2) {           │
│                  │            │   PrintMessage("[COBOL] CALL")      │
│                  │            │ }                                    │
│                  │            │ [linkage setup]                     │
│                  │            │ RunTask(CALLED_PROGRAM)             │
└──────────────────┴────────────┴─────────────────────────────────────┘

Compile flags:
  - No flag: Debug blocks stripped
  - -D or -D1: Level 1 only
  - -D2: Levels 1-2
  - -D3: Levels 1-3 (all)

=============================================================================
RULE 8: ERROR CONDITIONS
=============================================================================

ERROR: Main() generated but empty
CAUSE: All programs have LINKAGE SECTION (library-only compilation)
FIX: Generate Main with just ReturnValue(0)

ERROR: Program runs but does nothing
CAUSE: Main calls pool init only, not entry paragraph
FIX: Apply RULE 1 - program subroutine must call entry paragraph

ERROR: "Subroutine not found" at runtime
CAUSE: Paragraph name mismatch or missing RunTask in program subroutine
FIX: Check paragraph naming convention (PROGRAM_PARA format)

ERROR: Variables not initialized
CAUSE: Pool not initialized before entry paragraph called
FIX: Ensure pool init happens BEFORE RunTask(entry_paragraph)

=============================================================================
IMPLEMENTATION CHECKLIST
=============================================================================

[ ] create_program_subroutine():
    [ ] Initialize pool/arrays
    [ ] Determine entry paragraph (strategies 1-4)
    [ ] Add RunTask(entry_paragraph)
    [ ] Add ReturnValue(0)

[ ] convert_compilation_unit():
    [ ] Build linkage_params registry
    [ ] Convert all programs
    [ ] Generate Main() using RULE 2 truth table
    [ ] Filter nested programs from Main
    [ ] Filter LINKAGE programs from Main

[ ] Paragraph generation:
    [ ] Use PROGRAM_PARAGRAPH naming
    [ ] Convert all statements
    [ ] Add ReturnValue(0)

[ ] Debug wrapper:
    [ ] Apply level-based wrapping per RULE 7
    [ ] Preserve return types (If returns If, not List)
    [ ] Flatten only for statements that return List

=============================================================================
TEST CASES FOR VALIDATION
=============================================================================

TEST 1: Single program, no LINKAGE
  Input: PROGRAM-ID. TEST1. [has PROCEDURE DIVISION]
  Expected Main: RunTask(TEST1)
  Expected TEST1: Initialize vars, RunTask(TEST1_entry), Return

TEST 2: Program with LINKAGE
  Input: PROGRAM-ID. SUBPROG. [has LINKAGE SECTION]
  Expected Main: Empty or skip SUBPROG
  Expected SUBPROG: Normal subroutine (callable via CALL)

TEST 3: Multiple programs
  Input: PROGRAM-ID. PROG1. ... PROGRAM-ID. PROG2.
  Expected Main: RunTask(PROG1), RunTask(PROG2)

TEST 4: Nested program
  Input: PROGRAM-ID. PARENT. ... PROGRAM-ID. CHILD IS COMMON.
  Expected Main: RunTask(PARENT) only
  Expected: Both PARENT and CHILD subroutines exist

TEST 5: EXEC85 case (from bug report)
  Input: EXEC85 with A10-MAIN as entry
  Expected TEST1: 
    SubRoutine.EXEC85 {
      [init arrays]
      RunTask(EXEC85_A10_MAIN)
      ReturnValue(0)
    }
  Expected Main: RunTask(EXEC85)
  Expected Output: Program executes A10-MAIN logic

=============================================================================
END OF CONVERSION RULES
=============================================================================