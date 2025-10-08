* ================================================
      * Test 1: Simple Array Declaration
      * ================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-OCCURS-1.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUMBER-ARRAY OCCURS 5 PIC 9(2) VALUE 1.
       
       PROCEDURE DIVISION.
           DISPLAY "Array declaration test passed".
           STOP RUN.

      * ================================================
      * Test 2: Array Write and Read
      * ================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-OCCURS-2.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUMBERS OCCURS 6 PIC 9(2) VALUE 1.
       
       PROCEDURE DIVISION.
           MOVE 42 TO NUMBERS(2).
           MOVE 17 TO NUMBERS(3).
           MOVE 99 TO NUMBERS(4).
           
           DISPLAY "First element:".
           DISPLAY NUMBERS(2).
           DISPLAY "Second element:".
           DISPLAY NUMBERS(3).
           DISPLAY "Third element:".
           DISPLAY NUMBERS(4).
           
           STOP RUN.

      * ================================================
      * Test 3: Array with PERFORM VARYING Loop
      * ================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-OCCURS-3.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUMBERS OCCURS 6 PIC 9(2) VALUE 1.
       01 I PIC 9(1) VALUE 1.
       
       PROCEDURE DIVISION.
           DISPLAY "Populating array...".
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > 6
               MOVE I TO NUMBERS(I)
           END-PERFORM.
           
           DISPLAY "Array contents:".
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > 6
               DISPLAY NUMBERS(I)
           END-PERFORM.
           
           STOP RUN.

      * ================================================
      * Test 4: String Arrays
      * ================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-OCCURS-4.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NAMES OCCURS 4 PIC X(10) VALUE SPACES.
       
       PROCEDURE DIVISION.
           MOVE "Alice" TO NAMES(2).
           MOVE "Bob" TO NAMES(3).
           MOVE "Charlie" TO NAMES(4).
           
           DISPLAY "Name 1:".
           DISPLAY NAMES(2).
           DISPLAY "Name 2:".
           DISPLAY NAMES(3).
           DISPLAY "Name 3:".
           DISPLAY NAMES(4).
           
           STOP RUN.

      * ================================================
      * Test 5: Accumulator with Arrays
      * ================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-OCCURS-5.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUMBERS OCCURS 6 PIC 9(2) VALUE 1.
       01 I PIC 9(1) VALUE 1.
       01 SUM-VAL PIC 9(4) VALUE 1.
       
       PROCEDURE DIVISION.
           MOVE 10 TO NUMBERS(2).
           MOVE 20 TO NUMBERS(3).
           MOVE 30 TO NUMBERS(4).
           MOVE 40 TO NUMBERS(5).
           MOVE 50 TO NUMBERS(6).
           
           DISPLAY "Computing sum of array...".
           MOVE 0 TO SUM-VAL.
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > 6
               ADD NUMBERS(I) TO SUM-VAL
           END-PERFORM.
           
           DISPLAY "Sum: ".
           DISPLAY SUM-VAL.
           
           STOP RUN.

      * ================================================
      * Test 6: Simple Calculator (from simpcalc.cbl)
      * ================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIMPLE-CALC.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUM1 PIC 9(4).
       01 NUM2 PIC 9(4).
       01 RESULT PIC 9(5).
       01 OPERATION PIC X.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "Enter first number: ".
           ACCEPT NUM1.
           DISPLAY "Enter operation (+, -, *, /): ".
           ACCEPT OPERATION.
           DISPLAY "Enter second number: ".
           ACCEPT NUM2.
           
           EVALUATE OPERATION
               WHEN "+"
                   ADD NUM1 TO NUM2 GIVING RESULT
                   DISPLAY "Result: " RESULT
               WHEN "-"
                   SUBTRACT NUM2 FROM NUM1 GIVING RESULT
                   DISPLAY "Result: " RESULT
               WHEN "*"
                   MULTIPLY NUM1 BY NUM2 GIVING RESULT
                   DISPLAY "Result: " RESULT
               WHEN "/"
                   DIVIDE NUM1 BY NUM2 GIVING RESULT
                   DISPLAY "Result: " RESULT
               WHEN OTHER
                   DISPLAY "Invalid operation"
           END-EVALUATE.
           
           STOP RUN.

      * ================================================
      * Test 7: UPPER-CASE Function (from test_upper_case.cbl)
      * ================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-UPPER.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 INPUT-TEXT PIC X(20) VALUE "hello world".
       01 UPPER-TEXT PIC X(20).
       01 TEST-VAR PIC X(10) VALUE "MixedCase".
       
       PROCEDURE DIVISION.
           DISPLAY "Test 1: Basic UPPER-CASE".
           MOVE FUNCTION UPPER-CASE(INPUT-TEXT) TO UPPER-TEXT.
           DISPLAY "Input: " INPUT-TEXT.
           DISPLAY "Upper: " UPPER-TEXT.
           
           DISPLAY "Test 2: UPPER-CASE in EVALUATE".
           EVALUATE FUNCTION UPPER-CASE(TEST-VAR)
               WHEN "MIXEDCASE"
                   DISPLAY "Matched MIXEDCASE"
               WHEN OTHER
                   DISPLAY "No match"
           END-EVALUATE.
           
           DISPLAY "Test 3: Direct display".
           DISPLAY "Original: " TEST-VAR.
           DISPLAY "Uppercase: " FUNCTION UPPER-CASE(TEST-VAR).
           
           STOP RUN.

      * ================================================
      * Test 8: EVALUATE statement
      * ================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-EVALUATE.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 OPERATION-CODE PIC 9 VALUE 1.
       01 RESULT PIC 9(4) VALUE 1.
       01 STATUS-MSG PIC X(20).
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "Testing EVALUATE statement".
           
           EVALUATE OPERATION-CODE
               WHEN 1
                   MOVE 100 TO RESULT
                   DISPLAY "Operation 1: Result = 100"
               WHEN 2
                   MOVE 200 TO RESULT
                   DISPLAY "Operation 2: Result = 200"
               WHEN 3
                   MOVE 300 TO RESULT
                   DISPLAY "Operation 3: Result = 300"
               WHEN OTHER
                   MOVE 999 TO RESULT
                   DISPLAY "Unknown operation"
           END-EVALUATE.
           
           DISPLAY "Final result: ".
           DISPLAY RESULT.
           STOP RUN.

      * ================================================
      * Test 9: Complex Array Logic with IF and COMPUTE
      * ================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-COMPLEX-ARRAY-LOGIC.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 INPUT-NUMS OCCURS 6 PIC 9(2) VALUE 10.
       01 OUTPUT-NUMS OCCURS 6 PIC 9(3) VALUE 1.
       01 I PIC 9(1).
       
       PROCEDURE DIVISION.
           DISPLAY "Testing complex array logic...".
           
           * Modify one value to make the IF condition interesting
           MOVE 5 TO INPUT-NUMS(4).

           PERFORM VARYING I FROM 2 BY 1 UNTIL I > 6
               IF INPUT-NUMS(I) > 8
                   COMPUTE OUTPUT-NUMS(I) = INPUT-NUMS(I) * 10
               ELSE
                   COMPUTE OUTPUT-NUMS(I) = INPUT-NUMS(I) + 100
               END-IF
           END-PERFORM.
           
           DISPLAY "Output array contents:".
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > 6
               DISPLAY "Element " I ": " OUTPUT-NUMS(I)
           END-PERFORM.
           
           STOP RUN.
           
      * ================================================
      * Test 10: PERFORM...TIMES
      * ================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-PERFORM-TIMES.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 COUNTER       PIC 9(2) VALUE 1.
       01 TIMES-VAR     PIC 9(2) VALUE 3.

       PROCEDURE DIVISION.
           DISPLAY "Test 1: Inline PERFORM TIMES".
           PERFORM 3 TIMES
               ADD 1 TO COUNTER
               DISPLAY "Counter: " COUNTER
           END-PERFORM.

           DISPLAY "Test 2: Paragraph PERFORM TIMES".
           PERFORM SHOW-MESSAGE 5 TIMES.

           DISPLAY "Test 3: Variable TIMES".
           MOVE 2 TO TIMES-VAR.
           PERFORM TIMES-VAR TIMES
               DISPLAY "Var loop"
           END-PERFORM.

           DISPLAY "All tests complete".
           STOP RUN.

       SHOW-MESSAGE.
           DISPLAY "Hello from paragraph".

      * ================================================
      * Test 11: STRING - Basic Concatenation
      * ================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-STRING-1.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 FIRST-NAME PIC X(10) VALUE "John".
       01 LAST-NAME PIC X(10) VALUE "Smith".
       01 FULL-NAME PIC X(25).
       
       PROCEDURE DIVISION.
           DISPLAY "Test STRING: Basic concatenation".
           STRING FIRST-NAME DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  LAST-NAME DELIMITED BY SIZE
                  INTO FULL-NAME.
           DISPLAY "Full name: " FULL-NAME.
           STOP RUN.

      * ================================================
      * Test 12: STRING - Multiple Fields
      * ================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-STRING-2.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 GREETING PIC X(10) VALUE "Hello".
       01 SEPARATOR PIC X(5) VALUE ", ".
       01 NAME-FIELD PIC X(10) VALUE "World".
       01 EXCLAIM PIC X(1) VALUE "!".
       01 RESULT PIC X(30).
       
       PROCEDURE DIVISION.
           DISPLAY "Test STRING: Multiple fields".
           STRING GREETING DELIMITED BY SIZE
                  SEPARATOR DELIMITED BY SIZE
                  NAME-FIELD DELIMITED BY SIZE
                  EXCLAIM DELIMITED BY SIZE
                  INTO RESULT.
           DISPLAY "Result: " RESULT.
           STOP RUN.

      * ================================================
      * Test 13: STRING - With POINTER
      * ================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-STRING-3.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PART1 PIC X(5) VALUE "ABC".
       01 PART2 PIC X(5) VALUE "XYZ".
       01 OUTPUT-STR PIC X(20).
       01 STR-POINTER PIC 9(2) VALUE 1.
       
       PROCEDURE DIVISION.
           DISPLAY "Test STRING: With POINTER clause".
           STRING PART1 DELIMITED BY SIZE
                  PART2 DELIMITED BY SIZE
                  INTO OUTPUT-STR
                  WITH POINTER STR-POINTER.
           DISPLAY "Output: " OUTPUT-STR.
           DISPLAY "Pointer position: " STR-POINTER.
           STOP RUN.

      * ================================================
      * Test 14: UNSTRING - Basic Split
      * ================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-UNSTRING-1.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 FULL-NAME PIC X(25) VALUE "John Smith".
       01 FIRST-NAME PIC X(10).
       01 LAST-NAME PIC X(10).
       
       PROCEDURE DIVISION.
           DISPLAY "Test UNSTRING: Basic split".
           UNSTRING FULL-NAME DELIMITED BY " "
                    INTO FIRST-NAME LAST-NAME.
           DISPLAY "First name: " FIRST-NAME.
           DISPLAY "Last name: " LAST-NAME.
           STOP RUN.

      * ================================================
      * Test 15: UNSTRING - Multiple Delimiters
      * ================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-UNSTRING-2.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 INPUT-DATA PIC X(30) VALUE "A,B,C,D,E".
       01 FIELD1 PIC X(5).
       01 FIELD2 PIC X(5).
       01 FIELD3 PIC X(5).
       01 FIELD4 PIC X(5).
       01 FIELD5 PIC X(5).
       
       PROCEDURE DIVISION.
           DISPLAY "Test UNSTRING: Multiple delimiters".
           UNSTRING INPUT-DATA DELIMITED BY ","
                    INTO FIELD1 FIELD2 FIELD3 FIELD4 FIELD5.
           DISPLAY "Field 1: " FIELD1.
           DISPLAY "Field 2: " FIELD2.
           DISPLAY "Field 3: " FIELD3.
           DISPLAY "Field 4: " FIELD4.
           DISPLAY "Field 5: " FIELD5.
           STOP RUN.

      * ================================================
      * Test 16: UNSTRING - With TALLYING
      * ================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-UNSTRING-3.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 INPUT-STR PIC X(30) VALUE "ONE TWO THREE".
       01 WORD1 PIC X(10) VALUE SPACES.
       01 WORD2 PIC X(10).
       01 WORD3 PIC X(10).
       01 WORD-COUNT PIC 9(2) VALUE 1.
       
       PROCEDURE DIVISION.
           DISPLAY "Test UNSTRING: With TALLYING".
           UNSTRING INPUT-STR DELIMITED BY " "
                    INTO WORD1 WORD2 WORD3
                    TALLYING IN WORD-COUNT.
           DISPLAY "Word 1: " WORD1.
           DISPLAY "Word 2: " WORD2.
           DISPLAY "Word 3: " WORD3.
           DISPLAY "Word count: " WORD-COUNT.
           STOP RUN.

      * ================================================
      * Test 17: INSPECT - REPLACING ALL
      * ================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-INSPECT-1.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MESSAGE PIC X(20) VALUE "Hello World".
       
       PROCEDURE DIVISION.
           DISPLAY "Test INSPECT: REPLACING ALL".
           DISPLAY "Before: " MESSAGE.
           INSPECT MESSAGE REPLACING ALL "o" BY "0".
           DISPLAY "After: " MESSAGE.
           STOP RUN.

      * ================================================
      * Test 18: INSPECT - REPLACING Multiple Characters
      * ================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-INSPECT-2.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TEXT-DATA PIC X(30) VALUE "COBOL is great!".
       
       PROCEDURE DIVISION.
           DISPLAY "Test INSPECT: Multiple replacements".
           DISPLAY "Before: " TEXT-DATA.
           INSPECT TEXT-DATA REPLACING ALL "O" BY "0".
           INSPECT TEXT-DATA REPLACING ALL "!" BY ".".
           DISPLAY "After: " TEXT-DATA.
           STOP RUN.

      * ================================================
      * Test 19: INSPECT - TALLYING Occurrences
      * ================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-INSPECT-3.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 SAMPLE-TEXT PIC X(30) VALUE "The quick brown fox".
       01 LETTER-COUNT PIC 9(20) VALUE 10.
       
       PROCEDURE DIVISION.
           DISPLAY "Test INSPECT: TALLYING".
           DISPLAY "Text: " SAMPLE-TEXT.
           INSPECT SAMPLE-TEXT TALLYING LETTER-COUNT 
                   FOR ALL "5".
           DISPLAY "Count of '5': " LETTER-COUNT.
           STOP RUN.

      * ================================================
      * Test 20: INSPECT - Count Spaces
      * ================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-INSPECT-4.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 SENTENCE PIC X(40) VALUE "This is a test sentence".
       01 SPACE-COUNT PIC 9(2) VALUE 1.
       
       PROCEDURE DIVISION.
           DISPLAY "Test INSPECT: Count spaces".
           DISPLAY "Sentence: " SENTENCE.
           INSPECT SENTENCE TALLYING SPACE-COUNT 
                   FOR ALL " ".
           DISPLAY "Number of spaces: " SPACE-COUNT.
           STOP RUN.

      * ================================================
      * Test 21: Combined STRING and INSPECT
      * ================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-STRING-INSPECT.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 FIRST PIC X(10) VALUE "hello".
       01 SECOND PIC X(10) VALUE "world".
       01 COMBINED PIC X(25).
       
       PROCEDURE DIVISION.
           DISPLAY "Test: STRING then INSPECT".
           
           STRING FIRST DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  SECOND DELIMITED BY SIZE
                  INTO COMBINED.
           DISPLAY "After STRING: " COMBINED.
           
           INSPECT COMBINED REPLACING ALL "o" BY "O".
           DISPLAY "After INSPECT: " COMBINED.
           
           STOP RUN.

      * ================================================
      * Test 22: UNSTRING and STRING Round-trip
      * ================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-ROUND-TRIP.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ORIGINAL PIC X(30) VALUE "Alice,Bob,Charlie".
       01 NAME1 PIC X(10).
       01 NAME2 PIC X(10).
       01 NAME3 PIC X(10).
       01 REBUILT PIC X(30).
       
       PROCEDURE DIVISION.
           DISPLAY "Test: UNSTRING then STRING".
           DISPLAY "Original: " ORIGINAL.
           
           UNSTRING ORIGINAL DELIMITED BY ","
                    INTO NAME1 NAME2 NAME3.
           
           DISPLAY "Parsed names:".
           DISPLAY "  Name1: " NAME1.
           DISPLAY "  Name2: " NAME2.
           DISPLAY "  Name3: " NAME3.
           
           STRING NAME1 DELIMITED BY SIZE
                  " & " DELIMITED BY SIZE
                  NAME2 DELIMITED BY SIZE
                  " & " DELIMITED BY SIZE
                  NAME3 DELIMITED BY SIZE
                  INTO REBUILT.
           
           DISPLAY "Rebuilt: " REBUILT.
           STOP RUN.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-PERFORM-UNTIL.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 COUNTER PIC 9(2) VALUE 1.
       
       PROCEDURE DIVISION.
           DISPLAY "Test 1: Inline PERFORM UNTIL".
           PERFORM UNTIL COUNTER > 3
               ADD 1 TO COUNTER
               DISPLAY "Counter: " COUNTER
           END-PERFORM.
           
           MOVE 1 TO COUNTER.
           DISPLAY "Test 2: Paragraph PERFORM UNTIL".
           PERFORM SHOW-MESSAGE UNTIL COUNTER > 2.
           
           DISPLAY "All tests complete".
           STOP RUN.
       
       SHOW-MESSAGE.
           ADD 1 TO COUNTER. *> This will now start at 2
           DISPLAY "Hello from paragraph: " COUNTER.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-NESTED-PROGS.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 OUTER-CTR PIC 9(1) VALUE 1.
       
       PROCEDURE DIVISION.
           DISPLAY "Test nested programs".
           ADD 1 TO OUTER-CTR.
           DISPLAY "Outer counter: " OUTER-CTR.
           
           IDENTIFICATION DIVISION.
           PROGRAM-ID. NESTED-CHILD.
           
           DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 INNER-CTR PIC 9(1) VALUE 5.
           
           PROCEDURE DIVISION.
               DISPLAY "Nested child counter: " INNER-CTR.
               STOP RUN.
           END PROGRAM NESTED-CHILD.
           
           CALL "NESTED-CHILD".
           DISPLAY "After nested program".
           STOP RUN.
       END PROGRAM TEST-NESTED-PROGS.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN-PROG.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUM1 PIC 9(4) VALUE 10.
       01 NUM2 PIC 9(4) VALUE 20.
       01 RESULT PIC 9(4) VALUE 1.
       
       PROCEDURE DIVISION.
           DISPLAY "Calling ADD-NUMS with 10 and 20".
           CALL "ADD-NUMS" USING NUM1 NUM2 RESULT.
           DISPLAY "Result: " RESULT.
           STOP RUN.
           
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADD-NUMS.
       
       DATA DIVISION.
       LINKAGE SECTION.
       01 A PIC 9(4).
       01 B PIC 9(4).
       01 SUM PIC 9(4).
       
       PROCEDURE DIVISION USING A B SUM.
           COMPUTE SUM = A + B.
           DISPLAY "Inside ADD-NUMS: " A " + " B " = " SUM.
           GOBACK.
       END PROGRAM ADD-NUMS.
       END PROGRAM MAIN-PROG.

      ************************************************** 
      * PIC Format Comprehensive Test Suite
      * Tests all major PIC variations:
      * - Signed/Unsigned
      * - COMP/COMP-3/DISPLAY storage
      * - Display-edited formats
      * - Decimal precision
      **************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PIC-FORMATS-TEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      * ================================================
      * SECTION 1: Basic Unsigned Formats
      * ================================================
       01 BASIC-UNSIGNED.
           05 UNSIGNED-INT        PIC 9(4) VALUE 1234.
           05 UNSIGNED-DECIMAL    PIC 9(5)V99 VALUE 12345.67.
           05 STRING-DATA         PIC X(20) VALUE "HELLO WORLD".
       
      * ================================================
      * SECTION 2: Signed Formats (S prefix)
      * ================================================
       01 SIGNED-NUMBERS.
           05 SIGNED-INT          PIC S9(4) VALUE -100.
           05 SIGNED-DECIMAL      PIC S9(7)V99 VALUE -1234.56.
           05 SIGNED-LARGE        PIC S9(9)V99 VALUE 12345678.90.
       
      * ================================================
      * SECTION 3: USAGE COMP (Binary Storage)
      * ================================================
       01 COMP-NUMBERS.
           05 COMP-INT            PIC 9(4) USAGE COMP VALUE 500.
           05 COMP-SIGNED         PIC S9(6) COMP VALUE -12345.
           05 COMP-DECIMAL        PIC S9(7)V99 COMP VALUE 999.99.
           05 COMP-SMALL          PIC 99 COMP VALUE 42.
       
      * ================================================
      * SECTION 4: USAGE COMP-3 (Packed Decimal)
      * ================================================
       01 COMP3-NUMBERS.
           05 PACKED-INT          PIC 9(5) COMP-3 VALUE 12345.
           05 PACKED-SIGNED       PIC S9(7) COMP-3 VALUE -123456.
           05 PACKED-DECIMAL      PIC S9(9)V99 COMP-3 VALUE 1234567.89.
       
      * ================================================
      * SECTION 5: USAGE DISPLAY (Default - Character)
      * ================================================
       01 DISPLAY-NUMBERS.
           05 DISPLAY-INT         PIC 9(6) USAGE DISPLAY VALUE 123456.
           05 DISPLAY-SIGNED      PIC S9(5) DISPLAY VALUE -9999.
           05 DISPLAY-DECIMAL     PIC 9(4)V99 DISPLAY VALUE 12.34.
       
      * ================================================
      * SECTION 6: Display-Edited Formats (for output)
      * ================================================
       01 EDITED-FORMATS.
      *    Z = zero suppression
           05 ZERO-SUPPRESS       PIC ZZZ9 VALUE 0.
      *    $ = currency symbol
           05 CURRENCY-SIMPLE     PIC 999.99 VALUE 0.
      *    Comma insertion
           05 WITH-COMMAS         PIC 9,999,999 VALUE 0.
      *    Full currency format
           05 FULL-CURRENCY       PIC $$$$,$$9.99 VALUE 0.
      *    Credit/Debit indicators
           05 WITH-CR             PIC 9999CR VALUE 0.
           05 WITH-DB             PIC 9999DB VALUE 0.
      *    Leading asterisk fill
           05 ASTERISK-FILL       PIC ***9.99 VALUE 0.
      *    Plus/Minus signs
           05 WITH-SIGN           PIC +999.99 VALUE 0.
           05 TRAILING-SIGN       PIC 999.99- VALUE 0.
       
      * ================================================
      * SECTION 7: Mixed Formats for Calculations
      * ================================================
       01 CALCULATION-VARS.
           05 PRICE               PIC 9(5)V99 VALUE 123.45.
           05 QUANTITY            PIC 9(4) COMP VALUE 10.
           05 TAX-RATE            PIC V999 VALUE 0.085.
           05 SUBTOTAL            PIC 9(7)V99 COMP-3.
           05 TAX-AMOUNT          PIC 9(6)V99 COMP-3.
           05 TOTAL               PIC 9(8)V99 COMP-3.
           05 TOTAL-DISPLAY       PIC $$$,$$$,$$9.99.
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "=== PIC FORMAT TEST SUITE ===".
           DISPLAY " ".
           
           PERFORM TEST-BASIC-UNSIGNED.
           PERFORM TEST-SIGNED-NUMBERS.
           PERFORM TEST-COMP-STORAGE.
           PERFORM TEST-COMP3-STORAGE.
           PERFORM TEST-DISPLAY-STORAGE.
           PERFORM TEST-EDITED-FORMATS.
           PERFORM TEST-CALCULATIONS.
           
           DISPLAY " ".
           DISPLAY "=== ALL TESTS COMPLETE ===".
           STOP RUN.
       
      * ================================================
      * Test 1: Basic Unsigned
      * ================================================
       TEST-BASIC-UNSIGNED.
           DISPLAY "Test 1: Basic Unsigned Formats".
           DISPLAY "  Unsigned Int: " UNSIGNED-INT.
           DISPLAY "  Unsigned Decimal: " UNSIGNED-DECIMAL.
           DISPLAY "  String: " STRING-DATA.
           DISPLAY " ".
       
      * ================================================
      * Test 2: Signed Numbers
      * ================================================
       TEST-SIGNED-NUMBERS.
           DISPLAY "Test 2: Signed Formats".
           DISPLAY "  Signed Int: " SIGNED-INT.
           DISPLAY "  Signed Decimal: " SIGNED-DECIMAL.
           DISPLAY "  Signed Large: " SIGNED-LARGE.
           
      *    Test sign operations
           COMPUTE SIGNED-INT = SIGNED-INT * -1.
           DISPLAY "  After multiply by -1: " SIGNED-INT.
           DISPLAY " ".
       
      * ================================================
      * Test 3: COMP Storage
      * ================================================
       TEST-COMP-STORAGE.
           DISPLAY "Test 3: COMP (Binary) Storage".
           DISPLAY "  COMP Int: " COMP-INT.
           DISPLAY "  COMP Signed: " COMP-SIGNED.
           DISPLAY "  COMP Decimal: " COMP-DECIMAL.
           DISPLAY "  COMP Small: " COMP-SMALL.
           
      *    Test COMP arithmetic (should be fast!)
           COMPUTE COMP-INT = COMP-INT + COMP-SMALL.
           DISPLAY "  After addition: " COMP-INT.
           DISPLAY " ".
       
      * ================================================
      * Test 4: COMP-3 Storage
      * ================================================
       TEST-COMP3-STORAGE.
           DISPLAY "Test 4: COMP-3 (Packed) Storage".
           DISPLAY "  Packed Int: " PACKED-INT.
           DISPLAY "  Packed Signed: " PACKED-SIGNED.
           DISPLAY "  Packed Decimal: " PACKED-DECIMAL.
           DISPLAY " ".
       
      * ================================================
      * Test 5: DISPLAY Storage
      * ================================================
       TEST-DISPLAY-STORAGE.
           DISPLAY "Test 5: DISPLAY (Character) Storage".
           DISPLAY "  Display Int: " DISPLAY-INT.
           DISPLAY "  Display Signed: " DISPLAY-SIGNED.
           DISPLAY "  Display Decimal: " DISPLAY-DECIMAL.
           DISPLAY " ".
       
      * ================================================
      * Test 6: Display-Edited Formats
      * ================================================
       TEST-EDITED-FORMATS.
           DISPLAY "Test 6: Display-Edited Formats".
           
      *    Move values to edited fields
           MOVE 7 TO ZERO-SUPPRESS.
           MOVE 123.45 TO CURRENCY-SIMPLE.
           MOVE 1234567 TO WITH-COMMAS.
           MOVE 9876.54 TO FULL-CURRENCY.
           MOVE -1234 TO WITH-CR.
           MOVE 5678 TO WITH-DB.
           MOVE 99.99 TO ASTERISK-FILL.
           MOVE 123.45 TO WITH-SIGN.
           MOVE -67.89 TO TRAILING-SIGN.
           
           DISPLAY "  Zero Suppress: " ZERO-SUPPRESS.
           DISPLAY "  Currency: " CURRENCY-SIMPLE.
           DISPLAY "  With Commas: " WITH-COMMAS.
           DISPLAY "  Full Currency: " FULL-CURRENCY.
           DISPLAY "  Credit (CR): " WITH-CR.
           DISPLAY "  Debit (DB): " WITH-DB.
           DISPLAY "  Asterisk Fill: " ASTERISK-FILL.
           DISPLAY "  With Sign: " WITH-SIGN.
           DISPLAY "  Trailing Sign: " TRAILING-SIGN.
           DISPLAY " ".
       
      * ================================================
      * Test 7: Real Calculations with Mixed Types
      * ================================================
       TEST-CALCULATIONS.
           DISPLAY "Test 7: Calculations with Mixed Types".
           
      *    Calculate subtotal (DISPLAY * COMP)
           COMPUTE SUBTOTAL = PRICE * QUANTITY.
           DISPLAY "  Subtotal: " SUBTOTAL.
           
      *    Calculate tax (uses V999 format)
           COMPUTE TAX-AMOUNT = SUBTOTAL * TAX-RATE.
           DISPLAY "  Tax: " TAX-AMOUNT.
           
      *    Calculate total
           COMPUTE TOTAL = SUBTOTAL + TAX-AMOUNT.
           
      *    Format for display
           MOVE TOTAL TO TOTAL-DISPLAY.
           DISPLAY "  Total (formatted): " TOTAL-DISPLAY.
           DISPLAY " ".