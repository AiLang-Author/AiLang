IDENTIFICATION DIVISION.
       PROGRAM-ID. COMPLEX-TEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TOTAL PIC 9(5) VALUE 0.
       01 COUNTER PIC 9(3) VALUE 1.
       01 LIMIT PIC 9(3) VALUE 10.
       01 FACTORIAL PIC 9(8) VALUE 1.
       01 NUM PIC 9(2) VALUE 5.
       01 RESULT PIC 9(5) VALUE 0.
       01 TEMP PIC 9(3) VALUE 0.
       
       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "=== Complex COBOL Test ===".
           
           DISPLAY "Test 1: Sum of numbers 1 to 10".
           PERFORM CALCULATE-SUM.
           DISPLAY "Total: ".
           DISPLAY TOTAL.
           
           DISPLAY "Test 2: Factorial of 5".
           PERFORM CALCULATE-FACTORIAL.
           DISPLAY "5! = ".
           DISPLAY FACTORIAL.
           
           DISPLAY "Test 3: Nested conditionals".
           PERFORM TEST-CONDITIONALS.
           
           DISPLAY "Test 4: Multiple arithmetic operations".
           COMPUTE RESULT = NUM * 2 + 10.
           DISPLAY "Result of (5 * 2 + 10) = ".
           DISPLAY RESULT.
           
           DISPLAY "=== All tests complete ===".
           STOP RUN.
       
       CALCULATE-SUM.
           MOVE 0 TO TOTAL.
           MOVE 1 TO COUNTER.
           PERFORM UNTIL COUNTER > LIMIT
               ADD COUNTER TO TOTAL
               ADD 1 TO COUNTER
           END-PERFORM.
       
       CALCULATE-FACTORIAL.
           MOVE 1 TO FACTORIAL.
           MOVE 1 TO TEMP.
           PERFORM UNTIL TEMP > NUM
               MULTIPLY TEMP BY FACTORIAL
               ADD 1 TO TEMP
           END-PERFORM.
       
       TEST-CONDITIONALS.
           MOVE 15 TO TEMP.
           IF TEMP > 20
               DISPLAY "Temp is greater than 20"
           ELSE
               IF TEMP > 10
                   DISPLAY "Temp is between 11 and 20"
               ELSE
                   DISPLAY "Temp is 10 or less"
               END-IF
           END-IF.