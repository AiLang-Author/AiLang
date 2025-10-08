       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-PERFORM-TIMES.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 COUNTER PIC 9(2) VALUE 0.
       01 I PIC 9(1).
       
       PROCEDURE DIVISION.
           DISPLAY "Test 1: Inline PERFORM TIMES".
           PERFORM 3 TIMES
               ADD 1 TO COUNTER
               DISPLAY "Counter: " COUNTER
           END-PERFORM.
           
           DISPLAY "Test 2: Paragraph PERFORM TIMES".
           PERFORM SHOW-MESSAGE 5 TIMES.
           
           DISPLAY "All tests complete".
           STOP RUN.
       
       SHOW-MESSAGE.
           DISPLAY "Hello from paragraph".




       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-PERFORM-UNTIL.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 COUNTER PIC 9(2) VALUE 0.
       
       PROCEDURE DIVISION.
           DISPLAY "Test 1: Inline PERFORM UNTIL".
           PERFORM UNTIL COUNTER > 3
               ADD 1 TO COUNTER
               DISPLAY "Counter: " COUNTER
           END-PERFORM.
           
           MOVE 0 TO COUNTER.
           DISPLAY "Test 2: Paragraph PERFORM UNTIL".
           PERFORM SHOW-MESSAGE UNTIL COUNTER > 2.
           
           DISPLAY "All tests complete".
           STOP RUN.
       
       SHOW-MESSAGE.
           ADD 1 TO COUNTER.
           DISPLAY "Hello from paragraph: " COUNTER.