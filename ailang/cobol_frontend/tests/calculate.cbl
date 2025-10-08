IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATE-SUM.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUM1 PIC 9(4) VALUE 100.
       01 NUM2 PIC 9(4) VALUE 250.
       01 RESULT PIC 9(5) VALUE 0.
       01 ITERATIONS PIC 9(2) VALUE 0.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "Starting calculations...".
           
           COMPUTE RESULT = NUM1 + NUM2.
           DISPLAY "Sum result: ".
           DISPLAY RESULT.
           
           IF RESULT > 300
               DISPLAY "This is a large sum"
           ELSE
               DISPLAY "This is a small sum"
           END-IF.
           
           MOVE 0 TO ITERATIONS.
           PERFORM COUNT-TO-FIVE.
           
           DISPLAY "All done!".
           STOP RUN.
       
       COUNT-TO-FIVE.
           PERFORM UNTIL ITERATIONS = 5
               ADD 1 TO ITERATIONS
               DISPLAY "Iteration: "
               DISPLAY ITERATIONS
           END-PERFORM.