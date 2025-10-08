IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MESSAGE PIC X(30) VALUE "Hello from COBOL!".
       01 COUNTER PIC 9(4) VALUE 0.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY MESSAGE.
           MOVE 42 TO COUNTER.
           DISPLAY "The answer is: ".
           DISPLAY COUNTER.
           
           IF COUNTER > 10
               DISPLAY "Counter is high"
           ELSE
               DISPLAY "Counter is low"
           END-IF.
           
           STOP RUN.