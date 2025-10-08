IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-NEW-FEATURES.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUM1 PIC 9(4) VALUE 100.
       01 NUM2 PIC 9(4) VALUE 50.
       01 RESULT PIC 9(4) VALUE 0.
       01 COUNTER PIC 9(2) VALUE 0.
       
       PROCEDURE DIVISION.
       
      *    Test >= operator
           IF NUM1 >= 100
               DISPLAY "NUM1 is >= 100"
           END-IF.
           
      *    Test <= operator  
           IF NUM2 <= 50
               DISPLAY "NUM2 is <= 50"
           END-IF.
           
      *    Test DIVIDE with GIVING clause
           DIVIDE NUM1 BY NUM2 GIVING RESULT.
           DISPLAY "Division result: ".
           DISPLAY RESULT.
           
      *    Test PERFORM...TIMES with inline paragraph
           PERFORM SHOW-MESSAGE 3 TIMES.
           
      *    Test combination of >= with loop
           PERFORM UNTIL COUNTER >= 5
               ADD 1 TO COUNTER
               DISPLAY COUNTER
           END-PERFORM.
           
           STOP RUN.
       
       SHOW-MESSAGE.
           DISPLAY "Hello from PERFORM TIMES!".