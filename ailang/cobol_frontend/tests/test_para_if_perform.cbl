       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 GO-TO-DEEP PIC 9.
       PROCEDURE DIVISION.
       GO--C-F2-5.
           MOVE 3 TO GO-TO-DEEP.
           GO TO GO--D-F2-5.
       GO--D-F2-5.
           IF GO-TO-DEEP EQUAL TO 2
              PERFORM PASS GO TO GO--WRITE-F2-5.
       GO--FAIL-F2-5.
           DISPLAY "FAIL".
       GO--WRITE-F2-5.
           DISPLAY "WRITE".
       PASS.
           DISPLAY "PASS".
