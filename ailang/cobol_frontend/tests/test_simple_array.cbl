       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-ARRAY.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUMBERS OCCURS 5 PIC 9(2) VALUE 1.
       
       PROCEDURE DIVISION.
           DISPLAY "First element:".
           DISPLAY NUMBERS(1).
           DISPLAY "Second element:".
           DISPLAY NUMBERS(2).
           STOP RUN.
