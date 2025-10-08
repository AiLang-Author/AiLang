IDENTIFICATION DIVISION.
       PROGRAM-ID. OUTER-PROG.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 OUTER-VAR PIC 9(2) VALUE 10.
       
       PROCEDURE DIVISION.
           DISPLAY "OUTER: Start".
           DISPLAY "OUTER-VAR: " OUTER-VAR.
           
           IDENTIFICATION DIVISION.
           PROGRAM-ID. INNER-PROG.
           
           DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 INNER-VAR PIC 9(2) VALUE 20.
           
           PROCEDURE DIVISION.
               DISPLAY "INNER: Start".
               DISPLAY "INNER-VAR: " INNER-VAR.
               STOP RUN.
           END PROGRAM INNER-PROG.
           
           DISPLAY "OUTER: After nested program".
       END PROGRAM OUTER-PROG.


       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MULTI-NESTED.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MAIN-VAR PIC 9(2) VALUE 5.
       
       PROCEDURE DIVISION.
           DISPLAY "MULTI: Main start".
           
           IDENTIFICATION DIVISION.
           PROGRAM-ID. CHILD1.
           
           PROCEDURE DIVISION.
               DISPLAY "CHILD1: Executing".
               STOP RUN.
           END PROGRAM CHILD1.
           
           IDENTIFICATION DIVISION.
           PROGRAM-ID. CHILD2.
           
           PROCEDURE DIVISION.
               DISPLAY "CHILD2: Executing".
               STOP RUN.
           END PROGRAM CHILD2.
           
           DISPLAY "MULTI: Main end".
       END PROGRAM MULTI-NESTED.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEVEL1.
       
       PROCEDURE DIVISION.
           DISPLAY "LEVEL1: Start".
           
           IDENTIFICATION DIVISION.
           PROGRAM-ID. LEVEL2.
           
           PROCEDURE DIVISION.
               DISPLAY "LEVEL2: Start".
               
               IDENTIFICATION DIVISION.
               PROGRAM-ID. LEVEL3.
               
               PROCEDURE DIVISION.
                   DISPLAY "LEVEL3: Deepest level".
                   STOP RUN.
               END PROGRAM LEVEL3.
               
               DISPLAY "LEVEL2: After level 3".
               STOP RUN.
           END PROGRAM LEVEL2.
           
           DISPLAY "LEVEL1: After level 2".
           STOP RUN.
       END PROGRAM LEVEL1.