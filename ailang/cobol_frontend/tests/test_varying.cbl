IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-VARYING.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 I PIC 9(2) VALUE 0.
       01 J PIC 9(2) VALUE 0.
       01 SUM-VAL PIC 9(4) VALUE 0.
       
       PROCEDURE DIVISION.
       
      *    Simple counting loop
           DISPLAY "Count 1 to 5:".
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
               DISPLAY I
           END-PERFORM.
           
      *    Count by 2s
           DISPLAY "Count by 2s:".
           PERFORM VARYING J FROM 0 BY 2 UNTIL J >= 10
               DISPLAY J
           END-PERFORM.
           
      *    Accumulator pattern
           DISPLAY "Sum 1 to 10:".
           MOVE 0 TO SUM-VAL.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
               ADD I TO SUM-VAL
           END-PERFORM.
           DISPLAY "Total: ".
           DISPLAY SUM-VAL.
           
           STOP RUN.