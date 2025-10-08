IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-EVALUATE.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 DAY-NUM PIC 9 VALUE 3.
       01 GRADE PIC 99 VALUE 85.
       01 STATUS-CODE PIC 9 VALUE 1.
       
       PROCEDURE DIVISION.
       
      *    Test 1: Simple EVALUATE with numbers
           DISPLAY "Day of week test:".
           EVALUATE DAY-NUM
               WHEN 1
                   DISPLAY "Monday"
               WHEN 2
                   DISPLAY "Tuesday"
               WHEN 3
                   DISPLAY "Wednesday"
               WHEN 4
                   DISPLAY "Thursday"
               WHEN 5
                   DISPLAY "Friday"
               WHEN OTHER
                   DISPLAY "Weekend"
           END-EVALUATE.
           
      *    Test 2: EVALUATE with grade ranges (using simple values)
           DISPLAY "Grade test:".
           EVALUATE GRADE
               WHEN 90
                   DISPLAY "A - Excellent"
               WHEN 85
                   DISPLAY "B - Good"
               WHEN 75
                   DISPLAY "C - Average"
               WHEN OTHER
                   DISPLAY "Need improvement"
           END-EVALUATE.
           
      *    Test 3: EVALUATE in a loop
           DISPLAY "Status codes:".
           PERFORM VARYING STATUS-CODE FROM 1 BY 1 UNTIL STATUS-CODE > 4
               EVALUATE STATUS-CODE
                   WHEN 1
                       DISPLAY "1: Pending"
                   WHEN 2
                       DISPLAY "2: In Progress"
                   WHEN 3
                       DISPLAY "3: Complete"
                   WHEN OTHER
                       DISPLAY "Unknown status"
               END-EVALUATE
           END-PERFORM.
           
           STOP RUN.