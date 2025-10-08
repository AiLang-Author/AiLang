IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-ACCEPT.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 USER-NAME PIC X(20).
       01 USER-AGE PIC 99.
       01 FAVORITE-NUM PIC 9(3).
       
       PROCEDURE DIVISION.
       
      *    Test 1: Accept string input
           DISPLAY "Enter your name: ".
           ACCEPT USER-NAME.
           DISPLAY "Hello, " USER-NAME "!".
           
      *    Test 2: Accept numeric input
           DISPLAY "Enter your age: ".
           ACCEPT USER-AGE.
           DISPLAY "You are " USER-AGE " years old.".
           
      *    Test 3: Use accepted value in calculation
           DISPLAY "Enter your favorite number: ".
           ACCEPT FAVORITE-NUM.
           
           EVALUATE FAVORITE-NUM
               WHEN 7
                   DISPLAY "Lucky number 7!"
               WHEN 13
                   DISPLAY "Unlucky number 13!"
               WHEN OTHER
                   DISPLAY "That's a nice number!"
           END-EVALUATE.
           
           DISPLAY "Count test - enter numbers until 0".
               PERFORM UNTIL USER-AGE = 0
                   DISPLAY "Enter a number (0 to quit): "
                   ACCEPT USER-AGE
            IF USER-AGE > 0
                   DISPLAY "You entered: " USER-AGE
           END-IF
           END-PERFORM.