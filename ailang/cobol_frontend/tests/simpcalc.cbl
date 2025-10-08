IDENTIFICATION DIVISION.
       PROGRAM-ID. SIMPLE-CALC.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUM1 PIC 9(4).
       01 NUM2 PIC 9(4).
       01 RESULT PIC 9(5).
       01 OPERATION PIC X.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "Enter first number: ".
           ACCEPT NUM1.
           DISPLAY "Enter operation (+, -, *, /): ".
           ACCEPT OPERATION.
           DISPLAY "Enter second number: ".
           ACCEPT NUM2.
           
           EVALUATE OPERATION
               WHEN "+"
                   ADD NUM1 TO NUM2 GIVING RESULT
                   DISPLAY "Result: " RESULT
               WHEN "-"
                   SUBTRACT NUM2 FROM NUM1 GIVING RESULT
                   DISPLAY "Result: " RESULT
               WHEN "*"
                   MULTIPLY NUM1 BY NUM2 GIVING RESULT
                   DISPLAY "Result: " RESULT
               WHEN "/"
                   DIVIDE NUM1 BY NUM2 GIVING RESULT
                   DISPLAY "Result: " RESULT
               WHEN OTHER
                   DISPLAY "Invalid operation"
           END-EVALUATE.
           
           STOP RUN.