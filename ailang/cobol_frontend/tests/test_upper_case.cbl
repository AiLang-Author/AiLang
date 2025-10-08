IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-UPPER.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 INPUT-TEXT PIC X(20) VALUE "hello world".
       01 UPPER-TEXT PIC X(20).
       01 TEST-VAR PIC X(10) VALUE "MixedCase".
       
       PROCEDURE DIVISION.
       
      *    Test 1: Simple UPPER-CASE function
           DISPLAY "Test 1: Basic UPPER-CASE".
           MOVE FUNCTION UPPER-CASE(INPUT-TEXT) TO UPPER-TEXT.
           DISPLAY "Input: " INPUT-TEXT.
           DISPLAY "Upper: " UPPER-TEXT.
           
      *    Test 2: UPPER-CASE in EVALUATE
           DISPLAY "Test 2: UPPER-CASE in EVALUATE".
           EVALUATE FUNCTION UPPER-CASE(TEST-VAR)
               WHEN "MIXEDCASE"
                   DISPLAY "Matched MIXEDCASE"
               WHEN OTHER
                   DISPLAY "No match"
           END-EVALUATE.
           
      *    Test 3: Direct display
           DISPLAY "Test 3: Direct display".
           DISPLAY "Original: " TEST-VAR.
           DISPLAY "Uppercase: " FUNCTION UPPER-CASE(TEST-VAR).
           
           STOP RUN.