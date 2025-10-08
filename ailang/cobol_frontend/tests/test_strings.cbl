IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-STRINGS.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 FIRST-NAME PIC X(10) VALUE "John".
       01 LAST-NAME PIC X(10) VALUE "Doe".
       01 FULL-NAME PIC X(30).
       01 EMAIL PIC X(50) VALUE "user@example.com".
       01 EMAIL-PARTS.
          05 USERNAME PIC X(20).
          05 DOMAIN PIC X(30).
       01 PARTS-COUNT PIC 99.
       01 DATA-FIELD PIC X(20) VALUE "Hello World".
       01 X-COUNT PIC 99.
       01 TEST-NUM PIC 99.
       
       PROCEDURE DIVISION.
       
      *    Test 1: STRING concatenation
           DISPLAY "Test 1: STRING concatenation".
           STRING FIRST-NAME " " LAST-NAME 
               DELIMITED BY SIZE 
               INTO FULL-NAME.
           DISPLAY "Full name: " FULL-NAME.
           
      *    Test 2: UNSTRING splitting
           DISPLAY "Test 2: UNSTRING splitting".
           UNSTRING EMAIL DELIMITED BY "@"
               INTO USERNAME DOMAIN
               TALLYING IN PARTS-COUNT.
           DISPLAY "Username: " USERNAME.
           DISPLAY "Domain: " DOMAIN.
           DISPLAY "Parts count: " PARTS-COUNT.
           
      *    Test 3: INSPECT REPLACING
           DISPLAY "Test 3: INSPECT REPLACING".
           INSPECT DATA-FIELD REPLACING ALL "o" BY "0".
           DISPLAY "Modified: " DATA-FIELD.
           
      *    Test 4: INSPECT TALLYING
           DISPLAY "Test 4: INSPECT TALLYING".
           MOVE "xxxYxxxYxxx" TO DATA-FIELD.
           INSPECT DATA-FIELD TALLYING X-COUNT FOR ALL "x".
           DISPLAY "Count of 'x': " X-COUNT.
           
      *    Test 5: Symbol operators < and >
           DISPLAY "Test 5: Symbol operators".
           MOVE 10 TO TEST-NUM.
           IF TEST-NUM > 5
               DISPLAY "10 > 5 is TRUE"
           END-IF.
           
           IF TEST-NUM < 20
               DISPLAY "10 < 20 is TRUE"
           END-IF.
           
           STOP RUN.