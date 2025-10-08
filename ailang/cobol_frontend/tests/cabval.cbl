      **********************************************************
      * COBVALU                                                *
      *                                                        *
      * A simple subprogram that calculates present value      *
      * for a series of cash flows.                            *
      *                                                        *
      **********************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBVALU.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CHAR-DATA.
           05  INPUT-1           PIC X(10).
           05  PAYMENT-OUT       PIC $$$$,$$$,$$9.99 USAGE DISPLAY.
           05  INTEREST-IN       PIC X(5).
           05  NO-OF-PERIODS-IN  PIC X(3).
           05  INPUT-BUFFER      PIC X(10) VALUE "5069837544".
           05  BUFFER-ARRAY   REDEFINES INPUT-BUFFER
                              OCCURS 5 TIMES
                                 PIC XX.
           05  OUTPUT-LINE       PIC X(79).
       01 NUM-DATA.
           05  PAYMENT           PIC S9(9)V99 USAGE COMP.
           05  INTEREST          PIC S9(3)V99 USAGE COMP.
           05  COUNTER           PIC 99 USAGE COMP.
           05  NO-OF-PERIODS     PIC 99 USAGE COMP.
           05  VALUE-AMOUNT   OCCURS 99 PIC S9(7)V99 COMP.
       LINKAGE SECTION.
       01  PARM-1.
           05  CALL-FEEDBACK  PIC XX.
       PROCEDURE DIVISION USING PARM-1.
           MOVE "NO" TO CALL-FEEDBACK.
           MOVE ".12 5 " TO INPUT-1.
           UNSTRING INPUT-1 DELIMITED BY "," OR ALL " "                VALU1 
            INTO INTEREST-IN NO-OF-PERIODS-IN.
      * Convert to numeric values
           COMPUTE INTEREST = FUNCTION NUMVAL(INTEREST-IN).            VALU2 
           COMPUTE NO-OF-PERIODS = FUNCTION NUMVAL(NO-OF-PERIODS-IN).
      * Get cash flows
           PERFORM GET-AMOUNTS VARYING COUNTER FROM 1 BY 1 UNTIL
             COUNTER IS GREATER THAN NO-OF-PERIODS.
      * Calculate present value
           COMPUTE PAYMENT =
               FUNCTION PRESENT-VALUE(INTEREST VALUE-AMOUNT(ALL) ).    VALU3 
      * Make it presentable
           MOVE PAYMENT TO PAYMENT-OUT.
           STRING "COBVALU:_Present_value_for_rate_of_"
                  INTEREST-IN "_given_amounts_"
                  BUFFER-ARRAY (1) ",_"
                  BUFFER-ARRAY (2) ",_"
                  BUFFER-ARRAY (3) ",_"
                  BUFFER-ARRAY (4) ",_"
                  BUFFER-ARRAY (5) "_is:_"
               DELIMITED BY SPACES
               INTO OUTPUT-LINE.
           INSPECT OUTPUT-LINE REPLACING ALL "_" BY SPACES.
           DISPLAY OUTPUT-LINE PAYMENT-OUT.
           MOVE "OK" TO CALL-FEEDBACK.
           GOBACK.
      *
      * Get cash flows for each period
      *
       GET-AMOUNTS.
           MOVE BUFFER-ARRAY (COUNTER) TO INPUT-1.
           COMPUTE VALUE-AMOUNT (COUNTER) = FUNCTION NUMVAL(INPUT-1).