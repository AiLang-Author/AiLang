      **********************************************************
      * COBLOAN                                                *
      *                                                        *
      * A simple subprogram that calculates payment amount     *
      * for a loan.                                            *
      *                                                        *
      **********************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBLOAN.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FIELDS.
           05  INPUT-1           PIC X(26).
           05  PAYMENT           PIC S9(9)V99 USAGE COMP.
           05  PAYMENT-OUT       PIC $$$$,$$$,$$9.99 USAGE DISPLAY.
           05  LOAN-AMOUNT       PIC S9(7)V99 USAGE COMP.
           05  LOAN-AMOUNT-IN    PIC X(16).
           05  INTEREST-IN       PIC X(5).
           05  INTEREST          PIC S9(3)V99 USAGE COMP.
           05  NO-OF-PERIODS-IN  PIC X(3).
           05  NO-OF-PERIODS     PIC 99 USAGE COMP.
           05  OUTPUT-LINE       PIC X(79).
       LINKAGE SECTION.
       01  PARM-1.
           05  CALL-FEEDBACK     PIC XX.
       PROCEDURE DIVISION USING PARM-1.
           MOVE "NO" TO CALL-FEEDBACK.
           MOVE "30000 .09 24 " TO INPUT-1.
           UNSTRING INPUT-1 DELIMITED BY ALL " "
             INTO LOAN-AMOUNT-IN INTEREST-IN NO-OF-PERIODS-IN.
      * Convert to numeric values
           COMPUTE LOAN-AMOUNT = FUNCTION NUMVAL(LOAN-AMOUNT-IN).
           COMPUTE INTEREST = FUNCTION NUMVAL(INTEREST-IN).
           COMPUTE NO-OF-PERIODS = FUNCTION NUMVAL(NO-OF-PERIODS-IN).
      * Calculate annuity amount required
           COMPUTE PAYMENT = LOAN-AMOUNT *
               FUNCTION ANNUITY((INTEREST / 12 ) NO-OF-PERIODS).
      * Make it presentable
           MOVE SPACES TO OUTPUT-LINE
           MOVE PAYMENT TO PAYMENT-OUT.
           STRING "COBLOAN:_Repayment_amount_for_a_" NO-OF-PERIODS-IN
                   "_month_loan_of_" LOAN-AMOUNT-IN
                   "_at_" INTEREST-IN "_interest_is:_"
               DELIMITED BY SPACES
               INTO OUTPUT-LINE.
           INSPECT OUTPUT-LINE REPLACING ALL "_" BY SPACES.
           DISPLAY OUTPUT-LINE PAYMENT-OUT.
           MOVE "OK" TO CALL-FEEDBACK.
           GOBACK.