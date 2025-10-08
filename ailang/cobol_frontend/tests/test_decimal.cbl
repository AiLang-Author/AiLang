IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-DECIMAL.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PRICE    PIC 9(5)V9(2) VALUE 123.45.
       01 TAX-RATE PIC 9(1)V9(2) VALUE 0.08.
       01 TAX-AMT  PIC 9(4)V9(2).
       01 TOTAL    PIC 9(6)V9(2).
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "Testing COBOL Decimal Arithmetic".
           
           DISPLAY "Price: " PRICE.
           DISPLAY "Tax Rate: " TAX-RATE.
           
      *    Calculate tax: 123.45 * 0.08 = 9.88 (rounded)
           COMPUTE TAX-AMT = PRICE * TAX-RATE.
           DISPLAY "Tax Amount: " TAX-AMT.
           
      *    Calculate total: 123.45 + 9.88 = 133.33
           COMPUTE TOTAL = PRICE + TAX-AMT.
           DISPLAY "Total: " TOTAL.
           
      *    Test subtraction
           COMPUTE TOTAL = TOTAL - TAX-AMT.
           DISPLAY "Total minus tax: " TOTAL.
           
           STOP RUN.