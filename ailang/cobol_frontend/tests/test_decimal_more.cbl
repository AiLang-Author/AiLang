************************************************** 
      * PIC Format Comprehensive Test Suite
      * Tests all major PIC variations:
      * - Signed/Unsigned
      * - COMP/COMP-3/DISPLAY storage
      * - Display-edited formats
      * - Decimal precision
      **************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PIC-FORMATS-TEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      * ================================================
      * SECTION 1: Basic Unsigned Formats
      * ================================================
       01 BASIC-UNSIGNED.
           05 UNSIGNED-INT        PIC 9(4) VALUE 1234.
           05 UNSIGNED-DECIMAL    PIC 9(5)V99 VALUE 12345.67.
           05 STRING-DATA         PIC X(20) VALUE "HELLO WORLD".
       
      * ================================================
      * SECTION 2: Signed Formats (S prefix)
      * ================================================
       01 SIGNED-NUMBERS.
           05 SIGNED-INT          PIC S9(4) VALUE -100.
           05 SIGNED-DECIMAL      PIC S9(7)V99 VALUE -1234.56.
           05 SIGNED-LARGE        PIC S9(9)V99 VALUE 12345678.90.
       
      * ================================================
      * SECTION 3: USAGE COMP (Binary Storage)
      * ================================================
       01 COMP-NUMBERS.
           05 COMP-INT            PIC 9(4) USAGE COMP VALUE 500.
           05 COMP-SIGNED         PIC S9(6) COMP VALUE -12345.
           05 COMP-DECIMAL        PIC S9(7)V99 COMP VALUE 999.99.
           05 COMP-SMALL          PIC 99 COMP VALUE 42.
       
      * ================================================
      * SECTION 4: USAGE COMP-3 (Packed Decimal)
      * ================================================
       01 COMP3-NUMBERS.
           05 PACKED-INT          PIC 9(5) COMP-3 VALUE 12345.
           05 PACKED-SIGNED       PIC S9(7) COMP-3 VALUE -123456.
           05 PACKED-DECIMAL      PIC S9(9)V99 COMP-3 VALUE 1234567.89.
       
      * ================================================
      * SECTION 5: USAGE DISPLAY (Default - Character)
      * ================================================
       01 DISPLAY-NUMBERS.
           05 DISPLAY-INT         PIC 9(6) USAGE DISPLAY VALUE 123456.
           05 DISPLAY-SIGNED      PIC S9(5) DISPLAY VALUE -9999.
           05 DISPLAY-DECIMAL     PIC 9(4)V99 DISPLAY VALUE 12.34.
       
      * ================================================
      * SECTION 6: Display-Edited Formats (for output)
      * ================================================
       01 EDITED-FORMATS.
      *    Z = zero suppression
           05 ZERO-SUPPRESS       PIC ZZZ9 VALUE 0.
      *    $ = currency symbol
           05 CURRENCY-SIMPLE     PIC 999.99 VALUE 0.
      *    Comma insertion
           05 WITH-COMMAS         PIC 9,999,999 VALUE 0.
      *    Full currency format
           05 FULL-CURRENCY       PIC $$$$,$$9.99 VALUE 0.
      *    Credit/Debit indicators
           05 WITH-CR             PIC 9999CR VALUE 0.
           05 WITH-DB             PIC 9999DB VALUE 0.
      *    Leading asterisk fill
           05 ASTERISK-FILL       PIC ***9.99 VALUE 0.
      *    Plus/Minus signs
           05 WITH-SIGN           PIC +999.99 VALUE 0.
           05 TRAILING-SIGN       PIC 999.99- VALUE 0.
       
      * ================================================
      * SECTION 7: Mixed Formats for Calculations
      * ================================================
       01 CALCULATION-VARS.
           05 PRICE               PIC 9(5)V99 VALUE 123.45.
           05 QUANTITY            PIC 9(4) COMP VALUE 10.
           05 TAX-RATE            PIC V999 VALUE 0.085.
           05 SUBTOTAL            PIC 9(7)V99 COMP-3.
           05 TAX-AMOUNT          PIC 9(6)V99 COMP-3.
           05 TOTAL               PIC 9(8)V99 COMP-3.
           05 TOTAL-DISPLAY       PIC $$$,$$$,$$9.99.
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "=== PIC FORMAT TEST SUITE ===".
           DISPLAY " ".
           
           PERFORM TEST-BASIC-UNSIGNED.
           PERFORM TEST-SIGNED-NUMBERS.
           PERFORM TEST-COMP-STORAGE.
           PERFORM TEST-COMP3-STORAGE.
           PERFORM TEST-DISPLAY-STORAGE.
           PERFORM TEST-EDITED-FORMATS.
           PERFORM TEST-CALCULATIONS.
           
           DISPLAY " ".
           DISPLAY "=== ALL TESTS COMPLETE ===".
           STOP RUN.
       
      * ================================================
      * Test 1: Basic Unsigned
      * ================================================
       TEST-BASIC-UNSIGNED.
           DISPLAY "Test 1: Basic Unsigned Formats".
           DISPLAY "  Unsigned Int: " UNSIGNED-INT.
           DISPLAY "  Unsigned Decimal: " UNSIGNED-DECIMAL.
           DISPLAY "  String: " STRING-DATA.
           DISPLAY " ".
       
      * ================================================
      * Test 2: Signed Numbers
      * ================================================
       TEST-SIGNED-NUMBERS.
           DISPLAY "Test 2: Signed Formats".
           DISPLAY "  Signed Int: " SIGNED-INT.
           DISPLAY "  Signed Decimal: " SIGNED-DECIMAL.
           DISPLAY "  Signed Large: " SIGNED-LARGE.
           
      *    Test sign operations
           COMPUTE SIGNED-INT = SIGNED-INT * -1.
           DISPLAY "  After multiply by -1: " SIGNED-INT.
           DISPLAY " ".
       
      * ================================================
      * Test 3: COMP Storage
      * ================================================
       TEST-COMP-STORAGE.
           DISPLAY "Test 3: COMP (Binary) Storage".
           DISPLAY "  COMP Int: " COMP-INT.
           DISPLAY "  COMP Signed: " COMP-SIGNED.
           DISPLAY "  COMP Decimal: " COMP-DECIMAL.
           DISPLAY "  COMP Small: " COMP-SMALL.
           
      *    Test COMP arithmetic (should be fast!)
           COMPUTE COMP-INT = COMP-INT + COMP-SMALL.
           DISPLAY "  After addition: " COMP-INT.
           DISPLAY " ".
       
      * ================================================
      * Test 4: COMP-3 Storage
      * ================================================
       TEST-COMP3-STORAGE.
           DISPLAY "Test 4: COMP-3 (Packed) Storage".
           DISPLAY "  Packed Int: " PACKED-INT.
           DISPLAY "  Packed Signed: " PACKED-SIGNED.
           DISPLAY "  Packed Decimal: " PACKED-DECIMAL.
           DISPLAY " ".
       
      * ================================================
      * Test 5: DISPLAY Storage
      * ================================================
       TEST-DISPLAY-STORAGE.
           DISPLAY "Test 5: DISPLAY (Character) Storage".
           DISPLAY "  Display Int: " DISPLAY-INT.
           DISPLAY "  Display Signed: " DISPLAY-SIGNED.
           DISPLAY "  Display Decimal: " DISPLAY-DECIMAL.
           DISPLAY " ".
       
      * ================================================
      * Test 6: Display-Edited Formats
      * ================================================
       TEST-EDITED-FORMATS.
           DISPLAY "Test 6: Display-Edited Formats".
           
      *    Move values to edited fields
           MOVE 7 TO ZERO-SUPPRESS.
           MOVE 123.45 TO CURRENCY-SIMPLE.
           MOVE 1234567 TO WITH-COMMAS.
           MOVE 9876.54 TO FULL-CURRENCY.
           MOVE -1234 TO WITH-CR.
           MOVE 5678 TO WITH-DB.
           MOVE 99.99 TO ASTERISK-FILL.
           MOVE 123.45 TO WITH-SIGN.
           MOVE -67.89 TO TRAILING-SIGN.
           
           DISPLAY "  Zero Suppress: " ZERO-SUPPRESS.
           DISPLAY "  Currency: " CURRENCY-SIMPLE.
           DISPLAY "  With Commas: " WITH-COMMAS.
           DISPLAY "  Full Currency: " FULL-CURRENCY.
           DISPLAY "  Credit (CR): " WITH-CR.
           DISPLAY "  Debit (DB): " WITH-DB.
           DISPLAY "  Asterisk Fill: " ASTERISK-FILL.
           DISPLAY "  With Sign: " WITH-SIGN.
           DISPLAY "  Trailing Sign: " TRAILING-SIGN.
           DISPLAY " ".
       
      * ================================================
      * Test 7: Real Calculations with Mixed Types
      * ================================================
       TEST-CALCULATIONS.
           DISPLAY "Test 7: Calculations with Mixed Types".
           
      *    Calculate subtotal (DISPLAY * COMP)
           COMPUTE SUBTOTAL = PRICE * QUANTITY.
           DISPLAY "  Subtotal: " SUBTOTAL.
           
      *    Calculate tax (uses V999 format)
           COMPUTE TAX-AMOUNT = SUBTOTAL * TAX-RATE.
           DISPLAY "  Tax: " TAX-AMOUNT.
           
      *    Calculate total
           COMPUTE TOTAL = SUBTOTAL + TAX-AMOUNT.
           
      *    Format for display
           MOVE TOTAL TO TOTAL-DISPLAY.
           DISPLAY "  Total (formatted): " TOTAL-DISPLAY.
           DISPLAY " ".