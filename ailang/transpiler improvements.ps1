LinkagePool Transpiler Target Patterns
Here are concrete before/after patterns for the transpiler to target when handling LinkagePools:
Pattern 1: Basic COBOL Data Movement
COBOL Input:
cobolLINKAGE SECTION.
01  PATIENT-RECORD.
    05  PAT-ID        PIC X(10).
    05  PAT-HEIGHT    PIC 9(3)V9(2).
    05  PAT-WEIGHT    PIC 9(3)V9(2).
    05  PAT-AGE       PIC 999.

PROCEDURE DIVISION USING PATIENT-RECORD.
    MOVE 175.50 TO PAT-HEIGHT.
    MOVE 80.25  TO PAT-WEIGHT.
    COMPUTE PAT-AGE = CURRENT-YEAR - BIRTH-YEAR.
Target AILANG Output:
ailangLinkagePool.PATIENT_RECORD {
    "PAT_ID": Initialize=""
    "PAT_HEIGHT": Initialize=0
    "PAT_WEIGHT": Initialize=0
    "PAT_AGE": Initialize=0
}

SubRoutine.ProcessPatient {
    Input: patient_rec: LinkagePool.PATIENT_RECORD
    Body: {
        // Direct field access - no allocation needed for input
        patient_rec.PAT_HEIGHT = 17550  // Store as integer (implied decimal)
        patient_rec.PAT_WEIGHT = 8025   
        patient_rec.PAT_AGE = Subtract(CURRENT_YEAR, BIRTH_YEAR)
        
        ReturnValue(0)
    }
}
Pattern 2: Multiple Linkage Sections
COBOL Input:
cobolLINKAGE SECTION.
01  BILL-DATA.
    05  B-PATIENT-ID   PIC X(10).
    05  B-AMOUNT       PIC 9(7)V99.
    
01  PAYMENT-DATA.
    05  P-PROVIDER     PIC X(6).
    05  P-RATE         PIC 9(5)V99.

PROCEDURE DIVISION USING BILL-DATA PAYMENT-DATA.
    COMPUTE B-AMOUNT = B-AMOUNT * P-RATE.
Target AILANG Output:
ailangLinkagePool.BILL_DATA {
    "B_PATIENT_ID": Initialize=""
    "B_AMOUNT": Initialize=0
}

LinkagePool.PAYMENT_DATA {
    "P_PROVIDER": Initialize=""
    "P_RATE": Initialize=0
}

SubRoutine.CalculatePayment {
    Input: bill: LinkagePool.BILL_DATA
    Input: payment: LinkagePool.PAYMENT_DATA
    Body: {
        // Both linkage pools passed as parameters
        bill.B_AMOUNT = FixedPoint.Multiply(bill.B_AMOUNT, payment.P_RATE)
        
        ReturnValue(0)
    }
}
Pattern 3: Nested Group Items
COBOL Input:
cobolLINKAGE SECTION.
01  CLAIM-RECORD.
    05  CLAIM-HEADER.
        10  CLAIM-ID      PIC X(15).
        10  CLAIM-DATE    PIC 9(8).
    05  CLAIM-DETAILS.
        10  DIAGNOSIS     PIC X(10).
        10  PROCEDURE-CD  PIC X(5).
        10  AMOUNT        PIC 9(7)V99.
Target AILANG Output (Flattened):
ailangLinkagePool.CLAIM_RECORD {
    // Flatten nested structures with prefixes
    "CLAIM_HEADER_CLAIM_ID": Initialize=""
    "CLAIM_HEADER_CLAIM_DATE": Initialize=0
    "CLAIM_DETAILS_DIAGNOSIS": Initialize=""
    "CLAIM_DETAILS_PROCEDURE_CD": Initialize=""
    "CLAIM_DETAILS_AMOUNT": Initialize=0
}

// Or use shortened names if unambiguous
LinkagePool.CLAIM_RECORD_SHORT {
    "CLAIM_ID": Initialize=""
    "CLAIM_DATE": Initialize=0
    "DIAGNOSIS": Initialize=""
    "PROCEDURE_CD": Initialize=""
    "AMOUNT": Initialize=0
}
Pattern 4: Working Storage to LinkagePool Interaction
COBOL Input:
cobolWORKING-STORAGE SECTION.
01  WS-CALC-FIELDS.
    05  WS-TEMP-AMT    PIC 9(9)V99.
    05  WS-MULTIPLIER  PIC 9V9999.

LINKAGE SECTION.
01  RESULT-DATA.
    05  FINAL-AMOUNT   PIC 9(9)V99.

PROCEDURE DIVISION USING RESULT-DATA.
    COMPUTE WS-TEMP-AMT = 1000.00 * WS-MULTIPLIER.
    MOVE WS-TEMP-AMT TO FINAL-AMOUNT.
Target AILANG Output:
ailangFixedPool.WS_CALC_FIELDS {
    "WS_TEMP_AMT": Initialize=0
    "WS_MULTIPLIER": Initialize=10000  // 1.0000 as integer
}

LinkagePool.RESULT_DATA {
    "FINAL_AMOUNT": Initialize=0
}

SubRoutine.Calculate {
    Input: result: LinkagePool.RESULT_DATA
    Body: {
        // Working storage is module-level, linkage is parameter
        WS_CALC_FIELDS.WS_TEMP_AMT = FixedPoint.Multiply(100000, WS_CALC_FIELDS.WS_MULTIPLIER)
        
        // Move from working storage to linkage
        result.FINAL_AMOUNT = WS_CALC_FIELDS.WS_TEMP_AMT
        
        ReturnValue(0)
    }
}
Pattern 5: Conditional Linkage Access
COBOL Input:
cobolLINKAGE SECTION.
01  OPTIONAL-DATA.
    05  OPT-FLAG       PIC X.
    05  OPT-VALUE      PIC 9(5).

PROCEDURE DIVISION USING OPTIONAL-DATA.
    IF OPT-FLAG = 'Y'
        ADD 100 TO OPT-VALUE
    END-IF.
Target AILANG Output:
ailangLinkagePool.OPTIONAL_DATA {
    "OPT_FLAG": Initialize=""
    "OPT_VALUE": Initialize=0
}

SubRoutine.ProcessOptional {
    Input: opt_data: LinkagePool.OPTIONAL_DATA
    Body: {
        IfCondition EqualTo(opt_data.OPT_FLAG, "Y") ThenBlock: {
            opt_data.OPT_VALUE = Add(opt_data.OPT_VALUE, 100)
        }
        
        ReturnValue(0)
    }
}
Pattern 6: Return Values via Linkage
COBOL Input:
cobolLINKAGE SECTION.
01  INPUT-PARMS.
    05  IN-VALUE-1     PIC 9(5).
    05  IN-VALUE-2     PIC 9(5).
01  OUTPUT-PARMS.
    05  OUT-SUM        PIC 9(6).
    05  OUT-PRODUCT   PIC 9(10).

PROCEDURE DIVISION USING INPUT-PARMS OUTPUT-PARMS.
    COMPUTE OUT-SUM = IN-VALUE-1 + IN-VALUE-2.
    COMPUTE OUT-PRODUCT = IN-VALUE-1 * IN-VALUE-2.
Target AILANG Output:
ailangLinkagePool.INPUT_PARMS {
    "IN_VALUE_1": Initialize=0
    "IN_VALUE_2": Initialize=0
}

LinkagePool.OUTPUT_PARMS {
    "OUT_SUM": Initialize=0
    "OUT_PRODUCT": Initialize=0
}

SubRoutine.Calculate {
    Input: inputs: LinkagePool.INPUT_PARMS
    Input: outputs: LinkagePool.OUTPUT_PARMS
    Body: {
        // Both input and output through linkage
        outputs.OUT_SUM = Add(inputs.IN_VALUE_1, inputs.IN_VALUE_2)
        outputs.OUT_PRODUCT = Multiply(inputs.IN_VALUE_1, inputs.IN_VALUE_2)
        
        ReturnValue(0)
    }
}
Pattern 7: Array/Table Handling in Linkage
COBOL Input:
cobolLINKAGE SECTION.
01  RATE-TABLE.
    05  RATE-ENTRY OCCURS 10 TIMES.
        10  RATE-CODE   PIC X(3).
        10  RATE-AMOUNT PIC 9(5)V99.
Target AILANG Output:
ailang// Option 1: Flatten arrays
LinkagePool.RATE_TABLE {
    "RATE_CODE_1": Initialize=""
    "RATE_AMOUNT_1": Initialize=0
    "RATE_CODE_2": Initialize=""
    "RATE_AMOUNT_2": Initialize=0
    // ... up to 10
}

// Option 2: Use array operations
LinkagePool.RATE_TABLE_ARRAYS {
    "RATE_CODES": Initialize=ArrayCreate(10)
    "RATE_AMOUNTS": Initialize=ArrayCreate(10)
}

SubRoutine.ProcessRates {
    Input: rates: LinkagePool.RATE_TABLE_ARRAYS
    Body: {
        // Access arrays
        i = 0
        WhileLoop LessThan(i, 10) {
            code = ArrayGet(rates.RATE_CODES, i)
            amount = ArrayGet(rates.RATE_AMOUNTS, i)
            // Process...
            i = Add(i, 1)
        }
        
        ReturnValue(0)
    }
}
Key Transpiler Rules:

LinkagePool = Function Parameters - Always passed as Input/Output to SubRoutines
FixedPool = Working Storage - Module-level state
Direct Access - Use dot notation (pool.field) not function calls
Implied Decimals - Store as integers (9(5)V99 → multiply by 100)
String Fields - Initialize="" for alphanumeric
Numeric Fields - Initialize=0 for numeric
No Explicit Allocation - LinkagePools are pre-allocated when passed

These patterns should give your transpiler clear targets for converting COBOL LINKAGE SECTION to AILANG LinkagePools!RetryTo run code, enable code execution and file creation in Settings > Capabilities.