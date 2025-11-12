#!/bin/bash

# Create a small test file with PERFORM THRU
cat > /tmp/test_perform_thru.cbl << 'EOF'
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTTHRU.
       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM START-PARA THRU END-PARA.
           STOP RUN.
       START-PARA.
           DISPLAY "START".
       MIDDLE-PARA.
           DISPLAY "MIDDLE".
       END-PARA.
           DISPLAY "END".
EOF

echo "Testing PERFORM...THRU conversion..."
python3 cobol_frontend/cobol_integration.py /tmp/test_perform_thru.cbl --ailang-only --io-backend jcl 2>&1 | head -50