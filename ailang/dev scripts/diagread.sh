#!/bin/bash
# Capture TRACE output from READ conversion

echo "========================================================================"
echo "Transpiling BANKING.cbl and capturing READ TRACE output..."
echo "========================================================================"

cd cobol_frontend
python3 cobol_integration.py tests/BANKING.cbl --io-backend jcl --ailang-only -o ../BANKING.ailang 2>&1 | tee /tmp/banking_trace.log

echo ""
echo "========================================================================"
echo "READ Statement TRACE Analysis"
echo "========================================================================"

# Look for READ traces
if grep -q "TRACE.*convert_read" /tmp/banking_trace.log; then
    echo "✅ Found READ conversion traces:"
    echo ""
    grep -A 10 "TRACE.*convert_read" /tmp/banking_trace.log
else
    echo "❌ NO READ conversion traces found!"
    echo "   This means convert_read() is never being called."
    echo "   Either:"
    echo "   1. Parser isn't creating COBOLRead objects"
    echo "   2. Statement dispatcher isn't routing to convert_read()"
fi

echo ""
echo "========================================================================"
echo "Full trace log saved to: /tmp/banking_trace.log"
echo "========================================================================"