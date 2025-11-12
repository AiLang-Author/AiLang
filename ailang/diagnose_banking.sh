#!/bin/bash
# FINAL FIX: Compile banking.cbl with JCL backend

echo "========================================="
echo "BANKING.CBL - FINAL FIX"
echo "========================================="
echo ""
echo "ROOT CAUSE: banking.cbl requires JCL backend,"
echo "            but was compiled with syscall backend (default)"
echo ""
echo "SOLUTION: Use --io-backend jcl flag"
echo ""

# Clean
rm -f banking.ailang banking_exec 2>/dev/null

# Compile with JCL backend
echo "Compiling with JCL backend..."
python3 cobol_frontend/cobol_integration.py \
    cobol_frontend/tests/BANKING.cbl \
    -o banking \
    --io-backend jcl \
    --ailang-only

if [ $? -ne 0 ]; then
    echo "❌ TRANSPILATION FAILED"
    exit 1
fi

echo "✓ Transpilation succeeded"
echo ""

# Verify JCL backend is set
if grep -q 'Cobol.io_backend_type = "jcl"' banking.ailang; then
    echo "✓ JCL backend correctly configured"
else
    echo "❌ WARNING: JCL backend NOT configured!"
    grep "io_backend_type" banking.ailang
fi

# Compile
echo ""
echo "Compiling to binary..."
python3 main.py banking.ailang

if [ $? -ne 0 ]; then
    echo "❌ COMPILATION FAILED"
    exit 1
fi

echo "✓ Compilation succeeded"
echo ""
echo "========================================="
echo "SUCCESS!"
echo "========================================="
echo ""
echo "Generated files:"
echo "  - banking.ailang (JCL backend)"
echo "  - banking_exec (executable)"
echo ""
echo "To run:"
echo "  export COBOL_API_REQUEST='{\"operation\":\"BALANCE\",\"account_id\":\"TEST001\"}'"
echo "  ./banking_exec"
echo ""