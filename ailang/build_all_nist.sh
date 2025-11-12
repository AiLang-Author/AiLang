#!/bin/bash
# build_all_nist.sh - Compile all NIST test programs in nist_output/

OUTPUT_DIR="cobol_frontend/tests/nist_output"
BINARY_DIR="${OUTPUT_DIR}/binaries"
LOGS_DIR="${OUTPUT_DIR}/logs"

# Create directories
mkdir -p "$BINARY_DIR"
mkdir -p "$LOGS_DIR"

# Counters
TOTAL=0
SUCCESS=0
FAILED=0

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "========================================"
echo "  NIST TEST SUITE - MASS COMPILATION"
echo "========================================"
echo ""

# Find all .ailang files
AILANG_FILES=("$OUTPUT_DIR"/*.ailang)

if [ ${#AILANG_FILES[@]} -eq 0 ]; then
    echo -e "${RED}ERROR: No .ailang files found in $OUTPUT_DIR${NC}"
    echo "Run transpilation first:"
    echo "  python3 cobol_frontend/cobol_integration.py cobol_frontend/tests/NC*.cbl \\"
    echo "      --output $OUTPUT_DIR --ailang-only --split-programs --io-backend jcl"
    exit 1
fi

echo "Found ${#AILANG_FILES[@]} programs to compile"
echo ""

# Compile each program
for AILANG_FILE in "${AILANG_FILES[@]}"; do
    BASENAME=$(basename "$AILANG_FILE" .ailang)
    BINARY="$BINARY_DIR/${BASENAME}_exec"
    LOGFILE="$LOGS_DIR/${BASENAME}.log"
    
    TOTAL=$((TOTAL + 1))
    
    printf "[%3d/%3d] Compiling %-15s ... " "$TOTAL" "${#AILANG_FILES[@]}" "$BASENAME"
    
    # Compile with main.py
    if python3 main.py "$AILANG_FILE" > "$LOGFILE" 2>&1; then
        # Check if binary was created
        EXPECTED_BINARY="${BASENAME}_exec"
        if [ -f "$EXPECTED_BINARY" ]; then
            # Move binary to binaries directory
            mv "$EXPECTED_BINARY" "$BINARY"
            echo -e "${GREEN}✓ SUCCESS${NC}"
            SUCCESS=$((SUCCESS + 1))
        else
            echo -e "${RED}✗ FAILED (no binary)${NC}"
            echo -e "${YELLOW}    └─ Check: cat $LOGFILE${NC}"
            FAILED=$((FAILED + 1))
        fi
    else
        echo -e "${RED}✗ FAILED (compile error)${NC}"
        # Show last 3 lines of error
        echo -e "${YELLOW}    Error:${NC}"
        tail -n 3 "$LOGFILE" | sed 's/^/      /'
        echo -e "${YELLOW}    └─ Full log: cat $LOGFILE${NC}"
        FAILED=$((FAILED + 1))
    fi
done

echo ""
echo "========================================"
echo "  COMPILATION RESULTS"
echo "========================================"
echo -e "Total programs:   ${YELLOW}$TOTAL${NC}"
echo -e "Successful:       ${GREEN}$SUCCESS${NC}"
echo -e "Failed:           ${RED}$FAILED${NC}"

if [ $TOTAL -gt 0 ]; then
    PERCENT=$((SUCCESS * 100 / TOTAL))
    echo -e "Success rate:     ${GREEN}${PERCENT}%${NC}"
fi

echo ""
echo "Binaries saved to: $BINARY_DIR"
echo "Logs saved to:     $LOGS_DIR"
echo ""

if [ $FAILED -gt 0 ]; then
    echo "To investigate failures, check:"
    echo "  cat $LOGS_DIR/<program>.log"
fi

exit 0