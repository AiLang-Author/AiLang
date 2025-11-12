#!/bin/bash
# Extract the failing IC22X programs

mkdir -p parse_failed_programs

for prog in IC222A IC223A IC224A IC225A IC226A IC227A IC228A; do
    echo "Extracting $prog..."
    
    # Find the *HEADER and *END-OF lines
    header_line=$(grep -n "^\*HEADER,.*,$prog" cobol_frontend/tests/newcob.cbl | cut -d: -f1)
    end_line=$(grep -n "^\*END-OF,$prog" cobol_frontend/tests/newcob.cbl | cut -d: -f1)
    
    if [ -n "$header_line" ] && [ -n "$end_line" ]; then
        sed -n "${header_line},${end_line}p" cobol_frontend/tests/newcob.cbl > "parse_failed_programs/${prog}.cbl"
        lines=$((end_line - header_line + 1))
        echo "  ✓ Extracted $lines lines to parse_failed_programs/${prog}.cbl"
    else
        echo "  ✗ Could not find boundaries for $prog"
    fi
done

echo ""
echo "Testing each program individually:"
echo "=================================="

for prog in parse_failed_programs/*.cbl; do
    name=$(basename "$prog" .cbl)
    echo ""
    echo "Testing $name..."
    python3 cobol_frontend/cobol_integration.py "$prog" \
        --ailang-only --io-backend jcl --split-programs 2>&1 | \
        grep -E "(Successfully parsed|Failed to parse|Parser error)" | tail -5
done