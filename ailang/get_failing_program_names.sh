#!/bin/bash
# Extract failing program names from log

echo "Programs with parse failures:"
echo "=============================="
echo ""

# Pattern: Look for the last successful program before "Failed to parse"
# The failing program is the NEXT one in that *HEADER group

grep -B 15 "Failed to parse" full_output.log | \
    grep "Successfully parsed program" | \
    sed 's/.*: //' | \
    sed 's/-[0-9]*$//' | \
    sort -u

echo ""
echo "Extracting these program groups from NIST file..."
echo ""

# For each base program name, extract the entire *HEADER section
for prog in $(grep -B 15 "Failed to parse" full_output.log | \
    grep "Successfully parsed program" | \
    sed 's/.*: //' | \
    sed 's/-[0-9]*$//' | \
    sort -u); do
    
    echo "Extracting: $prog"
    
    # Find *HEADER line for this program
    header_line=$(grep -n "^\*HEADER,.*,$prog" cobol_frontend/tests/newcob.cbl | head -1 | cut -d: -f1)
    end_line=$(grep -n "^\*END-OF,.*$prog" cobol_frontend/tests/newcob.cbl | head -1 | cut -d: -f1)
    
    if [ -n "$header_line" ] && [ -n "$end_line" ]; then
        mkdir -p parse_failed_programs
        sed -n "${header_line},${end_line}p" cobol_frontend/tests/newcob.cbl > "parse_failed_programs/${prog}.cbl"
        echo "  → parse_failed_programs/${prog}.cbl (lines $header_line-$end_line)"
    else
        echo "  ✗ Not found in source file"
    fi
done

echo ""
echo "=============================="
echo "Testing extracted programs:"
echo "=============================="
echo ""

cd parse_failed_programs
for prog in *.cbl; do
    echo "Testing $prog..."
    python3 ../cobol_frontend/cobol_integration.py "$prog" --ailang-only --io-backend jcl --split-programs 2>&1 | \
        grep -E "(Successfully parsed|Failed to parse)" | tail -3
    echo ""
done