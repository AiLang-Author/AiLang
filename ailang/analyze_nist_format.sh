#!/bin/bash
# Analyze the format of the NIST test suite file

FILE="cobol_frontend/tests/newcob.cbl"

echo "================================================================================"
echo "ANALYZING NIST TEST FILE FORMAT"
echo "================================================================================"

if [ ! -f "$FILE" ]; then
    echo "Error: $FILE not found"
    exit 1
fi

echo ""
echo "File: $FILE"
echo "Size: $(wc -c < "$FILE") bytes"
echo "Lines: $(wc -l < "$FILE") lines"

echo ""
echo "─────────────────────────────────────────────────────────────────────────────"
echo "First 50 lines:"
echo "─────────────────────────────────────────────────────────────────────────────"
head -50 "$FILE" | nl

echo ""
echo "─────────────────────────────────────────────────────────────────────────────"
echo "Looking for PROGRAM-ID patterns..."
echo "─────────────────────────────────────────────────────────────────────────────"
grep -n -i "PROGRAM-ID" "$FILE" | head -20

echo ""
echo "─────────────────────────────────────────────────────────────────────────────"
echo "Looking for NIST markers (*HEADER, *END-OF)..."
echo "─────────────────────────────────────────────────────────────────────────────"
grep -n "^\*HEADER\|^\*END-OF" "$FILE" | head -20

echo ""
echo "─────────────────────────────────────────────────────────────────────────────"
echo "Looking for IDENTIFICATION DIVISION patterns..."
echo "─────────────────────────────────────────────────────────────────────────────"
grep -n -i "IDENTIFICATION DIVISION" "$FILE" | head -20

echo ""
echo "─────────────────────────────────────────────────────────────────────────────"
echo "Sample around line 290 (first failure):"
echo "─────────────────────────────────────────────────────────────────────────────"
sed -n '285,295p' "$FILE" | nl -v 285

echo ""
echo "─────────────────────────────────────────────────────────────────────────────"
echo "Sample around line 45 (GO token failure):"
echo "─────────────────────────────────────────────────────────────────────────────"
sed -n '40,50p' "$FILE" | nl -v 40

echo ""
echo "─────────────────────────────────────────────────────────────────────────────"
echo "Checking file structure - program boundaries:"
echo "─────────────────────────────────────────────────────────────────────────────"

# Count different boundary markers
echo "IDENTIFICATION DIVISION: $(grep -ci "IDENTIFICATION DIVISION" "$FILE")"
echo "PROGRAM-ID: $(grep -ci "PROGRAM-ID\." "$FILE")"
echo "END PROGRAM: $(grep -ci "END PROGRAM" "$FILE")"
echo "*HEADER markers: $(grep -c "^\*HEADER" "$FILE")"
echo "*END-OF markers: $(grep -c "^\*END-OF" "$FILE")"

echo ""
echo "─────────────────────────────────────────────────────────────────────────────"
echo "Pattern check - How are programs separated?"
echo "─────────────────────────────────────────────────────────────────────────────"

# Show the pattern of the first few programs
echo ""
echo "First program boundary:"
grep -n -m1 -i "IDENTIFICATION DIVISION\|PROGRAM-ID\|\*HEADER" "$FILE"

echo ""
echo "Second program boundary:"
grep -n -i "IDENTIFICATION DIVISION\|PROGRAM-ID\|\*HEADER" "$FILE" | sed -n '2p'

echo ""
echo "Last program boundary:"
grep -n -i "IDENTIFICATION DIVISION\|PROGRAM-ID\|\*HEADER" "$FILE" | tail -1