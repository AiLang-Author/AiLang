#!/bin/bash
# Split NIST test suite into individual program files
# Required for government RFP validation

SOURCE="cobol_frontend/tests/newcob.cbl"
OUTPUT_DIR="cobol_frontend/tests/nist_programs"

echo "================================================================================"
echo "NIST TEST SUITE SPLITTER"
echo "================================================================================"
echo ""
echo "Purpose: Split 512-program mega-file for government validation"
echo "Source:  $SOURCE"
echo "Output:  $OUTPUT_DIR/"
echo ""

# Create output directory
mkdir -p "$OUTPUT_DIR"

# Track statistics
total_programs=0
total_lines=0

echo "Extracting programs..."
echo ""

# Read the file and split by *HEADER / *END-OF markers
awk '
BEGIN {
    in_program = 0
    program_name = ""
    output_file = ""
}

# Start of program: *HEADER,TYPE,PROGNAME
/^\*HEADER,/ {
    # Extract program name (third comma-separated field)
    split($0, parts, ",")
    program_name = parts[3]
    gsub(/^[ \t]+|[ \t]+$/, "", program_name)  # trim whitespace
    
    output_file = "'"$OUTPUT_DIR"'/" program_name ".cbl"
    in_program = 1
    
    print "[" NR "] Extracting: " program_name " → " output_file > "/dev/stderr"
}

# Write lines to current program file
in_program == 1 {
    print $0 > output_file
}

# End of program: *END-OF,PROGNAME
/^\*END-OF,/ {
    if (in_program == 1) {
        close(output_file)
        in_program = 0
    }
}
' "$SOURCE"

# Count results
program_count=$(ls -1 "$OUTPUT_DIR"/*.cbl 2>/dev/null | wc -l)

echo ""
echo "================================================================================"
echo "EXTRACTION COMPLETE"
echo "================================================================================"
echo ""
echo "Programs extracted: $program_count"
echo "Output directory:   $OUTPUT_DIR/"
echo ""

# Show first 10 programs
echo "Sample programs:"
ls -1 "$OUTPUT_DIR"/*.cbl | head -10 | while read f; do
    lines=$(wc -l < "$f")
    echo "  $(basename "$f"): $lines lines"
done

if [ $program_count -gt 10 ]; then
    echo "  ... and $((program_count - 10)) more"
fi

echo ""
echo "================================================================================"
echo "NEXT STEPS"
echo "================================================================================"
echo ""
echo "1. Validate extraction:"
echo "   ls -lh $OUTPUT_DIR/ | head -20"
echo ""
echo "2. Test individual programs:"
echo "   python3 cobol_frontend/cobol_integration.py $OUTPUT_DIR/EXEC85.cbl --ailang-only"
echo ""
echo "3. Batch test all programs:"
echo "   ./test_nist_suite.sh"
echo ""