#!/bin/bash
# bullmedicaid_enhanced.sh - Build Medicare programs with proper file filtering

cd /mnt/c/Users/Sean/Documents/AiLang/ailang

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Build configuration
OUTPUT_DIR="JCL/PROGRAMS"
SOURCE_DIR="cobol_frontend/tests/medicare"
LOG_DIR="build_logs"

# Create directories
mkdir -p "$OUTPUT_DIR" "$LOG_DIR"

echo "=========================================="
echo "Medicare ESRD Pricer Build System"
echo "=========================================="
echo ""

# ============================================================================
# FILE DISCOVERY AND CATEGORIZATION
# ============================================================================

echo -e "${BLUE}Scanning $SOURCE_DIR...${NC}"

declare -a programs
declare -a copybooks
declare -a datafiles
declare -a guifiles
declare -a otherfiles

for file in "$SOURCE_DIR"/*; do
    if [ -f "$file" ]; then
        filename=$(basename "$file")
        
        # Categorize by extension/suffix
        if [[ "$filename" == *.cpy ]]; then
            copybooks+=("$filename")
        elif [[ "$filename" == *.dat ]]; then
            datafiles+=("$filename")
        elif [[ "$filename" == *.gui ]]; then
            guifiles+=("$filename")
        elif [[ "$filename" == README* || "$filename" == MANIFEST* ]]; then
            otherfiles+=("$filename")
        else
            # It's a program - extract base name
            progname="${filename%.*}"
            programs+=("$progname")
        fi
    fi
done

# Summary
echo -e "${GREEN}  Programs:${NC}  ${#programs[@]}"
echo -e "${YELLOW}  Copybooks:${NC} ${#copybooks[@]}"
echo -e "${CYAN}  Data files:${NC} ${#datafiles[@]}"
echo -e "${CYAN}  GUI files:${NC} ${#guifiles[@]}"
echo -e "${YELLOW}  Other:${NC}     ${#otherfiles[@]}"

# Show details
if [ ${#copybooks[@]} -gt 0 ]; then
    echo -e "\n${YELLOW}Copybooks (auto-included during compilation):${NC}"
    for cpybook in "${copybooks[@]}"; do
        echo "    📋 $cpybook"
    done
fi

if [ ${#datafiles[@]} -gt 0 ]; then
    echo -e "\n${CYAN}Data files (for SQL import):${NC}"
    for datafile in "${datafiles[@]}"; do
        echo "    💾 $datafile"
    done
fi

if [ ${#guifiles[@]} -gt 0 ]; then
    echo -e "\n${CYAN}GUI definition files:${NC}"
    for guifile in "${guifiles[@]}"; do
        echo "    🖥️  $guifile"
    done
fi

if [ ${#otherfiles[@]} -gt 0 ]; then
    echo -e "\n${YELLOW}Other files (skipped):${NC}"
    for other in "${otherfiles[@]}"; do
        echo "    📄 $other"
    done
fi

echo ""
echo "=========================================="
echo "Building ${#programs[@]} Programs"
echo "=========================================="
echo ""

# ============================================================================
# BUILD PROGRAMS
# ============================================================================

success=0
failed=0
declare -a failed_programs
declare -a success_programs

for prog in "${programs[@]}"; do
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BLUE}Building: $prog${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    
    # Build with logging
    python3 cobol_frontend/cobol_integration.py \
        "$SOURCE_DIR/$prog" \
        --output "$OUTPUT_DIR/$prog.jcl" \
        --io-backend jcl \
        > "$LOG_DIR/${prog}_build.log" 2>&1
    
    build_status=$?
    
    # Check if executable was created
    if [ -f "$OUTPUT_DIR/${prog}.jcl_exec" ]; then
        echo -e "${GREEN}✓ SUCCESS${NC}"
        success=$((success + 1))
        success_programs+=("$prog")
        
        # Get file sizes
        if [[ "$OSTYPE" == "darwin"* ]]; then
            exec_size=$(stat -f%z "$OUTPUT_DIR/${prog}.jcl_exec")
            ailang_size=$(stat -f%z "$OUTPUT_DIR/${prog}.ailang" 2>/dev/null || echo "0")
        else
            exec_size=$(stat -c%s "$OUTPUT_DIR/${prog}.jcl_exec")
            ailang_size=$(stat -c%s "$OUTPUT_DIR/${prog}.ailang" 2>/dev/null || echo "0")
        fi
        
        echo "  📦 Executable: $(numfmt --to=iec-i --suffix=B $exec_size 2>/dev/null || echo "$exec_size bytes")"
        
        if [ -f "$OUTPUT_DIR/${prog}.ailang" ]; then
            echo "  📝 AiLang:    $(numfmt --to=iec-i --suffix=B $ailang_size 2>/dev/null || echo "$ailang_size bytes")"
        fi
        
    else
        echo -e "${RED}✗ FAILED${NC}"
        failed=$((failed + 1))
        failed_programs+=("$prog")
        
        echo "  Exit code: $build_status"
        echo "  Log: $LOG_DIR/${prog}_build.log"
        
        # Show error summary
        if [ -f "$LOG_DIR/${prog}_build.log" ]; then
            echo -e "\n${RED}  Error Summary:${NC}"
            
            # Try to extract meaningful error
            if grep -q "PERFORM VARYING" "$LOG_DIR/${prog}_build.log"; then
                echo "    ⚠️  PERFORM VARYING loop complexity issue"
            elif grep -q "No COBOL programs found" "$LOG_DIR/${prog}_build.log"; then
                echo "    ⚠️  No IDENTIFICATION DIVISION found"
            elif grep -q "Lexer error" "$LOG_DIR/${prog}_build.log"; then
                echo "    ⚠️  Syntax/lexer error in source"
            elif grep -q "Ailang compilation failed" "$LOG_DIR/${prog}_build.log"; then
                echo "    ⚠️  AiLang backend compilation failed"
            else
                echo "    ⚠️  Unknown error (see log for details)"
            fi
            
            echo -e "\n${RED}  Last 3 log lines:${NC}"
            tail -n 3 "$LOG_DIR/${prog}_build.log" | sed 's/^/    /'
        fi
    fi
    echo ""
done

# ============================================================================
# BUILD SUMMARY
# ============================================================================

echo "=========================================="
echo "Build Summary"
echo "=========================================="
echo ""

success_rate=0
if [ ${#programs[@]} -gt 0 ]; then
    success_rate=$((success * 100 / ${#programs[@]}))
fi

echo -e "${GREEN}✓ Success:${NC} $success / ${#programs[@]} (${success_rate}%)"
echo -e "${RED}✗ Failed:${NC}  $failed / ${#programs[@]}"

if [ $success -gt 0 ]; then
    echo ""
    echo -e "${GREEN}Successfully built programs:${NC}"
    for prog in "${success_programs[@]}"; do
        echo "  ✓ $prog"
    done
fi

if [ $failed -gt 0 ]; then
    echo ""
    echo -e "${RED}Failed programs:${NC}"
    for prog in "${failed_programs[@]}"; do
        # Categorize failure type
        error_type="unknown"
        if [ -f "$LOG_DIR/${prog}_build.log" ]; then
            if grep -q "PERFORM VARYING" "$LOG_DIR/${prog}_build.log"; then
                error_type="PERFORM VARYING"
            elif grep -q "No COBOL programs found" "$LOG_DIR/${prog}_build.log"; then
                error_type="No PROGRAM-ID"
            elif grep -q "Lexer error" "$LOG_DIR/${prog}_build.log"; then
                error_type="Syntax error"
            elif grep -q "Ailang compilation failed" "$LOG_DIR/${prog}_build.log"; then
                error_type="AiLang backend"
            fi
        fi
        echo "  ✗ $prog [$error_type]"
    done
fi

# ============================================================================
# OUTPUT FILES
# ============================================================================

echo ""
echo "=========================================="
echo "Output Files"
echo "=========================================="
echo ""

if [ $success -gt 0 ]; then
    echo -e "${GREEN}Executables in $OUTPUT_DIR:${NC}"
    ls -lh "$OUTPUT_DIR"/*.jcl_exec 2>/dev/null | awk '{
        # Extract size and filename
        size = $5
        file = $9
        gsub(/.*\//, "", file)  # Remove path
        printf "  %-20s %10s\n", file, size
    }'
    
    echo ""
    ailang_count=$(ls -1 "$OUTPUT_DIR"/*.ailang 2>/dev/null | wc -l)
    echo -e "${BLUE}AiLang sources:${NC} $ailang_count files"
else
    echo "  No executables generated"
fi

# ============================================================================
# NEXT STEPS
# ============================================================================

echo ""
echo "=========================================="
echo "Next Steps"
echo "=========================================="
echo ""

if [ $success -gt 0 ]; then
    echo -e "${GREEN}✓ Test a working program:${NC}"
    first_success="${success_programs[0]}"
    echo "  JCL/jcl_daemon.sh submit $first_success '{\"P-PROV-TYPE\":\"40\"}'"
    echo ""
fi

if [ ${#datafiles[@]} -gt 0 ]; then
    echo -e "${CYAN}📊 Load reference data:${NC}"
    echo "  ./load_medicare_data.sh"
    echo ""
fi

if [ ${#guifiles[@]} -gt 0 ]; then
    echo -e "${CYAN}🖥️  Generate web interface:${NC}"
    echo "  ./generate_web_gui.sh"
    echo ""
fi

if [ $failed -gt 0 ]; then
    echo -e "${RED}🔧 Debug failures:${NC}"
    echo "  cat $LOG_DIR/<program>_build.log"
    echo ""
    
    # Count failure types
    perform_count=0
    for prog in "${failed_programs[@]}"; do
        if [ -f "$LOG_DIR/${prog}_build.log" ] && grep -q "PERFORM VARYING" "$LOG_DIR/${prog}_build.log"; then
            perform_count=$((perform_count + 1))
        fi
    done
    
    if [ $perform_count -gt 0 ]; then
        echo -e "${YELLOW}  Note: $perform_count programs failed due to PERFORM VARYING complexity${NC}"
        echo "  Consider increasing parser loop limit in statement_parsers.py"
    fi
fi

echo "=========================================="