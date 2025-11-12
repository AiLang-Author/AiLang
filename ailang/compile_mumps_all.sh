#!/bin/bash
# build_mumps_all.sh - Build all MUMPS test programs with AILang compiler

cd /mnt/c/Users/Sean/Documents/AiLang/ailang

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Build configuration
SOURCE_DIR="mumps_frontend/mumps_tests"
LOG_DIR="mumps_frontend/build_logs"

# Create log directory
mkdir -p "$LOG_DIR"

echo "=========================================="
echo "MUMPS to AILang Build System"
echo "=========================================="
echo ""

# ============================================================================
# FILE DISCOVERY AND CATEGORIZATION
# ============================================================================

echo -e "${BLUE}Scanning $SOURCE_DIR...${NC}"

declare -a categories
declare -A category_files

# Discover all .ailang files organized by category
for category_dir in "$SOURCE_DIR"/*; do
    if [ -d "$category_dir" ]; then
        category=$(basename "$category_dir")
        categories+=("$category")
        
        ailang_dir="$category_dir/ailang"
        if [ -d "$ailang_dir" ]; then
            for ailang_file in "$ailang_dir"/*.ailang; do
                if [ -f "$ailang_file" ]; then
                    filename=$(basename "$ailang_file" .ailang)
                    category_files["$category"]+="$filename "
                fi
            done
        fi
    fi
done

# Summary
total_programs=0
echo ""
echo -e "${GREEN}Categories found:${NC}"
for category in "${categories[@]}"; do
    files=(${category_files["$category"]})
    count=${#files[@]}
    total_programs=$((total_programs + count))
    echo -e "  ${CYAN}$category:${NC} $count programs"
    for file in "${files[@]}"; do
        echo "    📄 $file"
    done
done

echo ""
echo "=========================================="
echo "Building $total_programs Programs"
echo "=========================================="
echo ""

# ============================================================================
# BUILD PROGRAMS
# ============================================================================

success=0
failed=0
declare -a failed_programs
declare -a success_programs
declare -A program_times

for category in "${categories[@]}"; do
    files=(${category_files["$category"]})
    
    if [ ${#files[@]} -eq 0 ]; then
        continue
    fi
    
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BLUE}Category: $category${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo ""
    
    for prog in "${files[@]}"; do
        ailang_file="$SOURCE_DIR/$category/ailang/${prog}.ailang"
        mumps_file="$SOURCE_DIR/$category/mumps/${prog}.m"
        exec_file="$SOURCE_DIR/$category/ailang/${prog}_exec"
        log_file="$LOG_DIR/${category}_${prog}.log"
        
        echo -e "${CYAN}Building: $prog${NC}"
        
        # Show original MUMPS source
        if [ -f "$mumps_file" ]; then
            echo -e "${YELLOW}  MUMPS source (${mumps_file}):${NC}"
            head -n 3 "$mumps_file" | sed 's/^/    /'
            if [ $(wc -l < "$mumps_file") -gt 3 ]; then
                echo "    ..."
            fi
        fi
        
        # Time the compilation
        start_time=$(date +%s.%N)
        
        # Compile
        python3 main.py "$ailang_file" > "$log_file" 2>&1
        compile_status=$?
        
        end_time=$(date +%s.%N)
        elapsed=$(echo "$end_time - $start_time" | bc)
        program_times["$prog"]=$elapsed
        
        # Check if executable was created
        if [ -f "$exec_file" ]; then
            echo -e "${GREEN}  ✓ SUCCESS${NC} (${elapsed}s)"
            success=$((success + 1))
            success_programs+=("$category/$prog")
            
            # Get file sizes
            if [[ "$OSTYPE" == "darwin"* ]]; then
                exec_size=$(stat -f%z "$exec_file")
                ailang_size=$(stat -f%z "$ailang_file")
            else
                exec_size=$(stat -c%s "$exec_file")
                ailang_size=$(stat -c%s "$ailang_file")
            fi
            
            echo "    📦 Executable: $(numfmt --to=iec-i --suffix=B $exec_size 2>/dev/null || echo "$exec_size bytes")"
            echo "    📝 AILang:    $(numfmt --to=iec-i --suffix=B $ailang_size 2>/dev/null || echo "$ailang_size bytes")"
            
            # Run the program and show output
            echo -e "${GREEN}    ▶ Running:${NC}"
            output=$("$exec_file" 2>&1 | head -n 5)
            echo "$output" | sed 's/^/      /'
            if [ $(echo "$output" | wc -l) -eq 5 ]; then
                echo "      ..."
            fi
            
        else
            echo -e "${RED}  ✗ FAILED${NC} (${elapsed}s)"
            failed=$((failed + 1))
            failed_programs+=("$category/$prog")
            
            echo "    Exit code: $compile_status"
            echo "    Log: $log_file"
            
            # Show error summary
            if [ -f "$log_file" ]; then
                echo -e "\n${RED}    Error Summary:${NC}"
                if grep -q "SyntaxError" "$log_file"; then
                    echo "      ⚠️  Python syntax error in transpiled code"
                elif grep -q "NameError" "$log_file"; then
                    echo "      ⚠️  Undefined name in AILang output"
                elif grep -q "compilation failed" "$log_file"; then
                    echo "      ⚠️  AILang backend compilation failed"
                else
                    echo "      ⚠️  Unknown error (see log for details)"
                fi
                
                echo -e "\n${RED}    Last 3 log lines:${NC}"
                tail -n 3 "$log_file" | sed 's/^/      /'
            fi
        fi
        echo ""
    done
done

# ============================================================================
# BUILD SUMMARY
# ============================================================================

echo "=========================================="
echo "Build Summary"
echo "=========================================="
echo ""

success_rate=0
if [ $total_programs -gt 0 ]; then
    success_rate=$((success * 100 / total_programs))
fi

echo -e "${GREEN}✓ Success:${NC} $success / $total_programs (${success_rate}%)"
echo -e "${RED}✗ Failed:${NC}  $failed / $total_programs"

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
        echo "  ✗ $prog"
    done
fi

# ============================================================================
# PERFORMANCE STATS
# ============================================================================

if [ $success -gt 0 ]; then
    echo ""
    echo "=========================================="
    echo "Performance Statistics"
    echo "=========================================="
    echo ""
    
    # Calculate average compile time
    total_time=0
    count=0
    for prog in "${!program_times[@]}"; do
        time=${program_times[$prog]}
        total_time=$(echo "$total_time + $time" | bc)
        count=$((count + 1))
    done
    
    if [ $count -gt 0 ]; then
        avg_time=$(echo "scale=3; $total_time / $count" | bc)
        echo -e "${CYAN}Average compile time:${NC} ${avg_time}s"
        echo -e "${CYAN}Total compile time:${NC}   ${total_time}s"
    fi
    
    # Show fastest and slowest
    echo ""
    echo "Fastest builds:"
    for prog in "${!program_times[@]}"; do
        echo "${program_times[$prog]} $prog"
    done | sort -n | head -n 3 | while read time prog; do
        echo "  ⚡ $prog: ${time}s"
    done
    
    echo ""
    echo "Slowest builds:"
    for prog in "${!program_times[@]}"; do
        echo "${program_times[$prog]} $prog"
    done | sort -rn | head -n 3 | while read time prog; do
        echo "  🐌 $prog: ${time}s"
    done
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
    echo -e "${GREEN}✓ Run individual programs:${NC}"
    
    # Show one example from each category
    for category in "${categories[@]}"; do
        files=(${category_files["$category"]})
        if [ ${#files[@]} -gt 0 ]; then
            first_prog="${files[0]}"
            exec_file="$SOURCE_DIR/$category/ailang/${first_prog}_exec"
            if [ -f "$exec_file" ]; then
                echo "  ./$exec_file  # $category/$first_prog"
            fi
        fi
    done
    echo ""
fi

if [ $failed -gt 0 ]; then
    echo -e "${RED}🔧 Debug failures:${NC}"
    echo "  cat $LOG_DIR/<category>_<program>.log"
    echo ""
fi

echo -e "${CYAN}📊 View all test results:${NC}"
echo "  ls -lh $SOURCE_DIR/*/ailang/*_exec"
echo ""

echo -e "${BLUE}🔄 Re-transpile MUMPS source:${NC}"
echo "  cd mumps_frontend && python3 mumps_test_runner.py"
echo ""

echo "=========================================="

# Exit with appropriate code
if [ $failed -gt 0 ]; then
    exit 1
else
    exit 0
fi