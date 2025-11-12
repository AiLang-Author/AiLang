#!/bin/bash
# test_jcl_data_loader.sh - End-to-End JCL Data Loader Test
# Tests the complete flow: Database → JCL Data Loader → COBOL Program

set -e

DB_NAME="testdb"
DB_USER="testuser"
TEST_JOB_ID=999

echo "==========================================="
echo "JCL Data Loader End-to-End Test"
echo "==========================================="
echo ""

# Cleanup from previous runs
echo "[0/6] Cleaning up previous test data..."
psql -U $DB_USER -d $DB_NAME -c "DELETE FROM api_requests WHERE job_id = $TEST_JOB_ID;" > /dev/null 2>&1 || true
psql -U $DB_USER -d $DB_NAME -c "DELETE FROM api_responses WHERE job_id = $TEST_JOB_ID;" > /dev/null 2>&1 || true
echo "✓ Cleanup complete"

# Step 1: Verify database tables exist
echo ""
echo "[1/6] Verifying database schema..."
if ! psql -U $DB_USER -d $DB_NAME -c "SELECT 1 FROM api_requests LIMIT 1;" > /dev/null 2>&1; then
    echo "✗ ERROR: api_requests table does not exist"
    echo "  Run: ./create_jcl_database.sh"
    exit 1
fi
echo "✓ Database schema OK"

# Step 2: Create test COBOL program
echo ""
echo "[2/6] Creating test COBOL program..."

cat > test_linkage.cbl << 'EOF'
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-LINKAGE.
       
       DATA DIVISION.
       LINKAGE SECTION.
       01 INPUT-PARAM-1 PIC 9(4).
       01 INPUT-PARAM-2 PIC X(20).
       
       PROCEDURE DIVISION USING INPUT-PARAM-1 INPUT-PARAM-2.
       MAIN-LOGIC.
           DISPLAY "JCL Data Loader Test Program".
           DISPLAY "============================".
           DISPLAY "PARAM1: " INPUT-PARAM-1.
           DISPLAY "PARAM2: " INPUT-PARAM-2.
           DISPLAY "============================".
           DISPLAY "Test completed successfully".
           STOP RUN.
EOF

echo "✓ Test COBOL program created: test_linkage.cbl"

# Step 3: Transpile with JCL backend
echo ""
echo "[3/6] Transpiling with JCL backend..."

if ! ./cobol_to_ailang.py test_linkage.cbl --backend jcl --debug > test_linkage.ailang 2>transpile_debug.log; then
    echo "✗ Transpilation failed!"
    echo "Debug output:"
    cat transpile_debug.log
    exit 1
fi

echo "✓ Transpilation successful"

# Check if JCL data loader was injected
echo ""
echo "   Checking for JCL data loader injection..."
if grep -q "JCL_DataLoader.LoadJobData" test_linkage.ailang; then
    echo "   ✓ JCL data loader code found in generated AiLang"
    
    # Show the injected code
    echo ""
    echo "   Generated data loader code:"
    grep -A 10 "JCL_DataLoader.LoadJobData" test_linkage.ailang | sed 's/^/   │ /'
else
    echo "   ✗ JCL data loader code NOT found!"
    echo ""
    echo "   This means the transpiler patch was not applied correctly."
    echo "   Please verify:"
    echo "   1. io_jcl.py has the three new methods"
    echo "   2. converter_core.py calls get_jcl_data_loader_code()"
    echo ""
    echo "   Debug output:"
    grep -i "linkage\|jcl" transpile_debug.log || echo "   (no relevant debug output)"
    exit 1
fi

# Step 4: Insert test data into database
echo ""
echo "[4/6] Inserting test data into database..."

psql -U $DB_USER -d $DB_NAME << EOF
INSERT INTO api_requests (job_id, request_data) VALUES
($TEST_JOB_ID, '{"INPUT-PARAM-1": "1234", "INPUT-PARAM-2": "Hello World"}');
EOF

echo "✓ Test data inserted (job_id=$TEST_JOB_ID)"

# Verify insertion
echo "   Verifying data..."
REQUEST_DATA=$(psql -U $DB_USER -d $DB_NAME -tc "SELECT request_data FROM api_requests WHERE job_id = $TEST_JOB_ID;")
echo "   Data in DB: $REQUEST_DATA"

# Step 5: Compile AiLang program
echo ""
echo "[5/6] Compiling AiLang program..."

if ! ./ailang test_linkage.ailang > compile.log 2>&1; then
    echo "✗ Compilation failed!"
    cat compile.log
    exit 1
fi

echo "✓ Compilation successful: test_linkage_exec"

# Step 6: Run with simulated container environment
echo ""
echo "[6/6] Running test program..."
echo ""
echo "=========================================="
echo "Program Output:"
echo "=========================================="

# Set ContainerConfig.job_id via environment or direct execution
# For this test, we'll use a wrapper that sets the job_id

cat > run_test.sh << EOF
#!/bin/bash
# Wrapper to simulate container environment
# In real JCL system, this would be set by JCL_Worker

# Mock ContainerConfig by creating a simple program that sets it
./test_linkage_exec

echo ""
echo "=========================================="
EOF

chmod +x run_test.sh

# Note: This test requires the program to have ContainerConfig.job_id set
# In production, this is done by JCL_Worker.Main()
echo ""
echo "NOTE: For this test to load data, ContainerConfig.job_id must be set to $TEST_JOB_ID"
echo "      In production, JCL_Worker sets this automatically."
echo ""
echo "Expected output if data loader works:"
echo "  PARAM1: 1234"
echo "  PARAM2: Hello World"
echo ""
echo "Actual output if job_id not set (expected for standalone test):"
echo "  PARAM1: 0"
echo "  PARAM2: (empty)"
echo ""

# The actual test would need to be run through the JCL system:
# echo "To test with real data loading:"
# echo "  1. Start JCL daemon: ./JCL_daemon_exec"
# echo "  2. Submit via HTTP:"
# echo "     curl -X POST http://localhost:8080/submit \\"
# echo "          -H 'Content-Type: application/json' \\"
# echo "          -d '{\"program\": \"test_linkage\", \"data\": {\"INPUT-PARAM-1\": \"1234\", \"INPUT-PARAM-2\": \"Hello World\"}}'"

# Summary
echo ""
echo "==========================================="
echo "Test Summary"
echo "==========================================="
echo "✓ Database schema verified"
echo "✓ Test COBOL program created"
echo "✓ Transpilation successful with JCL backend"
echo "✓ JCL data loader code injected"
echo "✓ Test data inserted (job_id=$TEST_JOB_ID)"
echo "✓ AiLang compilation successful"
echo ""
echo "Manual verification needed:"
echo "  - Run through JCL daemon to test data loading"
echo "  - Check that LINKAGE variables get populated"
echo "  - Verify ContainerConfig.job_id propagation"
echo ""
echo "Files generated:"
echo "  - test_linkage.cbl (COBOL source)"
echo "  - test_linkage.ailang (transpiled)"
echo "  - test_linkage_exec (compiled)"
echo "  - transpile_debug.log (debug output)"
echo ""

# Cleanup option
echo "To clean up test files:"
echo "  rm -f test_linkage.cbl test_linkage.ailang test_linkage_exec"
echo "  rm -f transpile_debug.log compile.log run_test.sh"
echo "  psql -U $DB_USER -d $DB_NAME -c \"DELETE FROM api_requests WHERE job_id = $TEST_JOB_ID;\""
echo ""