#!/bin/bash
# EXEC85 Realistic Test Data Loader and Runner
# This simulates a real COBOL version control session

echo "=========================================="
echo "EXEC85 Realistic Test Data Loader"
echo "=========================================="
echo ""

# Load the test data
echo "Step 1: Loading control cards into database..."
psql -U testuser -d testdb << 'EOF'

-- Clear existing data
TRUNCATE TABLE CONTROL_CARD_FILE_seq;

-- Insert control cards for a realistic EXEC85 session
INSERT INTO CONTROL_CARD_FILE_seq (record_data) VALUES
-- Monitor section (configuration)
('*START                                                                          '),
('*LIST PROGRAMS                                                                  '),
('*LIST X-CARDS                                                                   '),
('*SELECT-MODULE TEST01                                                           '),
('*END-MONITOR                                                                    '),

-- Begin update section
('*BEGIN-UPDATE                                                                   '),

-- Simulated COBOL program header
('X-TEST01    000100 IDENTIFICATION DIVISION.                                    '),
('X-TEST01    000200 PROGRAM-ID. TEST01.                                         '),
('X-TEST01    000300 AUTHOR. NIST TEST SUITE.                                    '),
('X-TEST01    000400*                                                             '),
('X-TEST01    000500 ENVIRONMENT DIVISION.                                       '),
('X-TEST01    000600 CONFIGURATION SECTION.                                      '),
('X-TEST01    000700 SOURCE-COMPUTER. IBM-370.                                   '),
('X-TEST01    000800 OBJECT-COMPUTER. IBM-370.                                   '),
('X-TEST01    000900*                                                             '),
('X-TEST01    001000 DATA DIVISION.                                              '),
('X-TEST01    001100 WORKING-STORAGE SECTION.                                    '),
('X-TEST01    001200 01 WS-COUNTER PIC 9(4) VALUE 0.                             '),
('X-TEST01    001300 01 WS-MESSAGE PIC X(30) VALUE "HELLO FROM TEST01".          '),
('X-TEST01    001400*                                                             '),
('X-TEST01    001500 PROCEDURE DIVISION.                                         '),
('X-TEST01    001600 MAIN-PARA.                                                  '),
('X-TEST01    001700     DISPLAY WS-MESSAGE.                                     '),
('X-TEST01    001800     ADD 1 TO WS-COUNTER.                                    '),
('X-TEST01    001900     DISPLAY "COUNTER: " WS-COUNTER.                         '),
('X-TEST01    002000     STOP RUN.                                               '),

-- End update section
('*END-UPDATE                                                                     ');

-- Show what was loaded
\echo ''
\echo 'Control cards loaded:'
SELECT count(*) as total_cards FROM CONTROL_CARD_FILE_seq;

\echo ''
\echo 'First 5 cards:'
SELECT seq_id, left(record_data, 60) as card_preview 
FROM CONTROL_CARD_FILE_seq 
ORDER BY seq_id LIMIT 5;

EOF

if [ $? -ne 0 ]; then
    echo "ERROR: Failed to load test data!"
    exit 1
fi

echo ""
echo "Step 2: Clearing old output..."
psql -U testuser -d testdb -c "TRUNCATE TABLE PRINT_FILE_seq;" > /dev/null

echo ""
echo "Step 3: Running EXEC85 with realistic data..."
echo "=========================================="
./EXEC85_exec 2>&1 | tee exec85_realistic_output.log

EXIT_CODE=$?
echo ""
echo "=========================================="
echo "Step 4: Checking results..."
echo "Exit code: $EXIT_CODE"

# Check the output
echo ""
echo "Output from PRINT_FILE (first 30 non-blank lines):"
psql -U testuser -d testdb << 'EOF'
SELECT 
    seq_id, 
    '|' || record_data || '|' as output 
FROM PRINT_FILE_seq 
WHERE trim(record_data) != '' 
ORDER BY seq_id 
LIMIT 30;
EOF

echo ""
echo "Total lines written:"
psql -U testuser -d testdb -c "SELECT count(*) as total_lines FROM PRINT_FILE_seq;" -t

echo ""
echo "Last 20 lines of execution log:"
tail -20 exec85_realistic_output.log

echo ""
echo "=========================================="
echo "EXEC85 Test Complete!"
echo "=========================================="