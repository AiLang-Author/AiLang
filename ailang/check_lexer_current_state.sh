#!/bin/bash
# Check what's actually in the current lexer file

echo "==================================================================="
echo "CHECKING ACTUAL cobol_lexer.py FILE"
echo "==================================================================="

# Check if preprocess_continuation_lines filters comment lines
echo -e "\n1. Checking comment filtering in preprocess_continuation_lines:"
grep -A 5 "# Rule 5: Comment" cobol_frontend/cobol_lexer.py | head -10

# Check __init__ method
echo -e "\n2. Checking __init__ method:"
grep -A 8 "def __init__" cobol_frontend/cobol_lexer.py | head -10

# Check for any in_identification or metadata tracking
echo -e "\n3. Checking for metadata state tracking:"
grep -n "in_identification\|in_metadata\|metadata_state" cobol_frontend/cobol_lexer.py || echo "  No metadata state tracking found"

# Check _handle_special_lines for metadata handling
echo -e "\n4. Checking metadata handling in _handle_special_lines:"
grep -B 2 -A 10 "ROW 6.5.*IDENTIFICATION" cobol_frontend/cobol_lexer.py | head -15

# Run the actual failing test
echo -e "\n5. Running actual test on first 1000 lines:"
head -1000 cobol_frontend/tests/newval.cbl | python3 -c "
import sys
sys.path.insert(0, '.')
from cobol_frontend.cobol_lexer import COBOLLexer

source = sys.stdin.read()
try:
    lexer = COBOLLexer(source)
    tokens = lexer.tokenize()
    print(f'✅ SUCCESS: {len(tokens)} tokens')
except Exception as e:
    print(f'❌ FAILED: {e}')
"