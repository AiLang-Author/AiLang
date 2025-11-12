echo "==================== BENCHMARK ===================="
echo "AILang Cat vs GNU Cat - 330k line COBOL file"
echo "===================================================="
echo ""
echo "AILang Cat (128KB buffers + fadvise64):"
time ./UnitTestCode/cat_exec cobol_frontend/tests/newcob.cbl >/dev/null
echo ""
echo "GNU Cat (128KB buffers + fadvise64):"
time /bin/cat cobol_frontend/tests/newcob.cbl >/dev/null
echo ""
echo "==================== SYSCALL COUNT ===================="
strace -c ./UnitTestCode/cat_exec cobol_frontend/tests/newcob.cbl >/dev/null 2>&1 | tail -5
echo ""
strace -c /bin/cat cobol_frontend/tests/newcob.cbl >/dev/null 2>&1 | tail -5