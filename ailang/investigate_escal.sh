#!/bin/bash
echo "1. AcronymDefinitions?"
grep -n "AcronymDefinitions" ESCAL056.ailang

echo -e "\n2. ESCAL056 acronym?"
grep -n "ESCAL056:" ESCAL056.ailang

echo -e "\n3. Function definitions (first 20):"
grep -n "^Function\." ESCAL056.ailang | head -20

echo -e "\n4. The problematic function:"
grep -n "ESCAL056_0000_MAINLINE_CONTROL" ESCAL056.ailang | head -10

echo -e "\n5. Imports?"
grep -n "LibraryImport\|^Import" ESCAL056.ailang
