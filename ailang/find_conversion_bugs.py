#!/usr/bin/env python3
"""
Fix for find_conversion_bugs.py - PIC 999V validation

The validator incorrectly flags PIC 999V as problematic.
This is a VALID pattern meaning "3 integer digits, 0 decimal places"

TRUTH TABLE: PIC V Position Validation
┌─────────────┬──────────────────────────────────────┐
│ PIC Pattern │ Valid?                               │
├─────────────┼──────────────────────────────────────┤
│ 9999        │ YES (no decimal)                     │
│ 999V99      │ YES (V in middle, 2 decimals)        │
│ 999V        │ YES (V at end, 0 decimals) ✓         │
│ V99         │ YES (V at start, all decimals)       │
│ 999VV       │ NO (multiple V invalid)              │
│ V           │ NO (V alone invalid)                 │
└─────────────┴──────────────────────────────────────┘
"""

def validate_pic_clause(pic_string: str) -> list:
    """
    Validate PIC clause with truth table approach
    
    Returns list of issues (empty list = valid)
    """
    issues = []
    
    if not pic_string:
        issues.append("Empty PIC clause")
        return issues
    
    pic = pic_string.strip().upper()
    
    # ═══════════════════════════════════════════════════════════════
    # Rule 1: Check for multiple V (invalid)
    # ═══════════════════════════════════════════════════════════════
    v_count = pic.count('V')
    if v_count > 1:
        issues.append("Multiple V indicators (only one allowed)")
    
    # ═══════════════════════════════════════════════════════════════
    # Rule 2: Check for V alone (invalid)
    # ═══════════════════════════════════════════════════════════════
    if pic == 'V':
        issues.append("V alone without digits")
    
    # ═══════════════════════════════════════════════════════════════
    # Rule 3: V at end is VALID (previously flagged as error)
    # ═══════════════════════════════════════════════════════════════
    # Examples: 999V, 9(5)V - these are valid!
    # They mean "N digits with 0 decimal places"
    # DO NOT flag as error
    
    # ═══════════════════════════════════════════════════════════════
    # Rule 4: Check for invalid characters after V
    # ═══════════════════════════════════════════════════════════════
    if 'V' in pic and v_count == 1:
        v_pos = pic.find('V')
        after_v = pic[v_pos + 1:]
        
        # Truth table: What can appear after V?
        # ┌─────────────┬───────────────────────────┐
        # │ After V     │ Valid?                    │
        # ├─────────────┼───────────────────────────┤
        # │ (empty)     │ YES (999V)                │
        # │ 9, Z, *     │ YES (digit indicators)    │
        # │ (n)         │ YES (repetition)          │
        # │ .           │ YES (999V.)               │
        # │ Letters     │ NO (except valid codes)   │
        # └─────────────┴───────────────────────────┘
        
        if after_v:
            # Remove trailing period (valid)
            after_v = after_v.rstrip('.')
            
            if after_v:
                # Check for valid digit indicators and repetition
                import re
                valid_pattern = re.compile(r'^[9Z\*\+\-]+(\(\d+\))?$')
                if not valid_pattern.match(after_v):
                    # Only flag if truly invalid characters
                    invalid_chars = set(after_v) - set('9Z*+-()0123456789')
                    if invalid_chars:
                        issues.append(f"Invalid characters after V: {invalid_chars}")
    
    return issues


# Test cases to verify the fix
if __name__ == '__main__':
    test_cases = [
        ("999V", []),           # Valid - 0 decimals
        ("999V99", []),         # Valid - 2 decimals
        ("V99", []),            # Valid - all decimals
        ("9999", []),           # Valid - no decimal
        ("999VV", ["Multiple V indicators (only one allowed)"]),  # Invalid
        ("V", ["V alone without digits"]),  # Invalid
        ("999V.", []),          # Valid - trailing period
        ("9(5)V", []),          # Valid - repetition with V at end
    ]
    
    print("=" * 70)
    print("PIC VALIDATION TEST CASES")
    print("=" * 70)
    
    all_passed = True
    for pic, expected_issues in test_cases:
        actual_issues = validate_pic_clause(pic)
        passed = (len(actual_issues) == len(expected_issues))
        
        status = "✓" if passed else "✗"
        print(f"{status} PIC {pic:10s} → {actual_issues if actual_issues else 'VALID'}")
        
        if not passed:
            print(f"   Expected: {expected_issues}")
            print(f"   Got:      {actual_issues}")
            all_passed = False
    
    print("=" * 70)
    if all_passed:
        print("✅ All tests passed!")
    else:
        print("❌ Some tests failed")
    print("=" * 70)