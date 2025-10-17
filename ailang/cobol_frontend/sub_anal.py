#!/usr/bin/env python3
"""
Debug script to analyze IC107A subroutines and their parameters
"""

def analyze_ic107a_subroutines(ailang_file):
    """Analyze all IC107A-related subroutines"""
    with open(ailang_file, 'r') as f:
        lines = f.readlines()
    
    ic107a_subroutines = []
    current_sub = None
    brace_count = 0
    
    for i, line in enumerate(lines, 1):
        stripped = line.strip()
        
        # Detect SubRoutine.IC107A*
        if stripped.startswith('SubRoutine.IC107A'):
            sub_name = stripped.split('{')[0].replace('SubRoutine.', '').strip()
            current_sub = {
                'name': sub_name,
                'start_line': i,
                'content': [line],
                'linkage_refs': [],
                'vars_refs': []
            }
            brace_count = line.count('{') - line.count('}')
            ic107a_subroutines.append(current_sub)
        
        elif current_sub is not None:
            current_sub['content'].append(line)
            brace_count += line.count('{') - line.count('}')
            
            # Track pool references
            if 'COBOL_IC107A_LINKAGE.' in line:
                var = line.split('COBOL_IC107A_LINKAGE.')[1].split(None, 1)[0].rstrip(',)')
                current_sub['linkage_refs'].append((i, var, line.strip()))
            if 'COBOL_IC107A_VARS.' in line:
                var = line.split('COBOL_IC107A_VARS.')[1].split(None, 1)[0].rstrip(',)')
                current_sub['vars_refs'].append((i, var, line.strip()))
            
            # End of subroutine
            if brace_count == 0:
                current_sub['end_line'] = i
                current_sub = None
    
    return ic107a_subroutines

def print_analysis(subroutines):
    """Print analysis of subroutines"""
    print("=" * 80)
    print(f"FOUND {len(subroutines)} IC107A SUBROUTINES")
    print("=" * 80)
    
    for sub in subroutines:
        print(f"\n📍 SubRoutine.{sub['name']}")
        print(f"   Lines: {sub['start_line']}-{sub['end_line']}")
        print(f"   Size: {len(sub['content'])} lines")
        
        if sub['linkage_refs']:
            print(f"   🔗 LINKAGE refs ({len(sub['linkage_refs'])}):")
            for line_no, var, code in sub['linkage_refs'][:5]:  # Show first 5
                print(f"      L{line_no}: {var} -> {code[:60]}...")
        
        if sub['vars_refs']:
            print(f"   📦 VARS refs ({len(sub['vars_refs'])}):")
            for line_no, var, code in sub['vars_refs'][:5]:
                print(f"      L{line_no}: {var} -> {code[:60]}...")
        
        # Show first few lines of content
        print(f"   📝 First 5 lines:")
        for line in sub['content'][:5]:
            print(f"      {line.rstrip()}")
    
    print("\n" + "=" * 80)
    print("SUMMARY BY TYPE")
    print("=" * 80)
    
    # Group by what they do
    empty_subs = [s for s in subroutines if len(s['content']) <= 3]
    linkage_only = [s for s in subroutines if s['linkage_refs'] and not s['vars_refs']]
    vars_only = [s for s in subroutines if s['vars_refs'] and not s['linkage_refs']]
    mixed = [s for s in subroutines if s['linkage_refs'] and s['vars_refs']]
    
    print(f"Empty/minimal: {len(empty_subs)}")
    print(f"LINKAGE only: {len(linkage_only)}")
    print(f"VARS only: {len(vars_only)}")
    print(f"Mixed refs: {len(mixed)}")
    
    if empty_subs:
        print("\n🔹 Empty/Minimal SubRoutines:")
        for s in empty_subs[:10]:
            print(f"   - {s['name']}")

def find_idn2_paragraph(subroutines):
    """Find and analyze the IDN2 subroutine"""
    idn2_subs = [s for s in subroutines if 'IDN2' in s['name']]
    
    if idn2_subs:
        print("\n" + "=" * 80)
        print("🔍 IDN2-RELATED SUBROUTINES")
        print("=" * 80)
        for sub in idn2_subs:
            print(f"\n📌 {sub['name']}")
            print(f"   Lines: {sub['start_line']}-{sub['end_line']}")
            print(f"\n   Full content:")
            for line in sub['content']:
                print(f"   {line.rstrip()}")
    else:
        print("\n⚠️  No IDN2-related subroutines found!")

def analyze_linkage_confusion():
    """Check if level 77 LINKAGE variables are being confused with paragraphs"""
    print("\n" + "=" * 80)
    print("🔬 ANALYZING LINKAGE vs PARAGRAPH CONFUSION")
    print("=" * 80)
    print("\nIC107A LINKAGE SECTION should contain:")
    print("  77  IDN2  USAGE IS INDEX")
    print("  01  GROUP-1")
    print("      02  DN1 PICTURE X OCCURS 10 TIMES INDEXED BY IN3")
    print("  01  GROUP-2")
    print("      02  GROUP-21")
    print("          06  DN2 PIC X OCCURS 10 TIMES")
    print("\nIC107A PROCEDURE DIVISION paragraphs should be:")
    print("  LINK-TEST-02-01")
    print("  LINK-TEST-02-02")
    print("  LINK-TEST-03-01")
    print("  LINK-TEST-03-02")
    print("  LINK-TEST-04-01")
    print("  LINK-TEST-04-02")
    print("  LINK-TEST-05-01")
    print("  LINK-TEST-05-02")
    print("  LINK-TEST-06")
    print("  EXIT-IC107")
    print("\n⚠️  IDN2 is NOT a paragraph - it's a LINKAGE variable!")
    print("   If SubRoutine.IC107A_IDN2 exists, that's the bug!")

if __name__ == '__main__':
    import sys
    
    if len(sys.argv) < 2:
        print("Usage: python3 debug_ic107a_subroutines.py <ailang_file>")
        sys.exit(1)
    
    ailang_file = sys.argv[1]
    
    print("Analyzing", ailang_file)
    subs = analyze_ic107a_subroutines(ailang_file)
    print_analysis(subs)
    find_idn2_paragraph(subs)
    analyze_linkage_confusion()