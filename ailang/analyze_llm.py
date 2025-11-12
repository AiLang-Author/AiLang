#!/usr/bin/env python3
"""
Analyze the structure of large AILANG files to identify bottlenecks
"""

import sys
import re
from collections import defaultdict, Counter

def analyze_file(filename):
    """Analyze AILANG file structure"""
    
    print(f"\n{'='*70}")
    print(f"ANALYZING: {filename}")
    print(f"{'='*70}\n")
    
    stats = {
        'total_lines': 0,
        'fixedpool_count': 0,
        'fixedpool_fields': defaultdict(int),
        'branch_count': 0,
        'branch_cases': [],
        'function_count': 0,
        'loop_count': 0,
        'if_count': 0,
        'max_nesting': 0,
        'pool_by_prefix': Counter(),
    }
    
    current_nesting = 0
    max_nesting = 0
    current_pool = None
    current_branch_cases = 0
    
    with open(filename, 'r', encoding='utf-8', errors='ignore') as f:
        for line_num, line in enumerate(f, 1):
            stats['total_lines'] += 1
            
            # Progress indicator every 10K lines
            if line_num % 10000 == 0:
                print(f"  Processed {line_num:,} lines...")
            
            # Track nesting level
            if '{' in line:
                current_nesting += line.count('{')
            if '}' in line:
                current_nesting -= line.count('}')
            
            max_nesting = max(max_nesting, current_nesting)
            
            # Count structures
            if 'FixedPool.' in line:
                match = re.match(r'^\s*FixedPool\.([A-Za-z0-9_]+)', line)
                if match:
                    pool_name = match.group(1)
                    stats['fixedpool_count'] += 1
                    current_pool = pool_name
                    
                    # Extract prefix
                    prefix = pool_name.split('_')[0] if '_' in pool_name else pool_name
                    stats['pool_by_prefix'][prefix] += 1
            
            # Count fields in pools
            if current_pool and '"' in line and 'Initialize=' in line:
                stats['fixedpool_fields'][current_pool] += 1
            
            # Track when pool ends
            if current_pool and line.strip() == '}':
                current_pool = None
            
            # Count branches
            if 'Branch ' in line or 'Branch{' in line:
                stats['branch_count'] += 1
                current_branch_cases = 0
            
            # Count cases in branches
            if current_branch_cases >= 0 and ('Case ' in line or '"Case"' in line):
                current_branch_cases += 1
            
            # Branch ended - save case count
            if current_branch_cases > 0 and line.strip() == '}':
                stats['branch_cases'].append(current_branch_cases)
                current_branch_cases = 0
            
            # Count other structures
            if 'Function.' in line:
                stats['function_count'] += 1
            if 'WhileLoop' in line:
                stats['loop_count'] += 1
            if 'IfCondition' in line:
                stats['if_count'] += 1
    
    stats['max_nesting'] = max_nesting
    
    # Print summary
    print(f"\n{'='*70}")
    print("SUMMARY STATISTICS")
    print(f"{'='*70}\n")
    
    print(f"Total Lines:              {stats['total_lines']:,}")
    print(f"FixedPool Definitions:    {stats['fixedpool_count']:,}")
    print(f"Function Definitions:     {stats['function_count']:,}")
    print(f"Branch Statements:        {stats['branch_count']:,}")
    print(f"If Conditions:            {stats['if_count']:,}")
    print(f"While Loops:              {stats['loop_count']:,}")
    print(f"Maximum Nesting Depth:    {stats['max_nesting']}")
    
    # Analyze pools
    print(f"\n{'='*70}")
    print("FIXEDPOOL ANALYSIS")
    print(f"{'='*70}\n")
    
    total_fields = sum(stats['fixedpool_fields'].values())
    print(f"Total Pool Variables:     {total_fields:,}")
    print(f"Average Fields/Pool:      {total_fields / max(stats['fixedpool_count'], 1):.1f}")
    
    print(f"\nTop Pool Prefixes:")
    for prefix, count in stats['pool_by_prefix'].most_common(10):
        print(f"  {prefix:30s} {count:6,} pools")
    
    # Pool with most fields
    if stats['fixedpool_fields']:
        largest_pool = max(stats['fixedpool_fields'].items(), key=lambda x: x[1])
        print(f"\nLargest Pool: {largest_pool[0]} ({largest_pool[1]:,} fields)")
    
    # Analyze branches
    if stats['branch_cases']:
        print(f"\n{'='*70}")
        print("BRANCH ANALYSIS")
        print(f"{'='*70}\n")
        
        avg_cases = sum(stats['branch_cases']) / len(stats['branch_cases'])
        max_cases = max(stats['branch_cases'])
        min_cases = min(stats['branch_cases'])
        
        print(f"Total Branches:           {len(stats['branch_cases']):,}")
        print(f"Average Cases/Branch:     {avg_cases:.1f}")
        print(f"Max Cases in Branch:      {max_cases}")
        print(f"Min Cases in Branch:      {min_cases}")
        
        # Distribution
        case_dist = Counter(stats['branch_cases'])
        print(f"\nCase Count Distribution:")
        for cases, count in sorted(case_dist.items())[:20]:  # Top 20
            bar = '█' * min(50, count)
            print(f"  {cases:3d} cases: {count:6,} branches {bar}")
        
        # Calculate compilation impact
        total_comparisons = sum(cases * count for cases, count in case_dist.items())
        print(f"\nTotal Comparisons:        {total_comparisons:,}")
        print(f"  (Linear search through all branch cases)")
    
    # Memory estimates
    print(f"\n{'='*70}")
    print("MEMORY ESTIMATES")
    print(f"{'='*70}\n")
    
    pool_table_bytes = total_fields * 8
    print(f"Pool Table Size:          {pool_table_bytes:,} bytes ({pool_table_bytes / (1024*1024):.2f} MB)")
    
    # Estimate code size (rough)
    # Each branch with N cases ≈ 32 bytes per case + overhead
    estimated_branch_code = sum(cases * 32 for cases in stats['branch_cases'])
    print(f"Estimated Branch Code:    {estimated_branch_code:,} bytes ({estimated_branch_code / (1024*1024):.2f} MB)")
    
    # Estimate binary size
    estimated_binary = estimated_branch_code + pool_table_bytes + (stats['function_count'] * 1024)
    print(f"Estimated Binary Size:    {estimated_binary:,} bytes ({estimated_binary / (1024*1024):.2f} MB)")
    
    # Warnings
    print(f"\n{'='*70}")
    print("POTENTIAL ISSUES")
    print(f"{'='*70}\n")
    
    issues = []
    
    if total_fields > 100000:
        issues.append(f"⚠️  CRITICAL: {total_fields:,} pool variables (limit: ~131K)")
    
    if stats['max_nesting'] > 50:
        issues.append(f"⚠️  WARNING: Nesting depth {stats['max_nesting']} (may cause stack overflow)")
    
    if max_cases > 100:
        issues.append(f"⚠️  WARNING: Branch with {max_cases} cases (very slow linear search)")
    
    if stats['branch_count'] > 1000 and avg_cases > 20:
        issues.append(f"⚠️  CRITICAL: {stats['branch_count']:,} branches × {avg_cases:.0f} avg cases = {stats['branch_count'] * avg_cases:,.0f} comparisons")
        issues.append(f"    → Needs jump table optimization!")
    
    if estimated_binary > 50 * 1024 * 1024:
        issues.append(f"⚠️  WARNING: Estimated binary > 50MB (may exceed limits)")
    
    if issues:
        for issue in issues:
            print(issue)
    else:
        print("✓ No critical issues detected")
    
    print(f"\n{'='*70}\n")
    
    return stats


def main():
    if len(sys.argv) < 2:
        print("Usage: python3 analyze_smollm_structure.py <ailang_file>")
        sys.exit(1)
    
    filename = sys.argv[1]
    
    try:
        stats = analyze_file(filename)
        
        # Recommendations
        print("RECOMMENDATIONS:")
        print("="*70)
        
        if stats['branch_count'] > 100:
            print("\n1. IMPLEMENT JUMP TABLE OPTIMIZATION")
            print("   - Current: Linear search through all cases")
            print("   - Proposed: O(1) jump table for contiguous integer cases")
            print("   - Expected speedup: 10-100x for large branches")
        
        if stats['total_lines'] > 100000:
            print("\n2. ENABLE COMPILATION PROGRESS TRACKING")
            print("   - Add progress indicators every 10K nodes")
            print("   - Estimate time remaining")
        
        if stats['max_nesting'] > 30:
            print("\n3. INCREASE STACK SIZE")
            print("   - Call: resource.setrlimit(resource.RLIMIT_STACK, (16*1024*1024, -1))")
            print("   - Or run with: ulimit -s 16384")
        
        total_fields = sum(stats['fixedpool_fields'].values())
        if total_fields > 50000:
            print("\n4. POOL TABLE SIZE")
            print(f"   - Current: {total_fields:,} variables")
            print(f"   - Required: {total_fields * 8:,} bytes ({total_fields * 8 / 1024:.1f} KB)")
            print("   - Ensure allocate_pool_table() uses dynamic sizing")
        
        print("\n" + "="*70 + "\n")
        
    except FileNotFoundError:
        print(f"ERROR: File not found: {filename}")
        sys.exit(1)
    except Exception as e:
        print(f"ERROR: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()