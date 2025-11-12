#!/usr/bin/env python3
"""
NIST Test Suite Failure Pattern Analyzer

Analyzes the 28 failing tests to identify common patterns and prioritize fixes.
Run after compilation to categorize failures.
"""

import re
import json
from pathlib import Path
from collections import defaultdict
from typing import Dict, List, Tuple

class FailureAnalyzer:
    def __init__(self, test_dir: Path):
        self.test_dir = test_dir
        self.failures = []
        self.error_patterns = defaultdict(list)
        
    def load_failures_from_output(self, output_text: str):
        """Parse compilation output to extract failures"""
        lines = output_text.split('\n')
        
        current_file = None
        for line in lines:
            # Match: ✗ Failed at line X: error message
            if '✗ Failed' in line or 'Failed at line' in line:
                match = re.search(r'(\w+\.cbl).*line (\d+):\s*(.+)', line)
                if match:
                    filename, line_num, error = match.groups()
                    self.failures.append({
                        'file': filename,
                        'line': int(line_num),
                        'error': error.strip()
                    })
    
    def categorize_errors(self):
        """Group errors by pattern"""
        categories = {
            'european_decimal': [],
            'usage_clause': [],
            'pic_clause': [],
            'redefines': [],
            'occurs': [],
            'copy': [],
            'level_number': [],
            'sign_clause': [],
            'value_clause': [],
            'justified': [],
            'blank_when_zero': [],
            'synchronized': [],
            'missing_period': [],
            'unexpected_token': [],
            'invalid_syntax': [],
            'other': []
        }
        
        for failure in self.failures:
            error = failure['error'].lower()
            categorized = False
            
            # European decimal
            if 'european decimal' in error or ',44' in error:
                categories['european_decimal'].append(failure)
                categorized = True
            
            # USAGE issues
            elif 'usage' in error:
                categories['usage_clause'].append(failure)
                categorized = True
            
            # PIC issues
            elif 'pic' in error or 'picture' in error:
                categories['pic_clause'].append(failure)
                categorized = True
            
            # REDEFINES
            elif 'redefines' in error:
                categories['redefines'].append(failure)
                categorized = True
            
            # OCCURS
            elif 'occurs' in error:
                categories['occurs'].append(failure)
                categorized = True
            
            # COPY
            elif 'copy' in error:
                categories['copy'].append(failure)
                categorized = True
            
            # Level numbers
            elif 'level' in error:
                categories['level_number'].append(failure)
                categorized = True
            
            # SIGN clause
            elif 'sign' in error:
                categories['sign_clause'].append(failure)
                categorized = True
            
            # VALUE clause
            elif 'value' in error:
                categories['value_clause'].append(failure)
                categorized = True
            
            # JUSTIFIED
            elif 'justified' in error or 'just' in error:
                categories['justified'].append(failure)
                categorized = True
            
            # BLANK WHEN ZERO
            elif 'blank' in error:
                categories['blank_when_zero'].append(failure)
                categorized = True
            
            # SYNCHRONIZED
            elif 'synchronized' in error or 'sync' in error:
                categories['synchronized'].append(failure)
                categorized = True
            
            # Missing period
            elif 'period' in error or 'expected period' in error:
                categories['missing_period'].append(failure)
                categorized = True
            
            # Unexpected token
            elif 'unexpected token' in error:
                categories['unexpected_token'].append(failure)
                categorized = True
            
            # Invalid syntax (known bad tests)
            elif 'invalid' in error or 'malformed' in error:
                categories['invalid_syntax'].append(failure)
                categorized = True
            
            if not categorized:
                categories['other'].append(failure)
        
        return categories
    
    def print_report(self, categories: Dict[str, List]):
        """Print formatted analysis report"""
        print("\n" + "="*70)
        print("NIST TEST FAILURE ANALYSIS")
        print("="*70)
        print(f"\nTotal Failures: {len(self.failures)}")
        print(f"Pass Rate: {((459-28)/459)*100:.1f}%\n")
        
        # Sort categories by count
        sorted_cats = sorted(categories.items(), 
                           key=lambda x: len(x[1]), 
                           reverse=True)
        
        print("FAILURE BREAKDOWN BY CATEGORY:")
        print("-"*70)
        
        for category, failures in sorted_cats:
            if not failures:
                continue
            
            count = len(failures)
            pct = (count / len(self.failures)) * 100
            
            print(f"\n{category.upper().replace('_', ' ')}: {count} ({pct:.1f}%)")
            
            # Show first 3 examples
            for i, failure in enumerate(failures[:3], 1):
                print(f"  {i}. {failure['file']} line {failure['line']}")
                print(f"     {failure['error'][:60]}...")
            
            if len(failures) > 3:
                print(f"  ... and {len(failures)-3} more")
        
        print("\n" + "="*70)
        print("RECOMMENDED FIX PRIORITY:")
        print("="*70)
        
        # Prioritize by impact
        for category, failures in sorted_cats:
            if not failures:
                continue
            
            priority = self._get_priority(category, len(failures))
            fix_effort = self._get_effort(category)
            
            print(f"\n{priority} {category.replace('_', ' ').title()}")
            print(f"   Count: {len(failures)}")
            print(f"   Effort: {fix_effort}")
            print(f"   Impact: {len(failures)} tests")
    
    def _get_priority(self, category: str, count: int) -> str:
        """Determine fix priority"""
        if count >= 10:
            return "🔴 HIGH:"
        elif count >= 5:
            return "🟡 MED: "
        else:
            return "🟢 LOW: "
    
    def _get_effort(self, category: str) -> str:
        """Estimate fix effort"""
        easy = ['european_decimal', 'missing_period', 'invalid_syntax']
        medium = ['usage_clause', 'pic_clause', 'value_clause']
        hard = ['redefines', 'occurs', 'copy']
        
        if category in easy:
            return "Low (diagnostic/validation)"
        elif category in medium:
            return "Medium (parser enhancement)"
        elif category in hard:
            return "High (complex logic)"
        else:
            return "Unknown"
    
    def export_json(self, output_path: Path, categories: Dict):
        """Export analysis to JSON for further processing"""
        report = {
            'total_failures': len(self.failures),
            'pass_rate': ((459-28)/459)*100,
            'categories': {}
        }
        
        for category, failures in categories.items():
            if failures:
                report['categories'][category] = {
                    'count': len(failures),
                    'failures': failures
                }
        
        with open(output_path, 'w') as f:
            json.dump(report, f, indent=2)
        
        print(f"\nDetailed report exported to: {output_path}")


def main():
    """
    Usage:
    1. Run COBOL compilation and capture output
    2. Pass output to this script
    3. Review categorized failures
    4. Prioritize fixes
    
    Example:
        python3 failure_analyzer.py < compilation_output.txt
    """
    import sys
    
    # Read compilation output from stdin or file
    if len(sys.argv) > 1:
        with open(sys.argv[1]) as f:
            output = f.read()
    else:
        print("Reading from stdin... (paste compilation output, Ctrl+D when done)")
        output = sys.stdin.read()
    
    analyzer = FailureAnalyzer(Path("cobol_frontend/tests"))
    analyzer.load_failures_from_output(output)
    
    if not analyzer.failures:
        print("No failures detected in output!")
        return
    
    categories = analyzer.categorize_errors()
    analyzer.print_report(categories)
    
    # Export detailed JSON report
    analyzer.export_json(Path("failure_analysis.json"), categories)


if __name__ == '__main__':
    main()