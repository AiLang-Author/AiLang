#!/usr/bin/env python3
"""
AILang Compiler Deep Issue Scanner
Finds ALL architectural problems and outputs actionable report
"""

import os
import re
import ast
import json
from pathlib import Path
from collections import defaultdict
from datetime import datetime

class CompilerIssueHunter:
    def __init__(self, root="/mnt/c/Users/Sean/Documents/Ailang/ailang"):
        self.root = Path(root)
        self.issues = []
        self.stats = defaultdict(int)
        
        # File patterns to scan
        self.compiler_files = list((self.root / "ailang_compiler").rglob("*.py"))
        self.parser_files = list((self.root / "ailang_parser").rglob("*.py"))
        
    def hunt(self):
        """Run all issue detection"""
        print("🔍 HUNTING FOR COMPILER ISSUES...\n")
        
        # Variable handling issues
        self.find_variable_chaos()
        
        # AST traversal issues
        self.find_traversal_mess()
        
        # Memory management issues
        self.find_memory_problems()
        
        # Compilation phase issues
        self.find_phase_violations()
        
        # Module coupling issues
        self.find_coupling_problems()
        
        # Forward reference issues
        self.find_forward_refs()
        
        # Error handling issues
        self.find_error_handling()
        
        # Stack corruption risks
        self.find_stack_risks()
        
        # Generate report
        self.generate_report()
        
    def find_variable_chaos(self):
        """Find all the places variables are handled differently"""
        print("Scanning for variable handling chaos...")
        
        patterns = [
            # Direct variable manipulation
            (r'self\.compiler\.variables\[([^\]]+)\]\s*=', 'Direct variable assignment'),
            (r'self\.variables\[([^\]]+)\]\s*=', 'Local variable assignment'),
            (r'if\s+\w+\s+not\s+in\s+self\.\w*\.?variables:', 'Variable existence check'),
            (r'self\.compiler\.stack_size\s*\+=', 'Stack size manipulation'),
            (r'self\.stack_size\s*\+=', 'Local stack manipulation'),
            
            # Variable resolution
            (r'resolve_acronym_identifier', 'Acronym resolution'),
            (r'resolve_identifier', 'Identifier resolution'),
            
            # Variable discovery
            (r'calculate_stack_size', 'Stack calculation'),
            (r'scan_for_locals', 'Local scanning'),
            (r'discover_pool_variables', 'Pool discovery'),
            
            # Registration patterns
            (r'register_function', 'Function registration'),
            (r'register_variable', 'Variable registration'),
        ]
        
        for file in self.compiler_files:
            content = file.read_text()
            for pattern, desc in patterns:
                matches = list(re.finditer(pattern, content))
                if matches:
                    for match in matches:
                        line_no = content[:match.start()].count('\n') + 1
                        self.issues.append({
                            'category': 'VARIABLE_CHAOS',
                            'file': str(file.relative_to(self.root)),
                            'line': line_no,
                            'issue': desc,
                            'code': match.group(0)[:80]
                        })
                        self.stats['variable_chaos'] += 1
                        
    def find_traversal_mess(self):
        """Find all different AST traversal patterns"""
        print("Scanning for AST traversal inconsistencies...")
        
        patterns = [
            # Manual traversal
            (r'for\s+\w+\s+in\s+node\.\w+:', 'Manual node iteration'),
            (r'if\s+hasattr\(node,\s*[\'"](\w+)[\'"]\):', 'hasattr checking'),
            (r'getattr\(node,\s*[\'"]?(\w+)', 'Dynamic attribute access'),
            
            # Type checking
            (r'type\((\w+)\)\.__name__\s*==', 'Type name comparison'),
            (r'isinstance\(\w+,\s*(\w+)\)', 'isinstance check'),
            (r'if\s+node\.\w+\s*==\s*[\'"](\w+)[\'"]:', 'Node type field check'),
            
            # Visitor patterns
            (r'def\s+compile_(\w+)\(self,\s*node', 'compile_X pattern'),
            (r'def\s+visit_(\w+)\(self', 'visit_X pattern'),
            (r'self\.compile_node\(', 'Recursive compile'),
            
            # Incomplete traversal
            (r'#\s*TODO.*traverse', 'TODO traverse comment'),
            (r'pass\s*#.*handle.*later', 'Deferred handling'),
        ]
        
        for file in self.compiler_files:
            content = file.read_text()
            for pattern, desc in patterns:
                matches = list(re.finditer(pattern, content, re.IGNORECASE))
                if matches:
                    for match in matches:
                        line_no = content[:match.start()].count('\n') + 1
                        self.issues.append({
                            'category': 'TRAVERSAL_MESS',
                            'file': str(file.relative_to(self.root)),
                            'line': line_no,
                            'issue': desc,
                            'code': match.group(0)[:80]
                        })
                        self.stats['traversal_mess'] += 1
                        
    def find_memory_problems(self):
        """Find memory management issues"""
        print("Scanning for memory management problems...")
        
        patterns = [
            # Multiple allocation methods
            (r'Allocate\(', 'Direct Allocate call'),
            (r'mmap.*syscall', 'Direct mmap'),
            (r'PoolAllocate', 'Pool allocation'),
            (r'ArrayCreate', 'Array allocation'),
            (r'self\.heap\.', 'Heap allocator'),
            
            # Missing cleanup
            (r'Allocate.*\n(?!.*Free)', 'Allocation without free'),
            (r'mmap(?!.*munmap)', 'mmap without munmap'),
            
            # Stack issues
            (r'rbp\s*-\s*\d+', 'Hardcoded stack offset'),
            (r'rsp.*0x[0-9a-f]+', 'Direct RSP manipulation'),
            
            # Alignment issues
            (r'[^&]\s+7\s*\)', 'Potential alignment issue'),
        ]
        
        for file in self.compiler_files:
            content = file.read_text()
            for pattern, desc in patterns:
                matches = list(re.finditer(pattern, content))
                if matches:
                    for match in matches:
                        line_no = content[:match.start()].count('\n') + 1
                        self.issues.append({
                            'category': 'MEMORY_PROBLEMS',
                            'file': str(file.relative_to(self.root)),
                            'line': line_no,
                            'issue': desc,
                            'code': match.group(0)[:80]
                        })
                        self.stats['memory_problems'] += 1
                        
    def find_phase_violations(self):
        """Find compilation phase violations"""
        print("Scanning for compilation phase violations...")
        
        patterns = [
            # Phase mixing
            (r'self\.asm\.emit.*\n.*self\.compiler\.variables\[', 'Code emission during discovery'),
            (r'calculate_stack_size.*\n.*emit_', 'Emission in pre-pass'),
            (r'register_function.*\n.*compile_', 'Registration during compilation'),
            
            # Forward references
            (r'#.*forward.*ref', 'Forward reference comment'),
            (r'fixup.*later', 'Deferred fixup'),
            (r'TODO.*resolve', 'Unresolved TODO'),
            
            # Order dependencies
            (r'must.*before', 'Order dependency'),
            (r'CRITICAL.*order', 'Critical ordering'),
        ]
        
        for file in self.compiler_files:
            content = file.read_text()
            for pattern, desc in patterns:
                matches = list(re.finditer(pattern, content, re.IGNORECASE))
                if matches:
                    for match in matches:
                        line_no = content[:match.start()].count('\n') + 1
                        self.issues.append({
                            'category': 'PHASE_VIOLATIONS',
                            'file': str(file.relative_to(self.root)),
                            'line': line_no,
                            'issue': desc,
                            'code': match.group(0)[:80]
                        })
                        self.stats['phase_violations'] += 1
                        
    def find_coupling_problems(self):
        """Find tight coupling between modules"""
        print("Scanning for module coupling issues...")
        
        for file in self.compiler_files:
            content = file.read_text()
            
            # Count cross-module references
            refs = re.findall(r'self\.compiler\.(\w+)\.', content)
            if len(refs) > 10:
                unique_refs = list(set(refs))  # Convert set to list first
                self.issues.append({
                    'category': 'HIGH_COUPLING',
                    'file': str(file.relative_to(self.root)),
                    'line': 0,
                    'issue': f'High coupling - {len(refs)} cross-module refs',
                    'code': f'References: {", ".join(unique_refs[:5])}...'
                })
                self.stats['coupling'] += 1
                
    def find_forward_refs(self):
        """Find forward reference problems"""
        print("Scanning for forward reference issues...")
        
        patterns = [
            (r'self\.\w+_fixups', 'Fixup list'),
            (r'pending_\w+', 'Pending operation'),
            (r'deferred_\w+', 'Deferred operation'),
            (r'#.*resolve.*later', 'Resolve later comment'),
        ]
        
        for file in self.compiler_files:
            content = file.read_text()
            for pattern, desc in patterns:
                matches = list(re.finditer(pattern, content, re.IGNORECASE))
                for match in matches:
                    line_no = content[:match.start()].count('\n') + 1
                    self.issues.append({
                        'category': 'FORWARD_REFS',
                        'file': str(file.relative_to(self.root)),
                        'line': line_no,
                        'issue': desc,
                        'code': match.group(0)
                    })
                    self.stats['forward_refs'] += 1
                    
    def find_error_handling(self):
        """Find missing or bad error handling"""
        print("Scanning for error handling issues...")
        
        patterns = [
            (r'except:(?:\s*$|\s*pass)', 'Bare except'),
            (r'except.*:.*pass', 'Silent exception'),
            (r'#.*TODO.*error', 'TODO error handling'),
            (r'raise\s+ValueError\([\'"]Undeclared', 'Late error detection'),
        ]
        
        for file in self.compiler_files:
            content = file.read_text()
            for pattern, desc in patterns:
                matches = list(re.finditer(pattern, content, re.IGNORECASE))
                for match in matches:
                    line_no = content[:match.start()].count('\n') + 1
                    self.issues.append({
                        'category': 'ERROR_HANDLING',
                        'file': str(file.relative_to(self.root)),
                        'line': line_no,
                        'issue': desc,
                        'code': match.group(0)
                    })
                    self.stats['error_handling'] += 1
                    
    def find_stack_risks(self):
        """Find stack corruption risks"""
        print("Scanning for stack corruption risks...")
        
        patterns = [
            (r'-offset.*\s*\+\s*\d+', 'Offset arithmetic'),
            (r'rbp.*-.*\].*=', 'Stack write'),
            (r'struct\.pack.*-\w+', 'Negative offset packing'),
            (r'0x[0-9a-f]+.*rbp', 'Hardcoded RBP offset'),
        ]
        
        for file in self.compiler_files:
            content = file.read_text()
            for pattern, desc in patterns:
                matches = list(re.finditer(pattern, content))
                for match in matches:
                    line_no = content[:match.start()].count('\n') + 1
                    self.issues.append({
                        'category': 'STACK_RISK',
                        'file': str(file.relative_to(self.root)),
                        'line': line_no,
                        'issue': desc,
                        'code': match.group(0)
                    })
                    self.stats['stack_risks'] += 1
                    
    def generate_report(self):
        """Generate comprehensive report"""
        report_file = self.root / "COMPILER_ISSUES_REPORT.md"
        
        with open(report_file, 'w') as f:
            f.write("# AILang Compiler Issues Report\n")
            f.write(f"Generated: {datetime.now()}\n\n")
            
            # Summary
            f.write("## Summary\n")
            f.write(f"- Total Issues Found: {len(self.issues)}\n")
            for category, count in self.stats.items():
                f.write(f"- {category}: {count} issues\n")
            f.write("\n")
            
            # Critical issues
            f.write("## CRITICAL ISSUES TO FIX FIRST\n\n")
            
            # Group by category
            by_category = defaultdict(list)
            for issue in self.issues:
                by_category[issue['category']].append(issue)
                
            # Variable chaos is most critical
            if 'VARIABLE_CHAOS' in by_category:
                f.write("### 1. Variable Registration Chaos\n")
                f.write("Variables are registered in multiple places with no coordination:\n\n")
                for issue in by_category['VARIABLE_CHAOS'][:10]:
                    f.write(f"- `{issue['file']}:{issue['line']}` - {issue['issue']}\n")
                    f.write(f"  ```{issue['code']}```\n")
                f.write("\n")
                
            # Traversal mess
            if 'TRAVERSAL_MESS' in by_category:
                f.write("### 2. AST Traversal Inconsistency\n")
                f.write("Multiple different traversal patterns causing incomplete coverage:\n\n")
                for issue in by_category['TRAVERSAL_MESS'][:10]:
                    f.write(f"- `{issue['file']}:{issue['line']}` - {issue['issue']}\n")
                f.write("\n")
                
            # Full details
            f.write("## Full Issue List\n\n")
            for category, issues in by_category.items():
                f.write(f"### {category} ({len(issues)} issues)\n")
                for issue in issues[:20]:  # First 20 of each
                    f.write(f"- `{issue['file']}:{issue['line']}`: {issue['issue']}\n")
                if len(issues) > 20:
                    f.write(f"- ... and {len(issues)-20} more\n")
                f.write("\n")
                
            # Action items
            f.write("## IMMEDIATE ACTION ITEMS\n\n")
            f.write("""
1. **Fix calculate_stack_size in memory_manager.py**
   - Add Function/SubRoutine body traversal
   - Line ~176
   
2. **Create symbol_table.py**
   - Single source of truth for all symbols
   - Replace scattered variable handling
   
3. **Create semantic_analyzer.py**
   - Single-pass complete AST traversal
   - Find all symbols before code generation
   
4. **Refactor compile() in ailang_compiler.py**
   - Clear phase separation
   - No variable allocation during code generation
   
5. **Remove variable handling from:**
   - lowlevel_ops.py
   - string_ops.py  
   - user_functions.py
   - All modules except symbol_table
""")
            
        # Also save as JSON for programmatic use
        json_file = self.root / "compiler_issues.json"
        with open(json_file, 'w') as f:
            json.dump({
                'summary': dict(self.stats),
                'issues': self.issues
            }, f, indent=2)
            
        print(f"\n📋 Report saved to: {report_file}")
        print(f"📊 JSON data saved to: {json_file}")
        print(f"\n🎯 Total issues found: {len(self.issues)}")
        print("\nLoad these files into your project knowledge for targeted refactoring!")

if __name__ == "__main__":
    hunter = CompilerIssueHunter()
    hunter.hunt()