#!/usr/bin/env python3
"""
AILang Compiler Infrastructure Analysis
Based on actual project structure
"""

import os
import re
from pathlib import Path
from collections import defaultdict

class AILangCompilerAnalyzer:
    def __init__(self, project_root="/mnt/c/Users/Sean/Documents/Ailang/ailang"):
        self.project_root = Path(project_root)
        self.compiler_dir = self.project_root / "ailang_compiler"
        self.modules_dir = self.compiler_dir / "modules"
        self.parser_dir = self.project_root / "ailang_parser"
        
        # Track issues
        self.variable_registrations = defaultdict(list)
        self.ast_traversals = defaultdict(list)
        self.compilation_phases = []
        self.module_dependencies = defaultdict(set)
        
    def analyze(self):
        """Run complete analysis"""
        print("=== AILANG COMPILER INFRASTRUCTURE ANALYSIS ===\n")
        print(f"Project root: {self.project_root}")
        print(f"Compiler modules: {self.modules_dir}\n")
        
        # Core modules we know exist from the project
        core_modules = [
            'memory_manager.py',
            'heap_allocator.py', 
            'lowlevel_ops.py',
            'registers.py',
            'stack_ops.py',
            'user_functions.py',
            'expressions.py',
            'control_flow.py',
            'string_ops.py'
        ]
        
        print("ANALYZING VARIABLE REGISTRATION POINTS:")
        print("-" * 40)
        
        # Find all variable registration points
        for module in core_modules:
            module_path = self.modules_dir / module
            if module_path.exists():
                self.analyze_module_variables(module_path)
        
        # Also check main compiler
        main_compiler = self.compiler_dir / "ailang_compiler.py"
        if main_compiler.exists():
            self.analyze_module_variables(main_compiler)
            
        print("\nANALYZING AST TRAVERSAL PATTERNS:")
        print("-" * 40)
        
        # Find traversal patterns
        for module in core_modules:
            module_path = self.modules_dir / module
            if module_path.exists():
                self.analyze_traversal_patterns(module_path)
                
        print("\nANALYZING COMPILATION FLOW:")
        print("-" * 40)
        self.analyze_compilation_flow()
        
        print("\n=== IDENTIFIED PROBLEMS ===\n")
        self.identify_problems()
        
        print("\n=== REFACTORING PLAN ===\n")
        self.generate_refactoring_plan()
        
    def analyze_module_variables(self, module_path):
        """Find variable handling in a module"""
        with open(module_path, 'r') as f:
            content = f.read()
            
        patterns = [
            (r'self\.compiler\.variables\[', 'Direct variable assignment'),
            (r'self\.variables\[', 'Local variable tracking'),
            (r'calculate_stack_size', 'Stack size calculation'),
            (r'stack_size \+=', 'Stack allocation'),
            (r'register_function', 'Function registration'),
            (r'resolve_acronym', 'Acronym resolution'),
            (r'compile_assignment', 'Assignment compilation')
        ]
        
        module_name = module_path.name
        found_any = False
        
        for pattern, description in patterns:
            matches = re.finditer(pattern, content)
            positions = []
            for match in matches:
                line_no = content[:match.start()].count('\n') + 1
                positions.append(line_no)
                
            if positions:
                if not found_any:
                    print(f"\n{module_name}:")
                    found_any = True
                print(f"  - {description}: lines {positions[:3]}{'...' if len(positions) > 3 else ''}")
                self.variable_registrations[module_name].append({
                    'type': description,
                    'lines': positions
                })
                
    def analyze_traversal_patterns(self, module_path):
        """Find AST traversal patterns"""
        with open(module_path, 'r') as f:
            content = f.read()
            
        patterns = [
            (r'def compile_(\w+)\(self, node\)', 'compile_X methods'),
            (r'for \w+ in node\.\w+:', 'Manual iteration'),
            (r'if type\((\w+)\).__name__', 'Type checking by name'),
            (r'isinstance\(\w+, \w+\)', 'isinstance checks'),
            (r'hasattr\(node, [\'"](\w+)[\'"]\)', 'hasattr checks'),
            (r'getattr\(node, \w+\)', 'Dynamic attribute access')
        ]
        
        module_name = module_path.name
        for pattern, description in patterns:
            matches = list(re.finditer(pattern, content))
            if matches:
                self.ast_traversals[module_name].append({
                    'pattern': description,
                    'count': len(matches)
                })
                
    def analyze_compilation_flow(self):
        """Extract compilation phases from main compiler"""
        main_compiler = self.compiler_dir / "ailang_compiler.py"
        if not main_compiler.exists():
            return
            
        with open(main_compiler, 'r') as f:
            lines = f.readlines()
            
        in_compile_method = False
        for i, line in enumerate(lines):
            if 'def compile(self' in line:
                in_compile_method = True
            elif in_compile_method and 'def ' in line:
                break
                
            if in_compile_method:
                if 'Phase' in line or 'PASS' in line or '#' in line:
                    phase_desc = line.strip()
                    if phase_desc:
                        print(f"  {phase_desc}")
                        self.compilation_phases.append(phase_desc)
                        
    def identify_problems(self):
        """Identify architectural problems"""
        problems = []
        
        # Problem 1: Variable registration scattered
        if len(self.variable_registrations) > 3:
            modules_with_vars = list(self.variable_registrations.keys())
            print(f"❌ Variable registration scattered across {len(modules_with_vars)} modules:")
            print(f"   {', '.join(modules_with_vars)}")
            problems.append("scattered_variables")
            
        # Problem 2: Multiple traversal patterns
        total_patterns = sum(len(patterns) for patterns in self.ast_traversals.values())
        if total_patterns > 5:
            print(f"❌ Found {total_patterns} different AST traversal patterns")
            unique_patterns = set()
            for patterns in self.ast_traversals.values():
                for p in patterns:
                    unique_patterns.add(p['pattern'])
            print(f"   Types: {', '.join(unique_patterns)}")
            problems.append("inconsistent_traversal")
            
        # Problem 3: calculate_stack_size incomplete
        print("❌ calculate_stack_size doesn't traverse Function/SubRoutine bodies")
        print("   This is why 'count' and other variables aren't found")
        problems.append("incomplete_prepass")
        
        # Problem 4: Multiple compilation models
        print("❌ Mixed compilation models:")
        print("   - Some variables allocated during pre-pass")
        print("   - Some during compilation") 
        print("   - Some at runtime (heap)")
        problems.append("mixed_models")
        
        return problems
        
    def generate_refactoring_plan(self):
        """Generate specific refactoring plan"""
        
        print("IMMEDIATE FIX (to get things working):")
        print("-" * 40)
        print("""
In memory_manager.py, update calculate_stack_size to be comprehensive:

def calculate_stack_size(self, node, depth=0):
    # Visit EVERYTHING
    if not hasattr(node, '__dict__'):
        return
        
    node_type = type(node).__name__
    
    # Handle assignments
    if node_type == 'Assignment':
        resolved = self.compiler.resolve_acronym_identifier(node.target)
        if resolved not in self.compiler.variables:
            self.compiler.stack_size += 16
            self.compiler.variables[resolved] = self.compiler.stack_size
            
    # CRITICAL: Handle Function and SubRoutine bodies
    if node_type in ('Function', 'FunctionDefinition', 'SubRoutine'):
        print(f"Scanning {node_type} {getattr(node, 'name', 'anonymous')}")
        if hasattr(node, 'body'):
            for stmt in node.body:
                self.calculate_stack_size(stmt, depth + 1)
                
    # Handle all other node types generically
    for attr in dir(node):
        if not attr.startswith('_'):
            val = getattr(node, attr)
            if hasattr(val, '__dict__'):
                self.calculate_stack_size(val, depth + 1)
            elif isinstance(val, list):
                for item in val:
                    if hasattr(item, '__dict__'):
                        self.calculate_stack_size(item, depth + 1)
""")

        print("\nLONG-TERM REFACTORING:")
        print("-" * 40)
        print("""
1. Create semantic_analyzer.py:
   - Single AST visitor that finds ALL symbols
   - Handles scoping properly
   - Runs ONCE before any code generation
   
2. Create symbol_table.py:
   - Unified symbol management
   - Tracks variables, functions, types
   - Handles name resolution
   
3. Simplify compile() in ailang_compiler.py:
   Phase 1: Parse all files
   Phase 2: Semantic analysis (new)
   Phase 3: Code generation
   Phase 4: Assembly
   
4. Remove scattered variable handling:
   - No more inline variable allocation
   - All symbols known before codegen
   - Clear separation of concerns
""")

if __name__ == "__main__":
    analyzer = AILangCompilerAnalyzer()
    analyzer.analyze()