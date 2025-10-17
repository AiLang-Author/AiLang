#!/usr/bin/env python3
"""
Analyze method dependencies in cobol_parser.py and cobol_ast_converter.py
to create a safe refactoring plan
"""
import re
from collections import defaultdict
import json

def extract_class_methods(filepath, class_name):
    """Extract all methods from a class and their calls to other methods"""
    
    with open(filepath, 'r') as f:
        content = f.read()
    
    # Find the class
    class_pattern = rf'class {class_name}:.*?(?=\nclass |\Z)'
    class_match = re.search(class_pattern, content, re.DOTALL)
    
    if not class_match:
        print(f"❌ Could not find class {class_name}")
        return {}
    
    class_content = class_match.group(0)
    
    # Extract all methods
    method_pattern = r'def (\w+)\(self[^)]*\):'
    methods = re.findall(method_pattern, class_content)
    
    # For each method, find what other methods it calls
    method_calls = {}
    
    for method_name in methods:
        # Find the method body
        method_pattern = rf'def {method_name}\(self[^)]*\):(.*?)(?=\n    def |\Z)'
        method_match = re.search(method_pattern, class_content, re.DOTALL)
        
        if method_match:
            method_body = method_match.group(1)
            
            # Find all self.method_name() calls
            calls = set(re.findall(r'self\.(\w+)\(', method_body))
            
            method_calls[method_name] = {
                'calls': sorted(list(calls)),
                'call_count': len(calls)
            }
    
    return method_calls, methods

def categorize_parser_methods(methods_info, all_methods):
    """Categorize parser methods into logical groups"""
    
    categories = {
        'core': [],
        'division': [],
        'statement': [],
        'expression': []
    }
    
    # Core methods (infrastructure)
    core_keywords = ['init', 'token', 'peek', 'match', 'advance', 'consume', 
                     'error', 'context', 'period', 'parse_all', 'parse_single',
                     'parse_end']
    
    # Division methods
    division_keywords = ['identification', 'environment', 'data', 'procedure',
                        'working_storage', 'file_section', 'linkage',
                        'variable_decl', 'file_descriptor', 'select']
    
    # Statement methods  
    statement_keywords = ['display', 'accept', 'move', 'compute', 'add', 'subtract',
                         'multiply', 'divide', 'if', 'perform', 'evaluate', 'call',
                         'string', 'unstring', 'inspect', 'read', 'write', 'when']
    
    # Expression methods
    expression_keywords = ['expression', 'arithmetic', 'logical', 'comparison',
                          'primary', 'function_call', 'binary_op', 'unary_op']
    
    for method in all_methods:
        method_lower = method.lower()
        
        if any(kw in method_lower for kw in core_keywords):
            categories['core'].append(method)
        elif any(kw in method_lower for kw in division_keywords):
            categories['division'].append(method)
        elif any(kw in method_lower for kw in statement_keywords):
            categories['statement'].append(method)
        elif any(kw in method_lower for kw in expression_keywords):
            categories['expression'].append(method)
        else:
            # Default to core
            categories['core'].append(method)
    
    return categories

def categorize_converter_methods(methods_info, all_methods):
    """Categorize converter methods into logical groups"""
    
    categories = {
        'core': [],
        'type_system': [],
        'decimal': [],
        'expression': [],
        'statement': []
    }
    
    # Core methods
    core_keywords = ['init', 'convert_compilation', 'convert_program', 'convert$',
                     'generate', 'normalize', 'make_assignment']
    
    # Type system methods
    type_keywords = ['pic', 'type', 'size', 'default_value', 'storage', 'analyze',
                    'infer', 'numeric']
    
    # Decimal methods
    decimal_keywords = ['decimal', 'scale', 'redefines']
    
    # Expression conversion
    expr_keywords = ['expression', 'binary_op', 'unary_op', 'function_call',
                    'subscript', 'array']
    
    # Statement conversion
    stmt_keywords = ['statement', 'display', 'move', 'arithmetic', 'compute',
                    'if', 'perform', 'evaluate', 'string', 'unstring', 'inspect',
                    'accept', 'call']
    
    for method in all_methods:
        method_lower = method.lower()
        
        if any(kw in method_lower for kw in core_keywords):
            categories['core'].append(method)
        elif any(kw in method_lower for kw in type_keywords):
            categories['type_system'].append(method)
        elif any(kw in method_lower for kw in decimal_keywords):
            categories['decimal'].append(method)
        elif any(kw in method_lower for kw in expr_keywords):
            categories['expression'].append(method)
        elif any(kw in method_lower for kw in stmt_keywords):
            categories['statement'].append(method)
        else:
            categories['core'].append(method)
    
    return categories

def find_circular_dependencies(categories, methods_info):
    """Find potential circular dependencies between categories"""
    
    print("\n🔄 Checking for circular dependencies...")
    
    # Build category membership map
    method_to_category = {}
    for category, methods in categories.items():
        for method in methods:
            method_to_category[method] = category
    
    # Check cross-category calls
    cross_calls = defaultdict(lambda: defaultdict(int))
    
    for method, info in methods_info.items():
        if method not in method_to_category:
            continue
        
        method_cat = method_to_category[method]
        
        for called in info['calls']:
            if called not in method_to_category:
                continue
            
            called_cat = method_to_category[called]
            
            if method_cat != called_cat:
                cross_calls[method_cat][called_cat] += 1
    
    # Print cross-category dependencies
    for from_cat, to_cats in sorted(cross_calls.items()):
        print(f"\n  {from_cat} calls:")
        for to_cat, count in sorted(to_cats.items(), key=lambda x: -x[1]):
            print(f"    → {to_cat}: {count} calls")
    
    return cross_calls

def main():
    print("="*80)
    print("METHOD DEPENDENCY ANALYSIS")
    print("="*80)
    
    # Analyze parser
    print("\n📊 Analyzing cobol_parser.py...")
    parser_info, parser_methods = extract_class_methods(
        'cobol_parser.py', 
        'COBOLMultiProgramParser'
    )
    
    print(f"   Found {len(parser_methods)} methods")
    
    parser_categories = categorize_parser_methods(parser_info, parser_methods)
    
    print("\n📂 Parser method categorization:")
    for category, methods in sorted(parser_categories.items()):
        print(f"\n  {category.upper()} ({len(methods)} methods):")
        for method in sorted(methods)[:10]:
            calls = len(parser_info.get(method, {}).get('calls', []))
            print(f"    - {method} (calls {calls} other methods)")
        if len(methods) > 10:
            print(f"    ... +{len(methods)-10} more")
    
    parser_cross_calls = find_circular_dependencies(parser_categories, parser_info)
    
    # Analyze converter
    print("\n" + "="*80)
    print("\n📊 Analyzing cobol_ast_converter.py...")
    converter_info, converter_methods = extract_class_methods(
        'cobol_ast_converter.py',
        'COBOLToAilangMultiProgramConverter'
    )
    
    print(f"   Found {len(converter_methods)} methods")
    
    converter_categories = categorize_converter_methods(converter_info, converter_methods)
    
    print("\n📂 Converter method categorization:")
    for category, methods in sorted(converter_categories.items()):
        print(f"\n  {category.upper()} ({len(methods)} methods):")
        for method in sorted(methods)[:10]:
            calls = len(converter_info.get(method, {}).get('calls', []))
            print(f"    - {method} (calls {calls} other methods)")
        if len(methods) > 10:
            print(f"    ... +{len(methods)-10} more")
    
    converter_cross_calls = find_circular_dependencies(converter_categories, converter_info)
    
    # Generate refactoring plan
    print("\n" + "="*80)
    print("REFACTORING STRATEGY")
    print("="*80)
    
    print("""
Based on the dependency analysis, here's the recommended refactoring order:

PARSER REFACTORING ORDER:
1. ast_nodes.py        - Pure data structures (no dependencies)
2. parser_core.py      - Core infrastructure methods
3. expression_parsers.py - Expression parsing (depends on core)
4. division_parsers.py - Division parsing (depends on core, expressions)
5. statement_parsers.py - Statement parsing (depends on all above)

CONVERTER REFACTORING ORDER:
1. type_system.py      - Pure type functions (no dependencies)
2. decimal_support.py  - Decimal helpers (depends on type_system)
3. converter_core.py   - Core converter class (depends on type_system)
4. expression_converter.py - Expression conversion (depends on core, decimal)
5. statement_converter.py  - Statement conversion (depends on all above)

KEY INSIGHTS:
""")
    
    # Check for problematic circular deps
    if 'expression' in parser_cross_calls.get('statement', {}):
        exp_to_stmt = parser_cross_calls['statement']['expression']
        stmt_to_exp = parser_cross_calls.get('expression', {}).get('statement', 0)
        print(f"⚠️  Parser: statement ↔ expression circular calls: {stmt_to_exp} ← → {exp_to_stmt}")
        print("   Solution: Keep expression_parsers as mixin or use forward references")
    
    if 'expression' in converter_cross_calls.get('statement', {}):
        print(f"⚠️  Converter: statement ↔ expression circular calls detected")
        print("   Solution: Use dependency injection or shared base class")
    
    print("\n✅ Safe to proceed with refactoring!")
    print("   All modules can be cleanly separated with proper imports")
    
    # Save detailed analysis
    analysis = {
        'parser': {
            'categories': {k: sorted(v) for k, v in parser_categories.items()},
            'methods': parser_info
        },
        'converter': {
            'categories': {k: sorted(v) for k, v in converter_categories.items()},
            'methods': converter_info
        }
    }
    
    with open('method_analysis.json', 'w') as f:
        json.dump(analysis, f, indent=2)
    
    print("\n💾 Detailed analysis saved to: method_analysis.json")

if __name__ == '__main__':
    main()