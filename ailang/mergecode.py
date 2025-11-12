#!/usr/bin/env python3
"""
Merge generated embedding code into the console template
"""

import re
import sys

def read_file(filename):
    """Read a file and return its contents"""
    try:
        with open(filename, 'r') as f:
            return f.read()
    except FileNotFoundError:
        print(f"ERROR: File not found: {filename}")
        sys.exit(1)

def extract_fixedpools(content):
    """Extract all FixedPool declarations"""
    # Find all FixedPool blocks
    pattern = r'(FixedPool\.Embeddings_Token_\d+\s*\{[^}]+\})'
    matches = re.findall(pattern, content, re.DOTALL)
    if matches:
        return "\n\n".join(matches)
    return None

def extract_subroutine(content, subroutine_name):
    """Extract a complete subroutine"""
    # Find SubRoutine.Name { ... }
    pattern = f'(SubRoutine\\.{subroutine_name}\\s*\\{{.*?^\\}})'
    match = re.search(pattern, content, re.MULTILINE | re.DOTALL)
    if match:
        return match.group(1)
    return None

def main():
    if len(sys.argv) < 3:
        print("Usage: python3 merge_generated_code.py <generated_file> <template_file> [output_file]")
        print()
        print("Example:")
        print("  python3 merge_generated_code.py generated_embeddings.ailang smollm_console_with_real_data.ailang smollm_merged.ailang")
        sys.exit(1)
    
    generated_file = sys.argv[1]
    template_file = sys.argv[2]
    output_file = sys.argv[3] if len(sys.argv) > 3 else "smollm_console_merged.ailang"
    
    print(f"Reading generated code from: {generated_file}")
    print(f"Reading template from: {template_file}")
    
    # Read files
    generated_content = read_file(generated_file)
    template_content = read_file(template_file)
    
    # Extract sections from generated file
    print("\nExtracting sections...")
    
    # Section 1: FixedPools
    fixedpools = extract_fixedpools(generated_content)
    
    # Section 2: SQL Loader
    sql_loader = extract_subroutine(generated_content, "BulkLoadEmbeddings")
    
    # Section 3: Lookup function
    lookup_func = extract_subroutine(generated_content, "LoadEmbeddingFromMemory")
    
    if not fixedpools:
        print("ERROR: Could not extract FixedPools!")
        print("Looking for pattern: FixedPool.Embeddings_Token_N { ... }")
        sys.exit(1)
    
    if not sql_loader:
        print("ERROR: Could not extract BulkLoadEmbeddings subroutine!")
        print("Looking for: SubRoutine.BulkLoadEmbeddings { ... }")
        sys.exit(1)
        
    if not lookup_func:
        print("ERROR: Could not extract LoadEmbeddingFromMemory subroutine!")
        print("Looking for: SubRoutine.LoadEmbeddingFromMemory { ... }")
        sys.exit(1)
    
    print("  ✓ Extracted FixedPools")
    print("  ✓ Extracted BulkLoadEmbeddings")
    print("  ✓ Extracted LoadEmbeddingFromMemory")
    
    # Count tokens
    token_count = len(re.findall(r'FixedPool\.Embeddings_Token_\d+', fixedpools))
    print(f"\n  Found {token_count} token embeddings")
    
    # Replace placeholders in template
    print("\nMerging sections into template...")
    
    result = template_content
    
    # Replace FixedPools - find the placeholder section
    placeholder_pattern = r'(// PLACEHOLDER - Replace with generated FixedPools.*?)(FixedPool\.Embeddings_Token_\d+\s*\{[^}]+\}.*?)(// ====)'
    if re.search(placeholder_pattern, result, re.DOTALL):
        result = re.sub(
            placeholder_pattern,
            r'\1\n' + fixedpools + '\n\n\3',
            result,
            flags=re.DOTALL
        )
        print("  ✓ Merged FixedPools")
    else:
        print("  ⚠ Could not find FixedPools placeholder, appending to file")
        # Just insert before transformer data structures
        result = result.replace(
            "// TRANSFORMER DATA STRUCTURES",
            fixedpools + "\n\n// TRANSFORMER DATA STRUCTURES"
        )
    
    # Replace BulkLoadEmbeddings
    bulk_pattern = r'(SubRoutine\.BulkLoadEmbeddings\s*\{.*?^\})'
    if re.search(bulk_pattern, result, re.MULTILINE | re.DOTALL):
        result = re.sub(
            bulk_pattern,
            sql_loader,
            result,
            flags=re.MULTILINE | re.DOTALL
        )
        print("  ✓ Merged BulkLoadEmbeddings")
    else:
        print("  ⚠ Could not find BulkLoadEmbeddings, appending")
    
    # Replace LoadEmbeddingFromMemory
    lookup_pattern = r'(SubRoutine\.LoadEmbeddingFromMemory\s*\{.*?^\})'
    if re.search(lookup_pattern, result, re.MULTILINE | re.DOTALL):
        result = re.sub(
            lookup_pattern,
            lookup_func,
            result,
            flags=re.MULTILINE | re.DOTALL
        )
        print("  ✓ Merged LoadEmbeddingFromMemory")
    else:
        print("  ⚠ Could not find LoadEmbeddingFromMemory, appending")
    
    # Write output
    print(f"\nWriting merged file to: {output_file}")
    with open(output_file, 'w') as f:
        f.write(result)
    
    print(f"\n✓ SUCCESS! Merged code written to {output_file}")
    print(f"\nYou can now compile it:")
    print(f"  ./ailang_compiler {output_file}")

if __name__ == "__main__":
    main()