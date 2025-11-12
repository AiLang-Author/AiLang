#!/usr/bin/env python3
"""
generate_5000_tokens.py
Generate FixedPool definitions and SQL loader for 5000 tokens × 32 dimensions
"""

def generate_token_pools(num_tokens=5000, num_dims=32):
    """Generate FixedPool definitions for tokens"""
    output = []
    output.append("// ============================================================================")
    output.append("// TOKEN EMBEDDINGS - 5000 tokens × 32 dimensions")
    output.append("// Replace your existing Embeddings_Token_* FixedPools with this")
    output.append("// ============================================================================\n")
    
    for token_id in range(num_tokens):
        output.append(f"FixedPool.Embeddings_Token_{token_id} {{")
        
        # Generate in groups of 8 per line for readability
        for dim_group in range(0, num_dims, 8):
            line = "    "
            for i in range(8):
                dim = dim_group + i
                if dim < num_dims:
                    line += f'"d{dim}": Initialize=0   '
            output.append(line.rstrip())
        
        output.append("}\n")
        
        # Progress indicator every 500 tokens
        if (token_id + 1) % 500 == 0:
            output.append(f"// Progress: {token_id + 1}/{num_tokens} tokens defined\n")
    
    return "\n".join(output)

def generate_sql_loader(num_tokens=5000, num_dims=32):
    """Generate BulkLoadEmbeddings subroutine"""
    output = []
    output.append("// ============================================================================")
    output.append("// SQL LOADER - Loads 5000 tokens from PostgreSQL")
    output.append("// Replace your existing SubRoutine.BulkLoadEmbeddings with this")
    output.append("// ============================================================================\n")
    
    output.append("SubRoutine.BulkLoadEmbeddings {")
    output.append('    Debug("smollm.trace", level=1) {')
    output.append('        PrintMessage("[BulkLoadEmbeddings] ENTER\\n")')
    output.append('    }')
    output.append('    PrintMessage("[Startup] Loading embeddings from database...\\n")')
    output.append('    PrintMessage("  Loading 5000 tokens × 32 dimensions...\\n")')
    output.append("")
    output.append("    LoadStats.tokens_loaded = 0")
    output.append("")
    
    for token_id in range(num_tokens):
        output.append(f"    // Token {token_id}")
        output.append(f'    query = "SELECT dimension, value FROM embeddings WHERE token_id = {token_id} AND dimension < {num_dims} ORDER BY dimension"')
        output.append(f'    result = PG_Query(SmolLM_DB.pg_conn, query)')
        output.append("")
        output.append(f'    IfCondition GreaterThan(result, 0) ThenBlock: {{')
        output.append(f'        row_count = XArray.XSize(result)')
        output.append(f'        i = 0')
        output.append(f'        WhileLoop LessThan(i, row_count) {{')
        output.append(f'            row = XArray.XGet(result, i)')
        output.append(f'            dim_str = HashMap.HGetSimple(row, "dimension")')
        output.append(f'            val_str = HashMap.HGetSimple(row, "value")')
        output.append(f'            dim = StringToInt(dim_str)')
        output.append(f'            val = StringToInt(val_str)')
        output.append(f'')
        output.append(f'            Branch dim {{')
        
        # Generate all dimension cases
        for dim in range(num_dims):
            output.append(f'                Case {dim}: {{ Embeddings_Token_{token_id}.d{dim} = val }}')
        
        output.append(f'            }}')
        output.append(f'')
        output.append(f'            i = Add(i, 1)')
        output.append(f'        }}')
        output.append(f'        LoadStats.tokens_loaded = Add(LoadStats.tokens_loaded, 1)')
        output.append(f'        XStream.XDestroy(result)')
        output.append(f'    }}')
        output.append("")
        
        # Progress indicator every 500 tokens
        if (token_id + 1) % 500 == 0:
            output.append(f'    PrintMessage("  Loaded {token_id + 1}/5000 tokens...\\n")')
            output.append("")
    
    output.append('    PrintMessage("  ✓ Loaded ")')
    output.append('    PrintNumber(LoadStats.tokens_loaded)')
    output.append('    PrintMessage(" tokens\\n")')
    output.append('    Debug("smollm.trace", level=1) {')
    output.append('        PrintMessage("[BulkLoadEmbeddings] EXIT\\n")')
    output.append('    }')
    output.append("}")
    
    return "\n".join(output)

def main():
    import sys
    
    print("Generating 5000 tokens × 32 dimensions...")
    print("This will create ~2 files:")
    print("  1. token_pools_5000.ailang (~600KB) - FixedPool definitions")
    print("  2. sql_loader_5000.ailang (~8MB) - SQL loading code")
    print()
    
    # Generate token pools
    print("Generating token pools...")
    with open("token_pools_5000.ailang", "w") as f:
        f.write(generate_token_pools(5000, 32))
    print("✓ Created token_pools_5000.ailang")
    
    # Generate SQL loader
    print("Generating SQL loader...")
    with open("sql_loader_5000.ailang", "w") as f:
        f.write(generate_sql_loader(5000, 32))
    print("✓ Created sql_loader_5000.ailang")
    
    print()
    print("Integration steps:")
    print("  1. Find your existing Embeddings_Token_* FixedPools")
    print("  2. Replace them with contents of token_pools_5000.ailang")
    print("  3. Find SubRoutine.BulkLoadEmbeddings")
    print("  4. Replace it with contents of sql_loader_5000.ailang")
    print("  5. Compile (expect ~20-25 MB binary)")
    print("  6. First run will take 30-60 seconds to load from DB")
    print()
    print("WARNING: This will be a LARGE binary!")
    print("Expected compile time: 2-5 minutes")
    print("Expected binary size: 20-25 MB")
    print("Expected startup time: 30-60 seconds (loading from DB)")

if __name__ == "__main__":
    main()