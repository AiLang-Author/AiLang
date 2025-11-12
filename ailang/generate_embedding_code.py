#!/usr/bin/env python3
"""
Generate AILANG code for embedding pools and SQL loader
Fully automated - no manual work needed!
"""

def generate_fixedpool(token_id, num_dims=32):
    """Generate a FixedPool definition for one token's embeddings"""
    lines = [f"FixedPool.Embeddings_Token_{token_id} {{"]
    
    for i in range(0, num_dims, 4):
        dim_line = "    "
        for j in range(4):
            if i + j < num_dims:
                dim_line += f'"d{i+j}": Initialize=0   '
        lines.append(dim_line.rstrip())
    
    lines.append("}")
    return "\n".join(lines)

def generate_all_fixedpools(token_ids, num_dims=32):
    """Generate all FixedPool declarations"""
    code = []
    code.append("// ============================================================================")
    code.append("// EMBEDDING STORAGE (One FixedPool per token)")
    code.append("// ============================================================================")
    code.append("")
    
    for token_id in token_ids:
        code.append(generate_fixedpool(token_id, num_dims))
        code.append("")
    
    return "\n".join(code)

def generate_dimension_branch(token_id, num_dims=32):
    """Generate Branch statement for storing dimensions"""
    lines = ["            Branch dim {"]
    for d in range(num_dims):
        lines.append(f"                Case {d}: {{ Embeddings_Token_{token_id}.d{d} = val }}")
    lines.append("            }")
    return "\n".join(lines)

def generate_sql_loader(token_ids, num_dims=32):
    """Generate complete SQL loading code"""
    code = []
    
    code.append("// ============================================================================")
    code.append("// BULK SQL LOADER")
    code.append("// ============================================================================")
    code.append("")
    code.append("SubRoutine.BulkLoadEmbeddings {")
    code.append('    PrintMessage("[Startup] Loading embeddings from database...\\n")')
    code.append("")
    
    for token_id in token_ids:
        code.append(f"    // Token {token_id}")
        code.append(f'    query = "SELECT dimension, value FROM embeddings WHERE token_id = {token_id} AND dimension < {num_dims} ORDER BY dimension"')
        code.append(f'    result = PG_Query(SmolLM_DB.pg_conn, query)')
        code.append("")
        code.append('    IfCondition GreaterThan(result, 0) ThenBlock {')
        code.append('        row_count = XArray.XSize(result)')
        code.append('        i = 0')
        code.append('        WhileLoop LessThan(i, row_count) {')
        code.append('            row = XArray.XGet(result, i)')
        code.append('            dim_str = HashMap.HGetSimple(row, "dimension")')
        code.append('            val_str = HashMap.HGetSimple(row, "value")')
        code.append('            dim = StringToInt(dim_str)')
        code.append('            val = StringToInt(val_str)')
        code.append("")
        code.append(generate_dimension_branch(token_id, num_dims))
        code.append("")
        code.append('            i = Add(i, 1)')
        code.append('        }')
        code.append('        LoadStats.tokens_loaded = Add(LoadStats.tokens_loaded, 1)')
        code.append('        LoadStats.rows_processed = Add(LoadStats.rows_processed, row_count)')
        code.append('    }')
        code.append("")
    
    code.append('    PrintMessage("  ✓ Loaded ")')
    code.append('    PrintNumber(LoadStats.tokens_loaded)')
    code.append('    PrintMessage(" tokens\\n")')
    code.append("}")
    
    return "\n".join(code)

def generate_copy_code(token_id, num_dims=32):
    """Generate code to copy one token's embedding to CurrentEmbedding"""
    lines = [f"        Case {token_id}: {{"]
    for d in range(num_dims):
        lines.append(f"            CurrentEmbedding.d{d} = Embeddings_Token_{token_id}.d{d}")
    lines.append("        }")
    return "\n".join(lines)

def generate_lookup_function(token_ids, num_dims=32):
    """Generate lookup function to copy from pools to CurrentEmbedding"""
    code = []
    
    code.append("// ============================================================================")
    code.append("// LOOKUP FUNCTION (Copy from pool to CurrentEmbedding)")
    code.append("// ============================================================================")
    code.append("")
    code.append("SubRoutine.LoadEmbeddingFromMemory {")
    code.append("    Branch token_id {")
    
    for token_id in token_ids:
        code.append(generate_copy_code(token_id, num_dims))
    
    code.append("        Default: {")
    code.append('            PrintMessage("[WARNING] Unknown token_id: ")')
    code.append('            PrintNumber(token_id)')
    code.append('            PrintMessage("\\n")')
    code.append("            // Use fallback: modulo-based test data")
    code.append("            test_val = Modulo(token_id, 128)")
    for d in range(num_dims):
        code.append(f"            CurrentEmbedding.d{d} = Add(test_val, {d})")
    code.append("        }")
    code.append("    }")
    code.append("}")
    
    return "\n".join(code)

def main():
    import sys
    
    # Configurable token list
    # Start small, expand later
    token_ids = [
        0, 1, 2,           # Test tokens
        10,                # Newline
        32,                # Space
        97, 98, 99,        # a, b, c
        101,               # e
        104, 105,          # h, i
        108, 109, 110,     # l, m, n
        111,               # o
        114, 115, 116,     # r, s, t
        119, 121,          # w, y
    ]
    
    num_dims = 32  # Start with 32, expand to 576 later
    
    print("// ============================================================================")
    print("// AUTO-GENERATED EMBEDDING CODE")
    print(f"// Tokens: {len(token_ids)}, Dimensions: {num_dims}")
    print("// Generated by: generate_embedding_code.py")
    print("// ============================================================================")
    print()
    
    # Generate all three sections
    print(generate_all_fixedpools(token_ids, num_dims))
    print()
    print(generate_sql_loader(token_ids, num_dims))
    print()
    print(generate_lookup_function(token_ids, num_dims))

if __name__ == "__main__":
    main()