#!/usr/bin/env python3
"""
Load ONNX embeddings directly into PostgreSQL
"""
import onnx
from onnx import numpy_helper
import numpy as np
import psycopg2
import sys

def find_embedding_tensor(model):
    """Find the embedding weight tensor"""
    print("Searching for embedding tensor...")
    candidates = []
    
    for initializer in model.graph.initializer:
        name = initializer.name
        shape = list(initializer.dims)
        
        # Look for embedding-like tensors
        if len(shape) == 2 and shape[0] > 1000:
            candidates.append((name, shape, initializer))
            print(f"  Found: {name} - {shape}")
    
    if not candidates:
        print("ERROR: No embedding tensor found!")
        return None
    
    # Pick largest (usually token embeddings)
    candidates.sort(key=lambda x: x[1][0], reverse=True)
    return candidates[0]

def extract_int8_data(initializer):
    """Extract INT8 data from ONNX tensor"""
    raw_data = initializer.raw_data
    values = np.frombuffer(raw_data, dtype=np.int8)
    return values

def load_to_postgres(onnx_path, num_tokens=1000):
    print("="*60)
    print("ONNX → PostgreSQL Loader")
    print("="*60)
    
    # Load ONNX
    print(f"\nLoading {onnx_path}...")
    model = onnx.load(onnx_path)
    
    # Find embeddings
    result = find_embedding_tensor(model)
    if not result:
        sys.exit(1)
    
    name, shape, initializer = result
    vocab_size, hidden_dim = shape
    
    print(f"\nUsing: {name}")
    print(f"  Vocab: {vocab_size}")
    print(f"  Hidden dim: {hidden_dim}")
    
    # Extract INT8 values
    print("\nExtracting INT8 values...")
    values = extract_int8_data(initializer)
    values = values.reshape(vocab_size, hidden_dim)
    
    # Limit to num_tokens
    num_tokens = min(num_tokens, vocab_size)
    values = values[:num_tokens]
    
    print(f"  Selected: {num_tokens} tokens")
    print(f"  Total values: {num_tokens * hidden_dim:,}")
    print(f"  Range: [{values.min()}, {values.max()}]")
    
    # Connect to PostgreSQL
    print("\nConnecting to PostgreSQL...")
    conn = psycopg2.connect(
        dbname="smollm",
        user="sean",
        host="localhost"
    )
    cursor = conn.cursor()
    
    # Insert weights
    print(f"\nInserting weights...")
    batch = []
    batch_size = 10000
    total = 0
    non_zero = 0
    
    for token_id in range(num_tokens):
        if token_id % 100 == 0:
            print(f"  Token {token_id}/{num_tokens}...")
        
        for dim in range(hidden_dim):
            val = int(values[token_id, dim])
            batch.append((token_id, dim, val))
            
            if val != 0:
                non_zero += 1
            
            if len(batch) >= batch_size:
                cursor.executemany(
                    "INSERT INTO embeddings VALUES (%s, %s, %s)",
                    batch
                )
                conn.commit()
                total += len(batch)
                batch = []
    
    # Insert remaining
    if batch:
        cursor.executemany(
            "INSERT INTO embeddings VALUES (%s, %s, %s)",
            batch
        )
        conn.commit()
        total += len(batch)
    
    # Save stats
    cursor.execute("""
        INSERT INTO embedding_stats 
        (total_tokens, total_dimensions, non_zero_count, min_value, max_value)
        VALUES (%s, %s, %s, %s, %s)
    """, (num_tokens, hidden_dim, non_zero, int(values.min()), int(values.max())))
    conn.commit()
    
    cursor.close()
    conn.close()
    
    print(f"\n{'='*60}")
    print("SUCCESS!")
    print(f"{'='*60}")
    print(f"Inserted: {total:,} weights")
    print(f"Non-zero: {non_zero:,} ({100*non_zero/total:.1f}%)")
    print(f"\nVerify with:")
    print(f"  psql smollm -c 'SELECT COUNT(*) FROM embeddings;'")

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: python3 onnx_to_postgres.py model_int8.onnx [num_tokens]")
        sys.exit(1)
    
    onnx_path = sys.argv[1]
    num_tokens = int(sys.argv[2]) if len(sys.argv) > 2 else 1000
    
    load_to_postgres(onnx_path, num_tokens)
