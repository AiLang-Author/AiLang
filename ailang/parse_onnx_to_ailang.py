#!/usr/bin/env python3
"""
Parse ONNX model and extract embeddings to AiLang format
No transformers, no PyTorch - pure ONNX parsing
"""

import onnx
import numpy as np
import sys
import struct

def find_embedding_tensor(model):
    """Find the embedding weight tensor in ONNX model"""
    print("Searching for embedding tensor...")
    
    candidates = []
    for initializer in model.graph.initializer:
        name = initializer.name
        shape = list(initializer.dims)
        
        # Look for embedding-like tensors (vocab_size × hidden_dim)
        if len(shape) == 2 and shape[0] > 1000:  # Vocab size usually > 1000
            candidates.append((name, shape, initializer))
            print(f"  Found: {name} - {shape}")
    
    if not candidates:
        print("ERROR: No embedding tensor found!")
        return None
    
    # Pick the largest one (usually the token embeddings)
    candidates.sort(key=lambda x: x[1][0], reverse=True)
    return candidates[0]

def extract_int8_data(initializer):
    """Extract INT8 data from ONNX tensor"""
    # ONNX int8 is stored as raw bytes
    raw_data = initializer.raw_data
    values = np.frombuffer(raw_data, dtype=np.int8)
    return values

def generate_ailang_fixedpool(name, values, start_idx=0):
    """Generate AiLang FixedPool with compact format"""
    lines = [f"FixedPool.{name} {{"]
    
    # Pack 10 values per line for readability
    for i in range(0, len(values), 10):
        chunk = values[i:i+10]
        entries = ' '.join([f'"v{start_idx+i+j}": Initialize={int(val)}' 
                           for j, val in enumerate(chunk)])
        lines.append(f"    {entries}")
    
    lines.append("}")
    return "\n".join(lines)

def main():
    if len(sys.argv) < 2:
        print("Usage: python3 parse_onnx_to_ailang.py model_int8.onnx [num_tokens]")
        sys.exit(1)
    
    onnx_path = sys.argv[1]
    num_tokens = int(sys.argv[2]) if len(sys.argv) > 2 else 1000
    
    print("="*60)
    print("ONNX to AiLang Converter")
    print("="*60)
    print(f"Input: {onnx_path}")
    print(f"Extracting first {num_tokens} tokens\n")
    
    # Load ONNX model
    print("Loading ONNX model...")
    model = onnx.load(onnx_path)
    
    # Find embedding tensor
    result = find_embedding_tensor(model)
    if not result:
        sys.exit(1)
    
    name, shape, initializer = result
    vocab_size, hidden_dim = shape
    
    print(f"\nUsing tensor: {name}")
    print(f"  Vocabulary size: {vocab_size}")
    print(f"  Hidden dimension: {hidden_dim}")
    
    # Extract data
    print("\nExtracting INT8 values...")
    values = extract_int8_data(initializer)
    values = values.reshape(vocab_size, hidden_dim)
    
    # Limit to first N tokens
    num_tokens = min(num_tokens, vocab_size)
    values = values[:num_tokens]
    
    print(f"  Extracted: {num_tokens} × {hidden_dim} = {num_tokens * hidden_dim} values")
    print(f"  Range: [{values.min()}, {values.max()}]")
    
    # Generate AiLang code
    output_file = "smollm_embeddings.ailang"
    print(f"\nGenerating AiLang code: {output_file}")
    
    with open(output_file, 'w') as f:
        # Header
        f.write("// AUTO-GENERATED FROM ONNX MODEL\n")
        f.write(f"// Model: SmolLM-135M (INT8)\n")
        f.write(f"// Source: {onnx_path}\n")
        f.write(f"// Tokens: {num_tokens} / {vocab_size}\n")
        f.write(f"// Hidden dim: {hidden_dim}\n\n")
        
        # Config
        f.write("FixedPool.SmolLMConfig {\n")
        f.write(f'    "vocab_size": Initialize={num_tokens}\n')
        f.write(f'    "hidden_dim": Initialize={hidden_dim}\n')
        f.write(f'    "full_vocab": Initialize={vocab_size}\n')
        f.write("}\n\n")
        
        # Embeddings in chunks
        flat_values = values.flatten()
        chunk_size = 10000
        
        num_chunks = (len(flat_values) + chunk_size - 1) // chunk_size
        
        for i in range(num_chunks):
            start = i * chunk_size
            end = min(start + chunk_size, len(flat_values))
            chunk = flat_values[start:end]
            
            pool_name = f"Embeddings_{i}"
            f.write(generate_ailang_fixedpool(pool_name, chunk, start))
            f.write("\n\n")
            
            if i % 5 == 0:
                print(f"  Chunk {i+1}/{num_chunks}...")
    
    import os
    file_size = os.path.getsize(output_file)
    
    print(f"\n{'='*60}")
    print("SUCCESS!")
    print(f"{'='*60}")
    print(f"Generated: {output_file}")
    print(f"Size: {file_size / 1024:.1f} KB")
    print(f"Values: {len(flat_values):,}")
    print(f"\nNext steps:")
    print(f"  1. Review the generated .ailang file")
    print(f"  2. Integrate into transformer inference code")
    print(f"  3. Update embedding lookup dimensions")
    print(f"     - vocab_size: {num_tokens}")
    print(f"     - hidden_dim: {hidden_dim}")

if __name__ == '__main__':
    main()