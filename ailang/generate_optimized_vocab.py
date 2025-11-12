#!/usr/bin/env python3
"""
generate_optimized_vocab.py

Generates optimized vocabulary lookup from dictionary.txt
Uses chunked Branch statements - compiler-friendly approach
"""

import json
import re
import sys
from pathlib import Path


def parse_dictionary_entry(line):
    """Parse dictionary line: A1 /ei/ n. ..."""
    match = re.match(r'^([^\s/]+)', line)
    if match:
        word = match.group(1)
        definition = line[len(word):].strip()
        return word, definition
    return None, None


def load_dictionary(filepath):
    """Load dictionary.txt and build vocabulary mapping."""
    vocab = {}
    token_id = 0
    
    try:
        with open(filepath, 'r', encoding='utf-8') as f:
            content = f.read()
            
            # Try JSON first
            if content.strip().startswith('{'):
                try:
                    data = json.loads(content)
                    for key, value in data.items():
                        word, _ = parse_dictionary_entry(value)
                        if word:
                            vocab[token_id] = word
                            token_id += 1
                            if token_id >= 500:
                                break
                except json.JSONDecodeError:
                    pass
            
            # Parse as text if not JSON
            if not vocab:
                for line in content.split('\n'):
                    line = line.strip()
                    if not line:
                        continue
                    
                    word, definition = parse_dictionary_entry(line)
                    if word:
                        vocab[token_id] = word
                        token_id += 1
                        
                        if token_id >= 500:
                            print(f"Warning: Limiting to {token_id} words")
                            break
    
    except FileNotFoundError:
        print(f"Error: {filepath} not found")
        sys.exit(1)
    
    return vocab


def generate_optimized_vocab(vocab_dict, output_file):
    """Generate vocabulary with chunked Branch statements."""
    
    if not vocab_dict:
        print("Error: Empty vocabulary")
        sys.exit(1)
    
    max_token_id = max(vocab_dict.keys())
    
    print(f"Building vocabulary with {len(vocab_dict)} tokens (max ID: {max_token_id})")
    
    # Generate AILang code
    with open(output_file, 'w', encoding='utf-8') as f:
        f.write(f"""// Auto-generated SmolLM vocabulary
// {len(vocab_dict)} tokens using chunked Branch statements
// Each chunk handles up to 1000 tokens

""")
        
        # Generate Static strings - each word gets its own named entry
        f.write("// Static vocabulary strings\n")
        for token_id in sorted(vocab_dict.keys()):
            word = vocab_dict[token_id]
            
            # Escape for AILang string literal
            escaped = (word
                      .replace('\\', '\\\\')
                      .replace('"', '\\"')
                      .replace('\n', '\\n')
                      .replace('\r', '\\r')
                      .replace('\t', '\\t'))
            
            f.write(f'Static.Word_{token_id} = "{escaped}"\n')
        
        f.write("\n")
        
        # Generate chunked lookup
        chunk_size = 1000
        num_chunks = (max_token_id + chunk_size) // chunk_size
        
        f.write(f"""// Vocabulary lookup - chunked for compiler efficiency
// Input: State.token_id
// Output: generated_word_ptr

SubRoutine.GetWordFromID {{
    // Bounds check
    IfCondition Or(LessThan(State.token_id, 0), GreaterThan(State.token_id, {max_token_id})) ThenBlock: {{
        generated_word_ptr = 0
        ReturnValue(0)
    }}
    
    // Calculate chunk
    chunk_id = Divide(State.token_id, {chunk_size})
    
    // Route to chunk
    Branch chunk_id {{
""")
        
        # Generate chunk cases
        for chunk_idx in range(num_chunks):
            f.write(f'        Case {chunk_idx}: {{ RunTask(GetWord_Chunk_{chunk_idx}) }}\n')
        
        f.write("""        Default: { generated_word_ptr = 0 }
    }
}

""")
        
        # Generate chunk subroutines
        for chunk_idx in range(num_chunks):
            start_id = chunk_idx * chunk_size
            end_id = min(start_id + chunk_size, max_token_id + 1)
            
            f.write(f"""SubRoutine.GetWord_Chunk_{chunk_idx} {{
    Branch State.token_id {{
""")
            
            for token_id in range(start_id, end_id):
                if token_id in vocab_dict:
                    f.write(f'        Case {token_id}: {{ generated_word_ptr = AddressOf(Static.Word_{token_id}) }}\n')
            
            f.write("""        Default: { generated_word_ptr = 0 }
    }
}

""")
    
    print(f"\nGenerated: {output_file}")
    print(f"  {len(vocab_dict)} vocabulary entries")
    print(f"  {num_chunks} chunks of ~{chunk_size} entries each")
    print(f"  Chunk routing: O(1)")
    print(f"  Within chunk: O(log {chunk_size})")


def main():
    if len(sys.argv) < 2:
        print("Usage: python generate_optimized_vocab.py dictionary.txt [output.ailang]")
        sys.exit(1)
    
    input_file = sys.argv[1]
    output_file = sys.argv[2] if len(sys.argv) > 2 else "Library.smollm_vocabulary_static.ailang"
    
    print("=" * 70)
    print("SmolLM Vocabulary Generator")
    print("=" * 70)
    print()
    
    print(f"Loading: {input_file}")
    vocab = load_dictionary(input_file)
    
    if not vocab:
        print("Error: No vocabulary entries loaded")
        sys.exit(1)
    
    print(f"Loaded {len(vocab)} vocabulary entries")
    
    print("\nSample entries:")
    for i in range(min(5, len(vocab))):
        if i in vocab:
            print(f"  [{i}] = '{vocab[i]}'")
    print()
    
    generate_optimized_vocab(vocab, output_file)
    
    print("\n" + "=" * 70)
    print("✓ Complete!")
    print("=" * 70)


if __name__ == "__main__":
    main()