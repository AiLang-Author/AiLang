#!/usr/bin/env python3

# Copyright (c) 2025 Sean Collins, 2 Paws Machine and Engineering. All rights reserved.
#
# Licensed under the Sean Collins Software License (SCSL). See the LICENSE file in the root directory of this project
# for the full terms and conditions, including restrictions on forking, corporate use, and permissions for private/teaching purposes.

"""
Enhanced Tiny LLM Trainer - NumPy only version
Trains a small transformer matching the enhanced AILANG architecture
"""

import numpy as np
import struct
import time

class TinyTransformerNumPy:
    """
    Enhanced transformer using only NumPy
    Matches enhanced AILANG architecture with 2 layers, 256 hidden dim
    """
    
    def __init__(self, vocab_size):
        self.vocab_size = vocab_size
        self.hidden_dim = 512  # Bigger!
        self.num_layers = 2    # Now 2 layers
        self.num_heads = 16    # Bigger! (hidden_dim / head_dim)
        self.head_dim = 32
        self.max_seq_len = 128 # Doubled
        self.lr = 0.001       # Lower learning rate for bigger model
        
        # Adam optimizer parameters
        self.beta1 = 0.9
        self.beta2 = 0.999
        self.epsilon = 1e-8
        self.t = 0 # Timestep for Adam
        
        # Calculate total parameters
        self.total_params = (
            self.vocab_size * self.hidden_dim +  # embeddings
            self.num_layers * 3 * self.hidden_dim * self.hidden_dim + # Q,K,V per layer
            self.hidden_dim * self.vocab_size    # Output projection
        )
        
        print(f"ðŸ§  Model Configuration:")
        print(f"  Vocab: {self.vocab_size}")
        print(f"  Hidden: {self.hidden_dim}")
        print(f"  Layers: {self.num_layers}")
        print(f"  Heads: {self.num_heads}")
        print(f"  Total Parameters: {self.total_params:,}")
        print()
        
        # Initialize weights with Xavier/He initialization
        scale = np.sqrt(2.0 / self.hidden_dim)
        
        self.embedding = np.random.randn(self.vocab_size, self.hidden_dim) * scale * 0.02
        
        # Multiple layers of attention weights
        self.w_q = []
        self.w_k = []
        self.w_v = []
        
        for layer in range(self.num_layers):
            self.w_q.append(np.random.randn(self.hidden_dim, self.hidden_dim) * scale)
            self.w_k.append(np.random.randn(self.hidden_dim, self.hidden_dim) * scale)
            self.w_v.append(np.random.randn(self.hidden_dim, self.hidden_dim) * scale)
        
        # Output projection
        self.output_proj = np.random.randn(self.hidden_dim, self.vocab_size) * scale
        
        # Initialize Adam optimizer moments
        self.m_embedding = np.zeros_like(self.embedding)
        self.v_embedding = np.zeros_like(self.embedding)
        
        self.m_w_q = [np.zeros_like(w) for w in self.w_q]
        self.v_w_q = [np.zeros_like(w) for w in self.w_q]
        self.m_w_k = [np.zeros_like(w) for w in self.w_k]
        self.v_w_k = [np.zeros_like(w) for w in self.w_k]
        self.m_w_v = [np.zeros_like(w) for w in self.w_v]
        self.v_w_v = [np.zeros_like(w) for w in self.w_v]
        
        self.m_output_proj = np.zeros_like(self.output_proj)
        self.v_output_proj = np.zeros_like(self.output_proj)
        
        # Training statistics
        self.loss_history = []
    
    def gelu(self, x):
        """
        GELU Approximation (HardSwish variant) to perfectly match the AILANG implementation.
        This is critical for preventing train/inference skew.
        """
        # Matches: x * clamp( (x * 1.702) * 0.2 + 0.5, 0, 1 )
        hard_sigmoid = np.clip((x * 1.702) * 0.2 + 0.5, 0, 1)
        return x * hard_sigmoid

    def adam_update(self, weight, grad, m, v):
        """Performs an Adam optimization step."""
        # Update biased first moment estimate
        m = self.beta1 * m + (1 - self.beta1) * grad
        # Update biased second raw moment estimate
        v = self.beta2 * v + (1 - self.beta2) * (grad**2)
        
        # Compute bias-corrected first moment estimate
        m_hat = m / (1 - self.beta1**self.t)
        # Compute bias-corrected second raw moment estimate
        v_hat = v / (1 - self.beta2**self.t)
        
        # Update parameters
        weight -= self.lr * m_hat / (np.sqrt(v_hat) + self.epsilon)
        
        return m, v
    
    def softmax(self, x, temperature=1.0):
        """Stable softmax with temperature"""
        x = x / temperature
        exp_x = np.exp(x - np.max(x, axis=-1, keepdims=True))
        return exp_x / np.sum(exp_x, axis=-1, keepdims=True)
    
    def layer_norm(self, x, eps=1e-5):
        """Layer normalization"""
        mean = np.mean(x, axis=-1, keepdims=True)
        var = np.var(x, axis=-1, keepdims=True)
        return (x - mean) / np.sqrt(var + eps)
    
    def attention(self, Q, K, V, mask=None):
        """Multi-head attention"""
        seq_len = Q.shape[0]
        
        # Scaled dot-product attention
        scores = Q @ K.T / np.sqrt(self.head_dim) # Correct scaling is by head_dim
        
        if mask is not None:
            scores = scores + mask
        
        attn_weights = self.softmax(scores)
        output = attn_weights @ V
        
        return output, attn_weights
    
    def transformer_block(self, x, layer_idx):
        """Single transformer block"""
        # Multi-head attention
        Q = x @ self.w_q[layer_idx]
        K = x @ self.w_k[layer_idx]
        V = x @ self.w_v[layer_idx]
        
        # Create causal mask
        seq_len = x.shape[0]
        mask = np.triu(np.ones((seq_len, seq_len)), k=1) * -1e9
        
        attn_output, _ = self.attention(Q, K, V, mask)
        
        # Residual connection and layer norm
        x = self.layer_norm(x + attn_output)
        
        # Feedforward with GELU
        ff_output = self.gelu(x)
        
        # Another residual connection
        x = self.layer_norm(x + ff_output)
        
        return x
    
    def forward(self, tokens):
        """Forward pass through the model"""
        seq_len = len(tokens)
        
        # Embedding lookup
        x = np.array([self.embedding[t] for t in tokens])
        
        # Pass through transformer layers
        # Store intermediate outputs for backpropagation
        intermediate_outputs = [x]
        for layer_idx in range(self.num_layers):
            x = self.transformer_block(x, layer_idx)
            intermediate_outputs.append(x)
        
        # Output projection
        logits = x @ self.output_proj
        
        # Return logits and the list of all layer outputs
        return logits, intermediate_outputs
    
    def train_step(self, input_tokens, target_tokens):
        """Training step with proper gradient computation"""
        self.t += 1 # Increment timestep for Adam

        # Forward pass
        logits, intermediate_outputs = self.forward(input_tokens)
        transformer_output = intermediate_outputs[-1] # Final layer's output
        
        # Cross-entropy loss
        probs = self.softmax(logits)
        loss = -np.mean(np.log(probs[np.arange(len(target_tokens)), target_tokens] + 1e-9))

        # --- Backpropagation ---
        # This version backprops through the output layer, embeddings, and
        # provides a gradient approximation for the final attention layer.
        
        # 1. Gradient of the loss w.r.t. logits
        d_logits = probs.copy()
        d_logits[np.arange(len(target_tokens)), target_tokens] -= 1
        d_logits /= len(target_tokens) # Normalize gradient

        # 2. Gradient for the output projection weights
        # d_output_proj = transformer_output.T @ d_logits
        d_output_proj = transformer_output.T @ d_logits
        
        # 3. Gradient for the transformer output (to be backpropped to embeddings)
        # d_transformer_output = d_logits @ self.output_proj.T
        d_transformer_output = d_logits @ self.output_proj.T

        # --- Weight Updates ---
        
        np.clip(d_output_proj, -1.0, 1.0, out=d_output_proj)
        self.m_output_proj, self.v_output_proj = self.adam_update(
            self.output_proj, d_output_proj, self.m_output_proj, self.v_output_proj
        )
        
        # Update embeddings
        # We create a zero-gradient for the whole embedding table and add the gradients
        # for the tokens that were actually used in the input sequence.
        d_embedding = np.zeros_like(self.embedding)
        for i, token_idx in enumerate(input_tokens):
            # Add the gradient for this token's embedding
            d_embedding[token_idx] += d_transformer_output[i]
        np.clip(d_embedding, -1.0, 1.0, out=d_embedding)
        self.m_embedding, self.v_embedding = self.adam_update(
            self.embedding, d_embedding, self.m_embedding, self.v_embedding
        )

        # --- Propagate gradient back one layer (approximately) ---
        # This gives us an error signal for the output of the first layer.
        # We approximate the gradient w.r.t the input of the final block by
        # back-propagating through the final layer's Q,K,V weights.
        d_layer1_output_approx = d_transformer_output @ (self.w_q[-1].T + self.w_k[-1].T + self.w_v[-1].T)

        # Update attention weights
        for layer_idx in range(self.num_layers):
            if layer_idx == self.num_layers - 1:
                # Update Layer 1 (the last layer)
                d_attn_approx = d_transformer_output
                x_in = intermediate_outputs[layer_idx] # Input to this layer
                
                d_w_q = x_in.T @ d_attn_approx
                d_w_k = x_in.T @ d_attn_approx
                d_w_v = x_in.T @ d_attn_approx
                
                np.clip(d_w_q, -1.0, 1.0, out=d_w_q)
                np.clip(d_w_k, -1.0, 1.0, out=d_w_k)
                np.clip(d_w_v, -1.0, 1.0, out=d_w_v)

                self.m_w_q[layer_idx], self.v_w_q[layer_idx] = self.adam_update(
                    self.w_q[layer_idx], d_w_q, self.m_w_q[layer_idx], self.v_w_q[layer_idx]
                )
                self.m_w_k[layer_idx], self.v_w_k[layer_idx] = self.adam_update(
                    self.w_k[layer_idx], d_w_k, self.m_w_k[layer_idx], self.v_w_k[layer_idx]
                )
                self.m_w_v[layer_idx], self.v_w_v[layer_idx] = self.adam_update(
                    self.w_v[layer_idx], d_w_v, self.m_w_v[layer_idx], self.v_w_v[layer_idx]
                )
            else:
                # Update Layer 0 (and any other earlier layers)
                d_attn_approx = d_layer1_output_approx # Use the back-propagated error
                x_in = intermediate_outputs[layer_idx] # Input to this layer
                
                d_w_q = x_in.T @ d_attn_approx
                d_w_k = x_in.T @ d_attn_approx
                d_w_v = x_in.T @ d_attn_approx

                np.clip(d_w_q, -1.0, 1.0, out=d_w_q)
                np.clip(d_w_k, -1.0, 1.0, out=d_w_k)
                np.clip(d_w_v, -1.0, 1.0, out=d_w_v)
                
                self.m_w_q[layer_idx], self.v_w_q[layer_idx] = self.adam_update(
                    self.w_q[layer_idx], d_w_q, self.m_w_q[layer_idx], self.v_w_q[layer_idx]
                )
                self.m_w_k[layer_idx], self.v_w_k[layer_idx] = self.adam_update(
                    self.w_k[layer_idx], d_w_k, self.m_w_k[layer_idx], self.v_w_k[layer_idx]
                )
                self.m_w_v[layer_idx], self.v_w_v[layer_idx] = self.adam_update(
                    self.w_v[layer_idx], d_w_v, self.m_w_v[layer_idx], self.v_w_v[layer_idx]
                )
        
        return loss
    
    def generate(self, prompt_tokens, max_length=50, temperature=0.8):
        """Generate text from a prompt"""
        generated = list(prompt_tokens)
        
        for _ in range(max_length - len(prompt_tokens)):
            # Get logits for current sequence
            logits, _ = self.forward(generated)
            
            # Take last token's logits
            next_token_logits = logits[-1]
            
            # Apply temperature and sample
            probs = self.softmax(next_token_logits, temperature)
            next_token = np.random.choice(self.vocab_size, p=probs)
            
            generated.append(next_token)
            
            # Stop if we generate end token (0 or newline)
            if next_token in [0, 10]:
                break
        
        return generated

# This function MUST be outside the class!
def export_weights_for_ailang(model, filename="tiny_llm.weights"):
    """Export weights in AILANG format"""
    
    print(f"\nðŸ“ Exporting weights to {filename}")
    
    # Convert to fixed-point (scale by 1000) and save as int64 for AILANG compatibility
    def to_fixed_point(array):
        return (array * 1000).round().astype(np.int64)
    
    with open(filename, 'wb') as f:
        print(f"  Total parameters: {model.total_params:,}")

        # Write vocab_size as a single 64-bit integer at the start of the file
        f.write(struct.pack('<q', model.vocab_size))
        print(f"  âœ“ Header: vocab_size ({model.vocab_size}) written")
        
        # Write weights in order expected by AILANG
        # Order: embeddings, then for each layer: W_q, W_k, W_v
        
        # Flatten and write embeddings - now as int64
        embeddings_fp = to_fixed_point(model.embedding.flatten())
        embeddings_fp.tofile(f)
        print(f"  âœ“ Embeddings: {len(embeddings_fp)} values")
        
        # Write attention weights for each layer - now as int64
        for layer_idx in range(model.num_layers):
            w_q_fp = to_fixed_point(model.w_q[layer_idx].flatten())
            w_k_fp = to_fixed_point(model.w_k[layer_idx].flatten())
            w_v_fp = to_fixed_point(model.w_v[layer_idx].flatten())
            
            w_q_fp.tofile(f)
            w_k_fp.tofile(f)
            w_v_fp.tofile(f)
            
            print(f"  âœ“ Layer {layer_idx}: Q,K,V weights written")

        # Flatten and write output projection weights
        output_proj_fp = to_fixed_point(model.output_proj.flatten())
        output_proj_fp.tofile(f)
        print(f"  âœ“ Output Projection: {len(output_proj_fp)} values")

        total_bytes = f.tell()
        print(f"  âœ“ Total bytes written: {total_bytes}")
        if total_bytes != (model.total_params * 8 + 8):
            print(f"  âš ï¸ WARNING: Mismatch! Expected {model.total_params * 8 + 8} bytes (weights + header).")
    
    # Write human-readable debug file
    debug_filename = filename.replace('.weights', '_debug.txt')
    with open(debug_filename, 'w') as f:
        f.write("Enhanced Tiny LLM Weights (NumPy version)\n")
        f.write("=" * 60 + "\n")
        f.write(f"Model Configuration:\n")
        f.write(f"  Vocab Size: {model.vocab_size}\n")
        f.write(f"  Hidden Dim: {model.hidden_dim}\n")
        f.write(f"  Num Layers: {model.num_layers}\n")
        f.write(f"  Total Params: {model.total_params:,}\n\n")
        
        f.write("Weight Statistics:\n")
        f.write(f"  Embedding: mean={model.embedding.mean():.4f}, std={model.embedding.std():.4f}\n")
        
        for i in range(model.num_layers):
            f.write(f"  Layer {i} W_q: mean={model.w_q[i].mean():.4f}, std={model.w_q[i].std():.4f}\n")
            f.write(f"  Layer {i} W_k: mean={model.w_k[i].mean():.4f}, std={model.w_k[i].std():.4f}\n")
            f.write(f"  Layer {i} W_v: mean={model.w_v[i].mean():.4f}, std={model.w_v[i].std():.4f}\n")
        f.write(f"  Output Proj: mean={model.output_proj.mean():.4f}, std={model.output_proj.std():.4f}\n")
        
        f.write(f"\nSample weights (first 10 embedding values):\n")
        for i in range(min(10, model.embedding.size)):
            fp_val = int(model.embedding.flatten()[i] * 1000)
            f.write(f"  [{i}]: {model.embedding.flatten()[i]:.6f} -> {fp_val} (fixed-point)\n")
    
    print(f"ðŸ“ Debug info: {debug_filename}")

# Rest of the functions remain the same...
def create_training_data(text, seq_len=64, chars=None):
    """Create training sequences from text"""
    if chars is None:
        chars = sorted(list(set(text)))
    char_to_int = {ch: i for i, ch in enumerate(chars)}
    
    # Convert text to tokens using the provided character map
    tokens = [char_to_int.get(c, 0) for c in text]
    
    sequences = []
    targets = []
    
    print("  Creating sequences (this may take a while)...", end='', flush=True)
    # Create overlapping sequences
    for i in range(0, len(tokens) - seq_len, seq_len // 2):
        sequences.append(tokens[i:i+seq_len])
        targets.append(tokens[i+1:i+seq_len+1])
        # Add a progress dot to show it's working
        if i > 0 and i % 50000 == 0:
            print(".", end="", flush=True)
    print(" Done.")
    
    return sequences, targets

def visualize_attention(model, text, char_to_int, int_to_char):
    """Visualize what the model is learning"""
    tokens = [char_to_int.get(c, 0) for c in text[:8]]
    text_to_show = "".join([int_to_char.get(t, "?") for t in tokens])
    print("\nðŸ‘ï¸ Attention Visualization")
    print(f"Input: '{text[:8]}'")
    
    # Get embeddings
    embedded = np.array([model.embedding[t] for t in tokens])
    
    # Get attention for first layer
    Q = embedded @ model.w_q[0]
    K = embedded @ model.w_k[0]
    
    scores = Q @ K.T / np.sqrt(model.hidden_dim)
    attn = model.softmax(scores)
    
    # Show attention pattern
    print("\nAttention weights (first 4x4):")
    for i in range(min(4, len(tokens))):
        print(f"  {text_to_show[i]}: ", end="")
        for j in range(min(4, len(tokens))):
            print(f"{attn[i,j]:.2f} ", end="")
        print()

def main():
    print("=" * 60)
    print("ðŸ¤– Enhanced Tiny LLM Trainer (NumPy Version)")
    print("=" * 60)
    print()
    
    # --- Load Training Data & Determine Vocab Size ---
    dictionary_file = "dictionary.txt"
    try:
        with open(dictionary_file, 'r', encoding='utf-8') as f:
            training_text = f.read()
        print(f"ðŸ“– Successfully loaded '{dictionary_file}' for training.")
    except FileNotFoundError:
        print(f"âš ï¸ '{dictionary_file}' not found. Falling back to small internal corpus.")
        training_text = "The quick brown fox jumps over the lazy dog. AILang is a systems programming language."

    chars = sorted(list(set(training_text)))
    vocab_size = len(chars)
    
    print(f"ðŸ“š Training corpus: {len(training_text)} characters")
    print(f"ðŸ“ Sample: '{training_text[:50]}...'\n")
    print(f"ðŸŒ Vocabulary size: {vocab_size}")
    
    # Create model
    
    model = TinyTransformerNumPy(vocab_size=vocab_size)
    
    # Prepare training data
    sequences, targets = create_training_data(training_text, seq_len=64, chars=chars)
    print(f"ðŸŽ¯ Training sequences: {len(sequences)}")
    
    # Training loop
    print("\nðŸƒ Starting training...")
    print("-" * 40)
    
    epochs = 2 
    best_loss = float('inf')
    
    start_time = time.time()
    
    for epoch in range(epochs):
        total_loss = 0
        # Fix: Convert to list before shuffling
        data = list(zip(sequences, targets))
        np.random.shuffle(data)

        num_sequences = len(data)
        epoch_start_time = time.time()
        for i, (seq, tgt) in enumerate(data):
            loss = model.train_step(seq, tgt)
            total_loss += loss

            # Update progress indicator more frequently and with timing info
            if (i + 1) % 100 == 0: # Update every 100 sequences
                sequences_per_sec = (i + 1) / (time.time() - epoch_start_time + 1e-9)
                print(f"\r    Epoch {epoch + 1} progress: [{i + 1:>6}/{num_sequences}] ({sequences_per_sec:.1f} seq/s)", end="", flush=True)
        
        # Clear the progress line before printing the final epoch summary
        print("\r" + " " * 80 + "\r", end="")

        avg_loss = total_loss / len(sequences)
        model.loss_history.append(avg_loss)
        
        # Track best model
        if avg_loss < best_loss:
            best_loss = avg_loss
            # Could save best weights here
        
        # Progress update every epoch
        elapsed = time.time() - start_time
        print(f"  Epoch {epoch + 1:3d}/{epochs} | Loss: {avg_loss:.4f} | "
              f"Best: {best_loss:.4f} | Time: {elapsed:.1f}s")
    
    print("-" * 40)
    print(f"âœ… Training complete! Final loss: {avg_loss:.4f}")
    print(f"â±ï¸ Total time: {time.time() - start_time:.1f} seconds")
    
    # Test generation
    print("\nðŸ”® Testing generation:")
    print("-" * 40)
    
    test_prompts = [
        "Hello",
        "The quick",
        "AILANG",
        "Machine",
        "Test"
    ]
    
    # Create char_to_int and int_to_char for generation
    char_to_int = {ch: i for i, ch in enumerate(chars)}
    int_to_char = {i: ch for i, ch in enumerate(chars)}
    
    for prompt in test_prompts:
        tokens = [char_to_int.get(c, 0) for c in prompt]
        generated_tokens = model.generate(tokens, max_length=20, temperature=0.8)
        generated_text = ''.join([int_to_char.get(t, '?') for t in generated_tokens])
        print(f"  '{prompt}' -> '{generated_text}'")
    
    # Visualize attention
    visualize_attention(model, "Hello world", char_to_int, int_to_char)
    
    # Export weights - THIS NOW WORKS!
    export_weights_for_ailang(model)
    
    # Show loss curve
    print("\nðŸ“Š Training Loss Curve (last 20 epochs):")
    recent_losses = model.loss_history[-20:]
    for i, loss in enumerate(recent_losses):
        bar_length = int(40 * (1 - loss / max(recent_losses)))
        bar = 'â–ˆ' * bar_length + 'â–‘' * (40 - bar_length)
        print(f"  E{len(model.loss_history)-20+i+1:3d}: {bar} {loss:.4f}")
    
    print("\n" + "=" * 60)
    print("ðŸŽ‰ Complete!")
    print("Next steps:")
    print("  1. tiny_llm.weights has been created")
    print("  2. Compile: python3 main.py tiny_llm.ailang")  
    print("  3. Run: ./tiny_llm_exec")
    print("=" * 60)

if __name__ == "__main__":
    main()