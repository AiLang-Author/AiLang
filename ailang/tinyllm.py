#!/usr/bin/env python3
"""
Optimized Tiny LLM Trainer - Fast version for overnight training
"""

import numpy as np
import struct
import time
import os

class FastTinyTransformer:
    def __init__(self, vocab_size=128):
        # Smaller architecture for faster training
        self.vocab_size = vocab_size
        self.hidden_dim = 256  # Reduced from 512
        self.num_layers = 1    # Single layer for speed
        self.num_heads = 8     
        self.head_dim = 32
        self.max_seq_len = 32  # Shorter sequences
        self.lr = 0.003       # Higher learning rate
        
        # Use float32 for speed
        self.dtype = np.float32
        
        print(f"🧠 Fast Model Configuration:")
        print(f"  Vocab: {self.vocab_size}")
        print(f"  Hidden: {self.hidden_dim}")
        print(f"  Layers: {self.num_layers}")
        print(f"  Seq Length: {self.max_seq_len}")
        
        # Initialize with float32
        scale = np.sqrt(2.0 / self.hidden_dim)
        self.embedding = (np.random.randn(self.vocab_size, self.hidden_dim) * scale * 0.02).astype(self.dtype)
        
        self.w_q = []
        self.w_k = []
        self.w_v = []
        
        for _ in range(self.num_layers):
            self.w_q.append((np.random.randn(self.hidden_dim, self.hidden_dim) * scale).astype(self.dtype))
            self.w_k.append((np.random.randn(self.hidden_dim, self.hidden_dim) * scale).astype(self.dtype))
            self.w_v.append((np.random.randn(self.hidden_dim, self.hidden_dim) * scale).astype(self.dtype))
        
        self.output_proj = (np.random.randn(self.hidden_dim, self.vocab_size) * scale).astype(self.dtype)
        
        # Simple SGD momentum for faster convergence
        self.momentum = 0.9
        self.v_embedding = np.zeros_like(self.embedding)
        self.v_w_q = [np.zeros_like(w) for w in self.w_q]
        self.v_w_k = [np.zeros_like(w) for w in self.w_k]
        self.v_w_v = [np.zeros_like(w) for w in self.w_v]
        self.v_output_proj = np.zeros_like(self.output_proj)
        
        self.total_params = (
            self.vocab_size * self.hidden_dim +
            self.num_layers * 3 * self.hidden_dim * self.hidden_dim +
            self.hidden_dim * self.vocab_size
        )
        print(f"  Total Parameters: {self.total_params:,}\n")
    
    def gelu_fast(self, x):
        """Fast GELU approximation"""
        return x * (1.0 / (1.0 + np.exp(-1.702 * x)))
    
    def softmax_fast(self, x):
        """Fast stable softmax"""
        e_x = np.exp(x - np.max(x, axis=-1, keepdims=True))
        return e_x / np.sum(e_x, axis=-1, keepdims=True)
    
    def forward_fast(self, tokens):
        """Simplified forward pass"""
        # Embedding
        x = self.embedding[tokens]
        
        # Single transformer layer (or loop if multiple)
        for layer_idx in range(self.num_layers):
            # Attention
            Q = x @ self.w_q[layer_idx]
            K = x @ self.w_k[layer_idx]
            V = x @ self.w_v[layer_idx]
            
            scores = (Q @ K.T) / np.sqrt(self.head_dim)
            
            # Causal mask
            seq_len = len(tokens)
            mask = np.triu(np.ones((seq_len, seq_len)), k=1) * -1e9
            scores = scores + mask
            
            attn = self.softmax_fast(scores)
            x_attn = attn @ V
            
            # Skip layer norm for speed, just residual
            x = x + x_attn
            
            # Simple feedforward
            x = x + self.gelu_fast(x)
        
        # Output projection
        logits = x @ self.output_proj
        return logits
    
    def train_step_fast(self, tokens, targets):
        """Fast training step with momentum SGD"""
        seq_len = len(tokens)
        
        # Forward
        logits = self.forward_fast(tokens)
        
        # Loss
        probs = self.softmax_fast(logits)
        loss = -np.mean(np.log(probs[np.arange(seq_len), targets] + 1e-9))
        
        # Backward (simplified)
        d_logits = probs
        d_logits[np.arange(seq_len), targets] -= 1
        d_logits /= seq_len
        
        # Get embeddings for gradient calculation
        x = self.embedding[tokens]
        
        # Output projection gradient
        d_output = x.T @ d_logits
        
        # Embedding gradients (vectorized)
        d_x = d_logits @ self.output_proj.T
        d_embedding = np.zeros_like(self.embedding)
        np.add.at(d_embedding, tokens, d_x)
        
        # Attention gradients (simplified)
        for layer_idx in range(self.num_layers):
            d_w_q = x.T @ d_x * 0.1  # Scale down gradients
            d_w_k = x.T @ d_x * 0.1
            d_w_v = x.T @ d_x * 0.1
            
            # Momentum update
            self.v_w_q[layer_idx] = self.momentum * self.v_w_q[layer_idx] - self.lr * d_w_q
            self.v_w_k[layer_idx] = self.momentum * self.v_w_k[layer_idx] - self.lr * d_w_k
            self.v_w_v[layer_idx] = self.momentum * self.v_w_v[layer_idx] - self.lr * d_w_v
            
            self.w_q[layer_idx] += self.v_w_q[layer_idx]
            self.w_k[layer_idx] += self.v_w_k[layer_idx]
            self.w_v[layer_idx] += self.v_w_v[layer_idx]
        
        # Update output projection and embeddings
        self.v_output_proj = self.momentum * self.v_output_proj - self.lr * d_output
        self.output_proj += self.v_output_proj
        
        self.v_embedding = self.momentum * self.v_embedding - self.lr * d_embedding
        self.embedding += self.v_embedding
        
        return loss
    
    def train_batch(self, sequences, targets, batch_size=32):
        """Train on mini-batches"""
        total_loss = 0
        num_batches = 0
        
        for i in range(0, len(sequences), batch_size):
            batch_loss = 0
            batch_end = min(i + batch_size, len(sequences))
            
            for j in range(i, batch_end):
                loss = self.train_step_fast(sequences[j], targets[j])
                batch_loss += loss
            
            total_loss += batch_loss
            num_batches += 1
        
        return total_loss / num_batches if num_batches > 0 else 0

def create_fast_training_data(text, seq_len=32):
    """Fast training data creation"""
    chars = sorted(list(set(text)))
    char_to_int = {ch: i for i, ch in enumerate(chars)}
    
    tokens = [char_to_int.get(c, 0) for c in text]
    
    sequences = []
    targets = []
    
    # Non-overlapping sequences for speed
    for i in range(0, len(tokens) - seq_len, seq_len):
        sequences.append(tokens[i:i+seq_len])
        targets.append(tokens[i+1:i+seq_len+1])
    
    return sequences, targets, chars

def export_weights(model, filename="tiny_llm.weights"):
    """Export weights for AILANG"""
    print(f"\n📁 Exporting weights to {filename}")
    
    with open(filename, 'wb') as f:
        # Write header
        f.write(struct.pack('<q', model.vocab_size))
        
        # Convert to fixed-point int64
        def to_fixed(arr):
            return (arr * 1000).round().astype(np.int64)
        
        # Write weights
        to_fixed(model.embedding.flatten()).tofile(f)
        
        for i in range(model.num_layers):
            to_fixed(model.w_q[i].flatten()).tofile(f)
            to_fixed(model.w_k[i].flatten()).tofile(f)
            to_fixed(model.w_v[i].flatten()).tofile(f)
        
        to_fixed(model.output_proj.flatten()).tofile(f)
    
    size = os.path.getsize(filename)
    print(f"  ✓ Exported {size:,} bytes")
    
    # Save config for AILANG
    with open("model_config.txt", 'w') as f:
        f.write(f"vocab_size={model.vocab_size}\n")
        f.write(f"hidden_dim={model.hidden_dim}\n")
        f.write(f"num_layers={model.num_layers}\n")
        f.write(f"trained_epochs={epochs_completed}\n")

def main():
    global epochs_completed
    
    print("=" * 60)
    print("🚀 FAST Tiny LLM Trainer")
    print("=" * 60)
    
    # Load training data
    try:
        with open("dictionary.txt", 'r', encoding='utf-8') as f:
            text = f.read()[:100000]  # Cap at 100k chars for speed
        print(f"📖 Loaded dictionary.txt ({len(text)} chars)")
    except:
        print("📝 Using default text")
        text = ("The quick brown fox jumps over the lazy dog. " * 50 +
                "AILANG is a systems programming language. " * 50)
    
    # Prepare data
    sequences, targets, chars = create_fast_training_data(text, seq_len=32)
    vocab_size = min(len(chars), 256)  # Cap vocab size
    
    print(f"📊 Training data: {len(sequences)} sequences")
    print(f"🔤 Vocabulary size: {vocab_size}")
    
    # Create model
    model = FastTinyTransformer(vocab_size=vocab_size)
    
    # Training settings
    epochs = 100  # More epochs since they're faster
    save_every = 10  # Save checkpoint every N epochs
    
    print(f"\n⏰ Starting training for {epochs} epochs...")
    print("=" * 60)
    
    start_time = time.time()
    best_loss = float('inf')
    epochs_completed = 0
    
    try:
        for epoch in range(epochs):
            # Shuffle data
            indices = np.random.permutation(len(sequences))
            sequences_shuffled = [sequences[i] for i in indices]
            targets_shuffled = [targets[i] for i in indices]
            
            # Train
            epoch_loss = model.train_batch(sequences_shuffled, targets_shuffled)
            
            # Update tracking
            epochs_completed = epoch + 1
            if epoch_loss < best_loss:
                best_loss = epoch_loss
            
            # Progress
            elapsed = time.time() - start_time
            eta = (elapsed / epochs_completed) * (epochs - epochs_completed)
            
            print(f"Epoch {epoch+1:3d}/{epochs} | Loss: {epoch_loss:.4f} | "
                  f"Best: {best_loss:.4f} | Time: {elapsed:.0f}s | ETA: {eta:.0f}s")
            
            # Save checkpoint
            if (epoch + 1) % save_every == 0:
                export_weights(model, f"checkpoint_{epoch+1}.weights")
                print(f"  💾 Checkpoint saved")
    
    except KeyboardInterrupt:
        print("\n\n⚠️  Training interrupted!")
    
    # Final export
    export_weights(model, "tiny_llm.weights")
    
    print("\n" + "=" * 60)
    print(f"✅ Training complete! {epochs_completed} epochs")
    print(f"⏱️  Total time: {time.time() - start_time:.1f} seconds")
    print(f"📊 Final loss: {best_loss:.4f}")
    print("\nFiles created:")
    print("  - tiny_llm.weights (main model)")
    print("  - model_config.txt (configuration)")
    print(f"  - checkpoint_*.weights (backups)")
    print("=" * 60)

if __name__ == "__main__":
    main()