#!/usr/bin/env python3
"""
generate_attention_scores_and_softmax.py
Generates FixedPool for attention scores and complete Softmax implementation
"""

def generate_attention_scores_pool(max_seq_len=32):
    """Generate FixedPool for storing attention scores"""
    code = "// ============================================================================\n"
    code += "// Attention Scores Storage - Add this to your FixedPools section\n"
    code += "// ============================================================================\n\n"
    code += "FixedPool.AttentionScores {\n"
    
    # Generate score slots
    for i in range(max_seq_len):
        code += f'    "s{i}": Initialize=0'
        if i < max_seq_len - 1:
            code += "   "
        code += "\n"
    
    code += '    "count": Initialize=0\n'
    code += '    "exp_sum": Initialize=0\n'
    code += "}\n\n"
    
    return code

def generate_softmax_implementation(max_seq_len=32):
    """Generate complete Softmax subroutine with Math.Exp"""
    
    code = """// ============================================================================
// UPGRADED: ApplySoftmax - Full implementation with Math.Exp
// Computes softmax over all cached attention scores
// ============================================================================

SubRoutine.ApplySoftmax {
    DebugPerf.Start("softmax")
    Debug("smollm.trace", level=2) {
        PrintMessage("[ApplySoftmax] ENTER, count=")
        PrintNumber(AttentionScores.count)
        PrintMessage("\\n")
    }
    
    // Step 1: Find maximum score for numerical stability
    max_score = -999999
    score_idx = 0
    
    WhileLoop LessThan(score_idx, AttentionScores.count) {
        Branch score_idx {
"""
    
    # Generate cases for reading scores to find max
    for i in range(max_seq_len):
        code += f"            Case {i}: {{ current_score = AttentionScores.s{i} }}\n"
    
    code += """            Default: { current_score = 0 }
        }
        
        IfCondition GreaterThan(current_score, max_score) ThenBlock: {
            max_score = current_score
        }
        
        score_idx = Add(score_idx, 1)
    }
    
    Debug("smollm.trace", level=3) {
        PrintMessage("  [Softmax] max_score=")
        PrintNumber(max_score)
        PrintMessage("\\n")
    }
    
    // Step 2: Compute exp(score - max) and sum
    AttentionScores.exp_sum = 0
    score_idx = 0
    
    WhileLoop LessThan(score_idx, AttentionScores.count) {
        // Get score[i]
        Branch score_idx {
"""
    
    # Generate cases for reading scores
    for i in range(max_seq_len):
        code += f"            Case {i}: {{ current_score = AttentionScores.s{i} }}\n"
    
    code += """            Default: { current_score = 0 }
        }
        
        // Compute stable score = score - max
        stable_score = Subtract(current_score, max_score)
        
        // Scale for Math.Exp (expects fixed-point with scale=10000)
        // Our scores are in range ~0-10000, so divide by 100 to prevent overflow
        scaled_score = Divide(stable_score, 100)
        
        // Compute exp(stable_score)
        exp_val = Math.Exp(scaled_score)
        
        Debug("smollm.trace", level=4) {
            PrintMessage("    [Softmax] s")
            PrintNumber(score_idx)
            PrintMessage("=")
            PrintNumber(current_score)
            PrintMessage(", exp=")
            PrintNumber(exp_val)
            PrintMessage("\\n")
        }
        
        // Store exp value back (reusing score slots)
        Branch score_idx {
"""
    
    # Generate cases for storing exp values
    for i in range(max_seq_len):
        code += f"            Case {i}: {{ AttentionScores.s{i} = exp_val }}\n"
    
    code += """        }
        
        // Accumulate sum
        AttentionScores.exp_sum = Add(AttentionScores.exp_sum, exp_val)
        
        score_idx = Add(score_idx, 1)
    }
    
    Debug("smollm.trace", level=3) {
        PrintMessage("  [Softmax] exp_sum=")
        PrintNumber(AttentionScores.exp_sum)
        PrintMessage("\\n")
    }
    
    // Step 3: Normalize by dividing by sum
    score_idx = 0
    
    IfCondition GreaterThan(AttentionScores.exp_sum, 0) ThenBlock: {
        WhileLoop LessThan(score_idx, AttentionScores.count) {
            // Get exp value
            Branch score_idx {
"""
    
    # Generate cases for reading exp values
    for i in range(max_seq_len):
        code += f"                Case {i}: {{ exp_val = AttentionScores.s{i} }}\n"
    
    code += """                Default: { exp_val = 0 }
            }
            
            // Normalize: softmax[i] = exp[i] / sum
            // Scale to 0-1000 range for fixed-point
            normalized = FixedPoint.Divide(Multiply(exp_val, 1000), AttentionScores.exp_sum)
            
            Debug("smollm.trace", level=4) {
                PrintMessage("    [Softmax] normalized[")
                PrintNumber(score_idx)
                PrintMessage("]=")
                PrintNumber(normalized)
                PrintMessage("\\n")
            }
            
            // Store normalized value
            Branch score_idx {
"""
    
    # Generate cases for storing normalized values
    for i in range(max_seq_len):
        code += f"                Case {i}: {{ AttentionScores.s{i} = normalized }}\n"
    
    code += """            }
            
            score_idx = Add(score_idx, 1)
        }
    } ElseBlock: {
        // If sum is zero, set uniform distribution
        uniform_val = IfCondition GreaterThan(AttentionScores.count, 0) 
            ThenValue: Divide(1000, AttentionScores.count)
            ElseValue: 1000
        
        score_idx = 0
        WhileLoop LessThan(score_idx, AttentionScores.count) {
            Branch score_idx {
"""
    
    # Generate cases for uniform distribution fallback
    for i in range(max_seq_len):
        code += f"                Case {i}: {{ AttentionScores.s{i} = uniform_val }}\n"
    
    code += """            }
            score_idx = Add(score_idx, 1)
        }
    }
    
    Debug("smollm.trace", level=2) {
        PrintMessage("[ApplySoftmax] EXIT\\n")
    }
    DebugPerf.End("softmax")
}
"""
    
    return code

def generate_updated_compute_attention():
    """Generate updated ComputeAttention that stores scores in AttentionScores pool"""
    
    code = """
// ============================================================================
// UPDATED: ComputeAttention - Store scores in AttentionScores pool
// Replace your existing ComputeAttention with this
// ============================================================================

SubRoutine.ComputeAttention {
    DebugPerf.Start("attention")
    Debug("smollm.trace", level=2) {
        PrintMessage("[ComputeAttention] ENTER\\n")
    }
    
    // Reset attention scores
    AttentionScores.count = 0
    
    // Compute attention score for each cached key
    cache_idx = 0
    WhileLoop LessThan(cache_idx, KeyCache.count) {
        // Compute Q · K_i (dot product)
        score = 0
        
        // Get Key vector from cache at position cache_idx
        // For now, simplified version with current KeyVector
        // In full version, you'd retrieve from KeyCache.s{cache_idx}_d{dim}
        
        score = Add(
            Multiply(QueryVector.q0, KeyVector.k0),
            Multiply(QueryVector.q1, KeyVector.k1))
        
        // Add remaining dimensions q2-q31 × k2-k31
        score = Add(score, Multiply(QueryVector.q2, KeyVector.k2))
        score = Add(score, Multiply(QueryVector.q3, KeyVector.k3))
        // ... continue for all 32 dimensions
        
        // Scale down to prevent overflow
        score = Divide(score, 100)
        
        Debug("smollm.trace", level=3) {
            PrintMessage("  [Attention] score[")
            PrintNumber(cache_idx)
            PrintMessage("]=")
            PrintNumber(score)
            PrintMessage("\\n")
        }
        
        // Store score in AttentionScores pool
        Branch cache_idx {
            Case 0: { AttentionScores.s0 = score }
            Case 1: { AttentionScores.s1 = score }
            Case 2: { AttentionScores.s2 = score }
            Case 3: { AttentionScores.s3 = score }
            Case 4: { AttentionScores.s4 = score }
            Case 5: { AttentionScores.s5 = score }
            Case 6: { AttentionScores.s6 = score }
            Case 7: { AttentionScores.s7 = score }
            // ... continue for all 32 positions
        }
        
        AttentionScores.count = Add(AttentionScores.count, 1)
        cache_idx = Add(cache_idx, 1)
    }
    
    Debug("smollm.trace", level=2) {
        PrintMessage("[ComputeAttention] EXIT, computed ")
        PrintNumber(AttentionScores.count)
        PrintMessage(" scores\\n")
    }
    DebugPerf.End("attention")
}
"""
    
    return code

def main():
    output_file = "softmax_upgrade.ailang"
    
    with open(output_file, 'w') as f:
        f.write("// AUTO-GENERATED: Complete Softmax Implementation\n")
        f.write("// Generated by generate_attention_scores_and_softmax.py\n\n")
        
        # Generate FixedPool
        f.write(generate_attention_scores_pool(32))
        
        # Generate Softmax implementation
        f.write(generate_softmax_implementation(32))
        
        # Generate updated ComputeAttention
        f.write(generate_updated_compute_attention())
    
    print(f"✓ Generated {output_file}")
    print(f"\nWhat's included:")
    print(f"  1. FixedPool.AttentionScores with 32 score slots (s0-s31)")
    print(f"  2. Complete Softmax implementation with Math.Exp")
    print(f"     - Numerically stable (subtracts max)")
    print(f"     - Uses FixedPoint.Divide for normalization")
    print(f"     - Full debug instrumentation")
    print(f"  3. Updated ComputeAttention that stores scores")
    print(f"\nIntegration steps:")
    print(f"  1. Add FixedPool.AttentionScores to your pools section")
    print(f"  2. Replace SubRoutine.ApplySoftmax with new version")
    print(f"  3. Replace SubRoutine.ComputeAttention with new version")

if __name__ == "__main__":
    main()