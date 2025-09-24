# test_read_weights.py
import numpy as np

# Read the first few weights
weights = np.fromfile('tiny_llm.weights', dtype=np.int32, count=10)
print("First 10 weights from file:")
for i, w in enumerate(weights):
    print(f"  [{i}] = {w}")