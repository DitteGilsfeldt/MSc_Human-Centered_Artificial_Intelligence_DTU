import numpy as np
import time
from numba import cuda

# CUDA kernel
@cuda.jit
def add_kernel(x, y, out):
    i = cuda.grid(1)
    if i < out.size:
        out[i] = x[i] + y[i]

# MAIN program
def main():
    N = 1_000_000

    # Create input data
    x = np.random.rand(N).astype(np.float32)
    y = np.random.rand(N).astype(np.float32)

    # Allocate output
    out = np.zeros_like(x)

    # Copy to GPU
    d_x = cuda.to_device(x)
    d_y = cuda.to_device(y)
    d_out = cuda.device_array_like(out)

    # Configure kernel
    threads_per_block = 256
    blocks = (N + threads_per_block - 1) // threads_per_block

    # Warm-up (JIT compilation)
    add_kernel[blocks, threads_per_block](d_x, d_y, d_out)
    cuda.synchronize()

    # Timing
    start = time.time()

    add_kernel[blocks, threads_per_block](d_x, d_y, d_out)
    cuda.synchronize() # VERY IMPORTANT

    end = time.time()

    # Copy result back (optional, but good practice)
    result = d_out.copy_to_host()

    print("Execution time (seconds):", end - start)

if __name__ == "__main__":
    main()