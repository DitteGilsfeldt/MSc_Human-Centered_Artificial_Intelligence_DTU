import sys
import numpy as np
from numba import cuda

##############################
TPB = 64  # Threads per block

@cuda.jit
def reduce_kernel(data, out, n):
    # Shared memory for this block
    sdata = cuda.shared.array(shape=TPB, dtype=cuda.float32)

    # Get the 1D grid and block indices
    tid = cuda.threadIdx.x
    i = cuda.grid(1)

    # Each thread loads one element
    sdata[tid] = data[i] if i < n else 0.0
    cuda.syncthreads() # Ensure all are done

    # Do reduction for threadblock
    s = 1
    while s < cuda.blockDim.x:
        # if tid % (2 * s) == 0 and tid + s < cuda.blockDim.x:
        #   sdata[tid] += sdata[tid + s]

        index = 2 * s * tid
        if (index < cuda.blockDim.x):
            sdata[index] += sdata[index + s]

        s *= 2
        cuda.syncthreads()  # Ensure block is synchronized

    # Write result for this block to global memory
    if tid == 0:
        out[cuda.blockIdx.x] = sdata[0]

def get_grid(n, tpb):
    return (n + (tpb - 1)) // tpb  # Blocks per grid

def reduce(x, n):
    # n = len(x)
    bpg = get_grid(n, TPB)
    out = cuda.device_array(bpg, dtype=x.dtype)
    while bpg > 1:
        reduce_kernel[bpg, TPB](x, out, n)
        n = bpg
        bpg = get_grid(n, TPB)
        # x[:n] = out[:n]
        x = out
    reduce_kernel[bpg, TPB](x, out, n)
    return out
##############################

def number_sum(n):
    random_floats32 = np.random.rand(n, ).astype(np.float32)
    data_device = cuda.to_device(random_floats32)
    sum = reduce(data_device, n)
    return sum.copy_to_host()[0]

n = sys.argv[1]
print(number_sum(int(n)))