import numpy as np
from time import perf_counter
import matplotlib.pyplot as plt

sizes = np.logspace(2, 8, num=20).astype(int)

size_kb_list = []
perf_list = []

repeats = 100

for SIZE in sizes:
    mat = np.random.rand(1, SIZE).astype('float32')
    start = perf_counter()
    for _ in range(repeats):
        2 * mat[0, :]
    end = perf_counter()

    time_per_op = (end - start) / repeats
    mflops = (SIZE / time_per_op) * 1e-6
    size_kb = (8 * SIZE) / 1024

    size_kb_list.append(size_kb)
    perf_list.append(mflops)

plt.figure()
plt.loglog(size_kb_list, perf_list)
plt.xlabel("Vector size (KB)")
plt.ylabel("Performance (MFLOP/s)")
plt.axvline(32, linestyle="--")
plt.axvline(1024, linestyle="--")
plt.axvline(22000, linestyle="--")
plt.tight_layout()
plt.savefig("row_vector_performance.png")