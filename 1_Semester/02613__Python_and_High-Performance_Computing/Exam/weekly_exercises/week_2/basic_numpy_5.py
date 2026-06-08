import sys
import numpy as np
import time

def matrix_multiplied_with_p(matrix_path, p):
    matrix = np.load(matrix_path)

    start = time.perf_counter()
    result = np.linalg.matrix_power(matrix, p + 1)
    end = time.perf_counter()

    np.save("result.npy", result)

    return end - start

matrix_path = sys.argv[1]
p = int(sys.argv[2])

print(matrix_multiplied_with_p(matrix_path, p))
