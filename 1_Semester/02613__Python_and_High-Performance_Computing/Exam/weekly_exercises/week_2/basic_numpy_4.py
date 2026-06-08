import sys

def clow_rows_extractor(matrix_path, p):
    matrix = np.load(matrix_path)

    import numpy as np
    import time

    start = time.perf_counter()
    result = matrix**(p+1)
    end = time.perf_counter()
    np.save("result.npy", result)

    print(end-start)

    
matrix_path = sys.argv[1]
p = int(sys.argv[2])

print(clow_rows_extractor(matrix_path))

