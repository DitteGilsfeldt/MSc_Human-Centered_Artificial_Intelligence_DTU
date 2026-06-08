import os
import blosc
import numpy as np
import sys
from time import perf_counter


def write_numpy(arr, file_name):
    np.save(f"{file_name}.npy", arr)
    os.sync()


def write_blosc(arr, file_name, cname="lz4"):
    b_arr = blosc.pack_array(arr, cname=cname)
    with open(f"{file_name}.bl", "wb") as w:
        w.write(b_arr)
    os.sync()


def read_numpy(file_name):
    return np.load(f"{file_name}.npy")


def read_blosc(file_name):
    with open(f"{file_name}.bl", "rb") as r:
        b_arr = r.read()
    return blosc.unpack_array(b_arr)


n = int(sys.argv[1])

arr = np.zeros((n, n, n), dtype='uint8')

# write_numpy
start = perf_counter()
write_numpy(arr, "data")
end = perf_counter()
write_numpy_t = end - start

# write_blosc
start = perf_counter()
write_blosc(arr, "data")
end = perf_counter()
write_blosc_t = end - start

# read_numpy
start = perf_counter()
read_numpy("data")
end = perf_counter()
read_numpy_t = end - start

# read_blosc
start = perf_counter()
read_blosc("data")
end = perf_counter()
read_blosc_t = end - start

print(write_numpy_t)
print(write_blosc_t)
print(read_numpy_t)
print(read_blosc_t)
