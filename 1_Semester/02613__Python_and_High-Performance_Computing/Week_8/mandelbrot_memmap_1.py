import numpy as np
import sys

def mandelbrot_escape_time(c):
    z = 0
    for i in range(100):
        z = z**2 + c
        if np.abs(z) > 2:
            return i
    return 100


def mandelbrot_memmap(size, filename = "mandelbrot.dat"):
    # Create memory-mapped array
    mandelbrot = np.memmap(filename, dtype=np.int32, mode="w+", shape=(size, size))

    xpt = np.linspace(-2, 2, size+1)[:-1]
    ypt = np.linspace(-2, 2, size+1)[:-1]

    for i in range(size):
        for j in range(size):
            c = xpt[i] + 1j * ypt[j]
            mandelbrot[i, j] = mandelbrot_escape_time(c)
            
    return mandelbrot

if __name__ == "__main__":
    size = int(sys.argv[1])
    mandelbrot = mandelbrot_memmap(size)
    mandelbrot.flush()