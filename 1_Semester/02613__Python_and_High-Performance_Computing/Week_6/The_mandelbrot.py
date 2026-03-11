import multiprocessing
import random
import numpy as np
import matplotlib.pyplot as plt
import sys

def mandelbrot_escape_time(c):
    z = 0
    for i in range(100):
        z = z**2 + c
        if np.abs(z) > 2.0:
            return i
    return 100

def compute_chunk(points_chunk):
    escape_times = []
    for c in points_chunk:
        z = 0
        for i in range(100):
            z = z**2 + c
            if abs(z) > 2:
                escape_times.append(i)
                break
        else:
            escape_times.append(100)
    return escape_times

def generate_mandelbrot_set(points, num_processes):
    ########################

    chunk_size = len(points) // num_processes

    chunks = [
        points[i*chunk_size:(i+1)*chunk_size] if i < num_processes - 1
        else points[i*chunk_size:]
        for i in range(num_processes)
    ]

    pool = multiprocessing.Pool(num_processes)

    results = pool.map(compute_chunk, chunks)

    pool.close()
    pool.join()

    escape_times = np.array([t for chunk in results for t in chunk])

    ########################
    return escape_times

def generate_mandelbrot_set_chunks(points, num_processes):
    chunk_size = 1000   # fixed chunk size

    chunks = [
        points[i:i+chunk_size]
        for i in range(0, len(points), chunk_size)
    ]

    pool = multiprocessing.Pool(num_processes)

    results = pool.map(compute_chunk, chunks)

    pool.close()
    pool.join()

    escape_times = np.array([t for chunk in results for t in chunk])
    return escape_times
    

def plot_mandelbrot(escape_times):
    plt.imshow(escape_times, cmap='hot', extent=(-2, 2, -2, 2))
    plt.axis('off')
    plt.savefig('mandelbrot.png', bbox_inches='tight', pad_inches=0)

if __name__ == "__main__":
    width = 800
    height = 800
    xmin, xmax = -2, 2
    ymin, ymax = -2, 2
    num_proc = int(sys.argv[1])

    # Precompute points
    x_values = np.linspace(xmin, xmax, width)
    y_values = np.linspace(ymin, ymax, height)
    points = np.array([complex(x, y) for x in x_values for y in y_values])

    # Compute set
    mandelbrot_set = generate_mandelbrot_set(points, num_proc)

    # Save set as image
    mandelbrot_set = mandelbrot_set.reshape((height, width))
    plot_mandelbrot(mandelbrot_set)