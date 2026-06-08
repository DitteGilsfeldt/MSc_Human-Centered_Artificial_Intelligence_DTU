import sys

def diagonal_matrix(numbers):
    numbers = [float(i) for i in numbers]
    size = len(numbers)
    matrix = [[0]*size for _ in range(size)]
    for i in range(size):
        matrix[i][i] = numbers[i]
    
    # export matrix to np file with np.save

    import numpy as np

    np.save("diagonal_matrix.npy", matrix)
    
    return matrix

numbers = sys.argv[1:]

print(diagonal_matrix(numbers))

