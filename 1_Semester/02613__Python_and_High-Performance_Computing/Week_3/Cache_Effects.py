import numpy as np
from time import time

SIZE = 100

mat = np.random.rand(SIZE, SIZE)

# Make a Python program that measures the execution time of 2 * mat[:, 0] and 2 * mat[0, :]. Measure the time for at least 1000 repetitions. 
# Hint: remember what you did in Week 2, Exercise 2.5.

t1 = time()
for _ in range(1000):
    double_column = 2 * mat[:, 0]
t1 = time() - t1

t2 = time()
for _ in range(1000):
    double_row = 2 * mat[0, :]
t2 = time() - t2

print("Column total:", t1)
print("Row total:", t2)
print("Column per rep:", t1/1000)
print("Row per rep:", t2/1000)