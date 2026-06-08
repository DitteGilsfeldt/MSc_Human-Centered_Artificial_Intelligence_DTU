import sys

def array_magnitude(vector):
    vector = [float(i) for i in vector]
    return sum(i**2 for i in vector)**(1/2)

vector = sys.argv[1:]

print(array_magnitude(vector))



