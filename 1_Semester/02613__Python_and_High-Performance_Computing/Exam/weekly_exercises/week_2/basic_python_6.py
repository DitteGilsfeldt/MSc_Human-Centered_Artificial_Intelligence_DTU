import sys

def even_numbers(numbers):
    filtered_numbers = []
    numbers = [int(number) for number in numbers]
    for i in numbers:
        if i % 2 == 0:
            filtered_numbers.append(i)
    return filtered_numbers

numbers = sys.argv[1:]

print(even_numbers(numbers))

