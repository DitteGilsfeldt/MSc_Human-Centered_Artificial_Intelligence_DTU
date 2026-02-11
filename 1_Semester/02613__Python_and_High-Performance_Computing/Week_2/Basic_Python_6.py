"""
Exercise:
Write a Python program that will receive any amount of numbers as command line arguments. It must then remove all odd numbers and print out the list of even numbers. \
Hint: you can use the builtin function filter or a list comprehension (see example in link).
Input: Any amount of numbers as command line arguments.
Output: All even numbers in the input.
Example: For the input 0, 1, 4, 2, 3, -2 the program should print the string [0, 4, 2, -2]".
"""

import sys

def filter_even_numbers(args):
    even_numbers = [int(num) for num in args if int(num) % 2 == 0]
    return even_numbers

args = sys.argv[1:] 
print(filter_even_numbers(args))


"""
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
"""