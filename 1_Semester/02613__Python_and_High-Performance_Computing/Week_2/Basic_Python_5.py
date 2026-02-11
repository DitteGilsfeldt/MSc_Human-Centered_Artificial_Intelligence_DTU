"""
Exercise:
Write a Python program that will receive any number of numerical grades as command line arguments. It must then compute the mean and print it back to the user followed by "Pass" if the mean is at least 5 and "Fail" otherwise. Hint: you can use sys.argv to access the commandline arguments.\
Input: Any number grades given as command line arguments. Each grade will be a number.
Output: The mean grade, followed by a space, followed by Pass", if the mean is at least 5, otherwiseFail".
Example: For the input 0, 2, 4 the program should print the string 4.0 Fail". For the input [4, 7, 10, 12] the program should print the string8.25 Pass".
"""

import sys

def grade_averages(grades):
    grade_list = []
    for grade in grades:
        grade_list.append(float(grade))
    mean_grade = sum(grade_list) / len(grade_list)
    if mean_grade >= 5:
        return f"{mean_grade} Pass"
    else:
        return f"{mean_grade} Fail"
    
grades = sys.argv[1:]
print(grade_averages(grades))
