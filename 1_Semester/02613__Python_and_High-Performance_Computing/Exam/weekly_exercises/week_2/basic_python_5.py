import sys

def grade_average(grades):
    grades = [float(grade) for grade in grades]
    mean_grade = sum(grades) / len(grades)
    if mean_grade >= 5.0:
        status =  "Pass"
    else:
        status = "Fail"
    
    return f"{mean_grade} {status}"

grades = sys.argv[1:]

print(grade_average(grades))
