import pandas as pd
from sklearn import datasets

from evidently.tests.suite import TestSuite
from evidently.tests import TestNumberOfMissingValues

reference_data = datasets.load_iris(as_frame=True).frame.rename(
    columns={
        "sepal length (cm)": "sepal_length",
        "sepal width (cm)": "sepal_width",
        "petal length (cm)": "petal_length",
        "petal width (cm)": "petal_width",
        "target": "target",
    }
)

current_data = (
    pd.read_csv("prediction_database.csv")
    .drop(columns=["time"])
    .rename(columns={"prediction": "target"})
)

data_test = TestSuite(tests=[TestNumberOfMissingValues()])
data_test.run(reference_data=reference_data, current_data=current_data)

result = data_test.as_dict()
print(result)
print("All tests passed:", result["summary"]["all_passed"])
