import pandas as pd
from sklearn import datasets
reference_data = datasets.load_iris(as_frame=True).frame
reference_data = reference_data.rename(
    columns={
        'sepal length (cm)': 'sepal_length',
        'sepal width (cm)': 'sepal_width',
        'petal length (cm)': 'petal_length',
        'petal width (cm)': 'petal_width',
        'target': 'target'
    }
)

current_data = pd.read_csv('prediction_database.csv')
current_data = current_data.drop(columns=['time'])
