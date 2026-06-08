import sys
import pandas as pd

def total_precip(filename):
    df = pd.read_csv(filename)
    total = df.loc[df['parameterId'] == 'precip_past10min', 'value'].sum()
    return total

filename = sys.argv[1]

result = total_precip(filename)
print(result)