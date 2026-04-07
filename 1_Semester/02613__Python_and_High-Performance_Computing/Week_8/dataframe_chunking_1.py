import sys
import pandas as pd

def compute_precipitation(filename, chunk_size):
    df = pd.read_csv(filename, chunksize=chunk_size)
    total_precip = 0
    for chunk in df:
        total_precip += chunk.loc[chunk['parameterId'] == 'precip_past10min', 'value'].sum()
    return total_precip

filename = sys.argv[1]
chunk_size = int(sys.argv[2])
result = compute_precipitation(filename, chunk_size)
print(result)

