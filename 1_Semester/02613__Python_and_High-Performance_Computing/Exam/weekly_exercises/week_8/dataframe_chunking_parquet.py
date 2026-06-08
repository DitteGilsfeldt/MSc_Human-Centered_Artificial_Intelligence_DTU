import sys
import pyarrow.parquet as pq

def compute_precipitation(filename, chunk_size):
    parquet_file = pq.ParquetFile(filename)
    total_precip = 0
    for batch in parquet_file.iter_batches(batch_size=chunk_size):
        df = batch.to_pandas()
        mask = df['parameterId'] == 'precip_past10min'
        total_precip += df.loc[mask, 'value'].sum()
    return total_precip


filename = sys.argv[1]
chunk_size = int(sys.argv[2])

result = compute_precipitation(filename, chunk_size)
print(result)