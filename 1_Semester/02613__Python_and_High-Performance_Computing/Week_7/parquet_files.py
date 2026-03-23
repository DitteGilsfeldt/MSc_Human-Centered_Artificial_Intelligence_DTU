import sys 

def parquet_files(filename):
    from pyarrow import csv
    import pyarrow.parquet as pq

    table = csv.read_csv(filename)
    pq.write_table(table, filename.replace(".csv", ".parquet"))

    return table

filename = sys.argv[1]

parquet_files(filename)
print("Files saved as parquet!")
