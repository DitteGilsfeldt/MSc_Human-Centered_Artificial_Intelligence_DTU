#!/bin/bash
#BSUB -J chunked_parallel
#BSUB -q hpc
#BSUB -W 5
#BSUB -R "span[hosts=1]"
#BSUB -R "select[model==XeonGold6126]"
#BSUB -R "rusage[mem=1024]"
#BSUB -n 20
#BSUB -B
#BSUB -N
#BSUB -o chunked_parallel.out
#BSUB -e chunked_parallel.err

cd $LS_SUBCWD

echo "CPU model:"
grep "model name" /proc/cpuinfo | head -n 1

echo "Running chunked parallel tests"

for p in 1 2 4 8 12 16 20
do
    echo "Processes: $p"
    time python3 chunked_parallel.py $p
    echo ""
done