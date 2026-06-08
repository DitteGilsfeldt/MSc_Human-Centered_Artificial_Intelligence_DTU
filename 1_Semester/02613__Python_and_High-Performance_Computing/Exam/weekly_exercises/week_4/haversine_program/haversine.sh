#!/bin/bash
#BSUB -J haversine
#BSUB -q hpc
#BSUB -W 2
#BSUB -R "span[hosts=1]"
#BSUB -R "select[model==XeonGold6126]"
#BSUB -R "rusage[mem=1024]"
#BSUB -n 1
#BSUB -B
#BSUB -N
#BSUB -o haversine.out
#BSUB -e haversine.err

cd $LS_SUBCWD

echo "CPU model on this node:"
grep "model name" /proc/cpuinfo | head -n 1

echo "Running Python script now!"

kernprof -l haversine.py /dtu/projects/02613_2025/data/locations/locations_5000.csv

python3 -m line_profiler haversine.py.lprof