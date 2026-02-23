#!/bin/bash
#BSUB -J Cache_Effects.py
#BSUB -q hpc
#BSUB -W 5
#BSUB -R "span[hosts=1]"
#BSUB -R "select[model==XeonGold6126]"
#BSUB -R "rusage[mem=3000]"
#BSUB -n 1
#BSUB -B
#BSUB -N
#BSUB -o Cache_Effects.out

echo "CPU model on this node:"
grep "model name" /proc/cpuinfo | head -n 1

echo "Running Python script now!"
python3 Chache_Effects.py