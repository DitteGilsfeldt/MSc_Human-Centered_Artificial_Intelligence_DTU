#!/bin/bash
#BSUB -J cache_effects_2
#BSUB -q hpc
#BSUB -W 2
#BSUB -R "span[hosts=1]"
#BSUB -R "select[model==XeonGold6126]"
#BSUB -R "rusage[mem=1024]"
#BSUB -n 1
#BSUB -B
#BSUB -N
#BSUB -o cache_effects_%J.out
#BSUB -e cache_effects_%J.err

echo "CPU model on this node:"
grep "model name" /proc/cpuinfo | head -n 1

echo "Running Python script now!"
python3 HPC_chache_effects.py


