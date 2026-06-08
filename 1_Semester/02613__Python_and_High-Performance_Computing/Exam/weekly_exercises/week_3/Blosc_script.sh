#!/bin/bash
#BSUB -J blosc_script
#BSUB -q hpc
#BSUB -W 5
#BSUB -R "span[hosts=1]"
#BSUB -R "select[model==XeonGold6126]"
#BSUB -R "rusage[mem=3000]"
#BSUB -n 1
#BSUB -B
#BSUB -N
#BSUB -o blosc_%J.out
#BSUB -e blosc_%J.err

echo "CPU model on this node:"
grep "model name" /proc/cpuinfo | head -n 1

echo "Running Blosc program!"

python3 Blosc_1.py 256
python3 Blosc_1.py 512
python3 Blosc_1.py 1024


