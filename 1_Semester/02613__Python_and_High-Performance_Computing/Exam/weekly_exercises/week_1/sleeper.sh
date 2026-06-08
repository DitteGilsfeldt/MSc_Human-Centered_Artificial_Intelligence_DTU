#!/bin/bash
#BSUB -J sleeper
#BSUB -q hpc
#BSUB -W 2
#BSUB -R "span[hosts=1]"
#BSUB -R "select[model==XeonGold6126]"
#BSUB -n 64 
#BSUB -B
#BSUB -N
#BSUB -o sleeper_%J.out
#BSUB -e sleeper_%J.err

echo "CPU model on this node:"
grep "model name" /proc/cpuinfo | head -n 1

/bin/sleep 60
