#!/bin/bash
#BSUB -J parallel_pi
#BSUB -q hpc
#BSUB -W 5
#BSUB -R "span[hosts=1]"
#BSUB -R "select[model==XeonGold6126]"
#BSUB -R "rusage[mem=1024]"
#BSUB -n 10
#BSUB -B
#BSUB -N
#BSUB -o parallel_pi.out
#BSUB -e parallel_pi.err

cd $LS_SUBCWD

echo "CPU model on this node:"
grep "model name" /proc/cpuinfo | head -n 1

echo "Serial:"
time python3 Fully_serial.py

echo "Fully parallel:"
time python3 Fully_parallel.py

echo "Chunked parallel:"
time python3 Chunked_parallel.py