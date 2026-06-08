#!/bin/bash
#BSUB -J cuda_vector_add
#BSUB -q c02613
#BSUB -W 2
#BSUB -R "span[hosts=1]"
#BSUB -R "rusage[mem=1024]"
#BSUB -R "select[gpu]"
#BSUB -gpu "num=1:mode=exclusive_process"
#BSUB -n 4
#BSUB -B
#BSUB -N
#BSUB -o cuda_add.out
#BSUB -e cuda_add.err

echo "Node info:"
hostname

echo "GPU info:"
nvidia-smi

echo "Running Python script now!"
python3 your_script.py