#!/bin/bash
#BSUB -J job3
#BSUB -q hpc
#BSUB -n 1
#BSUB -R "span[hosts=1]"
#BSUB -W 00:05
#BSUB -w 1234567
#BSUB -o job3_%J.out
#BSUB -e job3_%J.err

source /dtu/projects/02613_2025/conda/conda_init.sh

conda activate 02613

python preprocess.py input001.png