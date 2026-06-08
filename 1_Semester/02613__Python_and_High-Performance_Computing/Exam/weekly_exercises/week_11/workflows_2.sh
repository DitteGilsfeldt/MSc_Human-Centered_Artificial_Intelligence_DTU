#!/bin/bash
#BSUB -J myindices[2,29,71,73,127]
#BSUB -q hpc
#BSUB -n 1
#BSUB -R "span[hosts=1]"
#BSUB -W 00:05
#BSUB -o myindices_%J_%I.out
#BSUB -e myindices_%J_%I.err

source /dtu/projects/02613_2025/conda/conda_init.sh

conda activate 02613

python preprocess.py input001.png