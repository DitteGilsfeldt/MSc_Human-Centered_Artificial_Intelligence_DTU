#!/bin/bash
#BSUB -J myarray[1-10]
#BSUB -q hpc
#BSUB -n 1
#BSUB -R "span[hosts=1]"
#BSUB -W 00:05
#BSUB -o myarray_%J_%I.out
#BSUB -e myarray_%J_%I.err

source /dtu/projects/02613_2025/conda/conda_init.sh

conda activate 02613

python preprocess.py input001.png