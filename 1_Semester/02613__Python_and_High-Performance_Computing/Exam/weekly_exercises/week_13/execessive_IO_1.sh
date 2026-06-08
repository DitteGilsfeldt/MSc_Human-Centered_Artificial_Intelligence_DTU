#!/bin/bash
#BSUB -J story_job
#BSUB -q hpc
#BSUB -n 1
#BSUB -R "select[model == XeonE5_2650v4]"
#BSUB -W 00:05
#BSUB -o /work3/02613/dump/story_%J.out
#BSUB -e /work3/02613/dump/story_%J.err

source /dtu/projects/02613_2025/conda/conda_init.sh
conda activate 02613

python -u story.py \
1> /work3/02613/dump/output_${LSB_JOBID}.txt \
2> /work3/02613/dump/error_${LSB_JOBID}.txt