#!/bin/bash
#BSUB -J matmul_job
#BSUB -q hpc
#BSUB -n 8
#BSUB -R "select[model == XeonE5_2650v4]"
#BSUB -W 00:30
#BSUB -o /work3/02613/dump/matmul_%J.out
#BSUB -e /work3/02613/dump/matmul_%J.err

source /dtu/projects/02613_2025/conda/conda_init.sh
conda activate 02613

NUM_THREADS=8
OMP_NUM_THREADS=$NUM_THREADS
MPI_NUM_THREADS=$NUM_THREADS
MKL_NUM_THREADS=$NUM_THREADS
OPENBLAS_NUM_THREADS=$NUM_THREADS

python -u matmul.py \
    1> /work3/02613/dump/matmul_output_${LSB_JOBID}.txt \
    2> /work3/02613/dump/matmul_error_${LSB_JOBID}.txt