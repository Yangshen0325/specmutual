#!/bin/bash
#SBATCH --time=3-02:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=job_K1
#SBATCH --output=logs/%j.log
#SBATCH --mem=32GB
#SBATCH --partition=regular
#SBATCH --array=1-21

ml R

Rscript -e "devtools::install_github('Yangshen0325/specmutual')"

param_set=$SLURM_ARRAY_TASK_ID
Rscript ~/specmutual/script/job_K1.R ${param_set}
