#!/bin/bash
#SBATCH --time=2-02:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=job_estdist
#SBATCH --output=logs/%1403-j.log
#SBATCH --mem=64GB
#SBATCH --partition=regular
#SBATCH --array=1-21

ml R

Rscript -e "devtools::install_github('Yangshen0325/specmutual')"

param_set=$SLURM_ARRAY_TASK_ID
Rscript ~/specmutual/script/job_estdist.R ${param_set}
