#!/bin/bash
#SBATCH --time=2-02:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=job_mu02
#SBATCH --output=logs/s1209-%j.log
#SBATCH --mem=16GB
#SBATCH --partition=regular
#SBATCH --array=1-11

ml R

Rscript -e "devtools::install_github('Yangshen0325/specmutual')"

param_set=$SLURM_ARRAY_TASK_ID
Rscript ~/specmutual/script/job_mu02.R ${param_set}
