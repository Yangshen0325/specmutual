#!/bin/bash
#SBATCH --time = 20:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=job_par_comb0
#SBATCH --output=logs/test-%j.log
#SBATCH --mem=4GB
#SBATCH --partition=gelifes
#SBATCH --array=1-4

ml R

Rscript -e "devtools::install_github('Yangshen0325/specmutual')"

param_set=$SLURM_ARRAY_TASK_ID
Rscript ~/specmutual/script/job_testrep50.R ${param_set}
