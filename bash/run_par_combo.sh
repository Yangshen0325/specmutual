#!/bin/bash
#SBATCH --time=3-02:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=job_par_combo
#SBATCH --output=logs/0509-%j.log
#SBATCH --mem=8GB
#SBATCH --partition=regular
#SBATCH --array=1-18

ml R

Rscript -e "devtools::install_github('Yangshen0325/specmutual')"

param_set=$SLURM_ARRAY_TASK_ID
Rscript ~/specmutual/script/job_par_combo.R ${param_set}
