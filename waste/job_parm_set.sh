#!/bin/bash
#SBATCH --time=03:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=job_parm_set
#SBATCH --output=logs/job_parm_set-%j.log
#SBATCH --mem=4GB
#SBATCH --partition=regular

ml R

Rscript -e "devtools::install_github('Yangshen0325/specmutual')"

param_set=${1}
Rscript ~/specmutual/script/job_pars_pool.R ${param_set}
