#!/bin/bash
#SBATCH --time=12:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=job_par_combi
#SBATCH --output=logs/job_par_combi-%j.log
#SBATCH --mem=4GB
#SBATCH --partition=regular

ml R

Rscript -e "devtools::install_github('Yangshen0325/specmutual')"

param_set=${1}
Rscript ~/specmutual/script/job_par_combi.R ${param_set}