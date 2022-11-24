#!/bin/bash
#SBATCH --time=00:22:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=pars_pool
#SBATCH --output=logs/pars_pool-%j.log
#SBATCH --mem=4GB
#SBATCH --partition=regular

ml R/4.2.0-foss-2021b

Rscript -e "devtools::install_github('Yangshen0325/specmutual')"

param_set=${1}
Rscript ~/specmutual/script/job_pars_pool.R ${param_set}

