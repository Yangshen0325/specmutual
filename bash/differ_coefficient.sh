#!/bin/bash
#SBATCH --time=2-00:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=differ_coefficient
#SBATCH --output=logs/differ_coefficient-%j.log
#SBATCH --mem=4GB
#SBATCH --partition=regular

ml R/4.2.0-foss-2021b

Rscript -e "devtools::install_github('Yangshen0325/specmutual')"

param_set=${1}
Rscript ~/specmutual/script/job_differ_coefficient.R ${param_set}
