#!/bin/bash
#SBATCH --time=03:20:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=job_simu_two
#SBATCH --output=logs/job_simu_two-%j.log
#SBATCH --mem=4GB
#SBATCH --partition=regular

ml R

Rscript -e "devtools::install_github('Yangshen0325/specmutual')"

Rscript ~/specmutual/script/job_simu_two.R
