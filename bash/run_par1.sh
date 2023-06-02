#!/bin/bash
#SBATCH --time=12:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=job_par1
#SBATCH --output=logs/0206-%j.log
#SBATCH --mem=4GB
#SBATCH --partition=regular

ml R

Rscript -e "devtools::install_github('Yangshen0325/specmutual')"

sbatch ~/specmutual/script/job_par1.R
