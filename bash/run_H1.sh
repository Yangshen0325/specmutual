#!/bin/bash
#SBATCH --time=1-06:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=job_H1_age10
#SBATCH --output=logs/a0311-%j.log
#SBATCH --mem=32GB
#SBATCH --partition=regular

ml R

Rscript -e "devtools::install_github('Yangshen0325/specmutual')"

Rscript ~/specmutual/script/job_H1_age10.R
