#!/bin/bash
#SBATCH --time=22:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=job_par3
#SBATCH --output=logs/c-%j.log
#SBATCH --mem=4GB
#SBATCH --partition=regular

ml R

Rscript -e "remotes::install_github('Yangshen0325/specmutual')"

Rscript ~/specmutual/script/job_par3.R
