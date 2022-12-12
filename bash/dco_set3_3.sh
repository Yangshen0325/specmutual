#!/bin/bash
#SBATCH --time=10:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=dco_set3_3
#SBATCH --output=logs/dco_set3_3-%j.log
#SBATCH --mem=4GB
#SBATCH --partition=regular

ml R/4.2.0-foss-2021b

Rscript -e "devtools::install_github('Yangshen0325/specmutual')"

Rscript ~/specmutual/script/job_dcoset3_3.R
