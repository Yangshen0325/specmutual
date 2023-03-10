#!/bin/bash
#SBATCH --time=10:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=dco_set2_4
#SBATCH --output=logs/dco_set2_4-%j.log
#SBATCH --mem=4GB
#SBATCH --partition=regular

ml R/4.2.0-foss-2021b

Rscript -e "devtools::install_github('Yangshen0325/specmutual')"

Rscript ~/specmutual/script/job_dcoset2_4.R
