#!/bin/bash
#SBATCH --time=03:20:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=same_stt
#SBATCH --output=logs/same_stt-%j.log
#SBATCH --mem=4GB
#SBATCH --partition=regular

ml R

Rscript -e "devtools::install_github('Yangshen0325/specmutual')"

Rscript ~/specmutual/script/same_stt.R
