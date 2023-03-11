#!/bin/bash
#SBATCH --time=00:04:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=run_differ_coefficient
#SBATCH --output=logs/run_differ_coefficient-%j.log
#SBATCH --mem=1GB
#SBATCH --partition=short

ml R/4.2.0-foss-2021b

Rscript -e "devtools::install_github('Yangshen0325/specmutual')"

for (( param_set = 1; param_set <= 3; param_set++ ))
do
sbatch ~/specmutual/bash/differ_coefficient.sh ${param_set}
done
