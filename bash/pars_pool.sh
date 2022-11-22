#!/bin/bash
#SBATCH --time=00:22:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=job_pars_pool
#SBATCH --output=logs/job_pars_pool-%j.log
#SBATCH --mem=1GB
#SBATCH --partition=short

ml R

Rscript -e "devtools::install_github('Yangshen0325/specmutual')"

for (( param_set = 1; param_set <= 7; param_set++ ))
do
sbatch ~/specmutual/bash/pars_pool.sh ${param_set}
done
