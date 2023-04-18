#!/bin/bash
#SBATCH --time=00:20:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=start_run_jobparcombi
#SBATCH --output=logs/start_run_jobparcombi.log
#SBATCH --mem=1GB
#SBATCH --partition=regular

ml R

Rscript -e "devtools::install_github('Yangshen0325/specmutual')"

for (( param_set = 1; param_set <= 5; param_set++ ))
do
sbatch ~/specmutual/bash/run_job_par_combi.sh ${param_set}
done
