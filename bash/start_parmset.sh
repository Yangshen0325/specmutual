#!/bin/bash
#SBATCH --time=00:20:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=start_job_parmset
#SBATCH --output=logs/start_job_parmset.log
#SBATCH --mem=1GB
#SBATCH --partition=short

ml R

Rscript -e "devtools::install_github('Yangshen0325/specmutual')"

for (( param_set = 1; param_set <= 4; param_set++ ))
do
sbatch ~/specmutual/bash/job_parm_set.sh ${param_set}
done
