#!/bin/bash
#SBATCH --time=23:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=job_pars_combo
#SBATCH --output=logs/%j.log
#SBATCH --mem=15GB
#SBATCH --partition=regular
#SBATCH --array=1-16

module load R-bundle-CRAN/2023.12-foss-2023a
Rscript -e "devtools::install_github('Yangshen0325/specmutual')"

param_set=$SLURM_ARRAY_TASK_ID
Rscript ~/extinction_005_02/job_pars_combo.R ${param_set}
