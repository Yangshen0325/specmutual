#!/bin/bash
#SBATCH --time=1-20:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=job_par2
#SBATCH --output=logs/par2-%j.log
#SBATCH --mem=4GB
#SBATCH --partition=regular

ml R


Rscript ~/specmutual/script/job_par2.R
