#!/bin/bash
#SBATCH --job-name=part1_inverse
#SBATCH --output=/home1/p302656/specmutual/proj3_second/part1_logs/part1_%j.log
#SBATCH --time=12:00:00
#SBATCH --cpus-per-task=2
#SBATCH --mem=16G

set -euo pipefail

cd "$HOME/specmutual/proj3_second"

ml R

export PROJ3_PART1_CORES="${SLURM_CPUS_PER_TASK:-2}"

Rscript 12_part1_inverse_inference.R
