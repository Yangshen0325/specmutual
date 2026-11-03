#!/bin/bash
###############################################################################
# Submit from the specmutual repository root on the school cluster:
#   sbatch proj3_fourth_higher_range_50k/07_submit_part1_inverse_inference_higher_range.sh
#
# Collect completed summaries before submitting this RF job:
#   Rscript proj3_fourth_higher_range_50k/05_collect_results.R
# This job runs nine parameter analyses using only higher_range communities.
# It reads only this separate 50,000-row experiment.
# Logs go directly into the existing experiment folder so SLURM can open them
# before R creates any analysis-output directories. No new simulations are run.
###############################################################################
#SBATCH --job-name=proj4-50k-rf-higher
#SBATCH --output=proj3_fourth_higher_range_50k/part1-rf-higher_range-%j.log
#SBATCH --error=proj3_fourth_higher_range_50k/part1-rf-higher_range-%j.log
#SBATCH --time=1-12:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=9
#SBATCH --mem=64G
#SBATCH --partition=regular

set -euo pipefail
BASE_DIR="${SPEC_MUTUAL_BASE:-${SLURM_SUBMIT_DIR:?Submit from the repository root}}"
cd "${BASE_DIR}"
export SPEC_MUTUAL_BASE="${BASE_DIR}"

if command -v ml >/dev/null 2>&1; then
  ml R
elif command -v module >/dev/null 2>&1; then
  module load R
fi

# More rows increase forest and OOF-table memory; request 64 GB / 36 hours
# as initial headroom, not a measured requirement. Check partition limits.
# One worker per parameter; each ranger fit and prediction uses one thread.
# Keep numerical-library threads at one to avoid multiplying the CPU request.
export PROJ3_FOURTH_50K_PART1_CORES="${SLURM_CPUS_PER_TASK:-1}"
export PROJ3_FOURTH_50K_PART1_TREES="${PROJ3_FOURTH_50K_PART1_TREES:-500}"
export PROJ3_FOURTH_50K_PART1_PERMUTATIONS="${PROJ3_FOURTH_50K_PART1_PERMUTATIONS:-5}"
export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1

echo "Part I (higher_range) started at $(date --iso-8601=seconds); host: $(hostname)"
# Read data/processed/part1_analysis_ready_higher_range.rds produced by collector 05.
# Outputs go to data/processed/part1_inverse_inference_higher_range/.
Rscript proj3_fourth_higher_range_50k/06_part1_inverse_inference_higher_range.R
echo "Part I (higher_range) finished at $(date --iso-8601=seconds)"
