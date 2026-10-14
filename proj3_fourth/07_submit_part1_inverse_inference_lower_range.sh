#!/bin/bash
###############################################################################
# Submit from the specmutual repository root on the school cluster:
#   sbatch proj3_fourth/07_submit_part1_inverse_inference_lower_range.sh
#
# Prepare the band datasets ONCE before submitting either of the two RF jobs:
#   Rscript proj3_fourth/05_collect_results.R
# This job runs nine parameter analyses using only lower_range communities.
# It does not recollect or overwrite inputs shared with the other band jobs.
# Logs go directly into the existing proj3_fourth folder so SLURM can open them
# before R creates any analysis-output directories. No new simulations are run.
###############################################################################
#SBATCH --job-name=proj4-rf-lower_range
#SBATCH --output=proj3_fourth/part1-rf-lower_range-%j.log
#SBATCH --error=proj3_fourth/part1-rf-lower_range-%j.log
#SBATCH --time=15:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=9
#SBATCH --mem=6G
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

# One worker per parameter; each ranger fit and prediction uses one thread.
# Keep numerical-library threads at one to avoid multiplying the CPU request.
export PROJ3_FOURTH_PART1_CORES="${SLURM_CPUS_PER_TASK:-1}"
export PROJ3_FOURTH_PART1_TREES="${PROJ3_FOURTH_PART1_TREES:-500}"
export PROJ3_FOURTH_PART1_PERMUTATIONS="${PROJ3_FOURTH_PART1_PERMUTATIONS:-5}"
export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1

echo "Part I (lower_range) started at $(date --iso-8601=seconds); host: $(hostname)"
# Read data/processed/part1_analysis_ready_lower_range.rds produced by collector 05.
# Outputs go to data/processed/part1_inverse_inference_lower_range/.
Rscript proj3_fourth/06_part1_inverse_inference_lower_range.R
echo "Part I (lower_range) finished at $(date --iso-8601=seconds)"
