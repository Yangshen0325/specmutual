#!/bin/bash
###############################################################################
# Submit from the specmutual repository root on the school cluster:
#   sbatch proj3_third/07_submit_part1_inverse_inference.sh
#
# This is one analysis job, not a simulation array. It first collects the
# available results, then runs the nine inverse-inference parameter analyses.
# Logs go directly into the existing proj3_third folder so SLURM can open them
# before R creates any analysis-output directories. No new simulations are run.
###############################################################################
#SBATCH --job-name=proj3-part1-rf
#SBATCH --output=proj3_third/part1-rf-%j.log
#SBATCH --error=proj3_third/part1-rf-%j.log
#SBATCH --time=1-12:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=9
#SBATCH --mem=32G
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
export PROJ3_THIRD_PART1_CORES="${SLURM_CPUS_PER_TASK:-1}"
export PROJ3_THIRD_PART1_TREES="${PROJ3_THIRD_PART1_TREES:-500}"
export PROJ3_THIRD_PART1_PERMUTATIONS="${PROJ3_THIRD_PART1_PERMUTATIONS:-5}"
export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1

echo "Part I started at $(date --iso-8601=seconds); host: $(hostname)"
# Use a stable copy of the result directory during collection. It is fine for
# some design rows to be absent; the collector records them rather than imputing.
Rscript proj3_third/05_collect_stress_rescue_results.R
Rscript proj3_third/06_part1_inverse_inference.R
echo "Part I finished at $(date --iso-8601=seconds)"
