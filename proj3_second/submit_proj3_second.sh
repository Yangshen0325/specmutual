#!/bin/bash
###############################################################################
# SLURM array for the 1,000-combination proj3_second pilot.
#
# From the repository root:
#   Rscript proj3_second/00_generate_params.R
#   sbatch proj3_second/submit_proj3_second.sh
#
# Retry missing files after collection:
#   Rscript proj3_second/02_collect_results.R
#   FAILED=$(cat proj3_second/missing_array.txt)
#   if [ -n "${FAILED}" ]; then
#     sbatch --array="${FAILED}%25" proj3_second/submit_proj3_second.sh
#   fi
###############################################################################

#SBATCH --job-name=proj3pilot
#SBATCH --output=proj3_second/logs/pilot-%A_%a.log
#SBATCH --time=08:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=8G
#SBATCH --partition=regular
#SBATCH --array=1-1000%100

set -euo pipefail

BASE_DIR="${SPEC_MUTUAL_BASE:-${SLURM_SUBMIT_DIR:-$HOME/specmutual}}"
RUN_DIR="${BASE_DIR}/proj3_second"

cd "${BASE_DIR}"
mkdir -p "${RUN_DIR}/logs" "${RUN_DIR}/results"

if command -v ml >/dev/null 2>&1; then
  ml R
elif command -v module >/dev/null 2>&1; then
  module load R
fi

export SPEC_MUTUAL_BASE="${BASE_DIR}"
export PROJ3_TOTAL_TIME="${PROJ3_TOTAL_TIME:-10}"
export PROJ3_MAX_EVENTS="${PROJ3_MAX_EVENTS:-20000}"
export PROJ3_MAX_MATRIX_CELLS="${PROJ3_MAX_MATRIX_CELLS:-4000000}"
export PROJ3_MAX_SPECIES_PER_GUILD="${PROJ3_MAX_SPECIES_PER_GUILD:-1500}"
export PROJ3_MAX_RUNTIME_S="${PROJ3_MAX_RUNTIME_S:-25200}"
export PROJ3_SAVE_STATE="${PROJ3_SAVE_STATE:-false}"

combo_id="${SLURM_ARRAY_TASK_ID}"
echo "Starting combo ${combo_id} at $(date)"
echo "Host: $(hostname); job: ${SLURM_JOB_ID:-NA}; array task: ${combo_id}"
echo "Base directory: ${BASE_DIR}"
echo "Limits: time=${PROJ3_TOTAL_TIME}, events=${PROJ3_MAX_EVENTS}, cells=${PROJ3_MAX_MATRIX_CELLS}, runtime_s=${PROJ3_MAX_RUNTIME_S}"

Rscript "${RUN_DIR}/01_run_pilot.R" "${combo_id}"

echo "Finished combo ${combo_id} at $(date)"
