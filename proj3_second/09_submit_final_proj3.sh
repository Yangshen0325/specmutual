#!/bin/bash
###############################################################################
# SLURM array for the 1,000-row final Part II design.
#
# Prepare (do not run on a login node as a batch):
#   Rscript proj3_second/05_generate_final_params_proj3.R
#   Rscript proj3_second/08_validate_final_workflow_proj3.R
#
# Submit:
#   sbatch proj3_second/09_submit_final_proj3.sh
#
# Collect and retry failures/missing tasks:
#   Rscript proj3_second/07_collect_final_results_proj3.R
#   RETRY_IDS="$(cat proj3_second/final_retry_array.txt)"
#   if [[ -n "${RETRY_IDS}" ]]; then
#     sbatch --array="${RETRY_IDS}%20" proj3_second/09_submit_final_proj3.sh
#   fi
###############################################################################

#SBATCH --job-name=proj3-final
#SBATCH --output=proj3_second/final_logs/final-%A_%a.out
#SBATCH --error=proj3_second/final_logs/final-%A_%a.err
#SBATCH --time=08:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=8G
#SBATCH --partition=regular
#SBATCH --array=1-1000%40

set -euo pipefail

if [[ -n "${SPEC_MUTUAL_BASE:-}" ]]; then
  BASE_DIR="${SPEC_MUTUAL_BASE}"
elif [[ -n "${SLURM_SUBMIT_DIR:-}" ]]; then
  BASE_DIR="${SLURM_SUBMIT_DIR}"
else
  echo "Set SPEC_MUTUAL_BASE or submit from the repository root." >&2
  exit 2
fi

RUN_DIR="${BASE_DIR}/proj3_second"
PARAM_FILE="${RUN_DIR}/final_param_table.csv"
RUNNER="${RUN_DIR}/06_run_final_sim_proj3.R"

cd "${BASE_DIR}"

if [[ ! -f "${PARAM_FILE}" ]]; then
  echo "Missing final parameter table: ${PARAM_FILE}" >&2
  exit 2
fi
if [[ ! -f "${RUNNER}" ]]; then
  echo "Missing final simulation runner: ${RUNNER}" >&2
  exit 2
fi

mkdir -p "${RUN_DIR}/final_results" "${RUN_DIR}/final_logs"

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
export PROJ3_FORCE="${PROJ3_FORCE:-false}"
export PROJ3_RERUN_SAFETY_STOPPED="${PROJ3_RERUN_SAFETY_STOPPED:-false}"

simulation_id="${SLURM_ARRAY_TASK_ID}"
echo "Starting simulation ${simulation_id} at $(date --iso-8601=seconds)"
echo "Host: $(hostname); job: ${SLURM_JOB_ID:-NA}; array task: ${simulation_id}"
echo "Base directory: ${BASE_DIR}"
echo "Limits: model_time=${PROJ3_TOTAL_TIME}, events=${PROJ3_MAX_EVENTS}, matrix_cells=${PROJ3_MAX_MATRIX_CELLS}, runtime_s=${PROJ3_MAX_RUNTIME_S}"

Rscript "${RUNNER}" "${simulation_id}"

echo "Finished simulation ${simulation_id} at $(date --iso-8601=seconds)"
