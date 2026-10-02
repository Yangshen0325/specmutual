#!/bin/bash
###############################################################################
# One 1,000-task SLURM array. Each task runs ONE simulation, not 10 simulations.
# Submit all 10 batches with: bash proj3_third/04_submit_all_stress_rescue.sh
# Keep submissions rooted in the specmutual repository, as in the old workflow.
###############################################################################
#SBATCH --job-name=proj3-rescue
#SBATCH --output=proj3_third/stress_rescue_logs/rescue-%A_%a.log
#SBATCH --error=proj3_third/stress_rescue_logs/rescue-%A_%a.log
#SBATCH --time=1-12:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=8G
#SBATCH --partition=regular
#SBATCH --array=1-1000%40

set -euo pipefail
BASE_DIR="${SPEC_MUTUAL_BASE:-${SLURM_SUBMIT_DIR:?Submit from the repository root}}"
cd "${BASE_DIR}"
export SPEC_MUTUAL_BASE="${BASE_DIR}"

if command -v ml >/dev/null 2>&1; then
  ml R
elif command -v module >/dev/null 2>&1; then
  module load R
fi

# These are the previous safety caps, with 35 hours for the simulation loop
# inside a 36-hour allocation. A cap hit is recorded, never treated as extinction.
export PROJ3_THIRD_TOTAL_TIME="${PROJ3_THIRD_TOTAL_TIME:-10}"
export PROJ3_THIRD_MAX_EVENTS="${PROJ3_THIRD_MAX_EVENTS:-60000}"
export PROJ3_THIRD_MAX_MATRIX_CELLS="${PROJ3_THIRD_MAX_MATRIX_CELLS:-8000000}"
export PROJ3_THIRD_MAX_SPECIES_PER_GUILD="${PROJ3_THIRD_MAX_SPECIES_PER_GUILD:-6000}"
export PROJ3_THIRD_MAX_RUNTIME_S="${PROJ3_THIRD_MAX_RUNTIME_S:-126000}"
export PROJ3_THIRD_SAVE_STATE="${PROJ3_THIRD_SAVE_STATE:-false}"
export PROJ3_THIRD_FORCE="${PROJ3_THIRD_FORCE:-false}"
export PROJ3_THIRD_RERUN_SAFETY_STOPPED="${PROJ3_THIRD_RERUN_SAFETY_STOPPED:-false}"

# Offsets 0,1000,...,9000 translate the local array task into the stable CSV ID.
simulation_id=$(( ${PROJ3_THIRD_BATCH_OFFSET:-0} + SLURM_ARRAY_TASK_ID ))
echo "Starting simulation ${simulation_id} at $(date --iso-8601=seconds)"
echo "Host: $(hostname); job: ${SLURM_JOB_ID}; array task: ${SLURM_ARRAY_TASK_ID}"
Rscript proj3_third/02_run_stress_rescue_sim.R "${simulation_id}"
echo "Finished simulation ${simulation_id} at $(date --iso-8601=seconds)"
