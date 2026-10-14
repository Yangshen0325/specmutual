#!/bin/bash
###############################################################################
# One 1,000-task SLURM array. Each task runs ONE simulation, not 10 simulations.
# Submit available batches with: bash proj3_fourth/04_submit_all_sim.sh
# Rerun that command after queue slots are released to submit remaining batches.
# Keep submissions rooted in the specmutual repository, as in the old workflow.
###############################################################################
#SBATCH --job-name=proj4-range
#SBATCH --output=proj3_fourth/simulation_logs/sim-%A_%a.log
#SBATCH --error=proj3_fourth/simulation_logs/sim-%A_%a.log
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

# Previous runs needed about 18 hours and 5.16 GB. Request 36 hours and 8 GB
# per simulation as headroom, not a measured upper bound for the new ranges.
# Keep the internal 35-hour cap, leaving about one hour before SLURM's deadline
# for stopping and saving. The other model safety limits remain unchanged.
export PROJ3_FOURTH_TOTAL_TIME="${PROJ3_FOURTH_TOTAL_TIME:-10}"
export PROJ3_FOURTH_MAX_EVENTS="${PROJ3_FOURTH_MAX_EVENTS:-60000}"
export PROJ3_FOURTH_MAX_MATRIX_CELLS="${PROJ3_FOURTH_MAX_MATRIX_CELLS:-8000000}"
export PROJ3_FOURTH_MAX_SPECIES_PER_GUILD="${PROJ3_FOURTH_MAX_SPECIES_PER_GUILD:-6000}"
export PROJ3_FOURTH_MAX_RUNTIME_S="${PROJ3_FOURTH_MAX_RUNTIME_S:-126000}"
export PROJ3_FOURTH_SAVE_STATE="${PROJ3_FOURTH_SAVE_STATE:-false}"
export PROJ3_FOURTH_FORCE="${PROJ3_FOURTH_FORCE:-false}"
export PROJ3_FOURTH_RERUN_SAFETY_STOPPED="${PROJ3_FOURTH_RERUN_SAFETY_STOPPED:-false}"

# Offsets 0,1000,...,14000 translate the local array task into the stable CSV ID.
simulation_id=$(( ${PROJ3_FOURTH_BATCH_OFFSET:-0} + SLURM_ARRAY_TASK_ID ))
echo "Starting simulation ${simulation_id} at $(date --iso-8601=seconds)"
echo "Host: $(hostname); job: ${SLURM_JOB_ID}; array task: ${SLURM_ARRAY_TASK_ID}"
Rscript proj3_fourth/02_run_sim.R "${simulation_id}"
echo "Finished simulation ${simulation_id} at $(date --iso-8601=seconds)"
