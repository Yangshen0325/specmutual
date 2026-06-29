#!/bin/bash
###############################################################################
# proj3/part2_newSim/submit_proj3.sh
#
# SLURM submission script for proj3 broad parameter-space exploration.
#
# First pass, tuned for the many short/low-memory combinations:
#   sbatch proj3/part2_newSim/submit_proj3.sh
#
# Retry only missing/failed combinations with larger resources:
#   FAILED=$(Rscript proj3/part2_newSim/02_failed_array.R)
#   if [ -n "${FAILED}" ]; then
#     sbatch --array="${FAILED}%20" --time=3-00:00:00 --mem=20G \
#       proj3/part2_newSim/submit_proj3.sh
#   fi
#
# Each array task runs one parameter combination. 01_run_sim.R checkpoints
# after each completed replicate, so retry jobs resume instead of restarting.
#
###############################################################################

#SBATCH --job-name=proj3_sim
#SBATCH --output=proj3/part2_newSim/logs/proj3-%A_%a.log
#SBATCH --time=03:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=2G
#SBATCH --partition=regular
#SBATCH --array=1-1000%100

set -euo pipefail # stop immediately if anything goes wrong

BASE_DIR="${SPEC_MUTUAL_BASE:-$HOME/specmutual}"
RUN_DIR="${BASE_DIR}/proj3/part2_newSim"

# --- Create runtime directories if they do not exist --------------------------
mkdir -p "${RUN_DIR}/logs"
mkdir -p "${RUN_DIR}/results"
mkdir -p "${RUN_DIR}/checkpoints"
mkdir -p "${RUN_DIR}/status"

# --- Load R module ------------------------------------------------------------
ml R

# --- Run simulation for this array task ---------------------------------------
combo_id=$SLURM_ARRAY_TASK_ID
echo "Starting combo_id=${combo_id} at $(date)"
echo "SLURM job ${SLURM_JOB_ID:-NA}, task ${SLURM_ARRAY_TASK_ID:-NA}"
echo "Resources: cpus=${SLURM_CPUS_PER_TASK:-1}, mem_per_node=${SLURM_MEM_PER_NODE:-unknown} MB"

export SPEC_MUTUAL_BASE="${BASE_DIR}"
Rscript "${RUN_DIR}/01_run_sim.R" "${combo_id}"
echo "Finished combo_id=${combo_id} at $(date)"
