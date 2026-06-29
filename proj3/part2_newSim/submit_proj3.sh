#!/bin/bash
###############################################################################
# proj3/part2_newSim/submit_proj3.sh
#
# SLURM submission script for proj3 broad parameter-space exploration.
#
# Then submit:
#   sbatch proj3/part2_newSim/submit_proj3.sh
#
# This launches a 1000-task array job. Each task runs one parameter
# combination (10 replicates, total_time = 10 MY).
#
###############################################################################

#SBATCH --job-name=proj3_sim
#SBATCH --output=proj3/part2_newSim/logs/proj3-%A_%a.log
#SBATCH --time=4-00:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mem=40GB
#SBATCH --partition=regular
#SBATCH --array=1-1000%100

# --- Create log directory if it does not exist --------------------------------
mkdir -p ~/specmutual/proj3/part2_newSim/logs
mkdir -p ~/specmutual/proj3/part2_newSim/results
# mkdir -p ~/specmutual/proj3/results_Nomutual

set -euo pipefail # stop immediately if anything goes wrong

# --- Load R module ------------------------------------------------------------
ml R


# --- Run simulation for this array task ---------------------------------------
combo_id=$SLURM_ARRAY_TASK_ID
echo "Starting combo_id=${combo_id} at $(date)"
Rscript ~/specmutual/proj3/part2_newSim/01_run_sim.R ${combo_id}
echo "Finished combo_id=${combo_id} at $(date)"
