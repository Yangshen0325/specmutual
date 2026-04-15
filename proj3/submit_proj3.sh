#!/bin/bash
###############################################################################
# proj3/submit_proj3.sh
#
# SLURM submission script for proj3 broad parameter-space exploration.
#
# Before submitting, generate the parameter table on the login node:
#   cd ~/specmutual
#   Rscript proj3/00_generate_params.R
#
# Then submit:
#   sbatch proj3/submit_proj3.sh
#
# This launches a 500-task array job. Each task runs one parameter
# combination (5 replicates, total_time = 10 MY).
#
# Runtime estimate:
#   - Per task: ~2-12 hours (varies with parameter regime)
#   - Wall-time limit: 48 hours (generous safety margin)
#   - Array throttle: max 100 simultaneous tasks (%100)
#   - Expected calendar time: 3-5 days depending on cluster load
###############################################################################

#SBATCH --job-name=proj3_sim
#SBATCH --output=proj3/logs/proj3-%A_%a.log
#SBATCH --time=2-00:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mem=8GB
#SBATCH --partition=regular
#SBATCH --array=1-500%100

# --- Create log directory if it does not exist --------------------------------
mkdir -p ~/specmutual/proj3/logs
mkdir -p ~/specmutual/proj3/results

# --- Load R module ------------------------------------------------------------
ml R

# --- Install package (safe to re-run; skips if already installed) -------------
Rscript -e "if (!requireNamespace('specmutual', quietly = TRUE)) remotes::install_github('Yangshen0325/specmutual')"

# --- Run simulation for this array task ---------------------------------------
combo_id=$SLURM_ARRAY_TASK_ID
echo "Starting combo_id=${combo_id} at $(date)"
Rscript ~/specmutual/proj3/01_run_sim.R ${combo_id}
echo "Finished combo_id=${combo_id} at $(date)"
