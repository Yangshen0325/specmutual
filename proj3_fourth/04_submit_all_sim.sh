#!/bin/bash
###############################################################################
# Submit from the repository root, ON THE CLUSTER, after transferring the design.
# Fifteen arrays of 1,000 tasks cover all 15,000 simulations. Submit only while
# the user's queue has room under the 6,000-task limit, then exit (do not wait).
# Run this same command again after slots are released; recorded batches are
# skipped. afterany dependencies retain at most 40 running simulations.
# This script submits jobs; it never runs simulations on the login node.
###############################################################################
set -euo pipefail
export SPEC_MUTUAL_BASE="${SPEC_MUTUAL_BASE:-$(pwd)}"
cd "${SPEC_MUTUAL_BASE}"
test -f proj3_fourth/data/generated/param_table.csv || {
  echo "Missing parameter table. Transfer the prepared design first." >&2
  exit 2
}
# SLURM opens logs BEFORE the job script runs, so make this directory here.
mkdir -p proj3_fourth/simulation_logs proj3_fourth/simulation_results

submission_log="proj3_fourth/simulation_submissions.tsv"
submit_limit=6000
cluster_user=$(id -un)
# The log records accepted submissions, not successful simulation outcomes.
# Keep this file when resuming; rerunning failed simulations is a separate step.
# Run only one copy of this wrapper at a time.
previous_job=""
if [[ -s "${submission_log}" ]]; then
  previous_job=$(awk 'END {print $1}' "${submission_log}")
fi
for offset in 0 1000 2000 3000 4000 5000 6000 7000 8000 9000 10000 11000 12000 13000 14000; do
  if [[ -f "${submission_log}" ]] && \
     awk -v offset="${offset}" '$2 == offset {found=1} END {exit !found}' "${submission_log}"; then
    continue
  fi
  # -r expands arrays: each pending/running task counts, including dependencies
  # and jobs from other projects. Without -r, an array may occupy just one line.
  queued_ids=$(squeue --array --noheader --user="${cluster_user}" --format="%F")
  queued_count=$(printf '%s\n' "${queued_ids}" | awk 'NF {n++} END {print n+0}')
  if (( queued_count + 1000 > submit_limit )); then
    echo "${queued_count} tasks currently queued; fewer than 1,000 slots available."
    echo "No more batches submitted. Rerun this command after slots are released."
    exit 0
  fi

  export PROJ3_FOURTH_BATCH_OFFSET="${offset}"
  # Only depend on a batch still in the queue, not an expired SLURM job record.
  if [[ -n "${previous_job}" ]] && \
     printf '%s\n' "${queued_ids}" | awk -v job="${previous_job}" '$1 == job {found=1} END {exit !found}'; then
    response=$(sbatch --parsable --export=ALL \
      "--dependency=afterany:${previous_job}" proj3_fourth/03_submit_sim.sh)
  else
    response=$(sbatch --parsable --export=ALL proj3_fourth/03_submit_sim.sh)
  fi
  # Federation-enabled SLURM may return job_id;cluster_name.
  previous_job="${response%%;*}"
  echo "Submitted ${previous_job}: simulation IDs $((offset+1))-$((offset+1000))"
  printf '%s\t%s\n' "${previous_job}" "${offset}" \
    >> "${submission_log}"
done
echo "All 15 batches have been submitted. This does not mean all simulations completed."
