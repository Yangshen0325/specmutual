#!/bin/bash
###############################################################################
# Submit from the repository root, ON THE CLUSTER, after transferring the design.
# Ten arrays avoid requiring a cluster MaxArraySize above 10,000. The afterany
# dependency chains batches, retaining the old maximum of 40 simultaneous tasks
# across the whole experiment, even when individual tasks fail.
# This script submits jobs; it never runs simulations on the login node.
###############################################################################
set -euo pipefail
export SPEC_MUTUAL_BASE="${SPEC_MUTUAL_BASE:-$(pwd)}"
cd "${SPEC_MUTUAL_BASE}"
test -f proj3_third/stress_rescue_param_table.csv || {
  echo "Missing parameter table. Transfer the prepared design first." >&2
  exit 2
}
# SLURM opens logs BEFORE the job script runs, so make this directory here.
mkdir -p proj3_third/stress_rescue_logs proj3_third/stress_rescue_results

previous_job=""
for offset in 0 1000 2000 3000 4000 5000 6000 7000 8000 9000; do
  export PROJ3_THIRD_BATCH_OFFSET="${offset}"
  if [[ -n "${previous_job}" ]]; then
    response=$(sbatch --parsable --export=ALL \
      "--dependency=afterany:${previous_job}" proj3_third/03_submit_stress_rescue.sh)
  else
    response=$(sbatch --parsable --export=ALL proj3_third/03_submit_stress_rescue.sh)
  fi
  # Federation-enabled SLURM may return job_id;cluster_name.
  previous_job="${response%%;*}"
  echo "Submitted ${previous_job}: simulation IDs $((offset+1))-$((offset+1000))"
  printf '%s\t%s\n' "${previous_job}" "${offset}" \
    >> proj3_third/stress_rescue_submissions.tsv
done
