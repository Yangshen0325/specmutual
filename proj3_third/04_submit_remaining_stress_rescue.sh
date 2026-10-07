#!/bin/bash
###############################################################################
# Submit only the remaining stress-rescue simulations: IDs 6001-10000.
# The four arrays are chained with afterany so that only one batch is active
# at a time, preserving the concurrency control defined in
# 03_submit_stress_rescue.sh.
#
# Run from the repository root on the cluster:
#   bash proj3_third/04_submit_remaining_stress_rescue.sh
###############################################################################

set -euo pipefail

export SPEC_MUTUAL_BASE="${SPEC_MUTUAL_BASE:-$(pwd)}"
cd "${SPEC_MUTUAL_BASE}"

test -f proj3_third/stress_rescue_param_table.csv || {
  echo "Missing parameter table." >&2
  exit 2
}

mkdir -p \
  proj3_third/stress_rescue_logs \
  proj3_third/stress_rescue_results

previous_job=""

for offset in 6000 7000 8000 9000; do
  export PROJ3_THIRD_BATCH_OFFSET="${offset}"

  if [[ -n "${previous_job}" ]]; then
    response=$(sbatch --parsable --export=ALL \
      "--dependency=afterany:${previous_job}" \
      proj3_third/03_submit_stress_rescue.sh)
  else
    response=$(sbatch --parsable --export=ALL \
      proj3_third/03_submit_stress_rescue.sh)
  fi

  previous_job="${response%%;*}"

  echo "Submitted ${previous_job}: simulation IDs $((offset+1))-$((offset+1000))"

  printf '%s\t%s\n' "${previous_job}" "${offset}" \
    >> proj3_third/stress_rescue_submissions.tsv
done
