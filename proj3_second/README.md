# Proj3 second: provisional range-calibration pilot

This directory contains a 1,000-combination pilot for selecting the final parameter intervals. It is not the final Part I/Part II dataset.

## What changed from the old workflow

The old `proj3` workflow sampled 500 Latin-hypercube parameter combinations, attempted five accepted simulations at each combination, discarded realizations whose final network was smaller than 2 x 2, saved full simulation states, and averaged summary statistics across the five retained replicates.

The new pilot:

- uses 1,000 unique parameter combinations;
- runs exactly one stochastic realization per combination;
- does not reject sparse or empty outcomes;
- does not average simulations;
- retains the same 10-My model process and fixed mainland network;
- records 19 community summaries and process-level diagnostics;
- stores online event counts and response summaries instead of full rate matrices at every Gillespie event.

The existing `proj3/part2_newSim` attempt is not used because it still requests 10 replicates and retains the old ranges.

## Pilot design

The table is generated reproducibly by `00_generate_params.R`.

| Parameter | Provisional pilot domain | Sampling |
|---|---:|---|
| `lac_0` | 0.03-1.20 | log-uniform |
| `mu_0` | 0.02-0.80 | log-uniform |
| `gam_0` | 0.005-0.30 | log-uniform |
| `laa_0` | 0.03-2.50 | log-uniform |
| `K_0` | 15-140 | uniform |
| `K_1` | 0-90 | `90 u^3`, concentrated near zero |
| `mu_1` | 0-0.15 | uniform |
| `laa_1` | 0-0.25 | uniform |
| `lambda0` | 0-1.50 | uniform |

There are 900 maximin-LHS points over all nine parameters. The other 100 are unique diagnostic anchors: 20 no-mutualism points and 20 points with only `K_1`, `mu_1`, `laa_1`, or `lambda0` active. These are not repeated parameter combinations; their intrinsic parameters differ.

The domains are deliberately wider than intended final intervals. `K_1` is the exception to naive uniform widening: old high-`K_1` simulations produced explosive communities, so high values remain represented but are uncommon.

## Compact diagnostics

`R/sim_core_mutualism_proj3.R` uses the published rate and state-transition functions. It tracks:

- counts and cumulative hazards for all 11 event types;
- time- and species-weighted `mu_1 d_i`;
- time- and species-weighted `laa_1 D_i` and `(laa_1 D_i) / laa_0`;
- time- and species-weighted `K_1 d_i / K_0` for present species and for
  mainland immigration candidates;
- final degree and `D_i` landmarks;
- runtime, event count, maximum richness, and matrix dimensions;
- safety-stop reasons for explosive or excessively slow settings.

The final state is summarized and discarded before saving unless `PROJ3_SAVE_STATE=true`.

## Run

From the repository root:

```bash
Rscript proj3_second/00_generate_params.R
sbatch proj3_second/submit_proj3_second.sh
```

The submission script assumes the repository is the SLURM submission directory. To use another location:

```bash
SPEC_MUTUAL_BASE=/path/to/specmutual sbatch proj3_second/submit_proj3_second.sh
```

After the array finishes:

```bash
Rscript proj3_second/02_collect_results.R
Rscript proj3_second/03_analyze_pilot.R
```

Retry missing or unreadable tasks:

```bash
FAILED=$(cat proj3_second/missing_array.txt)
if [ -n "${FAILED}" ]; then
  sbatch --array="${FAILED}%25" proj3_second/submit_proj3_second.sh
fi
```

Safety-stopped simulations are retained as informative pathological outcomes. They are not listed as missing. A result stopped under a shorter local runtime cap is automatically rerun when a larger cap is supplied.

## Important outputs

- `param_table.csv`: the frozen 1,000-combination design.
- `pilot_ranges.csv`: machine-readable range specification and rationale.
- `results/combo_XXXX.rds`: compact per-combination results.
- `pilot_results_combined.csv` and `.rds`: collected analysis table.
- `PILOT_RESULTS_REPORT.md`: automatically generated post-run report.
- `figures/pilot_process_response.png`: parameter-to-process response plots.
- `PILOT_REPORT.md`: current handoff report, including old-output evidence and local validation.

## Validation

Run:

```bash
Rscript proj3_second/04_validate_core.R
```

With the same seed and parameters, the new core must exactly match the published core's final interaction matrix, plant and animal status vectors, island species table, and number of stochastic events.
