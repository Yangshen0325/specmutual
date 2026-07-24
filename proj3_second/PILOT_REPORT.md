# Proj3 second pilot: organized handoff report

## Executive status

The 1,000-combination provisional design and cluster workflow are ready. The full array was not run on the laptop because the available old runs show that a single five-replicate combination can require more than four hours; the user indicated that the cluster run would be launched after code review.

Four local validation results currently exist: three completed 10-My simulations and one deliberately difficult setting stopped by a 60-second local safety cap. These runs validate the pipeline but are not enough to choose final intervals. Final interval decisions should be based on `PILOT_RESULTS_REPORT.md` after the cluster has produced most of the 1,000 results.

## Reconstruction of the old simulation

The old Part II workflow:

1. Generated 500 continuous nine-parameter combinations with a random Latin hypercube.
2. Used uniform scaling for all parameters, including `K_1` from 0 to 150.
3. Tried to retain five simulations per parameter combination.
4. Rejected and reran outcomes whose final island network was smaller than 2 x 2.
5. Saved the full final state, network snapshots every 0.5 My, every event ID and time, and an event-by-event species-through-time table.
6. Calculated richness, endemism, network, and nLTT summaries for each retained simulation.
7. Averaged the five retained simulations and used one mean row per parameter combination for parameters-to-pattern random forests.

This means the old dataset was both conditionally selected against sparse communities and stripped of within-parameter stochastic variation by averaging.

The separate `proj3/part2_newSim` attempt generated 1,000 points but retained the old intervals and requested 10 accepted replicates per point, so it does not implement the present request.

## Evidence from the nine old combinations available locally

Only nine of the old 500 output files are present (`combo_001` to `combo_010`, excluding 005), containing 45 retained simulations in total. They are not representative enough for final bounds, but they reveal the main computational and mechanistic problems:

- Combination runtime ranged from 10.9 seconds to 15,264 seconds (4.24 hours); the median was 244 seconds.
- Saved files ranged from 70,577 bytes to 8,343,673 bytes.
- Final plant richness ranged from 3 to 1,524, and animal richness from 4 to 3,069.
- Within individual combinations, final median degree ranged from 2 to 926.
- Despite the old `mu_1` upper bound of 0.02, `mu_1 d_90` ranged from 0.071 to 12.43 across the nine combinations. Thus the old domain was inactive in some communities and already deeply saturated in others. A globally wider `mu_1` interval by itself would worsen the latter.
- For eligible final immigrant species, `D_i` was zero in eight of nine combinations. In the remaining combination only 1.4% were nonzero, with maximum `D_i = 1`.
- Fifty-one percent of the 45 retained simulations had zero cospeciation events; the median cospeciation count was zero.

The near-zero `D_i` is especially important. Under the current model, `D_i` changes through link gain/loss while `qgain = qloss = 0.001`; anagenesis often occurs before appreciable mismatch accumulates. If the time-weighted pilot diagnostics confirm this, failure to recover `laa_1` is not fixable by coefficient widening alone. It would instead require a scientific decision about the model formulation or fixed link-dynamics rates.

## Provisional design

The pilot contains 1,000 unique, non-replicated combinations:

- 900 maximin Latin-hypercube points over all nine parameters.
- 20 exact no-mutualism anchors.
- 20 `K_1`-only anchors.
- 20 `mu_1`-only anchors.
- 20 `laa_1`-only anchors.
- 20 `lambda0`-only anchors.

The anchor combinations have distinct intrinsic parameters and are not stochastic repeats.

| Parameter | Old Part II domain | New pilot domain | New sampling |
|---|---:|---:|---|
| `lac_0` | 0.10-1.00 | 0.03-1.20 | log-uniform |
| `mu_0` | 0.01-0.50 | 0.02-0.80 | log-uniform |
| `gam_0` | 0.01-0.20 | 0.005-0.30 | log-uniform |
| `laa_0` | 0.10-2.00 | 0.03-2.50 | log-uniform |
| `K_0` | 20-100 | 15-140 | uniform |
| `K_1` | 0-150 | 0-90 | `90u^3` |
| `mu_1` | 0-0.02 | 0-0.15 | uniform |
| `laa_1` | 0-0.02 | 0-0.25 | uniform |
| `lambda0` | 0-1.00 | 0-1.50 | uniform |

These are stress-test domains, not recommended final intervals. They were chosen independently of the intervals suggested in `documentation.docx`, while following its central principle: observe the actual response terms and retain mostly active but unsaturated behavior.

`K_1` is concentrated near zero because the available old outputs show strong high-`K_1` pathologies. `laa_1` is intentionally extended farther as a diagnostic stress test; if `D_i` remains zero, the pilot should conclude that no practical coefficient range solves the problem.

## Memory and runtime changes

The new `sim_core_mutualism_proj3()` calls the same published functions for rates, event sampling, and state transitions. It does not retain:

- a full rate-list object at every event;
- network snapshots every 0.5 My;
- a growing event table;
- a growing event-by-event species-through-time table.

Instead it updates scalar event counts, cumulative rate hazards, and response-term moments/fractions online. The runner computes final community summaries and discards the large final state before saving. Current compact pilot files are approximately 1.5-1.8 KB.

The matrix `Mt` and rate matrices are still required while a simulation is running. Therefore, the workflow adds explicit caps on event count, matrix cells, guild richness, and elapsed time. Reaching a cap is recorded as a pathological response, not silently discarded.

## Validation results

With the same seed and parameter values, the published and new cores produced identical:

- final `Mt`;
- plant status vector;
- animal status vector;
- island species table;
- stochastic event count.

The validation trajectory contained 39 events. The 10-My no-mutualism laptop check completed in 1.82 seconds with 375 events and ended with 28 plant and 57 animal species.

For that 39-event validation trajectory, the old full-rate diagnostic output occupied 82,173,312 bytes in memory, while the new online diagnostic object occupied 13,344 bytes (0.0162% as large). The final interaction matrix still dominates working memory during a run, but it is summarized and discarded before the compact result is saved.

A deliberately difficult broad-design point (`combo_0315`: high cladogenesis, low intrinsic extinction, high immigration, high `K_1`, and high `mu_1`) reached only 3.99 My before the 60-second local cap. It had already reached 424 plant and 912 animal species. For that partial trajectory:

- 49.4% of species-time exposure had `mu_1 d_i >= 3`, the strong/saturated extinction region;
- 82.9% had `K_1 d_i / K_0 >= 4`, the strong carrying-capacity region;
- 99.3% had `(laa_1 D_i) / laa_0 < 0.1`, so the anagenesis mutualism term remained inactive even with `laa_1 = 0.193`.

This single stress case supports the diagnostic design but is not a basis for final bounds.

## Post-cluster decision criteria

For every parameter, inspect both community outcomes and the process term that actually enters its rate:

- `mu_1`: use the species-time distribution of `mu_1 d_i` and `exp(-mu_1 d_i)`. Avoid a final range dominated by either values below 0.1 or values above 3.
- `laa_1`: use `(laa_1 D_i) / laa_0`, the nonzero frequency of `D_i`, and anagenesis counts/hazards. If `D_i` is almost always zero, label this structural/process inactivity rather than widening `laa_1` again.
- `K_1`: use `K_1 d_i / K_0` for both present species and mainland immigration candidates, together with richness, completion, runtime, and matrix growth. Prefer a domain whose majority lies below the explosive plateau while retaining a small strong-effect tail.
- `lambda0`: use the fraction of zero-cospeciation runs, cumulative cospeciation hazard, cospeciation counts relative to ordinary cladogenesis, and response plateaus at high `lambda0`.
- Intrinsic parameters: inspect empty/sparse outcomes, explosive outcomes, and compensation with the corresponding mutualism coefficient.

Do not infer final endpoints from only completed simulations. Safety-stopped settings are part of the response surface and identify pathological parameter regions.

## Commands for the full pilot

```bash
Rscript proj3_second/00_generate_params.R
sbatch proj3_second/submit_proj3_second.sh
```

After completion:

```bash
Rscript proj3_second/02_collect_results.R
Rscript proj3_second/03_analyze_pilot.R
```

The resulting `proj3_second/PILOT_RESULTS_REPORT.md` and `proj3_second/figures/pilot_process_response.png` are the files to use for the final interval discussion.
