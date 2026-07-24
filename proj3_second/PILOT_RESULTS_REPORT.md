# Proj3 second pilot results

**Status:** Preliminary report - do not select final intervals yet

## Coverage and computational behavior

- Result files collected: 4 / 1,000.
- Broad-design results: 3 / 900.
- Simulations reaching 10 My: 3 / 4 (75.0%).
- Runtime seconds (10th / median / 90th percentile): 0.954 / 1.423 / 60.000.
- Compact result size bytes (10th / median / 90th percentile): 1520 / 1650 / 1764.

Stop reasons:
  - completed: 3
  - max_runtime_s: 1

## Generated communities

- Final plant richness (10th / median / 90th): 3 / 7 / 28.
- Final animal richness (10th / median / 90th): 13 / 57 / 96.
- Final per-simulation 90th-percentile degree (10th / median / 90th): 0.0000 / 0.5167 / 1.0330.

## Mutualism response terms

The fractions below are time- and species-weighted across all available broad-design trajectories, including safety-stopped trajectories. The diagnostic bands are intended to label inactive, changing, and strong/saturated behavior; they are not proposed parameter intervals.

- Extinction term `mu_1 d_i`: median inactive fraction (<0.1) = 99.2%; median informative fraction (0.1-3) = 0.8%; median saturated fraction (>=3) = 0.0%.
- Anagenesis contribution `(laa_1 D_i) / laa_0`: median inactive fraction (<0.1) = 100.0%; median informative fraction (0.1-2) = 0.0%; median strong fraction (>=2) = 0.0%.
- Carrying-capacity term `K_1 d_i / K_0`: median inactive fraction (<0.1) = 96.1%; median changing fraction (0.1-4) = 3.9%; median strong fraction (>=4) = 0.0%.
- Immigration-candidate `K_1 d_i / K_0` diagnostics are not present in the pre-update local smoke files; cluster results will populate them.
- Simulations with zero cospeciation events: 100.0%.
- Cospeciation / ordinary cladogenesis ratio (10th / median / 90th): 0 / 0 / 0.

## Model-based landmarks for discussion

- Median final `d_90` among completed broad runs: 0.5167. The `mu_1` value giving a 95% extinction reduction at that degree is 5.798.
- 90th percentile of run-specific final `d_90`: 1.033. The corresponding 95%-reduction `mu_1` landmark is 2.899.
- Median final `D_90`: 0. A coefficient making `laa_1 D_90` equal the median `laa_0` is NA.

These landmarks must be interpreted together with the inactive and saturated fractions, event counts, failed/pathological runs, and the one-mechanism anchor groups. They are not automatic final bounds.

## Decision rule

Wait for the missing cluster results before choosing final intervals. Current values are useful only for validating the diagnostics and detecting severe pathologies.
