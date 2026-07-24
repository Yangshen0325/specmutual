###############################################################################
# Generate the 1,000-combination proj3_second provisional pilot.
#
# Design:
#   - 900 maximin Latin-hypercube combinations across all nine parameters.
#   - 100 diagnostic anchors: 20 no-mutualism settings and 20 settings for
#     each mutualism mechanism acting alone.
#   - One stochastic simulation per unique combination; no replicate averaging.
#
# Run from the package root:
#   Rscript proj3_second/00_generate_params.R
###############################################################################

rm(list = ls())

if (!requireNamespace("lhs", quietly = TRUE)) {
  stop("Package 'lhs' is required. Install it before generating the design.")
}

set.seed(73019)

run_dir <- "proj3_second"
dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)

n_broad <- 900L
n_anchor_per_group <- 20L
anchor_groups <- c(
  "no_mutualism", "K1_only", "mu1_only", "laa1_only", "lambda0_only"
)

log_scale <- function(u, lower, upper) {
  exp(log(lower) + u * (log(upper) - log(lower)))
}

scale_design <- function(unit_design) {
  data.frame(
    lac_0 = log_scale(unit_design[, 1], 0.03, 1.20),
    mu_0 = log_scale(unit_design[, 2], 0.02, 0.80),
    gam_0 = log_scale(unit_design[, 3], 0.005, 0.30),
    laa_0 = log_scale(unit_design[, 4], 0.03, 2.50),
    K_0 = 15 + unit_design[, 5] * (140 - 15),
    K_1 = 90 * unit_design[, 6]^3,
    mu_1 = 0.15 * unit_design[, 7],
    laa_1 = 0.25 * unit_design[, 8],
    lambda0 = 1.50 * unit_design[, 9],
    check.names = FALSE
  )
}

# Broad, fully crossed design.
broad_unit <- lhs::maximinLHS(n = n_broad, k = 9, dup = 5)
broad <- scale_design(broad_unit)
broad$design_group <- "broad_lhs"

# Anchors vary the five intrinsic parameters and one active-effect coordinate.
anchor_unit <- lhs::maximinLHS(
  n = n_anchor_per_group * length(anchor_groups),
  k = 6,
  dup = 5
)
anchor_base <- data.frame(
  lac_0 = log_scale(anchor_unit[, 1], 0.03, 1.20),
  mu_0 = log_scale(anchor_unit[, 2], 0.02, 0.80),
  gam_0 = log_scale(anchor_unit[, 3], 0.005, 0.30),
  laa_0 = log_scale(anchor_unit[, 4], 0.03, 2.50),
  K_0 = 15 + anchor_unit[, 5] * (140 - 15),
  K_1 = 0,
  mu_1 = 0,
  laa_1 = 0,
  lambda0 = 0,
  design_group = rep(anchor_groups, each = n_anchor_per_group),
  check.names = FALSE
)
active_u <- anchor_unit[, 6]
anchor_base$K_1[anchor_base$design_group == "K1_only"] <-
  90 * active_u[anchor_base$design_group == "K1_only"]^3
anchor_base$mu_1[anchor_base$design_group == "mu1_only"] <-
  0.15 * active_u[anchor_base$design_group == "mu1_only"]
anchor_base$laa_1[anchor_base$design_group == "laa1_only"] <-
  0.25 * active_u[anchor_base$design_group == "laa1_only"]
anchor_base$lambda0[anchor_base$design_group == "lambda0_only"] <-
  1.50 * active_u[anchor_base$design_group == "lambda0_only"]

params <- rbind(broad, anchor_base)
params <- params[sample(seq_len(nrow(params))), , drop = FALSE]
params$combo_id <- seq_len(nrow(params))
params$seed <- 910000L + params$combo_id * 104729L
params <- params[, c(
  "combo_id", "seed", "design_group", "lac_0", "mu_0", "gam_0",
  "laa_0", "K_0", "K_1", "mu_1", "laa_1", "lambda0"
)]

stopifnot(nrow(params) == 1000L)
stopifnot(!anyDuplicated(params[, c(
  "lac_0", "mu_0", "gam_0", "laa_0", "K_0",
  "K_1", "mu_1", "laa_1", "lambda0"
)]))
stopifnot(all(table(params$design_group)[anchor_groups] == n_anchor_per_group))
stopifnot(sum(params$design_group == "broad_lhs") == n_broad)

range_spec <- data.frame(
  parameter = c(
    "lac_0", "mu_0", "gam_0", "laa_0", "K_0",
    "K_1", "mu_1", "laa_1", "lambda0"
  ),
  minimum = c(0.03, 0.02, 0.005, 0.03, 15, 0, 0, 0, 0),
  maximum = c(1.20, 0.80, 0.30, 2.50, 140, 90, 0.15, 0.25, 1.50),
  sampling = c(
    rep("log-uniform", 4),
    "uniform",
    "90 * u^3 (strongly concentrated near zero)",
    "uniform",
    "uniform",
    "uniform"
  ),
  pilot_reason = c(
    "Brackets and extends the paper's cladogenesis levels while retaining low-rate regimes.",
    "Covers weak through high extinction and helps expose mu_0 by mu_1 compensation.",
    "Includes near-empty islands and rapid-assembly regimes.",
    "Broad intrinsic baseline is needed to judge laa_1 * D relative to laa_0.",
    "Covers low-opportunity through weakly constrained baseline communities.",
    "High old values produced explosive communities, so the broad tail is retained but sparsely sampled.",
    "Extends far enough to reveal saturation separately in low- and high-degree communities.",
    "A deliberately broad stress test because observed D was usually zero in old outputs.",
    "Extends beyond the old range to test zero-event, informative, and plateau regions."
  ),
  stringsAsFactors = FALSE
)

write.csv(
  params,
  file.path(run_dir, "param_table.csv"),
  row.names = FALSE,
  quote = TRUE
)
write.csv(
  range_spec,
  file.path(run_dir, "pilot_ranges.csv"),
  row.names = FALSE,
  quote = TRUE
)

cat("Generated", nrow(params), "unique parameter combinations.\n")
cat("Design groups:\n")
print(table(params$design_group))
cat("\nRealized broad-design ranges:\n")
print(summary(params[params$design_group == "broad_lhs", 4:12]))
cat("\nSaved proj3_second/param_table.csv and pilot_ranges.csv\n")
