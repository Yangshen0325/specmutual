###############################################################################
# Generate the final Part II parameter design.
#
# Design:
#   * 900 maximin Latin-hypercube points varying all nine parameters jointly.
#   * 20 matched anchor sets. Each set has one no-mutualism row and four rows
#     with only K_1, mu_1, laa_1, or lambda0 active (100 rows total).
#   * One stochastic simulation per row; no replicate averaging.
#
# Run from the package root:
#   Rscript proj3_second/05_generate_final_params_proj3.R
###############################################################################

rm(list = ls())

if (!requireNamespace("lhs", quietly = TRUE)) {
  stop("Package 'lhs' is required to generate the final design.")
}

run_dir <- "proj3_second"
dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)

design_seed <- 20260730L
n_broad <- 900L
n_anchor_sets <- 20L
parameter_names <- c(
  "lac_0", "mu_0", "gam_0", "laa_0", "K_0",
  "K_1", "mu_1", "laa_1", "lambda0"
)
anchor_groups <- c(
  "no_mutualism", "K1_only", "mu1_only", "laa1_only", "lambda0_only"
)
background_names <- c("lac_0", "mu_0", "gam_0", "laa_0", "K_0")

# The final bounds are decisions documented in FINAL_RANGE_REPORT.md. The
# pilot columns make the change from the stress-test design machine-readable.
range_spec <- data.frame(
  parameter = parameter_names,
  pilot_lower = c(0.03, 0.02, 0.005, 0.03, 15, 0, 0, 0, 0),
  pilot_upper = c(1.20, 0.80, 0.30, 2.50, 140, 90, 0.15, 0.25, 1.50),
  final_lower = c(0.04, 0.03, 0.01, 0.03, 40, 0.25, 0.005, 0, 0.05),
  final_upper = c(0.80, 0.60, 0.20, 0.80, 120, 40, 0.15, 0.25, 1.50),
  transformation = c(
    "log", "log", "log", "log", "linear",
    "log", "log", "linear", "log"
  ),
  exact_zero_in_anchors = c(
    FALSE, FALSE, FALSE, FALSE, FALSE,
    TRUE, TRUE, TRUE, TRUE
  ),
  stringsAsFactors = FALSE
)

log_scale <- function(u, lower, upper) {
  if (lower <= 0 || upper <= lower) {
    stop("Log-scaled bounds must satisfy 0 < lower < upper.")
  }
  exp(log(lower) + u * (log(upper) - log(lower)))
}

linear_scale <- function(u, lower, upper) {
  lower + u * (upper - lower)
}

scale_column <- function(u, parameter) {
  spec <- range_spec[range_spec$parameter == parameter, , drop = FALSE]
  if (nrow(spec) != 1L) {
    stop("No unique range specification for ", parameter)
  }
  if (spec$transformation == "log") {
    log_scale(u, spec$final_lower, spec$final_upper)
  } else if (spec$transformation == "linear") {
    linear_scale(u, spec$final_lower, spec$final_upper)
  } else {
    stop("Unsupported transformation for ", parameter)
  }
}

scale_design <- function(unit_design) {
  if (ncol(unit_design) != length(parameter_names)) {
    stop("Expected nine unit-scale design columns.")
  }
  out <- setNames(vector("list", length(parameter_names)), parameter_names)
  for (j in seq_along(parameter_names)) {
    out[[j]] <- scale_column(unit_design[, j], parameter_names[j])
  }
  as.data.frame(out, check.names = FALSE)
}

set.seed(design_seed)

# These are jointly varied LHS points, not a fully crossed design.
broad_unit <- lhs::maximinLHS(n = n_broad, k = 9L, dup = 10L)
broad <- scale_design(broad_unit)
broad$design_group <- "broad_lhs"
broad$anchor_set_id <- NA_integer_

# One LHS row supplies each matched set's five background values and the four
# mechanism strengths. All five rows in a set therefore have identical
# backgrounds and differ only in the named mutualism mechanism.
anchor_unit <- lhs::maximinLHS(n = n_anchor_sets, k = 9L, dup = 20L)
anchor_background <- data.frame(
  lac_0 = scale_column(anchor_unit[, 1], "lac_0"),
  mu_0 = scale_column(anchor_unit[, 2], "mu_0"),
  gam_0 = scale_column(anchor_unit[, 3], "gam_0"),
  laa_0 = scale_column(anchor_unit[, 4], "laa_0"),
  K_0 = scale_column(anchor_unit[, 5], "K_0"),
  check.names = FALSE
)
anchor_strengths <- data.frame(
  K_1 = scale_column(anchor_unit[, 6], "K_1"),
  mu_1 = scale_column(anchor_unit[, 7], "mu_1"),
  laa_1 = scale_column(anchor_unit[, 8], "laa_1"),
  lambda0 = scale_column(anchor_unit[, 9], "lambda0"),
  check.names = FALSE
)

anchor_rows <- vector("list", n_anchor_sets * length(anchor_groups))
row_index <- 0L
for (set_id in seq_len(n_anchor_sets)) {
  for (group in anchor_groups) {
    row_index <- row_index + 1L
    row <- anchor_background[set_id, , drop = FALSE]
    row$K_1 <- 0
    row$mu_1 <- 0
    row$laa_1 <- 0
    row$lambda0 <- 0
    if (group == "K1_only") row$K_1 <- anchor_strengths$K_1[set_id]
    if (group == "mu1_only") row$mu_1 <- anchor_strengths$mu_1[set_id]
    if (group == "laa1_only") row$laa_1 <- anchor_strengths$laa_1[set_id]
    if (group == "lambda0_only") {
      row$lambda0 <- anchor_strengths$lambda0[set_id]
    }
    row$design_group <- group
    row$anchor_set_id <- set_id
    anchor_rows[[row_index]] <- row
  }
}
anchors <- do.call(rbind, anchor_rows)
row.names(anchors) <- NULL

params <- rbind(broad, anchors)
params$simulation_id <- seq_len(nrow(params))
params$simulation_key <- sprintf("sim_%04d", params$simulation_id)
params$design_seed <- design_seed
params$simulation_seed <- 800000L + params$simulation_id * 104729L
params <- params[, c(
  "simulation_id", "simulation_key", "simulation_seed", "design_seed",
  "design_group", "anchor_set_id", parameter_names
)]

# Hard validation before any design file is written.
if (nrow(params) != 1000L) stop("Final design must contain 1,000 rows.")
if (anyNA(params[, c(
  "simulation_id", "simulation_key", "simulation_seed", "design_seed",
  "design_group", parameter_names
)])) {
  stop("Missing values detected in required design columns.")
}
if (anyDuplicated(params$simulation_id) ||
    anyDuplicated(params$simulation_key) ||
    anyDuplicated(params$simulation_seed)) {
  stop("Simulation IDs, keys, and seeds must be unique.")
}
if (anyDuplicated(params[, parameter_names])) {
  stop("Duplicate nine-parameter rows detected.")
}
group_counts <- table(params$design_group)
if (unname(group_counts["broad_lhs"]) != n_broad ||
    any(unname(group_counts[anchor_groups]) != n_anchor_sets)) {
  stop("Unexpected final design-group counts.")
}

broad_rows <- params$design_group == "broad_lhs"
for (parameter in parameter_names) {
  spec <- range_spec[range_spec$parameter == parameter, , drop = FALSE]
  values <- params[[parameter]][broad_rows]
  if (any(values < spec$final_lower | values > spec$final_upper)) {
    stop(parameter, " has a broad-design value outside its final bounds.")
  }
}

baseline <- params$design_group == "no_mutualism"
if (!all(params[baseline, c("K_1", "mu_1", "laa_1", "lambda0")] == 0)) {
  stop("The no-mutualism anchors must have four exact zeros.")
}
for (group in anchor_groups[-1]) {
  mechanism <- switch(
    group,
    K1_only = "K_1",
    mu1_only = "mu_1",
    laa1_only = "laa_1",
    lambda0_only = "lambda0"
  )
  rows <- params$design_group == group
  other <- setdiff(c("K_1", "mu_1", "laa_1", "lambda0"), mechanism)
  if (!all(params[[mechanism]][rows] > 0) ||
      !all(params[rows, other, drop = FALSE] == 0)) {
    stop("Invalid single-mechanism anchor group: ", group)
  }
}

for (set_id in seq_len(n_anchor_sets)) {
  set_rows <- params$anchor_set_id == set_id & !is.na(params$anchor_set_id)
  if (sum(set_rows) != length(anchor_groups)) {
    stop("Anchor set ", set_id, " does not contain five rows.")
  }
  for (parameter in background_names) {
    if (length(unique(params[[parameter]][set_rows])) != 1L) {
      stop("Anchor set ", set_id, " is not matched for ", parameter)
    }
  }
}

# Design diagnostics. Correlations are reported for the 900 joint LHS points;
# anchors intentionally contain exact zeros and matched backgrounds.
raw_correlations <- stats::cor(broad[, parameter_names])
all_row_correlations <- stats::cor(params[, parameter_names])
sampling_correlations <- stats::cor(broad_unit)
dimnames(sampling_correlations) <- list(parameter_names, parameter_names)
correlation_table <- function(matrix, scale_name) {
  grid <- expand.grid(
    parameter_1 = rownames(matrix),
    parameter_2 = colnames(matrix),
    stringsAsFactors = FALSE
  )
  grid$correlation <- as.vector(matrix)
  grid$sampling_scale <- scale_name
  grid
}

marginal_coverage <- do.call(rbind, lapply(
  seq_along(parameter_names),
  function(j) {
    bins <- cut(
      broad_unit[, j],
      breaks = seq(0, 1, by = 0.1),
      include.lowest = TRUE,
      right = TRUE
    )
    data.frame(
      parameter = parameter_names[j],
      decile = levels(bins),
      count = as.integer(table(bins)),
      realized_min = min(broad[[parameter_names[j]]]),
      realized_max = max(broad[[parameter_names[j]]]),
      declared_lower = range_spec$final_lower[j],
      declared_upper = range_spec$final_upper[j],
      transformation = range_spec$transformation[j],
      stringsAsFactors = FALSE
    )
  }
))

write.csv(
  params,
  file.path(run_dir, "final_param_table.csv"),
  row.names = FALSE,
  quote = TRUE
)
write.csv(
  range_spec,
  file.path(run_dir, "final_ranges.csv"),
  row.names = FALSE,
  quote = TRUE
)
write.csv(
  correlation_table(raw_correlations, "raw"),
  file.path(run_dir, "final_design_correlations_raw.csv"),
  row.names = FALSE
)
write.csv(
  correlation_table(all_row_correlations, "raw_all_rows"),
  file.path(run_dir, "final_design_correlations_all_rows.csv"),
  row.names = FALSE
)
write.csv(
  correlation_table(sampling_correlations, "transformed_unit"),
  file.path(run_dir, "final_design_correlations_sampling_scale.csv"),
  row.names = FALSE
)
write.csv(
  marginal_coverage,
  file.path(run_dir, "final_design_marginal_coverage.csv"),
  row.names = FALSE
)

off_diagonal <- row(raw_correlations) != col(raw_correlations)
off_diagonal_all <- row(all_row_correlations) != col(all_row_correlations)
off_diagonal_unit <- row(sampling_correlations) != col(sampling_correlations)
cat("Generated", nrow(params), "unique final parameter rows.\n")
cat("Design seed:", design_seed, "\n")
cat("Design groups:\n")
print(group_counts)
cat(
  "Maximum absolute broad-design pairwise correlation (raw / sampling scale):",
  sprintf(
    "%.4f / %.4f\n",
    max(abs(raw_correlations[off_diagonal])),
    max(abs(sampling_correlations[off_diagonal_unit]))
  )
)
cat(
  "Maximum absolute pairwise correlation including matched anchors:",
  sprintf("%.4f\n", max(abs(all_row_correlations[off_diagonal_all])))
)
cat("Broad-design ranges:\n")
print(summary(params[broad_rows, parameter_names]))
cat("Saved final parameter and design-diagnostic CSV files in proj3_second.\n")
