###############################################################################
# Validate the generated final design and its diagnostic files.
#
# Run after 05_generate_final_params_proj3.R:
#   Rscript proj3_second/08_validate_final_workflow_proj3.R
###############################################################################

rm(list = ls())

source("proj3_second/final_utils_proj3.R")

run_dir <- "proj3_second"
parameter_file <- file.path(run_dir, "final_param_table.csv")
range_file <- file.path(run_dir, "final_ranges.csv")
raw_correlation_file <- file.path(
  run_dir, "final_design_correlations_raw.csv"
)
all_row_correlation_file <- file.path(
  run_dir, "final_design_correlations_all_rows.csv"
)
sampling_correlation_file <- file.path(
  run_dir, "final_design_correlations_sampling_scale.csv"
)
coverage_file <- file.path(run_dir, "final_design_marginal_coverage.csv")

required_files <- c(
  parameter_file, range_file, raw_correlation_file, all_row_correlation_file,
  sampling_correlation_file, coverage_file
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files)) {
  stop("Missing final design files: ", paste(missing_files, collapse = ", "))
}

params <- read.csv(parameter_file, stringsAsFactors = FALSE, check.names = FALSE)
ranges <- read.csv(range_file, stringsAsFactors = FALSE, check.names = FALSE)
raw_correlations <- read.csv(
  raw_correlation_file, stringsAsFactors = FALSE, check.names = FALSE
)
all_row_correlations <- read.csv(
  all_row_correlation_file, stringsAsFactors = FALSE, check.names = FALSE
)
sampling_correlations <- read.csv(
  sampling_correlation_file, stringsAsFactors = FALSE, check.names = FALSE
)
coverage <- read.csv(coverage_file, stringsAsFactors = FALSE)

parameter_names <- proj3_final_parameter_names()
anchor_groups <- c(
  "no_mutualism", "K1_only", "mu1_only", "laa1_only", "lambda0_only"
)
background_names <- c("lac_0", "mu_0", "gam_0", "laa_0", "K_0")
mutualism_names <- c("K_1", "mu_1", "laa_1", "lambda0")

stopifnot(
  nrow(params) == 1000L,
  identical(params$simulation_id, seq_len(1000L)),
  !anyDuplicated(params$simulation_id),
  !anyDuplicated(params$simulation_key),
  !anyDuplicated(params$simulation_seed),
  !anyDuplicated(params[, parameter_names]),
  !anyNA(params[, c(
    "simulation_id", "simulation_key", "simulation_seed", "design_seed",
    "design_group", parameter_names
  )])
)

counts <- table(params$design_group)
stopifnot(
  unname(counts["broad_lhs"]) == 900L,
  all(unname(counts[anchor_groups]) == 20L),
  length(unique(params$design_seed)) == 1L,
  unique(params$design_seed) == 20260730L,
  all(params$simulation_seed > 0),
  all(params$simulation_seed <= .Machine$integer.max)
)

broad <- params[params$design_group == "broad_lhs", , drop = FALSE]
for (parameter in parameter_names) {
  spec <- ranges[ranges$parameter == parameter, , drop = FALSE]
  stopifnot(
    nrow(spec) == 1L,
    all(broad[[parameter]] >= spec$final_lower),
    all(broad[[parameter]] <= spec$final_upper)
  )
  if (spec$transformation == "log") {
    stopifnot(all(broad[[parameter]] > 0))
  }
}

baseline <- params[params$design_group == "no_mutualism", , drop = FALSE]
stopifnot(
  nrow(baseline) == 20L,
  all(baseline[, mutualism_names, drop = FALSE] == 0)
)

mechanisms <- c(
  K1_only = "K_1",
  mu1_only = "mu_1",
  laa1_only = "laa_1",
  lambda0_only = "lambda0"
)
for (group in names(mechanisms)) {
  mechanism <- mechanisms[[group]]
  rows <- params[params$design_group == group, , drop = FALSE]
  other <- setdiff(mutualism_names, mechanism)
  stopifnot(
    nrow(rows) == 20L,
    all(rows[[mechanism]] > 0),
    all(rows[, other, drop = FALSE] == 0)
  )
}

for (set_id in seq_len(20L)) {
  rows <- params[
    !is.na(params$anchor_set_id) & params$anchor_set_id == set_id,
    ,
    drop = FALSE
  ]
  stopifnot(
    nrow(rows) == 5L,
    setequal(rows$design_group, anchor_groups)
  )
  for (parameter in background_names) {
    stopifnot(length(unique(rows[[parameter]])) == 1L)
  }
  base_row <- rows[rows$design_group == "no_mutualism", , drop = FALSE]
  for (group in names(mechanisms)) {
    mechanism <- mechanisms[[group]]
    comparison <- rows[rows$design_group == group, , drop = FALSE]
    differences <- parameter_names[
      vapply(
        parameter_names,
        function(parameter) {
          !identical(comparison[[parameter]], base_row[[parameter]])
        },
        logical(1)
      )
    ]
    stopifnot(identical(differences, mechanism))
  }
}

off_diagonal_raw <- raw_correlations$parameter_1 !=
  raw_correlations$parameter_2
off_diagonal_sampling <- sampling_correlations$parameter_1 !=
  sampling_correlations$parameter_2
off_diagonal_all <- all_row_correlations$parameter_1 !=
  all_row_correlations$parameter_2
max_raw_correlation <- max(
  abs(raw_correlations$correlation[off_diagonal_raw])
)
max_sampling_correlation <- max(
  abs(sampling_correlations$correlation[off_diagonal_sampling])
)
max_all_row_correlation <- max(
  abs(all_row_correlations$correlation[off_diagonal_all])
)
stopifnot(
  is.finite(max_raw_correlation),
  is.finite(max_sampling_correlation),
  is.finite(max_all_row_correlation),
  max_raw_correlation < 0.15,
  max_sampling_correlation < 0.15
)

stopifnot(
  nrow(coverage) == length(parameter_names) * 10L,
  setequal(coverage$parameter, parameter_names),
  all(coverage$count == 90L),
  all(coverage$realized_min >= coverage$declared_lower),
  all(coverage$realized_max <= coverage$declared_upper)
)

dir.create(file.path(run_dir, "final_results"), recursive = TRUE,
           showWarnings = FALSE)
dir.create(file.path(run_dir, "final_logs"), recursive = TRUE,
           showWarnings = FALSE)

report_lines <- c(
  "Final Part II workflow validation",
  paste("Validated at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
  "Rows: 1000",
  "Broad maximin-LHS rows: 900",
  "Matched anchor sets: 20 (100 rows)",
  paste(
    "Group counts:",
    paste(names(counts), as.integer(counts), sep = "=", collapse = ", ")
  ),
  sprintf(
    "Maximum absolute broad correlation: raw=%.5f, sampling-scale=%.5f",
    max_raw_correlation, max_sampling_correlation
  ),
  sprintf(
    "Maximum absolute raw correlation including anchors: %.5f",
    max_all_row_correlation
  ),
  "Every sampling-scale decile contains 90 broad-design points.",
  "Bounds, missing values, duplicates, seeds, exact zeros, single mechanisms,",
  "and matched background parameters all passed."
)
writeLines(report_lines, file.path(run_dir, "final_validation_report.txt"))
cat(paste(report_lines, collapse = "\n"), "\n")
