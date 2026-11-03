###############################################################################
# Higher-range mutualism: 50,000 independent parameter rows and simulations.
# Ten times the original higher_range dataset; original bounds/scales unchanged.
# Adapted from proj3_fourth/01_generate_params.R.
###############################################################################

rm(list = ls())
run_dir <- "proj3_fourth_higher_range_50k"
generated_dir <- file.path(run_dir, "data", "generated")
param_file <- file.path(generated_dir, "param_table.csv")
if (file.exists(param_file)) {
  stop("Design already exists. Keep it fixed once jobs have been submitted: ", param_file)
}
dir.create(run_dir, showWarnings = FALSE)
dir.create(generated_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(run_dir, "data", "processed"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(run_dir, "simulation_logs"), showWarnings = FALSE)
dir.create(file.path(run_dir, "simulation_results"), showWarnings = FALSE)

design_seed <- 20261103L
n_backgrounds <- 50000L
set.seed(design_seed)

# Rates are per model-time unit. K_0 and K_1 remain continuous (not rounded).
# Intrinsic and higher_range bounds/scales are identical to proj3_fourth.
intrinsic_ranges <- data.frame(
  parameter = c("lac_0", "mu_0", "gam_0", "laa_0", "K_0"),
  original_lower = c(0.04, 0.03, 0.01, 0.03, 40),
  original_upper = c(0.80, 0.60, 0.20, 0.80, 120),
  lower = c(0.04, 0.03, 0.01, 0.03, 40),
  upper = c(0.80, 0.60, 0.20, 0.80, 120),
  sampling_scale = c("log", "log", "log", "log", "linear")
)

# Keep the original range metadata, including the lower band used in the RF
# normalization denominator. Sampling below selects only the higher band.
mutualism_ranges <- data.frame(
  design_group = rep(c("lower_range", "higher_range"), each = 4),
  parameter = rep(c("K_1", "mu_1", "laa_1", "lambda0"), 2),
  original_lower = rep(c(0.25, 0.005, 0, 0.05), 2),
  original_upper = rep(c(40, 0.15, 0.25, 1.50), 2),
  lower = c(0.25, 0.005, 0, 0.05,
            40, 0.15, 0.25, 1.50),
  upper = c(40, 0.15, 0.25, 1.50,
            400, 3, 2.50, 15),
  sampling_scale = rep(c("log", "log", "linear", "log"), 2)
)

scale_range <- function(u, lower, upper, sampling_scale) {
  if (sampling_scale == "log") {
    exp(log(lower) + u * (log(upper) - log(lower)))
  } else {
    lower + u * (upper - lower)
  }
}

# Draw independent uniform values, then map them to the log/linear design scales.
# runif() samples continuous ranges; sample() would require discrete candidates.
# Each background has one higher_range simulation in this experiment.
background_unit <- matrix(runif(n_backgrounds * 5L), nrow = n_backgrounds)
backgrounds <- data.frame(background_id = seq_len(n_backgrounds))
for (j in seq_len(nrow(intrinsic_ranges))) {
  r <- intrinsic_ranges[j, ]
  backgrounds[[r$parameter]] <- scale_range(
    background_unit[, j], r$lower, r$upper, r$sampling_scale
  )
}

groups <- "higher_range"
rows <- lapply(groups, function(group) {
  p <- backgrounds
  p$design_group <- group
  # Independent draws within the higher band, as in the original generator.
  mutualism_unit <- matrix(runif(n_backgrounds * 4L), nrow = n_backgrounds)
  mutualism_names <- unique(mutualism_ranges$parameter)
  for (j in seq_along(mutualism_names)) {
    parameter <- mutualism_names[j]
    r <- mutualism_ranges[
      mutualism_ranges$design_group == group &
        mutualism_ranges$parameter == parameter, ]
    p[[parameter]] <- scale_range(
      mutualism_unit[, j], r$lower, r$upper, r$sampling_scale
    )
  }
  p
})
params <- do.call(rbind, rows)
params <- params[order(params$background_id, match(params$design_group, groups)), ]
row.names(params) <- NULL
params$simulation_id <- seq_len(nrow(params))
params$simulation_key <- sprintf("sim_%05d", params$simulation_id)
params$design_seed <- design_seed
# The old 104729-stride formula overflows integer seeds beyond 20,314 rows.
# Use a separate, valid seed block above all seeds in the original 15,000-row design.
params$simulation_seed <- 1700000000L + params$simulation_id
params <- params[, c(
  "simulation_id", "simulation_key", "design_seed", "simulation_seed",
  "background_id", "design_group", intrinsic_ranges$parameter,
  unique(mutualism_ranges$parameter)
)]

# Check size, unique rows, valid seeds, and one simulation per background.
stopifnot(
  nrow(params) == 50000L,
  !anyNA(params$simulation_seed),
  all(params$simulation_seed > 0L),
  all(params$design_group == "higher_range"),
  !anyDuplicated(params$simulation_seed),
  !anyDuplicated(params[, c(intrinsic_ranges$parameter,
                            unique(mutualism_ranges$parameter))]),
  all(table(params$background_id, params$design_group) == 1L)
)
# Save the higher-range design, IDs and seeds (one row per simulation).
write.csv(params, param_file, row.names = FALSE)
# Save unchanged intrinsic bounds/scales for RF.
write.csv(intrinsic_ranges, file.path(generated_dir, "intrinsic_ranges.csv"),
          row.names = FALSE)
# Keep both bands as reference metadata so RF range normalization remains
# comparable to the original higher_range analysis (combined ON design bounds).
# Only higher_range rows are generated and simulated.
write.csv(mutualism_ranges, file.path(generated_dir, "mutualism_sampling_ranges.csv"),
          row.names = FALSE)

cat("Generated", nrow(params), "higher_range rows:", param_file, "\n")
