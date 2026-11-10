###############################################################################
# Broad intrinsic backgrounds matched across no/lower/higher/mid-range mutualism.
# Keep the original 15,000 rows and append 5,000 independently seeded mid-range rows.
# Adapted from proj3_third/01_generate_stress_rescue_params.R.
###############################################################################

rm(list = ls())
run_dir <- "proj3_fourth"
generated_dir <- file.path(run_dir, "data", "generated")
param_file <- file.path(generated_dir, "param_table.csv")
dir.create(generated_dir, recursive = TRUE, showWarnings = FALSE)

design_seed <- 20261014L
n_backgrounds <- 5000L
set.seed(design_seed)

# Rates are per model-time unit. K_0 and K_1 remain continuous (not rounded),
# just as in the previous final design. The lower_range case restores all nine
# parameter ranges from the earlier 900-simulation continuous final design.
intrinsic_ranges <- data.frame(
  parameter = c("lac_0", "mu_0", "gam_0", "laa_0", "K_0"),
  original_lower = c(0.04, 0.03, 0.01, 0.03, 40),
  original_upper = c(0.80, 0.60, 0.20, 0.80, 120),
  lower = c(0.04, 0.03, 0.01, 0.03, 40),
  upper = c(0.80, 0.60, 0.20, 0.80, 120),
  sampling_scale = c("log", "log", "log", "log", "linear")
)

# Mutualism parameters are sampled independently WITHIN each band.
# Across bands
# they rise together: this is a joint-support experiment, not a factorial design
# for separating the causal contribution of the four mutualism mechanisms.
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

# Use the rounded bounds from mid_try with the original sampling scales.
mid_range <- mutualism_ranges[mutualism_ranges$design_group == "lower_range", ]
mid_range$design_group <- "mid_range"
mid_range$lower <- c(3, 0.03, 0.13, 0.27)
mid_range$upper <- c(125, 0.7, 1.4, 4.7)
mutualism_ranges <- rbind(mutualism_ranges, mid_range)

scale_range <- function(u, lower, upper, sampling_scale) {
  if (sampling_scale == "log") {
    exp(log(lower) + u * (log(upper) - log(lower)))
  } else {
    lower + u * (upper - lower)
  }
}

# Draw independent uniform values, then map them to the log/linear design scales.
# runif() samples continuous ranges; sample() would require discrete candidates.
# Reuse these backgrounds in all four scenarios to retain matched comparisons.
background_unit <- matrix(runif(n_backgrounds * 5L), nrow = n_backgrounds)
backgrounds <- data.frame(background_id = seq_len(n_backgrounds))
for (j in seq_len(nrow(intrinsic_ranges))) {
  r <- intrinsic_ranges[j, ]
  backgrounds[[r$parameter]] <- scale_range(
    background_unit[, j], r$lower, r$upper, r$sampling_scale
  )
}

groups <- c("no_mutualism", "lower_range", "higher_range", "mid_range")
rows <- lapply(groups, function(group) {
  p <- backgrounds
  p$design_group <- group
  # Draw independent mutualism values for each ON band and shared background.
  if (group != "no_mutualism") {
    mutualism_unit <- matrix(runif(n_backgrounds * 4L), nrow = n_backgrounds)
  }
  mutualism_names <- unique(mutualism_ranges$parameter)
  for (j in seq_along(mutualism_names)) {
    parameter <- mutualism_names[j]
    if (group == "no_mutualism") {
      p[[parameter]] <- 0
    } else {
      r <- mutualism_ranges[
        mutualism_ranges$design_group == group &
          mutualism_ranges$parameter == parameter, ]
      p[[parameter]] <- scale_range(
        mutualism_unit[, j], r$lower, r$upper, r$sampling_scale
      )
    }
  }
  p
})
params <- do.call(rbind, rows)
# Keep the first three groups in the original order so their IDs and seeds stay fixed.
params <- params[order(params$design_group == "mid_range", params$background_id,
                       match(params$design_group, groups)), ]
row.names(params) <- NULL
params$simulation_id <- seq_len(nrow(params))
params$simulation_key <- sprintf("sim_%05d", params$simulation_id)
params$design_seed <- design_seed
# Different seeds mean different stochastic trajectories.
params$simulation_seed <- as.integer(20000000 + params$simulation_id * 104729)
params <- params[, c(
  "simulation_id", "simulation_key", "design_seed", "simulation_seed",
  "background_id", "design_group", intrinsic_ranges$parameter,
  unique(mutualism_ranges$parameter)
)]

# Only the essential design checks: unique rows/seeds and complete matched sets.
stopifnot(
  nrow(params) == 20000L,
  !anyNA(params$simulation_seed),
  !anyDuplicated(params$simulation_seed),
  !anyDuplicated(params[, c(intrinsic_ranges$parameter,
                            unique(mutualism_ranges$parameter))]),
  all(table(params$background_id, params$design_group) == 1L)
)
# Accept the original table or this exact extension. Refuse to replace another design.
if (file.exists(param_file)) {
  existing <- read.csv(param_file, stringsAsFactors = FALSE)
  if (!nrow(existing) %in% c(15000L, 20000L) ||
      !isTRUE(all.equal(existing, params[seq_len(nrow(existing)), ], tolerance = 1e-12))) {
    stop("Existing parameter table differs from the expected design: ", param_file)
  }
}
# Save the full matched design, IDs and seeds (one row per simulation).
write.csv(params, param_file, row.names = FALSE)
# Save intrinsic bounds/scales used by the independent RF analyses.
write.csv(intrinsic_ranges, file.path(generated_dir, "intrinsic_ranges.csv"),
          row.names = FALSE)
# Save lower/higher/mid mutualism bounds/scales; the no-mutualism values are all zero.
write.csv(mutualism_ranges, file.path(generated_dir, "mutualism_sampling_ranges.csv"),
          row.names = FALSE)
