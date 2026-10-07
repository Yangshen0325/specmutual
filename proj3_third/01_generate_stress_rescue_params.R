###############################################################################
# Stressful intrinsic backgrounds with an expanded, continuous mutualism gradient.
# 2,500 backgrounds x 4 scenarios = 10,000 independently seeded simulations
###############################################################################

rm(list = ls())
run_dir <- "proj3_third"
param_file <- file.path(run_dir, "stress_rescue_param_table.csv")
if (file.exists(param_file)) {
  stop("Design already exists. Keep it fixed once jobs have been submitted: ", param_file)
}
dir.create(run_dir, showWarnings = FALSE)
dir.create(file.path(run_dir, "stress_rescue_logs"), showWarnings = FALSE)
dir.create(file.path(run_dir, "stress_rescue_results"), showWarnings = FALSE)

design_seed <- 20261002L
n_backgrounds <- 2500L
set.seed(design_seed)

# Rates are per model-time unit. K_0 and K_1 remain continuous (not rounded),
# just as in the previous final design. The stress box stays inside that design.
intrinsic_ranges <- data.frame(
  parameter = c("lac_0", "mu_0", "gam_0", "laa_0", "K_0"),
  original_lower = c(0.04, 0.03, 0.01, 0.03, 40),
  original_upper = c(0.80, 0.60, 0.20, 0.80, 120),
  lower = c(0.04, 0.30, 0.01, 0.03, 40),
  upper = c(0.12, 0.60, 0.03, 0.80, 120),
  sampling_scale = c("log", "log", "log", "log", "linear")
)

# Mutualism parameters use a separate four-dimensional LHS WITHIN each band.
# Across bands
# they rise together: this is a joint-support experiment, not a factorial design
# for separating the causal contribution of the four mutualism mechanisms.
mutualism_ranges <- data.frame(
  design_group = rep(c("weak", "intermediate", "strong"), each = 4),
  parameter = rep(c("K_1", "mu_1", "laa_1", "lambda0"), 3),
  original_lower = rep(c(0.25, 0.005, 0, 0.05), 3),
  original_upper = rep(c(40, 0.15, 0.25, 1.50), 3),
  lower = c(0.25, 0.005, 0, 0.05,
            4, 0.05, 0.05, 0.25,
            40, 0.15, 0.25, 1.50),
  upper = c(4, 0.05, 0.05, 0.25,
            40, 0.15, 0.25, 1.50,
            400, 3, 2.50, 15),
  sampling_scale = rep(c("log", "log", "linear", "log"), 3)
)

scale_range <- function(u, lower, upper, sampling_scale) {
  if (sampling_scale == "log") {
    exp(log(lower) + u * (log(upper) - log(lower)))
  } else {
    lower + u * (upper - lower)
  }
}

# Generate one five-dimensional LHS, then map its columns to the design scales.
# Reuse these backgrounds in all four scenarios to retain matched comparisons.
background_unit <- lhs::maximinLHS(n = n_backgrounds, k = 5L, dup = 10L)
backgrounds <- data.frame(background_id = seq_len(n_backgrounds))
for (j in seq_len(nrow(intrinsic_ranges))) {
  r <- intrinsic_ranges[j, ]
  backgrounds[[r$parameter]] <- scale_range(
    background_unit[, j], r$lower, r$upper, r$sampling_scale
  )
}

groups <- c("no_mutualism", "weak", "intermediate", "strong")
rows <- lapply(groups, function(group) {
  p <- backgrounds
  p$design_group <- group
  # Generate a separate LHS for each ON band.
  # Randomly assign its parameter combinations to the shared intrinsic backgrounds.
  if (group != "no_mutualism") {
    mutualism_unit <- lhs::maximinLHS(n = n_backgrounds, k = 4L, dup = 10L)
    mutualism_unit <- mutualism_unit[sample.int(n_backgrounds), , drop = FALSE]
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
params <- params[order(params$background_id, match(params$design_group, groups)), ]
row.names(params) <- NULL
params$simulation_id <- seq_len(nrow(params))
params$simulation_key <- sprintf("sim_%05d", params$simulation_id)
params$design_seed <- design_seed
# Different seeds mean different stochastic trajectories.
params$simulation_seed <- as.integer(10000000 + params$simulation_id * 104729)
params <- params[, c(
  "simulation_id", "simulation_key", "design_seed", "simulation_seed",
  "background_id", "design_group", intrinsic_ranges$parameter,
  unique(mutualism_ranges$parameter)
)]

# Only the essential design checks: unique rows/seeds and complete matched sets.
stopifnot(
  nrow(params) == 10000L,
  !anyDuplicated(params$simulation_seed),
  !anyDuplicated(params[, c(intrinsic_ranges$parameter,
                            unique(mutualism_ranges$parameter))]),
  all(table(params$background_id, params$design_group) == 1L)
)
write.csv(params, param_file, row.names = FALSE)
# write.csv(intrinsic_ranges, file.path(run_dir, "stress_intrinsic_ranges.csv"),
#          row.names = FALSE)
# write.csv(mutualism_ranges, file.path(run_dir, "mutualism_sampling_ranges.csv"),
#          row.names = FALSE)
