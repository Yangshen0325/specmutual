###############################################################################
# Validate that the lightweight core preserves the stochastic trajectory.
#
# The same seed and parameters are run through the published core and the new
# pilot core. Final states and event counts must agree exactly; only histories
# intentionally omitted by the pilot are allowed to differ.
###############################################################################

rm(list = ls())

source("proj3_second/utils_proj3_second.R")
base_dir <- proj3_second_base_dir()
proj3_second_load_model(base_dir)
M0 <- readRDS(file.path(base_dir, "script", "M0.rds"))

pars <- create_mutual_pars(
  lac_pars = c(0.25, 0.25),
  mu_pars = c(0.20, 0.20, 0.03, 0.03),
  K_pars = c(60, 60, 8, 8),
  gam_pars = c(0.10, 0.10),
  laa_pars = c(0.50, 0.50, 0.08, 0.08),
  qgain = 0.001,
  qloss = 0.001,
  lambda0 = 0.30,
  M0 = M0,
  transprob = 1,
  alpha = 100
)

validation_seed <- 88421L
validation_time <- 2

set.seed(validation_seed)
published <- sim_core_mutualism(
  total_time = validation_time,
  mutualism_pars = pars,
  return_parts = "island_parts"
)

set.seed(validation_seed)
pilot <- sim_core_mutualism_proj3(
  total_time = validation_time,
  mutualism_pars = pars,
  max_runtime_s = 120
)

set.seed(validation_seed)
published_full_rates <- sim_core_mutualism(
  total_time = validation_time,
  mutualism_pars = pars,
  return_parts = "additional_parts"
)

checks <- c(
  Mt = identical(published$Mt, pilot$state$Mt),
  status_p = identical(published$status_p, pilot$state$status_p),
  status_a = identical(published$status_a, pilot$state$status_a),
  island_spec = identical(
    unname(published$island_spec),
    unname(pilot$state$island_spec)
  ),
  event_count = nrow(published$evo_table) == pilot$diagnostics$n_events
)
if (!all(checks)) {
  stop("Pilot core validation failed: ", paste(names(checks)[!checks], collapse = ", "))
}

published_bytes <- as.numeric(object.size(published))
pilot_bytes <- as.numeric(object.size(pilot))
published_history_bytes <- as.numeric(object.size(list(
  M_true_list = published$M_true_list,
  evo_table = published$evo_table,
  stt_table = published$island$stt_table
)))
pilot_diagnostic_bytes <- as.numeric(object.size(pilot$diagnostics))
published_rate_trace_bytes <- as.numeric(object.size(published_full_rates))
lines <- c(
  "proj3_second lightweight-core validation",
  paste("Seed:", validation_seed),
  paste("Simulation time:", validation_time),
  paste("Events:", pilot$diagnostics$n_events),
  paste("All final-state checks passed:", all(checks)),
  paste("Published-core in-memory object bytes:", published_bytes),
  paste("Pilot-core in-memory object bytes:", pilot_bytes),
  paste("Published retained-history bytes:", published_history_bytes),
  paste("Published full-rate-trace bytes:", published_rate_trace_bytes),
  paste("Pilot online-diagnostic bytes:", pilot_diagnostic_bytes),
  paste(
    "Online-diagnostic/full-rate-trace size ratio:",
    signif(pilot_diagnostic_bytes / published_rate_trace_bytes, 4)
  ),
  "Note: final Mt dominates both in-memory objects; the production runner summarizes and discards that state before saving.",
  "",
  "Checks:",
  paste0("  ", names(checks), ": ", checks)
)
writeLines(lines, file.path(base_dir, "proj3_second", "validation_report.txt"))
cat(paste(lines, collapse = "\n"), "\n")
