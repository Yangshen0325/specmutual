###############################################################################
# Run exactly one proj3_second pilot combination.
#
# Usage:
#   Rscript proj3_second/01_run_pilot.R <combo_id>
#
# Environment overrides:
#   SPEC_MUTUAL_BASE, PROJ3_TOTAL_TIME, PROJ3_MAX_EVENTS,
#   PROJ3_MAX_MATRIX_CELLS, PROJ3_MAX_SPECIES_PER_GUILD,
#   PROJ3_MAX_RUNTIME_S, PROJ3_SAVE_STATE, PROJ3_FORCE
###############################################################################

rm(list = ls())

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1) {
  stop("Usage: Rscript proj3_second/01_run_pilot.R <combo_id>")
}
combo_id <- suppressWarnings(as.integer(args[1]))
if (is.na(combo_id) || combo_id < 1) {
  stop("combo_id must be a positive integer.")
}

source("proj3_second/utils_proj3_second.R")
base_dir <- proj3_second_base_dir()
run_dir <- file.path(base_dir, "proj3_second")
param_file <- file.path(run_dir, "param_table.csv")
out_dir <- file.path(run_dir, "results")
outfile <- file.path(out_dir, sprintf("combo_%04d.rds", combo_id))

env_number <- function(name, default, integer = FALSE) {
  value <- Sys.getenv(name, unset = as.character(default))
  parsed <- if (integer) suppressWarnings(as.integer(value)) else
    suppressWarnings(as.numeric(value))
  if (!is.finite(parsed) || parsed <= 0) {
    stop(name, " must be a positive number; received: ", value)
  }
  parsed
}

total_time <- env_number("PROJ3_TOTAL_TIME", 10)
max_events <- env_number("PROJ3_MAX_EVENTS", 20000, integer = TRUE)
max_matrix_cells <- env_number(
  "PROJ3_MAX_MATRIX_CELLS", 4000000, integer = TRUE
)
max_species_per_guild <- env_number(
  "PROJ3_MAX_SPECIES_PER_GUILD", 1500, integer = TRUE
)
max_runtime_s <- env_number("PROJ3_MAX_RUNTIME_S", 25200)
save_state <- identical(tolower(Sys.getenv("PROJ3_SAVE_STATE", "false")), "true")
force <- identical(tolower(Sys.getenv("PROJ3_FORCE", "false")), "true")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (file.exists(outfile) && !force) {
  existing <- tryCatch(readRDS(outfile), error = function(e) NULL)
  if (!is.null(existing) && identical(existing$combo_id, combo_id)) {
    used_runtime_cap <- suppressWarnings(as.numeric(
      existing$safety_caps$max_runtime_s
    ))
    stopped_for_shorter_runtime <- identical(
      existing$diagnostics$stop_reason,
      "max_runtime_s"
    ) &&
      is.finite(used_runtime_cap) &&
      max_runtime_s > used_runtime_cap
    if (!stopped_for_shorter_runtime) {
      cat("Result already exists for combo", combo_id, "- skipping.\n")
      quit(save = "no", status = 0)
    }
    cat(
      "Existing result used a shorter runtime cap (", used_runtime_cap,
      " s); rerunning with ", max_runtime_s, " s.\n",
      sep = ""
    )
  }
}

params <- read.csv(param_file, stringsAsFactors = FALSE, check.names = FALSE)
p <- params[params$combo_id == combo_id, , drop = FALSE]
if (nrow(p) != 1) {
  stop("combo_id ", combo_id, " not found exactly once in ", param_file)
}

set.seed(p$seed)
proj3_second_load_model(base_dir)
required_packages <- c("DAISIE", "testit", "igraph", "nLTT")
missing_packages <- required_packages[!vapply(
  required_packages,
  requireNamespace,
  logical(1),
  quietly = TRUE
)]
if (length(missing_packages)) {
  stop(
    "Missing required R packages: ",
    paste(missing_packages, collapse = ", ")
  )
}
M0 <- readRDS(file.path(base_dir, "script", "M0.rds"))

mutualism_pars <- create_mutual_pars(
  lac_pars = c(p$lac_0, p$lac_0),
  mu_pars = c(p$mu_0, p$mu_0, p$mu_1, p$mu_1),
  K_pars = c(p$K_0, p$K_0, p$K_1, p$K_1),
  gam_pars = c(p$gam_0, p$gam_0),
  laa_pars = c(p$laa_0, p$laa_0, p$laa_1, p$laa_1),
  qgain = 0.001,
  qloss = 0.001,
  lambda0 = p$lambda0,
  M0 = M0,
  transprob = 1,
  alpha = 100
)

cat("proj3_second combo", combo_id, "of", nrow(params), "\n")
cat("Design group:", p$design_group, "| seed:", p$seed, "\n")
print(p[, 4:12, drop = FALSE])

run_started <- as.character(Sys.time())
simulation_error <- NULL
simulation <- tryCatch(
  sim_core_mutualism_proj3(
    total_time = total_time,
    mutualism_pars = mutualism_pars,
    max_events = max_events,
    max_matrix_cells = max_matrix_cells,
    max_species_per_guild = max_species_per_guild,
    max_runtime_s = max_runtime_s,
    finalize_island = TRUE
  ),
  error = function(e) {
    simulation_error <<- conditionMessage(e)
    NULL
  }
)
run_finished <- as.character(Sys.time())

if (is.null(simulation)) {
  diagnostics <- list(
    completed = FALSE,
    stop_reason = "error",
    error_message = simulation_error,
    simulated_time = NA_real_,
    elapsed_s = NA_real_,
    n_events = NA_integer_
  )
  community_stats <- setNames(
    rep(NA_real_, 27),
    c(
      "island_p", "island_a",
      "island_endemic_p", "island_nonendemic_p",
      "island_endemic_a", "island_nonendemic_a",
      "connectance", "disconnect_p", "disconnect_a",
      "largest_component", "n_components", "plant_degree", "animal_degree",
      "nonend_nltt_p", "singleton_nltt_p", "multi_nltt_p",
      "nonend_nltt_a", "singleton_nltt_a", "multi_nltt_a",
      "final_degree_median", "final_degree_p90", "final_degree_p99",
      "final_degree_max", "final_D_median", "final_D_p90", "final_D_max",
      "final_D_nonzero_fraction"
    )
  )
} else {
  diagnostics <- simulation$diagnostics
  diagnostics$error_message <- NA_character_
  community_stats <- tryCatch(
    proj3_second_summarize_state(
      state = simulation$state,
      M0 = M0,
      completed = isTRUE(diagnostics$completed)
    ),
    error = function(e) {
      diagnostics$completed <<- FALSE
      diagnostics$stop_reason <<- "summary_error"
      diagnostics$error_message <<- conditionMessage(e)
      setNames(rep(NA_real_, 27), c(
        "island_p", "island_a",
        "island_endemic_p", "island_nonendemic_p",
        "island_endemic_a", "island_nonendemic_a",
        "connectance", "disconnect_p", "disconnect_a",
        "largest_component", "n_components", "plant_degree", "animal_degree",
        "nonend_nltt_p", "singleton_nltt_p", "multi_nltt_p",
        "nonend_nltt_a", "singleton_nltt_a", "multi_nltt_a",
        "final_degree_median", "final_degree_p90", "final_degree_p99",
        "final_degree_max", "final_D_median", "final_D_p90", "final_D_max",
        "final_D_nonzero_fraction"
      ))
    }
  )
}

result <- list(
  schema_version = "proj3_second_pilot_v1",
  combo_id = combo_id,
  seed = p$seed,
  design_group = p$design_group,
  run_started = run_started,
  run_finished = run_finished,
  total_time = total_time,
  fixed_parameters = list(
    qgain = 0.001,
    qloss = 0.001,
    transprob = 1,
    alpha = 100
  ),
  safety_caps = list(
    max_events = max_events,
    max_matrix_cells = max_matrix_cells,
    max_species_per_guild = max_species_per_guild,
    max_runtime_s = max_runtime_s
  ),
  params = as.list(p[, c(
    "design_group", "lac_0", "mu_0", "gam_0", "laa_0",
    "K_0", "K_1", "mu_1", "laa_1", "lambda0"
  )]),
  diagnostics = diagnostics,
  community_stats = community_stats
)
if (save_state && !is.null(simulation)) {
  result$state <- simulation$state
}

proj3_second_atomic_save_rds(result, outfile)
cat(
  "Saved", outfile,
  "| completed:", diagnostics$completed,
  "| reason:", diagnostics$stop_reason,
  "| events:", diagnostics$n_events,
  "\n"
)

if (is.null(simulation) || identical(diagnostics$stop_reason, "summary_error")) {
  quit(save = "no", status = 2)
}
