###############################################################################
# Run ONE row from the parameter design, on a cluster compute node.
# Adapted from proj3_second/06_run_final_sim_proj3.R; the stochastic model,
# fixed settings and summary definitions are unchanged.
#
###############################################################################

rm(list = ls())

source("proj3_third/summary_utils_proj3_third.R")
source("proj3_third/workflow_utils_proj3_third.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 1L) {
  stop("Usage: Rscript proj3_third/02_run_stress_rescue_sim.R [simulation_id]")
}
id_text <- if (length(args) == 1L) {
  args[1]
} else {
  Sys.getenv("SLURM_ARRAY_TASK_ID", unset = "")
}
simulation_id <- suppressWarnings(as.integer(id_text))
if (!nzchar(id_text) || is.na(simulation_id) || simulation_id < 1L) {
  stop("Provide a positive simulation ID or set SLURM_ARRAY_TASK_ID.")
}

base_dir <- proj3_third_base_dir()
run_dir <- file.path(base_dir, "proj3_third")
param_file <- file.path(run_dir, "stress_rescue_param_table.csv")
configured_output_dir <- Sys.getenv("PROJ3_THIRD_OUTPUT_DIR", unset = "")
out_dir <- if (nzchar(configured_output_dir)) {
  path.expand(configured_output_dir)
} else {
  file.path(run_dir, "stress_rescue_results")
}

if (!file.exists(param_file)) {
  stop("Final parameter table not found: ", param_file)
}
params <- read.csv(param_file, stringsAsFactors = FALSE, check.names = FALSE)
p <- params[params$simulation_id == simulation_id, , drop = FALSE]
if (nrow(p) != 1L) {
  stop("simulation_id ", simulation_id, " not found exactly once in ", param_file)
}
outfile <- file.path(out_dir, paste0(p$simulation_key, ".rds"))

total_time <- proj3_third_env_number("PROJ3_THIRD_TOTAL_TIME", 10)
max_events <- proj3_third_env_number(
  "PROJ3_THIRD_MAX_EVENTS", 60000, integer = TRUE
)
max_matrix_cells <- proj3_third_env_number(
  "PROJ3_THIRD_MAX_MATRIX_CELLS", 8000000, integer = TRUE
)
max_species_per_guild <- proj3_third_env_number(
  "PROJ3_THIRD_MAX_SPECIES_PER_GUILD", 6000, integer = TRUE
)
max_runtime_s <- proj3_third_env_number("PROJ3_THIRD_MAX_RUNTIME_S", 126000)
save_state <- proj3_third_env_flag("PROJ3_THIRD_SAVE_STATE", FALSE)
force <- proj3_third_env_flag("PROJ3_THIRD_FORCE", FALSE)
rerun_safety <- proj3_third_env_flag("PROJ3_THIRD_RERUN_SAFETY_STOPPED", FALSE)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (file.exists(outfile) && !force) {
  existing <- tryCatch(readRDS(outfile), error = function(e) NULL)
  if (proj3_third_result_matches(existing, p, total_time)) {
    if (identical(existing$success_status, "completed")) {
      cat("Valid completed result already exists - skipping:", outfile, "\n")
      quit(save = "no", status = 0)
    }
    if (identical(existing$success_status, "safety_stopped") &&
        !rerun_safety) {
      cat(
        "Valid safety-stopped result already exists - skipping. Set ",
        "PROJ3_THIRD_RERUN_SAFETY_STOPPED=true to rerun: ", outfile, "\n",
        sep = ""
      )
      quit(save = "no", status = 0)
    }
    # Saved simulation errors are deliberately rerunnable without PROJ3_THIRD_FORCE.
    cat("Rerunning prior non-successful result:", outfile, "\n")
  } else if (!is.null(existing)) {
    stop(
      "An existing result does not match this design row or total_time. ",
      "Refusing to overwrite it without PROJ3_THIRD_FORCE=true: ", outfile
    )
  } else {
    stop(
      "An unreadable result exists. Refusing to overwrite it without ",
      "PROJ3_THIRD_FORCE=true: ", outfile
    )
  }
}

cat(
  "Running stress/rescue simulation", simulation_id, "of", nrow(params),
  "|", p$simulation_key, "| group:", p$design_group,
  "| seed:", p$simulation_seed, "\n"
)
print(p[, proj3_third_parameter_names(), drop = FALSE])

run_started_time <- Sys.time()
elapsed_start <- proc.time()[["elapsed"]]
warning_messages <- character(0)
simulation <- NULL
M0 <- NULL
error_message <- NA_character_
community_stats <- proj3_third_empty_community_stats()
diagnostics <- list(
  completed = FALSE,
  stop_reason = "not_started",
  simulated_time = NA_real_,
  elapsed_s = NA_real_,
  n_events = NA_integer_
)

withCallingHandlers(
  tryCatch(
    {
      required_packages <- c("pkgload", "DAISIE", "testit", "igraph", "nLTT")
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

      proj3_third_load_model(base_dir)
      M0 <- readRDS(file.path(base_dir, "script", "M0.rds"))
      set.seed(p$simulation_seed)
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

      simulation <- sim_core_mutualism_proj3(
        total_time = total_time,
        mutualism_pars = mutualism_pars,
        max_events = max_events,
        max_matrix_cells = max_matrix_cells,
        max_species_per_guild = max_species_per_guild,
        max_runtime_s = max_runtime_s,
        finalize_island = TRUE
      )
      diagnostics <- simulation$diagnostics
      diagnostics$error_message <- NA_character_
      community_stats <- proj3_third_summarize_state(
        state = simulation$state,
        M0 = M0,
        completed = isTRUE(diagnostics$completed)
      )
      # A stopped trajectory is NOT a final-age community. Preserve its partial
      # summaries separately below, but never present them as endpoint outcomes.
    },
    error = function(error) {
      error_message <<- conditionMessage(error)
      diagnostics$completed <<- FALSE
      diagnostics$stop_reason <<- if (identical(
        diagnostics$stop_reason, "not_started"
      )) {
        "simulation_error"
      } else {
        "summary_error"
      }
      diagnostics$error_message <<- error_message
    }
  ),
  warning = function(warning) {
    warning_messages <<- c(warning_messages, conditionMessage(warning))
    invokeRestart("muffleWarning")
  }
)

run_finished_time <- Sys.time()
runtime_seconds <- proc.time()[["elapsed"]] - elapsed_start
success_status <- if (!is.na(error_message)) {
  "error"
} else if (isTRUE(diagnostics$completed)) {
  "completed"
} else {
  "safety_stopped"
}

result <- list(
  schema_version = "proj3_third_stress_rescue_v1",
  simulation_id = simulation_id,
  simulation_key = p$simulation_key,
  simulation_seed = p$simulation_seed,
  design_seed = p$design_seed,
  design_group = p$design_group,
  background_id = if (is.na(p$background_id)) NA_integer_ else
    as.integer(p$background_id),
  success_status = success_status,
  run_started = format(run_started_time, "%Y-%m-%d %H:%M:%OS6 %z"),
  run_finished = format(run_finished_time, "%Y-%m-%d %H:%M:%OS6 %z"),
  runtime_seconds = unname(runtime_seconds),
  total_time = total_time,
  warnings = unique(warning_messages),
  error_message = error_message,
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
  params = as.list(p[, proj3_third_parameter_names(), drop = FALSE]),
  diagnostics = diagnostics,
  community_stats = community_stats
)
if (success_status != "completed") {
  result$partial_community_stats <- community_stats
  result$community_stats <- proj3_third_empty_community_stats()
}
# Versions and source fingerprints allow later checks for changes across jobs.
result$session_info <- capture.output(sessionInfo())
result$rng_kind <- RNGkind()
source_files <- c(sort(list.files(file.path(base_dir, "R"),
                                 full.names = TRUE, pattern = "\\.[Rr]$")),
                  file.path(base_dir, "script", "M0.rds"),
                  sort(list.files(run_dir, full.names = TRUE, pattern = "\\.(R|sh)$")),
                  param_file)
result$source_md5 <- tools::md5sum(source_files)
names(result$source_md5) <- substring(source_files, nchar(base_dir) + 2L)
if (save_state && !is.null(simulation)) {
  result$state <- simulation$state
}

proj3_third_atomic_save_rds(result, outfile)
cat(
  "Saved", outfile,
  "| status:", success_status,
  "| reason:", diagnostics$stop_reason,
  "| runtime_s:", sprintf("%.3f", runtime_seconds),
  "| warnings:", length(unique(warning_messages)),
  "\n"
)

# Simulation-level failures are represented in their RDS result and must not
# abort unrelated SLURM array tasks. Configuration/design errors above still
# return non-zero because they are infrastructure failures.
quit(save = "no", status = 0)
