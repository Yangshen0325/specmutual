###############################################################################
# Run one row from the final Part II design.
#
# Usage:
#   Rscript proj3_second/06_run_final_sim_proj3.R <simulation_id>
#   PROJ3_DRY_RUN=true Rscript proj3_second/06_run_final_sim_proj3.R 1
#
# If no command-line ID is supplied, SLURM_ARRAY_TASK_ID is used.
#
# Optional environment variables:
#   SPEC_MUTUAL_BASE, PROJ3_TOTAL_TIME, PROJ3_MAX_EVENTS,
#   PROJ3_MAX_MATRIX_CELLS, PROJ3_MAX_SPECIES_PER_GUILD,
#   PROJ3_MAX_RUNTIME_S, PROJ3_SAVE_STATE, PROJ3_FORCE,
#   PROJ3_RERUN_SAFETY_STOPPED, PROJ3_DRY_RUN, PROJ3_OUTPUT_DIR.
###############################################################################

rm(list = ls())

source("proj3_second/utils_proj3_second.R")
source("proj3_second/final_utils_proj3.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 1L) {
  stop("Usage: Rscript proj3_second/06_run_final_sim_proj3.R [simulation_id]")
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

base_dir <- proj3_second_base_dir()
run_dir <- file.path(base_dir, "proj3_second")
param_file <- file.path(run_dir, "final_param_table.csv")
configured_output_dir <- Sys.getenv("PROJ3_OUTPUT_DIR", unset = "")
out_dir <- if (nzchar(configured_output_dir)) {
  path.expand(configured_output_dir)
} else {
  file.path(run_dir, "final_results")
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

total_time <- proj3_final_env_number("PROJ3_TOTAL_TIME", 10)
max_events <- proj3_final_env_number(
  "PROJ3_MAX_EVENTS", 20000, integer = TRUE
)
max_matrix_cells <- proj3_final_env_number(
  "PROJ3_MAX_MATRIX_CELLS", 4000000, integer = TRUE
)
max_species_per_guild <- proj3_final_env_number(
  "PROJ3_MAX_SPECIES_PER_GUILD", 1500, integer = TRUE
)
max_runtime_s <- proj3_final_env_number("PROJ3_MAX_RUNTIME_S", 25200)
save_state <- proj3_final_env_flag("PROJ3_SAVE_STATE", FALSE)
force <- proj3_final_env_flag("PROJ3_FORCE", FALSE)
rerun_safety <- proj3_final_env_flag("PROJ3_RERUN_SAFETY_STOPPED", FALSE)
dry_run <- proj3_final_env_flag("PROJ3_DRY_RUN", FALSE)

if (dry_run) {
  cat("DRY RUN - no simulation will be executed or result written.\n")
  cat("Simulation:", simulation_id, "| key:", p$simulation_key, "\n")
  cat(
    "Group:", p$design_group,
    "| anchor set:", ifelse(is.na(p$anchor_set_id), "NA", p$anchor_set_id),
    "| seed:", p$simulation_seed, "\n"
  )
  cat("Parameter file:", param_file, "\n")
  cat("Expected output:", outfile, "\n")
  cat("Parameters:\n")
  print(p[, proj3_final_parameter_names(), drop = FALSE])
  quit(save = "no", status = 0)
}

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (file.exists(outfile) && !force) {
  existing <- tryCatch(readRDS(outfile), error = function(e) NULL)
  if (proj3_final_result_matches(existing, p, total_time)) {
    if (identical(existing$success_status, "completed")) {
      cat("Valid completed result already exists - skipping:", outfile, "\n")
      quit(save = "no", status = 0)
    }
    if (identical(existing$success_status, "safety_stopped") &&
        !rerun_safety) {
      cat(
        "Valid safety-stopped result already exists - skipping. Set ",
        "PROJ3_RERUN_SAFETY_STOPPED=true to rerun: ", outfile, "\n",
        sep = ""
      )
      quit(save = "no", status = 0)
    }
    # Saved simulation errors are deliberately rerunnable without PROJ3_FORCE.
    cat("Rerunning prior non-successful result:", outfile, "\n")
  } else if (!is.null(existing)) {
    stop(
      "An existing result does not match this design row or total_time. ",
      "Refusing to overwrite it without PROJ3_FORCE=true: ", outfile
    )
  } else {
    stop(
      "An unreadable result exists. Refusing to overwrite it without ",
      "PROJ3_FORCE=true: ", outfile
    )
  }
}

cat(
  "Running final simulation", simulation_id, "of", nrow(params),
  "|", p$simulation_key, "| group:", p$design_group,
  "| seed:", p$simulation_seed, "\n"
)
print(p[, proj3_final_parameter_names(), drop = FALSE])

run_started_time <- Sys.time()
elapsed_start <- proc.time()[["elapsed"]]
warning_messages <- character(0)
simulation <- NULL
M0 <- NULL
error_message <- NA_character_
community_stats <- proj3_final_empty_community_stats()
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

      proj3_second_load_model(base_dir)
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
      community_stats <- proj3_second_summarize_state(
        state = simulation$state,
        M0 = M0,
        completed = isTRUE(diagnostics$completed)
      )
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
  schema_version = "proj3_second_final_v1",
  simulation_id = simulation_id,
  simulation_key = p$simulation_key,
  simulation_seed = p$simulation_seed,
  design_seed = p$design_seed,
  design_group = p$design_group,
  anchor_set_id = if (is.na(p$anchor_set_id)) NA_integer_ else
    as.integer(p$anchor_set_id),
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
  params = as.list(p[, proj3_final_parameter_names(), drop = FALSE]),
  diagnostics = diagnostics,
  community_stats = community_stats
)
if (save_state && !is.null(simulation)) {
  result$state <- simulation$state
}

proj3_second_atomic_save_rds(result, outfile)
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
