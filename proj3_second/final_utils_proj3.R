###############################################################################
# Small final-workflow helpers layered on utils_proj3_second.R.
###############################################################################

proj3_final_parameter_names <- function() {
  c(
    "lac_0", "mu_0", "gam_0", "laa_0", "K_0",
    "K_1", "mu_1", "laa_1", "lambda0"
  )
}

proj3_final_env_flag <- function(name, default = FALSE) {
  fallback <- if (isTRUE(default)) "true" else "false"
  value <- tolower(trimws(Sys.getenv(name, unset = fallback)))
  if (!value %in% c("true", "false", "1", "0", "yes", "no")) {
    stop(name, " must be true/false, 1/0, or yes/no; received: ", value)
  }
  value %in% c("true", "1", "yes")
}

proj3_final_env_number <- function(name, default, integer = FALSE) {
  value <- Sys.getenv(name, unset = as.character(default))
  parsed <- if (integer) {
    suppressWarnings(as.integer(value))
  } else {
    suppressWarnings(as.numeric(value))
  }
  if (length(parsed) != 1L || !is.finite(parsed) || parsed <= 0) {
    stop(name, " must be a positive number; received: ", value)
  }
  parsed
}

proj3_final_community_stat_names <- function() {
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
}

proj3_final_empty_community_stats <- function() {
  setNames(
    rep(NA_real_, length(proj3_final_community_stat_names())),
    proj3_final_community_stat_names()
  )
}

proj3_final_result_matches <- function(result, parameter_row, total_time) {
  if (is.null(result) ||
      !identical(result$schema_version, "proj3_second_final_v1") ||
      !identical(as.integer(result$simulation_id),
                 as.integer(parameter_row$simulation_id)) ||
      !isTRUE(all.equal(as.numeric(result$total_time), total_time))) {
    return(FALSE)
  }
  expected <- as.numeric(unlist(
    parameter_row[, proj3_final_parameter_names(), drop = FALSE],
    use.names = FALSE
  ))
  observed <- as.numeric(unlist(
    result$params[proj3_final_parameter_names()],
    use.names = FALSE
  ))
  length(observed) == length(expected) &&
    isTRUE(all.equal(observed, expected, tolerance = 0))
}

proj3_final_flatten_result <- function(result) {
  warnings_text <- if (length(result$warnings)) {
    paste(unique(result$warnings), collapse = " | ")
  } else {
    ""
  }
  values <- c(
    list(
      simulation_id = result$simulation_id,
      simulation_key = result$simulation_key,
      simulation_seed = result$simulation_seed,
      design_seed = result$design_seed,
      design_group = result$design_group,
      anchor_set_id = result$anchor_set_id,
      success_status = result$success_status,
      run_started = result$run_started,
      run_finished = result$run_finished,
      runtime_seconds = result$runtime_seconds,
      total_time = result$total_time,
      warnings = warnings_text,
      error_message = result$error_message
    ),
    result$params[proj3_final_parameter_names()],
    as.list(setNames(
      unlist(result$fixed_parameters),
      paste0("fixed_", names(result$fixed_parameters))
    )),
    as.list(setNames(
      unlist(result$safety_caps),
      paste0("cap_", names(result$safety_caps))
    )),
    result$diagnostics,
    as.list(result$community_stats)
  )
  # Avoid duplicate top-level error/status columns if diagnostics contain them.
  values <- values[!duplicated(names(values))]
  as.data.frame(values, stringsAsFactors = FALSE, check.names = FALSE)
}

proj3_final_bind_rows <- function(rows) {
  if (!length(rows)) return(data.frame())
  all_names <- unique(unlist(lapply(rows, names), use.names = FALSE))
  normalized <- lapply(rows, function(row) {
    missing <- setdiff(all_names, names(row))
    for (name in missing) row[[name]] <- NA
    row[, all_names, drop = FALSE]
  })
  out <- do.call(rbind, normalized)
  row.names(out) <- NULL
  out
}

proj3_final_compress_ids <- function(ids) {
  ids <- sort(unique(as.integer(ids)))
  ids <- ids[is.finite(ids)]
  if (!length(ids)) return("")
  runs <- split(ids, cumsum(c(TRUE, diff(ids) != 1L)))
  paste(vapply(
    runs,
    function(run) {
      if (length(run) == 1L) as.character(run) else
        paste0(run[1], "-", run[length(run)])
    },
    character(1)
  ), collapse = ",")
}
