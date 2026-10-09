###############################################################################
# Prepare community summaries before Part I random forests.
# Run from the repository root:
#   Rscript proj3_third/05_collect_stress_rescue_results.R
# Keep all design rows, even if their cluster results have not arrived.
# No imputation, averaging, or replacement of endpoints by partial trajectories.
###############################################################################
rm(list = ls())
source("proj3_third/summary_utils_proj3_third.R")
source("proj3_third/workflow_utils_proj3_third.R")

base_dir <- proj3_third_base_dir()
run_dir <- file.path(base_dir, "proj3_third")
param_file <- file.path(run_dir, "data", "generated", "stress_rescue_param_table.csv")
processed_dir <- file.path(run_dir, "data", "processed")
configured <- Sys.getenv("PROJ3_THIRD_OUTPUT_DIR", unset = "")
result_dir <- if (nzchar(configured)) path.expand(configured) else
  file.path(run_dir, "stress_rescue_results")
params <- read.csv(param_file, stringsAsFactors = FALSE)
total_time <- proj3_third_env_number("PROJ3_THIRD_TOTAL_TIME", 10)
summary_names <- proj3_third_community_stat_names()
on_groups <- c("weak", "intermediate", "strong")
if (anyDuplicated(params$simulation_id) || anyDuplicated(params$simulation_key)) {
  stop("The parameter table must have unique simulation IDs and keys.")
}

# Compact RDS files already contain community_stats calculated by 02 before
# discarding the full state. Without state, reuse those summaries rather than
# pretending to reconstruct histories from parameters. If state IS saved,
# calculate exactly the same summaries here. Undefined network values stay NA.
M0 <- NULL
get_community_stats <- function(result) {
  if (!is.null(result$state)) {
    if (is.null(result$state$island)) {
      stop("Saved state lacks finalized lineage histories needed for nLTT.")
    }
    if (is.null(M0)) M0 <<- readRDS(file.path(base_dir, "script", "M0.rds"))
    values <- proj3_third_summarize_state(result$state, M0, completed = TRUE)
    source <- "recomputed_from_state"
  } else {
    values <- result$community_stats
    source <- "saved_community_stats"
  }
  if (!all(summary_names %in% names(values))) {
    stop("Required summaries are absent and cannot be reconstructed without state.")
  }
  values <- unlist(values[summary_names])
  if (!is.numeric(values) || any(is.infinite(values))) {
    stop("Community summaries must be numeric, with NA only where undefined.")
  }
  list(values = values, source = source)
}

rows <- vector("list", nrow(params))
for (i in seq_len(nrow(params))) {
  p <- params[i, , drop = FALSE]
  path <- file.path(result_dir, paste0(p$simulation_key, ".rds"))
  result <- if (file.exists(path)) tryCatch(readRDS(path), error = function(e) NULL) else NULL
  status <- if (!file.exists(path)) "missing" else if (is.null(result)) "unreadable" else
    if (!proj3_third_result_matches(result, p, total_time)) "design_mismatch" else "matched"
  summary_status <- "not_available"
  summary_source <- NA_character_
  summary_error <- NA_character_

  if (status == "matched") {
    endpoint_complete <- identical(result$success_status, "completed") &&
      isTRUE(result$diagnostics$completed)
    if (endpoint_complete) {
      summaries <- tryCatch(get_community_stats(result), error = function(e) e)
      if (inherits(summaries, "error")) {
        summary_status <- "summary_error"
        summary_error <- conditionMessage(summaries)
        result$community_stats <- proj3_third_empty_community_stats()
      } else {
        result$community_stats <- summaries$values
        summary_status <- "available"
        summary_source <- summaries$source
      }
    } else {
      result$community_stats <- proj3_third_empty_community_stats()
      if (identical(result$success_status, "completed")) {
        summary_status <- "inconsistent_completion"
      }
    }
    row <- proj3_third_flatten_result(result)
    # result_matches() checks ID/key/seed/parameters; never match by file order.
    row <- cbind(p, row[, setdiff(names(row), names(p)), drop = FALSE])
  } else {
    row <- p
    row$success_status <- status
    row$stop_reason <- status
    row$completed <- FALSE
    row <- cbind(row, as.data.frame(as.list(proj3_third_empty_community_stats())))
  }
  row$summary_status <- summary_status
  row$summary_source <- summary_source
  row$summary_error <- summary_error
  row$result_file_bytes <- if (file.exists(path)) file.size(path) else NA_real_
  rows[[i]] <- row
}
combined <- proj3_third_bind_rows(rows)
endpoint_ok <- combined$success_status == "completed" &
  combined$summary_status == "available"
combined$analysis_eligible <- endpoint_ok & combined$design_group %in% on_groups
endpoints <- combined[endpoint_ok, , drop = FALSE]
analysis_df <- combined[combined$analysis_eligible, , drop = FALSE]
baselines <- endpoints[endpoints$design_group == "no_mutualism", , drop = FALSE]

dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
# Save every planned parameter row with completion and summary-availability status.
saveRDS(combined, file.path(processed_dir, "stress_rescue_results_combined.rds"))
# Save successfully summarized endpoints from all scenarios for later comparisons.
saveRDS(endpoints, file.path(processed_dir, "stress_rescue_completed.rds"))
# Save continuous ON rows for RF; identifiers are for grouping, never predictors.
saveRDS(analysis_df, file.path(processed_dir, "part1_analysis_ready.rds"))
# Save exact-zero controls separately from the continuous-parameter regressions.
saveRDS(baselines, file.path(processed_dir, "stress_rescue_no_mutualism.rds"))

counts <- as.data.frame(table(combined$design_group, combined$success_status,
                             combined$summary_status))
names(counts) <- c("design_group", "success_status", "summary_status", "n")
counts <- counts[counts$n > 0, ]
# Save a small status table to track partial delivery and incomplete simulations.
write.csv(counts, file.path(processed_dir, "stress_rescue_status_counts.csv"), row.names = FALSE)
retry <- combined[!endpoint_ok, c("simulation_id", "simulation_key", "design_group",
                                  "success_status", "summary_status", "summary_error")]
retry$batch_offset <- ((retry$simulation_id - 1L) %/% 1000L) * 1000L
retry$array_task_id <- retry$simulation_id - retry$batch_offset
# Save a REVIEW table, not an automatic retry command: absent files may be in transit.
write.csv(retry, file.path(processed_dir, "stress_rescue_retry_review.csv"), row.names = FALSE)
cat("Expected rows:", nrow(params), "| Available completed endpoints:", nrow(endpoints),
    "| Continuous ON rows for RF:", nrow(analysis_df), "\n")
print(counts)
