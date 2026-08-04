###############################################################################
# Collect final per-simulation RDS files and prepare a retry array.
#
# Usage:
#   Rscript proj3_second/07_collect_final_results_proj3.R
#
# By default, missing, unreadable, and simulation-error rows are retried.
# Set PROJ3_RETRY_SAFETY_STOPPED=true to add safety-stopped rows.
###############################################################################

rm(list = ls())

source("proj3_second/utils_proj3_second.R")
source("proj3_second/final_utils_proj3.R")

base_dir <- proj3_second_base_dir()
run_dir <- file.path(base_dir, "proj3_second")
param_file <- file.path(run_dir, "final_param_table.csv")
result_dir <- file.path(run_dir, "final_results")

if (!file.exists(param_file)) {
  stop("Final parameter table not found: ", param_file)
}
params <- read.csv(param_file, stringsAsFactors = FALSE, check.names = FALSE)
retry_safety <- proj3_final_env_flag("PROJ3_RETRY_SAFETY_STOPPED", FALSE)

rows <- list()
missing_ids <- integer(0)
unreadable_ids <- integer(0)
mismatched_ids <- integer(0)

for (simulation_id in params$simulation_id) {
  key <- params$simulation_key[params$simulation_id == simulation_id]
  path <- file.path(result_dir, paste0(key, ".rds"))
  if (!file.exists(path)) {
    missing_ids <- c(missing_ids, simulation_id)
    next
  }
  result <- tryCatch(readRDS(path), error = function(error) NULL)
  if (is.null(result)) {
    unreadable_ids <- c(unreadable_ids, simulation_id)
    next
  }
  if (!identical(as.integer(result$simulation_id), as.integer(simulation_id)) ||
      !identical(result$simulation_key, key)) {
    mismatched_ids <- c(mismatched_ids, simulation_id)
    next
  }
  row <- proj3_final_flatten_result(result)
  row$result_file_bytes <- file.size(path)
  rows[[as.character(simulation_id)]] <- row
}

combined <- proj3_final_bind_rows(rows)
if (nrow(combined)) {
  combined <- combined[order(as.integer(combined$simulation_id)), , drop = FALSE]
  row.names(combined) <- NULL
}

error_ids <- if (nrow(combined)) {
  as.integer(combined$simulation_id[combined$success_status == "error"])
} else {
  integer(0)
}
safety_ids <- if (retry_safety && nrow(combined)) {
  as.integer(
    combined$simulation_id[combined$success_status == "safety_stopped"]
  )
} else {
  integer(0)
}
retry_ids <- sort(unique(c(
  missing_ids, unreadable_ids, mismatched_ids, error_ids, safety_ids
)))
retry_array <- proj3_final_compress_ids(retry_ids)

saveRDS(combined, file.path(run_dir, "final_results_combined.rds"))
write.csv(
  combined,
  file.path(run_dir, "final_results_combined.csv"),
  row.names = FALSE,
  na = ""
)
writeLines(retry_array, file.path(run_dir, "final_retry_array.txt"))

cat("Expected simulations:", nrow(params), "\n")
cat("Readable, matching results:", nrow(combined), "\n")
cat("Missing:", length(missing_ids), "\n")
cat("Unreadable:", length(unreadable_ids), "\n")
cat("ID/key mismatch:", length(mismatched_ids), "\n")
if (nrow(combined)) {
  cat("Statuses:\n")
  print(table(combined$success_status, useNA = "ifany"))
}
cat("Tasks in retry array:", length(retry_ids), "\n")
if (nzchar(retry_array)) {
  cat("SLURM array specification:\n", retry_array, "\n", sep = "")
} else {
  cat("No failed or missing tasks require rerunning.\n")
}
