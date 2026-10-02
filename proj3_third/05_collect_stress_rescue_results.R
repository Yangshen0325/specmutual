###############################################################################
# Collect after cluster execution, from the repository root:
#   Rscript proj3_third/05_collect_stress_rescue_results.R
# Keep ALL 10,000 design rows, including missing/error/safety-stopped runs.
# No imputation and no averaging. Partial trajectories are not final outcomes.
###############################################################################
rm(list = ls())
source("proj3_third/summary_utils_proj3_third.R")
source("proj3_third/workflow_utils_proj3_third.R")
run_dir <- file.path(proj3_third_base_dir(), "proj3_third")
params <- read.csv(file.path(run_dir, "stress_rescue_param_table.csv"))
configured <- Sys.getenv("PROJ3_THIRD_OUTPUT_DIR", unset = "")
result_dir <- if (nzchar(configured)) path.expand(configured) else
  file.path(run_dir, "stress_rescue_results")
total_time <- proj3_third_env_number("PROJ3_THIRD_TOTAL_TIME", 10)

rows <- vector("list", nrow(params))
for (i in seq_len(nrow(params))) {
  p <- params[i, , drop = FALSE]
  path <- file.path(result_dir, paste0(p$simulation_key, ".rds"))
  result <- if (file.exists(path)) tryCatch(readRDS(path), error = function(e) NULL) else NULL
  status <- if (!file.exists(path)) "missing" else if (is.null(result)) "unreadable" else
    if (!proj3_third_result_matches(result, p, total_time)) "design_mismatch" else "matched"
  if (status == "matched") {
    row <- proj3_third_flatten_result(result)
    # Restore any new design columns while keeping the parameter CSV authoritative.
    row <- cbind(p, row[, setdiff(names(row), names(p)), drop = FALSE])
  } else {
    row <- p
    row$success_status <- status
    row$stop_reason <- status
    row$completed <- FALSE
    row <- cbind(row, as.data.frame(as.list(proj3_third_empty_community_stats())))
  }
  row$result_file_bytes <- if (file.exists(path)) file.size(path) else NA_real_
  rows[[i]] <- row
}
combined <- proj3_third_bind_rows(rows)
write.csv(combined, file.path(run_dir, "stress_rescue_results_combined.csv"),
          row.names = FALSE, na = "")
saveRDS(combined, file.path(run_dir, "stress_rescue_results_combined.rds"))

# A convenient clean endpoint table; always use the all-design table above to
# assess missingness and cap dependence before estimating rescue contrasts.
endpoints <- combined[combined$success_status == "completed", , drop = FALSE]
write.csv(endpoints, file.path(run_dir, "stress_rescue_completed.csv"),
          row.names = FALSE, na = "")
counts <- as.data.frame(table(combined$design_group, combined$success_status))
names(counts) <- c("design_group", "success_status", "n")
write.csv(counts, file.path(run_dir, "stress_rescue_status_counts.csv"), row.names = FALSE)

# Retry candidates are a REVIEW list, not a command that automatically overwrites
# results. Inspect cap hits and mismatches first; increased caps change censoring.
retry <- combined[combined$success_status != "completed",
                  c("simulation_id", "simulation_key", "design_group", "success_status")]
retry$batch_offset <- ((retry$simulation_id - 1L) %/% 1000L) * 1000L
retry$array_task_id <- retry$simulation_id - retry$batch_offset
write.csv(retry, file.path(run_dir, "stress_rescue_retry_review.csv"), row.names = FALSE)
cat("Expected rows:", nrow(params), "| Completed endpoints:", nrow(endpoints), "\n")
print(counts[counts$n > 0, ])
