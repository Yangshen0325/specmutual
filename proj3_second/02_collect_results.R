###############################################################################
# Collect compact per-combination results into one analysis table.
#
# Usage:
#   Rscript proj3_second/02_collect_results.R
###############################################################################

rm(list = ls())

suppressPackageStartupMessages(library(dplyr))
source("proj3_second/utils_proj3_second.R")

base_dir <- proj3_second_base_dir()
run_dir <- file.path(base_dir, "proj3_second")
params <- read.csv(
  file.path(run_dir, "param_table.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
result_dir <- file.path(run_dir, "results")

compress_ids <- function(ids) {
  ids <- sort(unique(as.integer(ids)))
  if (length(ids) == 0) {
    return("")
  }
  breaks <- c(TRUE, diff(ids) != 1)
  groups <- cumsum(breaks)
  runs <- split(ids, groups)
  paste(vapply(
    runs,
    function(x) {
      if (length(x) == 1) as.character(x) else paste0(x[1], "-", x[length(x)])
    },
    character(1)
  ), collapse = ",")
}

rows <- list()
unreadable <- integer(0)
for (combo_id in params$combo_id) {
  path <- file.path(result_dir, sprintf("combo_%04d.rds", combo_id))
  if (!file.exists(path)) {
    next
  }
  result <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(result)) {
    unreadable <- c(unreadable, combo_id)
    next
  }
  row <- proj3_second_flatten_result(result)
  row$result_file_bytes <- file.size(path)
  rows[[as.character(combo_id)]] <- row
}

if (length(rows) == 0) {
  combined <- data.frame()
} else {
  combined <- bind_rows(rows) %>% arrange(combo_id)
}

available_ids <- if (nrow(combined)) combined$combo_id else integer(0)
missing_ids <- setdiff(params$combo_id, available_ids)
expected_runtime_cap <- as.numeric(Sys.getenv(
  "PROJ3_MAX_RUNTIME_S",
  unset = "25200"
))
short_runtime_ids <- if (
  nrow(combined) &&
    all(c("stop_reason", "cap_max_runtime_s") %in% names(combined))
) {
  combined$combo_id[
    combined$stop_reason == "max_runtime_s" &
      as.numeric(combined$cap_max_runtime_s) < expected_runtime_cap
  ]
} else {
  integer(0)
}
retry_ids <- sort(unique(c(missing_ids, unreadable, short_runtime_ids)))
array_spec <- compress_ids(retry_ids)

saveRDS(combined, file.path(run_dir, "pilot_results_combined.rds"))
write.csv(
  combined,
  file.path(run_dir, "pilot_results_combined.csv"),
  row.names = FALSE,
  na = ""
)
writeLines(array_spec, file.path(run_dir, "missing_array.txt"))

cat("Parameter combinations:", nrow(params), "\n")
cat("Readable results:", nrow(combined), "\n")
cat("Tasks requiring run/rerun:", length(retry_ids), "\n")
if (length(short_runtime_ids)) {
  cat(
    "Readable but scheduled for rerun because of a shorter local runtime cap:",
    length(short_runtime_ids),
    "\n"
  )
}
if (nrow(combined) > 0 && "completed" %in% names(combined)) {
  cat("Completed to total_time:", sum(combined$completed %in% TRUE), "\n")
  cat("Stop reasons:\n")
  print(table(combined$stop_reason, useNA = "ifany"))
}
if (nzchar(array_spec)) {
  cat("\nSLURM retry array:\n", array_spec, "\n", sep = "")
} else {
  cat("\nAll 1,000 result files are readable.\n")
}
