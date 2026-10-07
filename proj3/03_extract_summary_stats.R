###############################################################################
# proj3/03_extract_summary_stats.R
#
# Extract community summary statistics from all proj3 simulation outputs.
#
# Input:  proj3/outputs/combo_*.rds   (or proj3/results/combo_*.rds)
# Output: proj3/combo_summary_stats.rds
#
# Each row corresponds to one combo_id, with mean and sd across replicates.
# Run from specmutual root:
#   Rscript proj3/03_extract_summary_stats.R
###############################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
})

source("proj3/utils_workflow.R")

# ---------------------------------------------------------------------------
# Settings
# ---------------------------------------------------------------------------
OUT_DIR   <- "proj3/outputs"        # adjust if your files are in proj3/results
# OUT_DIR <- "proj3/results"        # alternative location
SAVE_PATH <- "proj3/combo_summary_stats.rds"

# ---------------------------------------------------------------------------
# Discover files
# ---------------------------------------------------------------------------
file_tbl <- discover_outputs(OUT_DIR)

if (nrow(file_tbl) == 0) {
  stop("No output files found in ", OUT_DIR,
       ". Please check the path or run simulations first.")
}

cat("Found", nrow(file_tbl), "output files in", OUT_DIR, "\n")

# ---------------------------------------------------------------------------
# Loop over combos and extract stats
# ---------------------------------------------------------------------------
all_combo_stats <- list()

for (i in seq_len(nrow(file_tbl))) {
  cid  <- file_tbl$combo_id[i]
  fpath <- file_tbl$file_path[i]

  # Load combo result
  combo_res <- tryCatch(readRDS(fpath), error = function(e) NULL)
  if (is.null(combo_res)) {
    warning("Failed to read ", fpath, "; skipping.")
    next
  }

  sim_output <- combo_res$sim_output
  n_reps <- length(sim_output)

  if (n_reps == 0) {
    warning("No successful replicates for combo ", cid, "; skipping.")
    next
  }

  # Extract stats for each replicate
  rep_stats_list <- lapply(sim_output, function(rep) {
    tryCatch(get_replicate_stats(rep), error = function(e) NULL)
  })

  # Drop failed replicates
  rep_stats_list <- rep_stats_list[!sapply(rep_stats_list, is.null)]

  if (length(rep_stats_list) == 0) {
    warning("All replicates failed stat extraction for combo ", cid, "; skipping.")
    next
  }

  # Aggregate across replicates
  agg <- aggregate_combo_stats(rep_stats_list)
  agg$combo_id <- cid
  agg$n_reps   <- length(rep_stats_list)

  all_combo_stats[[as.character(cid)]] <- agg

  if (i %% 10 == 0 || i == nrow(file_tbl)) {
    cat("  Processed", i, "/", nrow(file_tbl), "combos (last: combo", cid, ")\n")
  }
}

# ---------------------------------------------------------------------------
# Combine and save
# ---------------------------------------------------------------------------
if (length(all_combo_stats) == 0) {
  stop("No combo stats were successfully extracted.")
}

summary_df <- bind_rows(all_combo_stats) %>%
  relocate(combo_id, n_reps)

cat("\nExtracted summary stats for", nrow(summary_df), "combos.\n")
cat("Columns:", ncol(summary_df), "\n")
print(head(summary_df[, 1:8]))

saveRDS(summary_df, SAVE_PATH)
cat("\nSaved to:", SAVE_PATH, "\n")
