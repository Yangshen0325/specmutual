###############################################################################
# proj3/04_build_dataset.R
#
# Merge summary statistics with parameter table and optionally compute
# mutualism effect sizes against no-mutualism reference simulations.
#
# Input:  proj3/combo_summary_stats.rds
#         proj3/param_table.csv
#         proj3/outputs_none/combo_*.rds  (optional, for effect sizes)
# Output: proj3/dataset_for_rf.rds
#
# Run from specmutual root:
#   Rscript proj3/04_build_dataset.R
###############################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
})

source("proj3/utils_workflow.R")

# ---------------------------------------------------------------------------
# Settings
# ---------------------------------------------------------------------------
SUMMARY_PATH <- "proj3/combo_summary_stats.rds"
PARAM_PATH   <- "proj3/param_table.csv"
NONE_DIR     <- "proj3/outputs_none"   # set to NULL if you don't have these yet
SAVE_PATH    <- "proj3/dataset_for_rf.rds"

# ---------------------------------------------------------------------------
# Load summary stats and parameters
# ---------------------------------------------------------------------------
if (!file.exists(SUMMARY_PATH)) {
  stop(SUMMARY_PATH, " not found. Run 03_extract_summary_stats.R first.")
}

summary_df <- readRDS(SUMMARY_PATH)
params_df  <- load_param_table(PARAM_PATH)

cat("Summary stats:", nrow(summary_df), "combos\n")
cat("Parameters:   ", nrow(params_df),  "combos\n")

# ---------------------------------------------------------------------------
# Merge
# ---------------------------------------------------------------------------
dataset <- params_df %>%
  left_join(summary_df, by = "combo_id")

cat("Merged dataset:", nrow(dataset), "rows x", ncol(dataset), "cols\n")

# ---------------------------------------------------------------------------
# Optional: compute mutualism effect sizes
# ---------------------------------------------------------------------------
# If you have a parallel set of simulations with K_1=mu_1=laa_1=lambda0=0
# (same intrinsic parameters), we can compute delta stats.
# ---------------------------------------------------------------------------
compute_effect_sizes <- FALSE
if (!is.null(NONE_DIR) && dir.exists(NONE_DIR)) {
  cat("\nNo-mutualism directory found:", NONE_DIR, "\n")
  cat("Computing effect sizes...\n")

  none_tbl <- discover_outputs(NONE_DIR)

  if (nrow(none_tbl) > 0) {
    compute_effect_sizes <- TRUE

    none_stats_list <- list()
    for (i in seq_len(nrow(none_tbl))) {
      cid   <- none_tbl$combo_id[i]
      fpath <- none_tbl$file_path[i]

      combo_res <- tryCatch(readRDS(fpath), error = function(e) NULL)
      if (is.null(combo_res)) next

      sim_output <- combo_res$sim_output
      if (length(sim_output) == 0) next

      rep_stats_list <- lapply(sim_output, function(rep) {
        tryCatch(get_replicate_stats(rep), error = function(e) NULL)
      })
      rep_stats_list <- rep_stats_list[!sapply(rep_stats_list, is.null)]

      if (length(rep_stats_list) == 0) next

      agg <- aggregate_combo_stats(rep_stats_list)
      agg$combo_id <- cid
      none_stats_list[[as.character(cid)]] <- agg
    }

    none_df <- bind_rows(none_stats_list) %>%
      select(combo_id, ends_with("_mean"))

    # Rename none columns
    mean_cols <- setdiff(names(none_df), "combo_id")
    none_df <- none_df %>%
      rename_with(~ paste0(., "_none"), all_of(mean_cols))

    # Merge and compute deltas
    dataset <- dataset %>%
      left_join(none_df, by = "combo_id")

    # Compute delta for each mean stat that exists in both
    stat_names <- gsub("_mean$", "", mean_cols)
    for (s in stat_names) {
      mut_col <- paste0(s, "_mean")
      non_col <- paste0(mut_col, "_none")
      if (mut_col %in% names(dataset) && non_col %in% names(dataset)) {
        dataset[[paste0(s, "_delta")]] <- dataset[[mut_col]] - dataset[[non_col]]
      }
    }

    cat("Effect sizes computed for", length(stat_names), "variables.\n")
  } else {
    cat("No files found in", NONE_DIR, "; skipping effect sizes.\n")
  }
} else {
  cat("No no-mutualism directory found; skipping effect sizes.\n")
}

# ---------------------------------------------------------------------------
# Define derived variables
# ---------------------------------------------------------------------------
# Composite mutualism-strength index (standardized and averaged)
mut_params <- c("K_1", "mu_1", "laa_1", "lambda0")
if (all(mut_params %in% names(dataset))) {
  # Standardize each to [0,1] based on their LHS ranges
  dataset$K_1_std     <- dataset$K_1 / 150
  dataset$mu_1_std    <- dataset$mu_1 / 0.02
  dataset$laa_1_std   <- dataset$laa_1 / 0.02
  dataset$lambda0_std <- dataset$lambda0 / 1.0

  dataset$mutualism_index <- rowMeans(
    dataset[, c("K_1_std", "mu_1_std", "laa_1_std", "lambda0_std")],
    na.rm = TRUE
  )
  cat("Created mutualism_index (mean of standardized K_1, mu_1, laa_1, lambda0).\n")
}

# Total richness and endemism
if (all(c("island_p_mean", "island_a_mean") %in% names(dataset))) {
  dataset$richness_total_mean <- dataset$island_p_mean + dataset$island_a_mean
}
if (all(c("island_endemic_p_mean", "island_endemic_a_mean") %in% names(dataset))) {
  dataset$endemic_total_mean <- dataset$island_endemic_p_mean + dataset$island_endemic_a_mean
}

# ---------------------------------------------------------------------------
# Save
# ---------------------------------------------------------------------------
attr(dataset, "compute_effect_sizes") <- compute_effect_sizes
attr(dataset, "n_combos") <- nrow(dataset)

saveRDS(dataset, SAVE_PATH)
cat("\nSaved master dataset to:", SAVE_PATH, "\n")
cat("Dimensions:", nrow(dataset), "x", ncol(dataset), "\n")
