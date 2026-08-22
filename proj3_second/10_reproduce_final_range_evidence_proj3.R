###############################################################################
# Reproduce the pilot evidence used by FINAL_RANGE_REPORT.md.
#
# The main PILOT_RESULTS_REPORT.md is a four-file local smoke report. This
# script reads the complete cluster result directory instead.
###############################################################################

rm(list = ls())
source("proj3_second/utils_proj3_second.R")

base_dir <- proj3_second_base_dir()
run_dir <- file.path(base_dir, "proj3_second")
default_result_dirs <- c(
  file.path(run_dir, "from_cluster_PILOT", "results"),
  file.path(run_dir, "from_cluster", "results")
)
default_result_dir <- default_result_dirs[dir.exists(default_result_dirs)][1]
if (is.na(default_result_dir)) default_result_dir <- default_result_dirs[1]
result_dir <- Sys.getenv("PROJ3_PILOT_RESULT_DIR", unset = default_result_dir)
if (!dir.exists(result_dir)) stop("Pilot result directory not found: ", result_dir)
files <- sort(list.files(result_dir, "^combo_[0-9]+[.]rds$", full.names = TRUE))
if (!length(files)) stop("No combo_*.rds files found in ", result_dir)

parameter_names <- c(
  "lac_0", "mu_0", "gam_0", "laa_0", "K_0",
  "K_1", "mu_1", "laa_1", "lambda0"
)
extract_one <- function(path) {
  result <- readRDS(path)
  p <- result$params
  d <- result$diagnostics
  s <- result$community_stats
  values <- c(
    list(
      combo_id = result$combo_id,
      design_group = result$design_group,
      completed = isTRUE(d$completed),
      stop_reason = d$stop_reason,
      simulated_time = d$simulated_time,
      elapsed_s = d$elapsed_s,
      n_events = d$n_events,
      island_p = unname(s["island_p"]),
      island_a = unname(s["island_a"]),
      final_D_nonzero_fraction = unname(s["final_D_nonzero_fraction"]),
      events_cospeciation = d$events_cospeciation,
      hazard_cospeciation = d$hazard_cospeciation,
      mu1d_all_weight = d$mu1d_all_weight,
      mu1d_all_mean = d$mu1d_all_mean,
      mu1d_all_frac_inactive = d$mu1d_all_frac_inactive,
      mu1d_all_frac_informative = d$mu1d_all_frac_informative,
      mu1d_all_frac_strong = d$mu1d_all_frac_strong,
      laa1D_over_laa0_all_weight = d$laa1D_over_laa0_all_weight,
      laa1D_over_laa0_all_mean = d$laa1D_over_laa0_all_mean,
      laa1D_over_laa0_all_frac_inactive = d$laa1D_over_laa0_all_frac_inactive,
      laa1D_over_laa0_all_frac_informative = d$laa1D_over_laa0_all_frac_informative,
      laa1D_over_laa0_all_frac_strong = d$laa1D_over_laa0_all_frac_strong,
      K1d_over_K0_all_weight = d$K1d_over_K0_all_weight,
      K1d_over_K0_all_mean = d$K1d_over_K0_all_mean,
      K1d_over_K0_all_frac_inactive = d$K1d_over_K0_all_frac_inactive,
      K1d_over_K0_all_frac_informative = d$K1d_over_K0_all_frac_informative,
      K1d_over_K0_all_frac_strong = d$K1d_over_K0_all_frac_strong
    ),
    p[parameter_names]
  )
  as.data.frame(values, stringsAsFactors = FALSE, check.names = FALSE)
}
rows <- lapply(files, extract_one)
all_names <- unique(unlist(lapply(rows, names), use.names = FALSE))
rows <- lapply(rows, function(row) {
  for (name in setdiff(all_names, names(row))) row[[name]] <- NA
  row[, all_names, drop = FALSE]
})
data <- do.call(rbind, rows)
row.names(data) <- NULL
data[] <- lapply(data, function(x) type.convert(x, as.is = TRUE))
broad <- data[data$design_group == "broad_lhs", , drop = FALSE]

weighted_bands <- function(prefix) {
  weight <- broad[[paste0(prefix, "_weight")]]
  fractions <- vapply(
    c("inactive", "informative", "strong"),
    function(label) broad[[paste0(prefix, "_frac_", label)]],
    numeric(nrow(broad))
  )
  # summarize all
  setNames(colSums(fractions * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
           c("inactive", "informative", "strong"))
}
summary <- data.frame(
  statistic = c(
    "result_files", "broad_files", "broad_completed", "broad_matrix_cap",
    "broad_sparse_final_community", "broad_D_evaluable",
    "broad_final_D_exactly_zero",
    "broad_cospeciation_zero"
  ),
  # "broad_matrix_cap": how many failed because of reaching the max matrix cell limit
  # "broad_sparse_final_community": compute the average of sparse community.
  # "broad_D_evaluable": the sum of all non NA final_D_nonzero_fraction (how many simulations)
  value = c(
    nrow(data), nrow(broad), sum(broad$completed),
    sum(broad$stop_reason == "max_matrix_cells"),
    mean(broad$island_p < 2 | broad$island_a < 2, na.rm = TRUE),
    sum(!is.na(broad$final_D_nonzero_fraction)),
    mean(broad$final_D_nonzero_fraction == 0, na.rm = TRUE),
    mean(broad$events_cospeciation == 0, na.rm = TRUE)
  ),
  source_field = c(
    "number of RDS files", "design_group", "diagnostics$completed",
    "diagnostics$stop_reason", "community_stats$island_p/island_a",
    "nonmissing community_stats$final_D_nonzero_fraction",
    "community_stats$final_D_nonzero_fraction",
    "diagnostics$events_cospeciation"
  ),
  stringsAsFactors = FALSE
)
for (prefix in c("mu1d_all", "laa1D_over_laa0_all", "K1d_over_K0_all")) {
  bands <- weighted_bands(prefix)
  summary <- rbind(summary, data.frame(
    statistic = paste0(prefix, "_weighted_", names(bands)),
    value = unname(bands),
    source_field = paste0("diagnostics$", prefix, "_weight * diagnostics$",
                          prefix, "_frac_*"),
    stringsAsFactors = FALSE
  ))
}

# only one row following all the rules can be chosen
selected <- with(
  broad,
  lac_0 >= 0.04 & lac_0 <= 0.80 & mu_0 >= 0.03 & mu_0 <= 0.60 &
    gam_0 >= 0.01 & gam_0 <= 0.20 & laa_0 >= 0.04 & laa_0 <= 2.00 &
    K_0 >= 40 & K_0 <= 120 & K_1 <= 40
)

summary <- rbind(summary, data.frame(
  statistic = c("retrospective_selected_n", "retrospective_selected_completed",
                "retrospective_selected_sparse", "retrospective_selected_matrix_cap"),
  value = c(sum(selected), mean(broad$completed[selected]),
            mean((broad$island_p < 2 | broad$island_a < 2)[selected]),
            mean(broad$stop_reason[selected] == "max_matrix_cells")),
  source_field = c("combined parameter predicate", "diagnostics$completed",
                   "community_stats$island_p/island_a", "diagnostics$stop_reason"),
  stringsAsFactors = FALSE
))

bin_specs <- list(
  lac_0 = c(-Inf, 0.04, 0.08, 0.15, 0.30, 0.50, 0.80, Inf),
  mu_0 = c(-Inf, 0.03, 0.05, 0.10, 0.20, 0.40, 0.60, Inf),
  gam_0 = c(-Inf, 0.01, 0.02, 0.04, 0.08, 0.15, 0.20, Inf),
  laa_0 = c(-Inf, 0.10, 0.80, 1.20, Inf),
  K_0 = c(-Inf, 40, 60, 80, 100, 120, Inf),
  K_1 = c(-Inf, 0.5, 5, 20, 40, Inf),
  lambda0 = c(-Inf, 0.30, 0.60, 0.90, 1.20, Inf)
)

# categorize parameters into each bins, then do statistic with each bin.
bin_rows <- lapply(names(bin_specs), function(parameter) {
  breaks <- bin_specs[[parameter]]
  values <- broad[[parameter]]
  do.call(rbind, lapply(seq_len(length(breaks) - 1L), function(j) {
    keep <- values >= breaks[j] &
      (values < breaks[j + 1L] | (j == length(breaks) - 1L &
        values <= breaks[j + 1L]))
    data.frame(
      parameter = parameter,
      lower = breaks[j],
      upper = breaks[j + 1L],
      n = sum(keep, na.rm = TRUE),
      completion_fraction = mean(broad$completed[keep], na.rm = TRUE),
      sparse_fraction = mean(
        (broad$island_p[keep] < 2 | broad$island_a[keep] < 2),
        na.rm = TRUE
      ),
      matrix_cap_fraction = mean(
        broad$stop_reason[keep] == "max_matrix_cells", na.rm = TRUE
      ),
      median_island_p = median(broad$island_p[keep], na.rm = TRUE),
      median_island_a = median(broad$island_a[keep], na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
})
bin_summary <- do.call(rbind, bin_rows)

dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
# all simulation summaries. 1000 obs
write.csv(data, file.path(run_dir, "pilot_full_evidence_by_run.csv"),
          row.names = FALSE, na = "")

# check out `summary` structure for details
write.csv(summary, file.path(run_dir, "pilot_full_evidence_summary.csv"),
          row.names = FALSE, na = "")
write.csv(bin_summary, file.path(run_dir, "pilot_full_evidence_parameter_bins.csv"),
          row.names = FALSE, na = "")
report <- c(
  "# Reproducible pilot evidence", "",
  paste0("Source directory: `", normalizePath(result_dir), "`"),
  paste0("RDS files read: ", nrow(data), "; broad rows: ", nrow(broad), "."),
  "",
  paste0("Broad matrix-cap rows: `diagnostics$stop_reason == ",
         "\"max_matrix_cells\"`: ",
         sum(broad$stop_reason == "max_matrix_cells"), " / ", nrow(broad), "."),
  paste0("Broad sparse rows: `community_stats$island_p < 2 | ",
         "community_stats$island_a < 2`: ",
         sum(broad$island_p < 2 | broad$island_a < 2, na.rm = TRUE),
         " / ", nrow(broad), "."),
  paste0("Broad exact-zero mismatch rows: `community_stats$",
         "final_D_nonzero_fraction == 0`: ",
         sum(broad$final_D_nonzero_fraction == 0, na.rm = TRUE),
         " / ", sum(!is.na(broad$final_D_nonzero_fraction)),
         " evaluable broad rows."),
  "",
  "Weighted exposure bands use diagnostic weight multiplied by the per-run",
  "fraction, divided by total weight. The summary CSV records source fields."
)
writeLines(report, file.path(run_dir, "PILOT_FULL_EVIDENCE.md"))
cat(paste(report, collapse = "\n"), "\n")
