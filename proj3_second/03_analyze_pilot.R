###############################################################################
# Analyze the collected pilot and write a report for final range selection.
#
# Usage:
#   Rscript proj3_second/02_collect_results.R
#   Rscript proj3_second/03_analyze_pilot.R
###############################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})
source("proj3_second/utils_proj3_second.R")

base_dir <- proj3_second_base_dir()
run_dir <- file.path(base_dir, "proj3_second")
data_path <- file.path(run_dir, "pilot_results_combined.rds")
if (!file.exists(data_path)) {
  stop("Run proj3_second/02_collect_results.R first.")
}
results <- readRDS(data_path)
if (nrow(results) == 0) {
  stop("No pilot results are available yet.")
}

results$completed <- results$completed %in% TRUE
broad <- results %>% filter(design_group == "broad_lhs")
complete <- results %>% filter(completed)
complete_broad <- broad %>% filter(completed)

qtext <- function(x, probabilities = c(0.1, 0.5, 0.9)) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return("not estimable")
  }
  values <- quantile(x, probabilities, na.rm = TRUE, names = FALSE, type = 8)
  paste(format(signif(values, 4), trim = TRUE), collapse = " / ")
}

pct <- function(x) {
  if (!is.finite(x)) return("NA")
  sprintf("%.1f%%", 100 * x)
}

median_or_na <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x)) median(x) else NA_real_
}

column_or_empty <- function(data, name) {
  if (name %in% names(data)) data[[name]] else numeric(0)
}

stop_table <- sort(table(results$stop_reason, useNA = "ifany"), decreasing = TRUE)
stop_lines <- paste0(
  "  - ", names(stop_table), ": ", as.integer(stop_table)
)

cospec_total <- complete$events_cospeciation
clado_total <- complete$events_cladogenesis_plant +
  complete$events_cladogenesis_animal
cospec_ratio <- cospec_total / pmax(1, clado_total)

typical_d90 <- median_or_na(complete_broad$final_degree_p90)
high_d90 <- if (nrow(complete_broad)) {
  proj3_second_quantile(complete_broad$final_degree_p90, 0.9)
} else {
  NA_real_
}
mu_endpoint_typical <- if (is.finite(typical_d90) && typical_d90 > 0) {
  -log(0.05) / typical_d90
} else {
  NA_real_
}
mu_endpoint_high_degree <- if (is.finite(high_d90) && high_d90 > 0) {
  -log(0.05) / high_d90
} else {
  NA_real_
}

typical_D90 <- median_or_na(complete_broad$final_D_p90)
typical_laa0 <- median_or_na(complete_broad$laa_0)
laa_endpoint_typical <- if (
  is.finite(typical_D90) && typical_D90 > 0 && is.finite(typical_laa0)
) {
  typical_laa0 / typical_D90
} else {
  NA_real_
}

immigration_capacity_line <- if (
  "K1d_over_K0_immigration_frac_inactive" %in% names(broad)
) {
  paste0(
    "- The same carrying-capacity term across mainland immigration candidates: median inactive / changing / strong fractions = ",
    pct(median_or_na(column_or_empty(
      broad, "K1d_over_K0_immigration_frac_inactive"
    ))),
    " / ",
    pct(median_or_na(column_or_empty(
      broad, "K1d_over_K0_immigration_frac_informative"
    ))),
    " / ",
    pct(median_or_na(column_or_empty(
      broad, "K1d_over_K0_immigration_frac_strong"
    ))),
    "."
  )
} else {
  "- Immigration-candidate `K_1 d_i / K_0` diagnostics are not present in the pre-update local smoke files; cluster results will populate them."
}

enough_for_decision <- nrow(results) >= 900 &&
  nrow(complete_broad) >= 700
decision_label <- if (enough_for_decision) {
  "Range-selection report"
} else {
  "Preliminary report - do not select final intervals yet"
}

report <- c(
  "# Proj3 second pilot results",
  "",
  paste0("**Status:** ", decision_label),
  "",
  "## Coverage and computational behavior",
  "",
  paste0("- Result files collected: ", nrow(results), " / 1,000."),
  paste0("- Broad-design results: ", nrow(broad), " / 900."),
  paste0(
    "- Simulations reaching 10 My: ", nrow(complete), " / ", nrow(results),
    " (", pct(mean(results$completed)), ")."
  ),
  paste0(
    "- Runtime seconds (10th / median / 90th percentile): ",
    qtext(results$elapsed_s), "."
  ),
  paste0(
    "- Compact result size bytes (10th / median / 90th percentile): ",
    qtext(results$result_file_bytes), "."
  ),
  "",
  "Stop reasons:",
  stop_lines,
  "",
  "## Generated communities",
  "",
  paste0(
    "- Final plant richness (10th / median / 90th): ",
    qtext(complete$island_p), "."
  ),
  paste0(
    "- Final animal richness (10th / median / 90th): ",
    qtext(complete$island_a), "."
  ),
  paste0(
    "- Final per-simulation 90th-percentile degree (10th / median / 90th): ",
    qtext(complete_broad$final_degree_p90), "."
  ),
  "",
  "## Mutualism response terms",
  "",
  "The fractions below are time- and species-weighted across all available broad-design trajectories, including safety-stopped trajectories. The diagnostic bands are intended to label inactive, changing, and strong/saturated behavior; they are not proposed parameter intervals.",
  "",
  paste0(
    "- Extinction term `mu_1 d_i`: median inactive fraction (<0.1) = ",
    pct(median_or_na(broad$mu1d_all_frac_inactive)),
    "; median informative fraction (0.1-3) = ",
    pct(median_or_na(broad$mu1d_all_frac_informative)),
    "; median saturated fraction (>=3) = ",
    pct(median_or_na(broad$mu1d_all_frac_strong)), "."
  ),
  paste0(
    "- Anagenesis contribution `(laa_1 D_i) / laa_0`: median inactive fraction (<0.1) = ",
    pct(median_or_na(broad$laa1D_over_laa0_all_frac_inactive)),
    "; median informative fraction (0.1-2) = ",
    pct(median_or_na(broad$laa1D_over_laa0_all_frac_informative)),
    "; median strong fraction (>=2) = ",
    pct(median_or_na(broad$laa1D_over_laa0_all_frac_strong)), "."
  ),
  paste0(
    "- Carrying-capacity term `K_1 d_i / K_0`: median inactive fraction (<0.1) = ",
    pct(median_or_na(broad$K1d_over_K0_all_frac_inactive)),
    "; median changing fraction (0.1-4) = ",
    pct(median_or_na(broad$K1d_over_K0_all_frac_informative)),
    "; median strong fraction (>=4) = ",
    pct(median_or_na(broad$K1d_over_K0_all_frac_strong)), "."
  ),
  immigration_capacity_line,
  paste0(
    "- Simulations with zero cospeciation events: ",
    pct(mean(cospec_total == 0, na.rm = TRUE)), "."
  ),
  paste0(
    "- Cospeciation / ordinary cladogenesis ratio (10th / median / 90th): ",
    qtext(cospec_ratio), "."
  ),
  "",
  "## Model-based landmarks for discussion",
  "",
  paste0(
    "- Median final `d_90` among completed broad runs: ",
    format(signif(typical_d90, 4), trim = TRUE),
    ". The `mu_1` value giving a 95% extinction reduction at that degree is ",
    format(signif(mu_endpoint_typical, 4), trim = TRUE), "."
  ),
  paste0(
    "- 90th percentile of run-specific final `d_90`: ",
    format(signif(high_d90, 4), trim = TRUE),
    ". The corresponding 95%-reduction `mu_1` landmark is ",
    format(signif(mu_endpoint_high_degree, 4), trim = TRUE), "."
  ),
  paste0(
    "- Median final `D_90`: ",
    format(signif(typical_D90, 4), trim = TRUE),
    ". A coefficient making `laa_1 D_90` equal the median `laa_0` is ",
    format(signif(laa_endpoint_typical, 4), trim = TRUE), "."
  ),
  "",
  "These landmarks must be interpreted together with the inactive and saturated fractions, event counts, failed/pathological runs, and the one-mechanism anchor groups. They are not automatic final bounds.",
  "",
  "## Decision rule",
  "",
  if (enough_for_decision) {
    "The pilot has enough coverage for interval discussion. Prefer ranges in which most species-time exposure is in the middle band, while keeping small inactive and strong tails. Treat a persistently zero `D_i` as a model/process limitation rather than evidence that `laa_1` merely needs a wider interval."
  } else {
    "Wait for the missing cluster results before choosing final intervals. Current values are useful only for validating the diagnostics and detecting severe pathologies."
  }
)

writeLines(report, file.path(run_dir, "PILOT_RESULTS_REPORT.md"))

figure_dir <- file.path(run_dir, "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

process_plot_data <- bind_rows(
  data.frame(
    parameter = broad$mu_1,
    response = broad$mu1d_all_mean,
    status = ifelse(broad$completed, "completed", "safety-stopped"),
    panel = "Extinction: time-weighted mean mu1*d"
  ),
  data.frame(
    parameter = broad$laa_1,
    response = broad$laa1D_over_laa0_all_mean,
    status = ifelse(broad$completed, "completed", "safety-stopped"),
    panel = "Anagenesis: mean (laa1*D)/laa0"
  ),
  data.frame(
    parameter = broad$K_1,
    response = broad$K1d_over_K0_all_mean,
    status = ifelse(broad$completed, "completed", "safety-stopped"),
    panel = "Capacity: mean K1*d/K0"
  ),
  data.frame(
    parameter = broad$lambda0,
    response = broad$events_cospeciation /
      pmax(
        1,
        broad$events_cladogenesis_plant +
          broad$events_cladogenesis_animal
      ),
    status = ifelse(broad$completed, "completed", "safety-stopped"),
    panel = "Cospeciation / cladogenesis events"
  )
)

plot_process <- ggplot(
  process_plot_data,
  aes(x = parameter, y = response, colour = status)
) +
  geom_point(alpha = 0.55, size = 1.4) +
  scale_colour_manual(values = c(
    completed = "#22577A",
    `safety-stopped` = "#C44536"
  )) +
  facet_wrap(~panel, scales = "free", ncol = 2) +
  labs(x = "Sampled parameter", y = "Observed process-level response") +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold"))
if (nrow(broad) >= 20) {
  plot_process <- plot_process +
    geom_smooth(
      method = "loess",
      formula = y ~ x,
      se = TRUE,
      colour = "#333333",
      inherit.aes = FALSE,
      aes(x = parameter, y = response)
    )
}

ggsave(
  file.path(figure_dir, "pilot_process_response.png"),
  plot_process,
  width = 10,
  height = 7,
  units = "in",
  dpi = 180
)

cat("Wrote proj3_second/PILOT_RESULTS_REPORT.md\n")
cat("Wrote proj3_second/figures/pilot_process_response.png\n")
