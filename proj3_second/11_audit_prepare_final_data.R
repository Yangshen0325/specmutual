###############################################################################
# Audit and prepare the completed final proj3_second simulations.
#
# This script deliberately does NOT fit the main Part I or Part II models.
# It preserves the cluster outputs and writes new audit/analysis artifacts to
# proj3_second/final_analysis_audit/. Failed/safety-stopped states are treated
# as censored simulations, never as completed final communities and never
# imputed.
#
# Run from the package root:
#   Rscript proj3_second/11_audit_prepare_final_data.R
###############################################################################

rm(list = ls())

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required for the correlation figure.")
}

source_dir <- file.path("proj3_second", "from_cluster_FINAL")
output_dir <- file.path("proj3_second", "final_analysis_audit")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

results_path <- file.path(source_dir, "final_results_combined.csv")
params_path <- file.path(source_dir, "final_param_table.csv")
ranges_path <- file.path(source_dir, "final_ranges.csv")
required <- c(results_path, params_path, ranges_path)
if (any(!file.exists(required))) {
  stop("Missing source file(s): ", paste(required[!file.exists(required)], collapse = ", "))
}

read_final_csv <- function(path) {
  read.csv(
    path, stringsAsFactors = FALSE, check.names = FALSE,
    na.strings = c("", "NA")
  )
}

d <- read_final_csv(results_path)
params <- read_final_csv(params_path)
ranges <- read_final_csv(ranges_path)

parameter_names <- c(
  "lac_0", "mu_0", "gam_0", "laa_0", "K_0",
  "K_1", "mu_1", "laa_1", "lambda0"
)
id_columns <- c(
  "simulation_id", "simulation_key", "simulation_seed", "design_seed",
  "design_group", "anchor_set_id"
)
run_columns <- c(
  "success_status", "run_started", "run_finished", "runtime_seconds",
  "total_time", "warnings", "error_message", "completed", "stop_reason",
  "simulated_time"
)

richness_summaries <- c(
  "island_p", "island_a", "island_endemic_p", "island_nonendemic_p",
  "island_endemic_a", "island_nonendemic_a"
)
network_summaries <- c(
  "connectance", "disconnect_p", "disconnect_a", "largest_component",
  "n_components", "plant_degree", "animal_degree", "final_degree_median",
  "final_degree_p90", "final_degree_p99", "final_degree_max"
)
nltt_summaries <- c(
  "nonend_nltt_p", "singleton_nltt_p", "multi_nltt_p",
  "nonend_nltt_a", "singleton_nltt_a", "multi_nltt_a"
)
internal_final_exposures <- c(
  "final_D_median", "final_D_p90", "final_D_max",
  "final_D_nonzero_fraction"
)
all_community_stats <- c(
  richness_summaries, network_summaries, nltt_summaries,
  internal_final_exposures
)


# Extract summaries for analysis ------------------------------------------

# The legacy inverse-inference scripts used these 17 nonredundant empirical
# summaries. island_p/a are exact sums of endemic + nonendemic richness and
# are kept as supplemental observables rather than duplicate predictors.

# The 17 summaries needed for the chapter
primary_summaries <- c(
  "island_endemic_p", "island_nonendemic_p",
  "island_endemic_a", "island_nonendemic_a",
  "connectance", "disconnect_p", "disconnect_a", "largest_component",
  "n_components", "plant_degree", "animal_degree",
  nltt_summaries
)
# As supplementary summaries
supplemental_observables <- c(
  "island_p", "island_a", "final_degree_median", "final_degree_p90",
  "final_degree_p99", "final_degree_max"
)
# Note: no internal summaries included in `observable_summaries`
observable_summaries <- c(primary_summaries, supplemental_observables)

# Check if missed any
missing_required_columns <- setdiff(
  c(id_columns, run_columns, parameter_names, all_community_stats), names(d)
)
if (length(missing_required_columns)) {
  stop("Combined results lack columns: ", paste(missing_required_columns, collapse = ", "))
}


# Core integrity checks against the design table and per-run file  --------

stopifnot(
  nrow(params) == 1000L,
  nrow(d) == 1000L,
  identical(as.integer(d$simulation_id), as.integer(params$simulation_id)),
  !anyDuplicated(d$simulation_id),
  !anyDuplicated(d$simulation_key),
  !anyDuplicated(d$simulation_seed),
  !anyDuplicated(params[, parameter_names]),
  all(vapply(parameter_names, function(x) {
    isTRUE(all.equal(d[[x]], params[[x]], tolerance = 0))
  }, logical(1)))
)
result_files <- list.files(
  file.path(source_dir, "final_results"), pattern = "^sim_[0-9]{4}\\.rds$",
  full.names = TRUE
)
stopifnot(length(result_files) == 1000L)

expected_groups <- c(
  broad_lhs = 900L, no_mutualism = 20L, K1_only = 20L,
  mu1_only = 20L, laa1_only = 20L, lambda0_only = 20L
)
observed_groups <- table(d$design_group)
stopifnot(all(observed_groups[names(expected_groups)] == expected_groups))


# Confirm exact richness identities before treating island_p/a as  --------
stopifnot(
  all(d$island_p == d$island_endemic_p + d$island_nonendemic_p),
  all(d$island_a == d$island_endemic_a + d$island_nonendemic_a)
)


# Extract data we need ("main_lhs", "anchors", etc) --------------------------------------------------

# Distinguish all obs "continuous_lhs" and "structural_anchor"(including "K1_only", etc)
# > unique(d$design_group)
# [1] "broad_lhs"    "no_mutualism" "K1_only"      "mu1_only"     "laa1_only"    "lambda0_only"
d$data_partition <- ifelse(
  d$design_group == "broad_lhs", "continuous_lhs", "structural_anchor"
)

# `primary_eligible` broad_lhs sampling + completed simulations
d$primary_eligible <- d$design_group == "broad_lhs" &
  d$success_status == "completed"
# failed simulations
d$censored_failure <- d$success_status != "completed"
# > sum(d$censored_failure)
# [1] 12

audit_master <- d[, c(
  id_columns, "data_partition", "primary_eligible", "censored_failure",
  run_columns, parameter_names, observable_summaries
)]
# Set all failed simulations summaries NA
audit_master[audit_master$censored_failure, observable_summaries] <- NA

# 888 completed broad_lhs
main_lhs <- audit_master[
  audit_master$data_partition == "continuous_lhs" &
    audit_master$success_status == "completed",
  , drop = FALSE
]

# 100 anchors
anchors <- audit_master[
  audit_master$data_partition == "structural_anchor" &
    audit_master$success_status == "completed",
  , drop = FALSE
]

# 12 failed
failures <- d[d$success_status != "completed", c(
  id_columns, run_columns, parameter_names,
  "n_events", "max_richness_p", "max_richness_a", "max_rows_Mt",
  "max_cols_Mt", "max_matrix_cells_observed", "result_file_bytes"
), drop = FALSE]



# Pre-process data, good for later recipe in RF-------------------------------------------------------------------------

# Model-internal diagnostics are deliberately isolated from empirical
# observables to prevent accidental leakage into Part I inverse inference.
# Only island community information are important for parameters inference, these
# are generated during simulation

# These are not allowed to predict the parameters
diagnostic_columns <- c(
  grep("^(fixed_|cap_|events_|hazard_|mu1d_|laa1D_|K1d_over_)", names(d), value = TRUE),
  "n_events", "max_richness_p", "max_richness_a", "max_rows_Mt",
  "max_cols_Mt", "max_matrix_cells_observed", internal_final_exposures,
  "elapsed_s", "result_file_bytes"
)
diagnostic_columns <- unique(diagnostic_columns)
# exposure_diagnostics <- d[, c(id_columns, run_columns, parameter_names, diagnostic_columns), drop = FALSE]
# > setdiff(names(d), names(exposure_diagnostics))
# [1] "island_p"            "island_a"            "island_endemic_p"    "island_nonendemic_p" "island_endemic_a"
# [6] "island_nonendemic_a" "connectance"         "disconnect_p"        "disconnect_a"        "largest_component"
# [11] "n_components"        "plant_degree"        "animal_degree"       "nonend_nltt_p"       "singleton_nltt_p"
# [16] "multi_nltt_p"        "nonend_nltt_a"       "singleton_nltt_a"    "multi_nltt_a"        "final_degree_median"
# [21] "final_degree_p90"    "final_degree_p99"    "final_degree_max"    "data_partition"      "primary_eligible"
# [26] "censored_failure"


# Write data  -------------------------------------------------------------

# 1000 obs with essential columns (51)
write.csv(
  audit_master, file.path(output_dir, "final_analysis_audit_master.csv"),
  row.names = FALSE, na = ""
)

# completed broad_lhs row with essential columns (51)
write.csv(
  main_lhs, file.path(output_dir, "final_analysis_ready_main_lhs_completed.csv"),
  row.names = FALSE, na = ""
)

# 100 anchor rows with essential columns (51)
write.csv(
  anchors, file.path(output_dir, "final_analysis_ready_structural_anchors.csv"),
  row.names = FALSE, na = ""
)

# disgnostics table
write.csv(
  exposure_diagnostics, file.path(output_dir, "final_exposure_diagnostics_internal_only.csv"),
  row.names = FALSE, na = ""
)

# 12 obs of failed simulations with essential columns (32, see code line 162)
write.csv(
  failures, file.path(output_dir, "final_failed_censored_simulations.csv"),
  row.names = FALSE, na = ""
)


# Define how do we like to process parameter in different tasks (Part I and Part II) ----------------------------------------------
parameter_labels <- c(
  lac_0 = "intrinsic cladogenesis rate",
  mu_0 = "intrinsic extinction rate",
  gam_0 = "intrinsic immigration rate",
  laa_0 = "intrinsic anagenesis rate",
  K_0 = "intrinsic carrying capacity",
  K_1 = "mutualism-related carrying capacity",
  mu_1 = "mutualism-related extinction",
  laa_1 = "mutualism-related anagenesis",
  lambda0 = "intrinsic cospeciation rate"
)
parameter_dictionary <- ranges[match(parameter_names, ranges$parameter), ]
parameter_dictionary$description <- unname(parameter_labels[parameter_dictionary$parameter])
parameter_dictionary$sampling_type <- ifelse(
  parameter_dictionary$transformation == "log", "continuous_log_uniform_LHS",
  "continuous_linear_uniform_LHS"
)
parameter_dictionary$integer_sampled <- FALSE

# Note: RF will infer log(theta) if theta is sampled on log scale, but our goal is infer theta
# so we have to back-transformed to the original scale for biological interpretation and evaluation.

# Part I: summaries (predictor) to parameter (response)
parameter_dictionary$part_I_primary_response_scale <- ifelse(
  parameter_dictionary$transformation == "log",
  "log_parameter",
  "raw_parameter"
)

# For Part II, parameters are predictor, it's not necessary to log it is it's sampled on log scale,
# because RF works on orders (e.g., raw value 0.03 as the separate node, values < 0.03 or log(values) < log(0.03) won't
# change the result).

# Part II: parameter (predictor) to summaries (response)
parameter_dictionary$part_II_model_input_scale <- ifelse(
  parameter_dictionary$transformation == "log",
  "log_parameter",
  "raw_parameter"
)

parameter_dictionary$part_II_display_scale <- "original_parameter_scale"
parameter_dictionary$part_II_normalized_design_scale <- "unit_LHS_scale"
# unit_LHS_scale: for log-sampled params: unit theta = (log(theta) - log(L)) / (log(U) - log(L)), unit theta will in (0, 1)
# This will make unit theta = 0, the sampled smallest theta, unit theta = 0.5, the mid point of log-sampled interval,  unit theta = 1,
# the upper bound value of the log-sampled interval.
# unit_LHS_scale: for linear-sampled params: unit that = (theta - L) / (U -L)
# unit_LHS_scale reasoning: parameters differ in magnitude. IF they are both in (0, 1), we can test
# the effect across the sampled parameter range.

# This is important for ALE.
# If raw theta, biologically interpretable, but values may be clustered to low values (left side)
# If log(theta), ALE plots look better
# If unit scale, x-axis is 0-1, different params ALE could compare together. But not recommend for
# the main analysis, could be supplementary material. I decided not to do this part. Drop unit scale.


parameter_dictionary$anchor_note <- ifelse(
  parameter_dictionary$exact_zero_in_anchors,
  "Exact zero occurs only in structural anchors; do not log or pool anchors with the positive LHS regression design.",
  "Background parameter remains positive in all designs."
)

# write the data
write.csv(
  parameter_dictionary, file.path(output_dir, "final_parameter_dictionary.csv"),
  row.names = FALSE, na = ""
)


# Define how do we like to process summaries in different tasks (Part I and Part II)-------------------------

summary_definitions <- c(
  island_p = "Number of plant species present on the islands.",
  island_a = "Number of animal species present on the islands.",
  island_endemic_p = "Endemic plant richness; anagenetic plus cladogenetic endemic plants.",
  island_nonendemic_p = "Non-endemic (immigrant) plant richness.",
  island_endemic_a = "Endemic animal richness; anagenetic plus cladogenetic endemic animals.",
  island_nonendemic_a = "Non-endemic (immigrant) animal richness.",
  connectance = "Fraction of possible plant-animal links realized; defined only when both guilds contain at least two species.",
  disconnect_p = "Number of present plant species with degree zero; undefined when no plants are present.",
  disconnect_a = "Number of present animal species with degree zero; undefined when no animals are present.",
  largest_component = "Number of species in the largest connected component of the terminal bipartite network.",
  n_components = "Number of connected components in the terminal bipartite network.",
  plant_degree = "Mean terminal degree across present plant species.",
  animal_degree = "Mean terminal degree across present animal species.",
  final_degree_median = "Median degree across terminal plants and animals.",
  final_degree_p90 = "90th percentile of degree across terminal plants and animals (type-8 quantile).",
  final_degree_p99 = "99th percentile of degree across terminal plants and animals (type-8 quantile).",
  final_degree_max = "Maximum degree across terminal plants and animals.",
  nonend_nltt_p = "Normalized Lineages-Through-Time (nLTT) for non-endemic plant species.",
  singleton_nltt_p = "Normalized Lineages-Through-Time (nLTT) for anagenetic plant species.",
  multi_nltt_p = "Normalized Lineages-Through-Time (nLTT) for cladogenetic plant species.",
  nonend_nltt_a = "Normalized Lineages-Through-Time (nLTT) for non-endemic animal species.",
  singleton_nltt_a = "Normalized Lineages-Through-Time (nLTT) for anagenetic animal species.",
  multi_nltt_a = "Normalized Lineages-Through-Time (nLTT) for cladogenetic animal species.",
  final_D_median = "Median terminal interaction mismatch D for non-endemic species; requires mainland M0.",
  final_D_p90 = "90th percentile of terminal interaction mismatch D; requires mainland M0.",
  final_D_max = "Maximum terminal interaction mismatch D; requires mainland M0.",
  final_D_nonzero_fraction = "Fraction of terminal non-endemic species with nonzero interaction mismatch D; requires mainland M0."
)

# It includes e.g., temporal degree data, but it has marked required 17 summaries
summary_dictionary <- data.frame(
  variable = all_community_stats,
  group = c(
    rep("Richness", length(richness_summaries)),
    rep("Network", length(network_summaries)),
    rep("nLTT", length(nltt_summaries)),
    rep("Internal exposure", length(internal_final_exposures))
  ),
  definition = unname(summary_definitions[all_community_stats]),
  empirically_available = !all_community_stats %in% internal_final_exposures,
  primary_legacy_17 = all_community_stats %in% primary_summaries,
  role = ifelse(
    all_community_stats %in% primary_summaries, "primary_observable",
    ifelse(all_community_stats %in% supplemental_observables,
           "supplemental_observable", "internal_diagnostic")
  ),
  stringsAsFactors = FALSE
)

# How many NA in primary analysis. `primary_eligible` in completed brioad_lhs simulations
summary_dictionary$missing_completed_main_lhs <- vapply(
  summary_dictionary$variable,
  function(x) sum(is.na(d[[x]][d$primary_eligible])), integer(1)
)

# The fraction of summaries in `primary_eligible` = 0.
summary_dictionary$zero_fraction_completed_main_lhs <- vapply(
  summary_dictionary$variable,
  function(x) mean(d[[x]][d$primary_eligible] == 0, na.rm = TRUE), numeric(1)
)

# Process data, note: skewness != transform
summary_dictionary$transformation_guidance <- ifelse(
  summary_dictionary$variable == "connectance",
  "Bounded proportion: retain raw for tree models; for parametric models consider a beta/logit-family model only after handling exact boundaries and varying denominators explicitly.",
  ifelse(
    summary_dictionary$group == "Richness",
    "Count outcome: raw scale preserves species-count interpretation; log1p or a count-family model is optional if diagnostics show mean-variance scaling, not merely because of skew.",
    ifelse(
      summary_dictionary$group == "Temporal diversification/nLTT",
      "Nonnegative and often zero: consider a two-part model or log1p only if residual/variance diagnostics justify it; zeros are scientific outcomes.",
      ifelse(
        summary_dictionary$group == "Network",
        "Retain raw for tree models; for parametric models choose count/proportion-aware likelihoods or log1p only after residual checks.",
        "Diagnostic only; exclude from primary inverse inference."
      )
    )
  )
)

write.csv(
  summary_dictionary, file.path(output_dir, "final_summary_statistic_dictionary.csv"),
  row.names = FALSE, na = ""
)


# Detailed definition of diagnostic table--------------------------------------------------------

# Administrative and diagnostic column dictionary. Exposure suffixes are
# duration-by-individual weighted summaries computed over the trajectory.
describe_diagnostic <- function(name) {
  if (name == "simulation_id") return("Unique integer simulation row ID (1-1000).")
  if (name == "simulation_key") return("Zero-padded file key sim_####.")
  if (name == "simulation_seed") return("Unique RNG seed used for this simulation.")
  if (name == "design_seed") return("RNG seed used to generate the complete LHS/anchor design.")
  if (name == "design_group") return("broad_lhs or one of five structural anchor groups.")
  if (name == "anchor_set_id") return("Matched background anchor-set ID 1-20; missing for LHS rows.")
  if (name == "success_status") return("completed, safety_stopped, or error.")
  if (name == "warnings") return("Unique warning messages concatenated with ' | '; empty in this bundle.")
  if (name == "runtime_seconds") return("Top-level wall/elapsed runtime for simulation plus summarization.")
  if (name == "elapsed_s") return("Elapsed time measured inside the simulation core.")
  if (name == "stop_reason") return("Core termination reason; completed or the safety/error condition.")
  if (name == "simulated_time") return("Model time reached; target is total_time=10.")
  if (name == "n_events") return("Number of stochastic events executed.")
  if (grepl("^events_", name)) return("Observed count of the named event over the simulated trajectory.")
  if (grepl("^hazard_", name)) return("Time integral of the named total event rate; conditional expected event count along the realized trajectory.")
  if (grepl("^max_richness_", name)) return("Maximum realized guild richness over the trajectory.")
  if (name %in% c("max_rows_Mt", "max_cols_Mt")) return("Maximum interaction-matrix dimension over the trajectory.")
  if (name == "max_matrix_cells_observed") return("Product max_rows_Mt * max_cols_Mt; safety-cap diagnostic.")
  if (grepl("^fixed_", name)) return("Fixed simulation parameter, constant across design rows.")
  if (grepl("^cap_", name)) return("Configured safety cap, constant across design rows.")
  if (grepl("^(mu1d_|laa1D_|K1d_over_)", name)) {
    base <- if (grepl("^mu1d_", name)) {
      "mu_1*d (degree-dependent extinction exposure; inactive <0.1, informative 0.1-<3, strong >=3)"
    } else if (grepl("^laa1D_over_laa0_", name)) {
      "laa_1*D/laa_0 (relative anagenesis exposure; inactive <0.1, informative 0.1-<2, strong >=2)"
    } else if (grepl("^laa1D_", name)) {
      "laa_1*D (absolute anagenesis exposure; inactive <0.05, informative 0.05-<1, strong >=1)"
    } else {
      "K_1*d/K_0 (relative carrying-capacity exposure; inactive <0.1, informative 0.1-<4, strong >=4)"
    }
    suffix <- sub("^.*_(weight|mean|sd|min|max|frac_inactive|frac_informative|frac_strong)$", "\\1", name)
    suffix_note <- switch(
      suffix,
      weight = "total duration-by-individual weight",
      mean = "duration-by-individual weighted mean",
      sd = "duration-by-individual weighted SD",
      min = "trajectory minimum",
      max = "trajectory maximum",
      frac_inactive = "weighted fraction below the inactive threshold",
      frac_informative = "weighted fraction in the intermediate interval",
      frac_strong = "weighted fraction at/above the strong threshold",
      "trajectory exposure summary"
    )
    return(paste0(base, "; ", suffix_note, "."))
  }
  if (name %in% internal_final_exposures) return(unname(summary_definitions[name]))
  if (name == "result_file_bytes") return("Size of the per-simulation RDS result file.")
  "Run metadata or internal simulation diagnostic."
}

dictionary_columns <- unique(c(id_columns, run_columns, diagnostic_columns))
column_dictionary <- data.frame(
  variable = dictionary_columns,
  category = ifelse(
    dictionary_columns %in% id_columns, "identifier/design",
    ifelse(dictionary_columns %in% run_columns, "status/runtime", "internal_diagnostic")
  ),
  definition = vapply(dictionary_columns, describe_diagnostic, character(1)),
  empirically_available = FALSE,
  allowed_as_primary_part_I_predictor = FALSE,
  stringsAsFactors = FALSE
)
write.csv(
  column_dictionary, file.path(output_dir, "final_metadata_diagnostic_dictionary.csv"),
  row.names = FALSE, na = ""
)


# Check the bias between completed and failed runs-------------------------------------------

# Failure-dependence screen: descriptive two-group comparisons only, not a
# predictive model. Rank-biserial sign is completed-minus-failed ordering.

# Categorize broad_LHS simulations: completed and failed
broad <- d[d$design_group == "broad_lhs", , drop = FALSE]
broad$failed <- broad$success_status != "completed"

# These metrics are screened.
screen_variables <- c(
  parameter_names, "island_p", "island_a", "max_richness_p",
  "max_richness_a", "max_matrix_cells_observed", "simulated_time",
  "runtime_seconds"
) # simulated_time indicates how far the simulation had progressed when the interruption occured.

# `rank_biserial_completed_minus_failed`, if approaching +1, the variables are larger in completed runs.
#  If approaching -1, the variables are larger in failed runs.
# If approaching 0, no obvious difference ordering
# For example:
# variable	completed median	failed median	rank-biserial	meaning
#   K_0	            50	          110	          -0.65	   failed runs prefer having high K_0
# lambda0	          0.12	       0.11	           0.03	   no obvious bias evidence
# Failures were descriptively overrepresented in simulations with high K_0
# and large realized community size, suggesting that excluding failed runs may
# underrepresent this region of the sampled design.


failure_screen <- do.call(rbind, lapply(screen_variables, function(variable) {
  x <- broad[[variable]]
  group <- broad$failed
  test <- suppressWarnings(wilcox.test(x ~ group, exact = FALSE)) # If params are distributed differently in "failed" or "completed"
  n_completed <- sum(!group & is.finite(x))
  n_failed <- sum(group & is.finite(x))
  data.frame(
    variable = variable,
    completed_n = n_completed,
    failed_n = n_failed,
    completed_median = median(x[!group], na.rm = TRUE),
    failed_median = median(x[group], na.rm = TRUE),
    completed_min = min(x[!group], na.rm = TRUE),
    completed_max = max(x[!group], na.rm = TRUE),
    failed_min = min(x[group], na.rm = TRUE),
    failed_max = max(x[group], na.rm = TRUE),
    rank_biserial_completed_minus_failed =
      2 * as.numeric(test$statistic) / (n_completed * n_failed) - 1, # difference and the direction
    wilcoxon_p = test$p.value, # p-value between "failed" and "completed" (unadjusted)
    stringsAsFactors = FALSE
  )
}))

failure_screen$BH_FDR <- p.adjust(failure_screen$wilcoxon_p, method = "BH")# agjusted by Benjamini–Hochberg FDR P-value
write.csv(
  failure_screen, file.path(output_dir, "final_failure_dependence_screen.csv"),
  row.names = FALSE, na = ""
)




# Publish-ready Spearman correlation plot ---------------------------------------------

# Publish-ready Spearman correlation plot for the 17 primary empirical
# summaries, using completed continuous LHS rows only. Pairwise-complete
# correlations retain outcomes where another summary is structurally undefined.
cor_data <- main_lhs[, primary_summaries, drop = FALSE]
cor_matrix <- stats::cor(cor_data, method = "spearman", use = "pairwise.complete.obs")
cor_n <- outer(
  primary_summaries, primary_summaries,
  Vectorize(function(x, y) sum(stats::complete.cases(cor_data[, c(x, y)])))
)
dimnames(cor_n) <- list(primary_summaries, primary_summaries)
cor_long <- expand.grid(
  variable_x = primary_summaries,
  variable_y = primary_summaries,
  stringsAsFactors = FALSE
)
cor_long$rho <- as.vector(cor_matrix)
cor_long$n_pairwise <- as.vector(cor_n)

group_for <- function(x) {
  ifelse(
    x %in% richness_summaries, "Richness",
    ifelse(x %in% network_summaries, "Network", "nLTT")
  )
}
cor_long$group_x <- group_for(cor_long$variable_x)
cor_long$group_y <- group_for(cor_long$variable_y)
write.csv(
  cor_long, file.path(output_dir, "final_summary_spearman_correlations.csv"),
  row.names = FALSE, na = ""
)

plot_labels <- c(
  island_endemic_p = "plant endemic",
  island_nonendemic_p = "plant nonendemic",
  island_endemic_a = "animal endemic",
  island_nonendemic_a = "animal nonendemic",
  connectance = "connectance",
  disconnect_p = "plant disconnect",
  disconnect_a = "animal disconnect",
  largest_component = "largest component",
  n_components = "# of components",
  plant_degree = "plant degree",
  animal_degree = "animal degree",
  nonend_nltt_p = "plant nonend nLTT",
  singleton_nltt_p = "plant singleton nLTT",
  multi_nltt_p = "plant multi nLTT",
  nonend_nltt_a = "animal nonend nLTT",
  singleton_nltt_a = "animal singleton nLTT",
  multi_nltt_a = "animal multi nLTT"
)
ordered <- rev(primary_summaries)
cor_long$variable_x <- factor(cor_long$variable_x, levels = primary_summaries)
cor_long$variable_y <- factor(cor_long$variable_y, levels = ordered)
cor_long$label <- ifelse(abs(cor_long$rho) >= 0.5, sprintf("%.2f", cor_long$rho), "")

p <- ggplot2::ggplot(cor_long, ggplot2::aes(variable_x, variable_y, fill = rho)) +
  ggplot2::geom_tile(color = "white", linewidth = 0.35) +
  ggplot2::geom_text(ggplot2::aes(label = label), size = 2.45, color = "#202020") +
  ggplot2::scale_fill_gradient2(
    low = "#2B6CB0", mid = "#F7F7F7", high = "#B8322A",
    midpoint = 0, limits = c(-1, 1), breaks = seq(-1, 1, 0.5),
    name = "Spearman\ncorrelation"
  ) +
  ggplot2::scale_x_discrete(labels = plot_labels, expand = c(0, 0)) +
  ggplot2::scale_y_discrete(labels = plot_labels, expand = c(0, 0)) +
  ggplot2::coord_fixed() +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Correlation among final island community summaries"
    #subtitle = "Completed continuous LHS simulations; pairwise-complete Spearman correlations"
  ) +
  ggplot2::theme_minimal(base_size = 10, base_family = "Arial") +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(angle = 48, hjust = 1, vjust = 1, color = "#222222"),
    axis.text.y = ggplot2::element_text(color = "#222222"),
    plot.title = ggplot2::element_text(face = "bold", size = 13),
    plot.subtitle = ggplot2::element_text(color = "#555555"),
    legend.title = ggplot2::element_text(size = 9),
    plot.margin = ggplot2::margin(10, 12, 8, 10)
  )

ggplot2::ggsave(
  file.path(output_dir, "final_summary_correlation_plot.png"), p,
  width = 10.5, height = 9.2, units = "in", dpi = 400, bg = "white"
)
ggplot2::ggsave(
  file.path(output_dir, "final_summary_correlation_plot.pdf"), p,
  width = 10.5, height = 9.2, units = "in", device = grDevices::cairo_pdf,
  bg = "white"
)

# caption <- paste0(
#   "Figure. Pairwise Spearman correlations among the 17 primary, empirically ",
#   "observable island-community summaries for the 888 completed continuous ",
#   "maximin Latin-hypercube simulations. Correlations use pairwise-complete ",
#   "observations because network statistics are structurally undefined in ",
#   "some species-poor communities; |rho| >= 0.50 is printed in the cells. ",
#   "Structural-zero anchors, safety-stopped simulations, and model-internal ",
#   "exposure diagnostics are excluded."
# )
# writeLines(caption, file.path(output_dir, "final_summary_correlation_caption.txt"))



# Overall report ----------------------------------------------------------

# Compact report assembled from audited quantities.
status_counts <- table(d$success_status)
main_missing <- sort(colSums(is.na(main_lhs[, primary_summaries, drop = FALSE])), decreasing = TRUE)
failed_parameter_rows <- failure_screen[failure_screen$variable %in% parameter_names, ]
failed_parameter_rows <- failed_parameter_rows[order(failed_parameter_rows$BH_FDR), ]

report <- c(
  "# Final simulation data audit (Parts I and II)",
  "",
  paste0("Audit generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
  "",
  "## Decision",
  "",
  paste0(
    "The bundle has 1,000 readable, uniquely keyed results: ",
    unname(status_counts["completed"]), " completed and ",
    unname(status_counts["safety_stopped"]), " safety-stopped. There are no missing/unreadable result files, errors, or recorded warnings."
  ),
  "",
  paste0(
    "**Analysis-invalidating issue for full-range claims:** all 12 failures occur in the continuous LHS design and stop at the 4,000,000-cell matrix cap. Failures are nonrandom: high lac_0 and high K_1 are strongly enriched, and failed runs have median terminal plant/animal richness ",
    median(broad$island_p[broad$failed]), "/", median(broad$island_a[broad$failed]),
    " versus ", median(broad$island_p[!broad$failed]), "/", median(broad$island_a[!broad$failed]),
    " in completed LHS runs. Thus models using the 888 completed LHS rows estimate relationships conditional on completion, not across the complete declared design range. Rerun these IDs with a scientifically acceptable higher capacity or explicitly narrow the target domain before making full-range claims."
  ),
  "",
  paste0("Failed simulation IDs: ", paste(failures$simulation_id, collapse = ", "), "."),
  "",
  "No failed simulation is imputed. Its partial-time community state is masked in the audit master and excluded from analysis-ready files and correlations.",
  "",
  "## Design and partitions",
  "",
  "- 900 jointly varied maximin Latin-hypercube points (not a fully crossed design); 888 completed and 12 safety-stopped.",
  "- 20 matched anchor sets. Each contains one exact no-mutualism row and four single-mechanism rows (K_1-only, mu_1-only, laa_1-only, lambda0-only); all 100 completed.",
  "- Exactly one stochastic simulation was run per unique nine-parameter row. There are no genuine stochastic replicates and no averaging is performed.",
  "- The 888 completed LHS rows are the provisional primary regression data. The 100 anchors are a separate matched structural experiment. Internal exposure/event/hazard diagnostics are isolated in a leakage-protected file.",
  "",
  "## Parameters",
  "",
  "The nine parameters and final continuous-LHS ranges are in `final_parameter_dictionary.csv`. Seven were sampled log-uniformly on the LHS scale (lac_0, mu_0, gam_0, laa_0, K_1, mu_1, lambda0); K_0 and laa_1 were sampled linearly. None was integer-sampled. Exact zeros for the four mutualism parameters occur only in structural anchors.",
  "",
  "## Summary statistics and missingness",
  "",
  "The current result schema stores 27 community statistics: 6 Richness, 11 Network, 6 temporal diversification/nLTT, and 4 final mismatch-D diagnostics. The primary empirical set follows the 17-statistic legacy analysis: 4 nonredundant richness components, 7 network statistics, and 6 nLTT statistics. Total plant and animal richness and four terminal degree quantiles are retained as supplemental observables. The four final-D summaries require M0 and are internal diagnostics, not empirical predictors.",
  "",
  paste0(
    "Among the 888 completed LHS rows, ", sum(stats::complete.cases(main_lhs[, primary_summaries])),
    " are complete on all 17 primary summaries. Connectance is missing in ", main_missing["connectance"],
    " species-poor communities because the code defines it only when both guilds have at least two species. Five other plant-side network summaries are missing in ", main_missing["plant_degree"],
    " rows with zero plants. These are structural undefined values, not simulation failures. A naive complete-case inverse analysis would selectively remove low-richness communities and is not acceptable without sensitivity analysis."
  ),
  "",
  "## Transformations",
  "",
  "- Part I parameter responses: analyzing the seven log-sampled positive parameters on the natural-log (or equivalent unit-LHS) scale is justified by the multiplicative design and equal sampling coverage on that scale. K_0 and laa_1 should remain on their linear scales. Anchors containing exact zeros must not be log-transformed or pooled into this regression.",
  "- Part II parameter predictors: use the original sampling scale (log for the seven log-sampled parameters; linear for K_0 and laa_1) for distance-, smoothness-, or regularization-sensitive models. Tree models do not require this transformation.",
  "- Summary responses: skewness alone is not a scientific reason to transform. Preserve raw species-count interpretation unless residual/variance diagnostics support log1p or a count likelihood. Preserve structural zeros in nLTT outcomes; a two-part model may be more defensible than automatic logging. Connectance is bounded and may warrant a proportion-aware model, but only with explicit treatment of boundaries and its richness-dependent denominator.",
  "",
  "## Proposed validation scheme (no models fitted here)",
  "",
  "**Part I (inverse inference).** Use only completed continuous-LHS rows for provisional training. Preassign common repeated outer folds for all nine parameter targets, with all tuning/preprocessing inside an inner CV. Report held-out R², RMSE and MAE on both analysis and original scales, Spearman correlation, and calibration slope; keep the same folds across targets. Handle structurally undefined summaries with models that natively support missingness and compare against outcome/predictor-set-specific complete-case sensitivity analyses. Never use exposure, event, hazard, cap, runtime, seed, status, or failure diagnostics as predictors. Evaluate anchors only as an external, out-of-design challenge set, not mixed into training.",
  "",
  "**Part II (forward analysis).** Fit one response model per summary using the nine parameters on their sampling scales, again with common nested/repeated folds assigned before outcome-specific missingness filters. Use response-appropriate metrics and likelihoods in addition to R²/MAE. Estimate effects/interactions only from held-out-validated models. Analyze the 20 anchor sets separately with anchor_set_id kept intact: paired background-matched contrasts of each single mechanism against no mutualism, with uncertainty across sets. Do not call anchor rows stochastic replicates; their seeds differ and each parameter row was simulated once.",
  "",
  "For both Parts, final claims require either successful reruns of the 12 capped LHS points or an explicit restriction to the completion-supported parameter/richness domain. Because there is one realization per parameter row, validation measures generalization across parameter space plus stochastic realization; it cannot separately estimate irreducible simulation variance.",
  "",
  "## Output guide",
  "",
  "- `final_analysis_audit_master.csv`: all 1,000 design rows, with failed outcomes masked and eligibility flags.",
  "- `final_analysis_ready_main_lhs_completed.csv`: 888 provisional primary LHS rows; observable summaries only.",
  "- `final_analysis_ready_structural_anchors.csv`: 100 completed anchors, kept separate.",
  "- `final_exposure_diagnostics_internal_only.csv`: internal diagnostics isolated from empirical predictors.",
  "- `final_failed_censored_simulations.csv` and `final_failure_dependence_screen.csv`: failure audit.",
  "- Parameter, summary-statistic, and metadata/diagnostic dictionaries document every analysis role.",
  "- `final_summary_correlation_plot.png`/`.pdf`, correlation table, and caption provide the publish-ready summary-statistic correlation figure."
)
writeLines(report, file.path(output_dir, "FINAL_DATA_AUDIT_REPORT.md"))

cat("Final audit outputs written to:", output_dir, "\n")
cat("Rows: master=", nrow(audit_master), ", main LHS completed=", nrow(main_lhs),
    ", anchors=", nrow(anchors), ", failures=", nrow(failures), "\n", sep = "")
cat("Primary complete cases:", sum(stats::complete.cases(main_lhs[, primary_summaries])),
    "of", nrow(main_lhs), "\n")
