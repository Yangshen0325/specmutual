###############################################################################
# Part I: recover model parameters from island community summaries.
#
# Research question:
# To what extent do observable Richness, Network, and nLTT summaries retain
# sufficient information to recover each underlying model parameter?
#
# Design:
#   * completed continuous LHS simulations only; (888)
#   * one random forest regression for each parameter and predictor set;
#   * 3 repeats x 5 outer folds for performance estimation;
#   * out-of-bag hyperparameter tuning inside each outer training set;
#   * identical outer folds across all competing models;
#   * out-of-fold predictions only for performance and diagnostic figures;
#   * exposure diagnostics excluded from all primary predictors.
#
# Run from the package root:
#   Rscript proj3_second/12_part1_inverse_inference.R
#
# Optional environment variables:
#   PROJ3_PART1_CORES, PROJ3_PART1_TREES, PROJ3_PART1_PERMUTATIONS
###############################################################################

rm(list = ls())

required_packages <- c("ranger")
missing_packages <- required_packages[!vapply(
  required_packages, requireNamespace, logical(1), quietly = TRUE
)]
if (length(missing_packages)) {
  stop("Missing required package(s): ", paste(missing_packages, collapse = ", "))
}


# Settings ----------------------------------------------------------------

input_dir <- file.path("proj3_second", "final_analysis_audit")
output_dir <- file.path("proj3_second", "part1_inverse_inference")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

data_path <- file.path(input_dir, "final_analysis_ready_main_lhs_completed.csv")
parameter_dictionary_path <- file.path(input_dir, "final_parameter_dictionary.csv")
diagnostics_path <- file.path(input_dir, "final_exposure_diagnostics_internal_only.csv")

needed_files <- c(data_path, parameter_dictionary_path, diagnostics_path)
if (any(!file.exists(needed_files))) {
  stop("Missing input file(s): ", paste(needed_files[!file.exists(needed_files)], collapse = ", "))
}

outer_seed <- 43425L
grid_seed <- 20260812L
forest_seed <- 62217L
permutation_seed <- 91831L

n_outer_folds <- 5L
n_outer_repeats <- 3L
n_trees <- as.integer(Sys.getenv("PROJ3_PART1_TREES", unset = "500"))
n_permutations <- as.integer(Sys.getenv("PROJ3_PART1_PERMUTATIONS", unset = "5"))
n_cores <- as.integer(Sys.getenv("PROJ3_PART1_CORES", unset = "2")) # set 2 cores so that R can fit up 2 parameters
# simultaneously.

if (!is.finite(n_trees) || n_trees < 100L) stop("PROJ3_PART1_TREES must be >= 100.")
if (!is.finite(n_permutations) || n_permutations < 1L) stop("PROJ3_PART1_PERMUTATIONS must be >= 1.")
if (!is.finite(n_cores) || n_cores < 1L) stop("PROJ3_PART1_CORES must be >= 1.")

parameter_names <- c(
  "lac_0", "mu_0", "gam_0", "laa_0", "K_0",
  "K_1", "mu_1", "laa_1", "lambda0"
)

richness_summaries <- c(
  "island_endemic_p", "island_nonendemic_p",
  "island_endemic_a", "island_nonendemic_a"
)
network_summaries <- c(
  "connectance", "disconnect_p", "disconnect_a", "largest_component",
  "n_components", "plant_degree", "animal_degree"
)
nltt_summaries <- c(
  "nonend_nltt_p", "singleton_nltt_p", "multi_nltt_p",
  "nonend_nltt_a", "singleton_nltt_a", "multi_nltt_a"
)
all_summaries <- c(richness_summaries, network_summaries, nltt_summaries)

# The first four predictor sets quantify absolute information content. The
# three omission sets quantify the unique predictive loss from removing one
# complete summary group while retaining the other two groups.
predictor_sets <- list(
  All = all_summaries,
  Richness = richness_summaries,
  Network = network_summaries,
  nLTT = nltt_summaries,
  All_minus_Richness = setdiff(all_summaries, richness_summaries),
  All_minus_Network = setdiff(all_summaries, network_summaries),
  All_minus_nLTT = setdiff(all_summaries, nltt_summaries)
)

parameter_groups <- c(
  lac_0 = "Intrinsic", mu_0 = "Intrinsic", gam_0 = "Intrinsic",
  laa_0 = "Intrinsic", K_0 = "Intrinsic", lambda0 = "Mutualism-related",
  K_1 = "Mutualism-related", mu_1 = "Mutualism-related",
  laa_1 = "Mutualism-related"
)


# Read and check data -----------------------------------------------------

read_analysis_csv <- function(path) {
  read.csv(
    path, stringsAsFactors = FALSE, check.names = FALSE,
    na.strings = c("", "NA")
  )
}

analysis_df <- read_analysis_csv(data_path)
parameter_dictionary <- read_analysis_csv(parameter_dictionary_path)

missing_columns <- setdiff(
  c("simulation_id", parameter_names, all_summaries), names(analysis_df)
)
if (length(missing_columns)) {
  stop("Analysis data lack columns: ", paste(missing_columns, collapse = ", "))
}
if (nrow(analysis_df) != 888L || anyDuplicated(analysis_df$simulation_id)) {
  stop("Expected 888 unique completed continuous-LHS simulations.")
}
if (anyNA(analysis_df[, parameter_names, drop = FALSE])) {
  stop("Parameter responses must not contain missing values.")
}

# This explicit leakage check ensures no internal exposure, trajectory, event,
# hazard, runtime, status, seed, or safety-cap column enters a primary model.
forbidden_patterns <- c(
  "^final_D_", "^mu1d_", "^laa1D_", "^K1d_over_", "^events_",
  "^hazard_", "^cap_", "runtime", "elapsed", "seed", "status",
  "stop_reason", "n_events", "max_richness", "matrix_cells"
)
forbidden_predictors <- unique(unlist(lapply(
  forbidden_patterns,
  function(pattern) grep(pattern, all_summaries, value = TRUE, ignore.case = TRUE)
)))
if (length(forbidden_predictors)) {
  stop("Forbidden internal predictor(s): ", paste(forbidden_predictors, collapse = ", "))
}

parameter_scale <- setNames(
  parameter_dictionary$transformation[match(parameter_names, parameter_dictionary$parameter)],
  parameter_names
)
if (anyNA(parameter_scale) || !all(parameter_scale %in% c("log", "linear"))) {
  stop("Parameter dictionary does not uniquely identify log/linear sampling scales.")
}

# Model responses on their sampling/design scale. Original values are retained
# beside transformed values for biological interpretation and back-transformed
# performance assessment.
for (parameter in parameter_names) {
  model_name <- paste0(parameter, "__design")
  analysis_df[[model_name]] <- if (parameter_scale[[parameter]] == "log") {
    log(analysis_df[[parameter]])
  } else {
    analysis_df[[parameter]]
  }
}


# Shared repeated nested resamples ---------------------------------------

# Fold membership is generated from simulation IDs only, not from any target.
# This permits identical folds for all nine responses and predictor sets.
make_balanced_folds <- function(ids, v, seed) {
  set.seed(seed)
  shuffled <- sample(ids, length(ids), replace = FALSE)
  assignments <- rep(seq_len(v), length.out = length(ids))
  out <- integer(length(ids))
  names(out) <- as.character(ids)
  out[as.character(shuffled)] <- assignments
  unname(out[as.character(ids)])
}

fold_rows <- list()

for (repeat_id in seq_len(n_outer_repeats)) {
  outer_fold <- make_balanced_folds(
    analysis_df$simulation_id, n_outer_folds,
    outer_seed + repeat_id * 1009L
  )
  for (outer_fold_id in seq_len(n_outer_folds)) {
    assessment <- outer_fold == outer_fold_id
    split_id <- sprintf("Repeat%02d_Fold%02d", repeat_id, outer_fold_id)

    fold_rows[[split_id]] <- data.frame(
      simulation_id = analysis_df$simulation_id,
      repeat_id = repeat_id,
      outer_fold = outer_fold_id,
      split_id = split_id,
      role = ifelse(assessment, "assessment", "analysis"),
      stringsAsFactors = FALSE
    )

  }
}

outer_fold_assignments <- do.call(rbind, fold_rows)
row.names(outer_fold_assignments) <- NULL
write.csv(
  outer_fold_assignments,
  file.path(output_dir, "part1_outer_fold_assignments.csv"),
  row.names = FALSE
)

# Random forest helpers --------------------------------------------------

safe_spearman <- function(observed, predicted) {
  keep <- is.finite(observed) & is.finite(predicted)
  if (sum(keep) < 3L || length(unique(predicted[keep])) < 2L) return(NA_real_)
  suppressWarnings(stats::cor(observed[keep], predicted[keep], method = "spearman"))
}

rsq_traditional <- function(observed, predicted) {
  keep <- is.finite(observed) & is.finite(predicted)
  observed <- observed[keep]
  predicted <- predicted[keep]
  denominator <- sum((observed - mean(observed))^2)
  if (!length(observed) || denominator <= 0) return(NA_real_)
  1 - sum((observed - predicted)^2) / denominator
}

metric_values <- function(observed, predicted) {
  keep <- is.finite(observed) & is.finite(predicted)
  observed <- observed[keep]
  predicted <- predicted[keep]
  c(
    rsq = rsq_traditional(observed, predicted),
    rmse = sqrt(mean((predicted - observed)^2)),
    mae = mean(abs(predicted - observed)),
    spearman = safe_spearman(observed, predicted)
  )
}

make_grid <- function(n_predictors, grid_size = 6L) {
  set.seed(grid_seed + n_predictors * 101L)
  mtry_values <- unique(pmax(
    1L,
    pmin(n_predictors, round(seq(1, n_predictors, length.out = min(4L, n_predictors)))) # maximum 4 values for `mtry`
  ))
  min_node_values <- c(3L, 7L, 15L, 25L)
  grid <- expand.grid(
    mtry = mtry_values,
    min.node.size = min_node_values,
    stringsAsFactors = FALSE
  )
  if (nrow(grid) <= grid_size) return(grid)
  selected <- unique(round(seq(1, nrow(grid), length.out = grid_size))) # keep 6 rows of combo at max
  grid[selected, , drop = FALSE]
}

fit_forest <- function(train_data, response, predictors, mtry, min_node_size, seed) {
  model_data <- train_data[, c(response, predictors), drop = FALSE] # No data leakage
  names(model_data)[1] <- ".outcome"
  ranger::ranger(
    dependent.variable.name = ".outcome",
    data = model_data,
    num.trees = n_trees,
    mtry = as.integer(mtry),
    min.node.size = as.integer(min_node_size),
    splitrule = "variance",
    replace = TRUE,
    sample.fraction = 0.8,
    na.action = "na.learn", # the tree will learn how to process NA
    seed = as.integer(seed),
    num.threads = 1L,
    importance = "none", # don'r calculate variable importance
    write.forest = TRUE
  )
}

predict_forest <- function(model, new_data, predictors) {
  as.numeric(predict(model, data = new_data[, predictors, drop = FALSE])$predictions)
}

tune_forest <- function(train_data, response, predictors, task_seed) {
  grid <- make_grid(length(predictors))
  grid$oob_rmse <- NA_real_

  for (grid_id in seq_len(nrow(grid))) {
    # ranger's OOB predictions use only bootstrap-excluded observations from
    # the current outer analysis set. The outer assessment rows are untouched.
    model <- fit_forest(
      train_data, response, predictors,
      grid$mtry[grid_id], grid$min.node.size[grid_id],
      task_seed + grid_id * 1009L
    )
    grid$oob_rmse[grid_id] <- sqrt(model$prediction.error)
  }
  grid <- grid[order(grid$oob_rmse, grid$mtry, grid$min.node.size), , drop = FALSE]
  list(best = grid[1, , drop = FALSE], tuning = grid)
}

back_transform <- function(value, parameter) {
  if (parameter_scale[[parameter]] == "log") exp(value) else value
}


# Fit one parameter across all predictor sets ----------------------------

fit_parameter <- function(parameter_index) {
  parameter <- parameter_names[parameter_index]
  response <- paste0(parameter, "__design")
  prediction_rows <- list()
  tuning_rows <- list()
  importance_rows <- list()
  task_counter <- 0L

  message("Starting parameter: ", parameter)

  for (repeat_id in seq_len(n_outer_repeats)) {
    for (outer_fold_id in seq_len(n_outer_folds)) {
      split_id <- sprintf("Repeat%02d_Fold%02d", repeat_id, outer_fold_id)
      membership <- outer_fold_assignments[
        outer_fold_assignments$split_id == split_id, , drop = FALSE
      ]
      train_ids <- membership$simulation_id[membership$role == "analysis"]
      test_ids <- membership$simulation_id[membership$role == "assessment"]
      train_data <- analysis_df[match(train_ids, analysis_df$simulation_id), , drop = FALSE]
      test_data <- analysis_df[match(test_ids, analysis_df$simulation_id), , drop = FALSE]
      observed_design <- test_data[[response]]
      observed_original <- test_data[[parameter]]
      train_mean_design <- mean(train_data[[response]])
      train_median_design <- stats::median(train_data[[response]])

      # Null predictions use only the outer training set.
      for (null_name in c("Null_mean", "Null_median")) {
        null_value <- if (null_name == "Null_mean") train_mean_design else train_median_design
        prediction_rows[[paste(parameter, split_id, null_name, sep = "_")]] <- data.frame(
          parameter = parameter,
          parameter_group = unname(parameter_groups[parameter]),
          response_scale = unname(parameter_scale[parameter]),
          predictor_set = null_name,
          repeat_id = repeat_id,
          outer_fold = outer_fold_id,
          split_id = split_id,
          simulation_id = test_data$simulation_id,
          observed_design = observed_design,
          predicted_design = null_value,
          observed_original = observed_original,
          predicted_original = back_transform(null_value, parameter),
          stringsAsFactors = FALSE
        )
      }

      for (predictor_set in names(predictor_sets)) {
        task_counter <- task_counter + 1L
        predictors <- predictor_sets[[predictor_set]]
        task_seed <- forest_seed + parameter_index * 1000000L +
          repeat_id * 10000L + outer_fold_id * 100L + task_counter

        tuned <- tune_forest(train_data, response, predictors, task_seed)
        best <- tuned$best # pick the best performance mtry and min_node
        model <- fit_forest(# then fit the specific(mtry = ~ and min_node = ~) model
          train_data, response, predictors,
          best$mtry, best$min.node.size, task_seed + 900001L
        )
        predicted_design <- predict_forest(model, test_data, predictors)

        prediction_rows[[paste(parameter, split_id, predictor_set, sep = "_")]] <- data.frame(
          parameter = parameter,
          parameter_group = unname(parameter_groups[parameter]),
          response_scale = unname(parameter_scale[parameter]),
          predictor_set = predictor_set,
          repeat_id = repeat_id,
          outer_fold = outer_fold_id,
          split_id = split_id,
          simulation_id = test_data$simulation_id,
          observed_design = observed_design,
          predicted_design = predicted_design,
          observed_original = observed_original,
          predicted_original = back_transform(predicted_design, parameter),
          stringsAsFactors = FALSE
        )

        tuning_table <- tuned$tuning
        tuning_table$parameter <- parameter
        tuning_table$predictor_set <- predictor_set
        tuning_table$repeat_id <- repeat_id
        tuning_table$outer_fold <- outer_fold_id
        tuning_table$split_id <- split_id
        tuning_table$selected <- seq_len(nrow(tuning_table)) == 1L
        tuning_rows[[paste(parameter, split_id, predictor_set, sep = "_")]] <- tuning_table

        # Full-model importance is calculated only on held-out outer-fold rows.
        # The score is the increase in held-out RMSE after independently
        # permuting one summary. It is never calculated from training fit.
        if (predictor_set == "All") {
          baseline_rmse <- metric_values(observed_design, predicted_design)["rmse"]
          for (summary_name in predictors) {
            permuted_rmse <- numeric(n_permutations)

            # For test data, we permute 5 times, and each time, we compute the rmse
            for (permutation_id in seq_len(n_permutations)) {
              set.seed(
                permutation_seed + parameter_index * 1000000L +
                  repeat_id * 10000L + outer_fold_id * 100L +
                  match(summary_name, predictors) * 10L + permutation_id
              )
              permuted_data <- test_data
              permuted_data[[summary_name]] <- sample(
                permuted_data[[summary_name]], nrow(permuted_data), replace = FALSE
              )
              permuted_prediction <- predict_forest(model, permuted_data, predictors)
              permuted_rmse[permutation_id] <- metric_values(
                observed_design, permuted_prediction
              )["rmse"]
            }

            importance_rows[[paste(parameter, split_id, summary_name, sep = "_")]] <- data.frame(
              parameter = parameter,
              parameter_group = unname(parameter_groups[parameter]),
              repeat_id = repeat_id,
              outer_fold = outer_fold_id,
              split_id = split_id,
              summary = summary_name,
              baseline_rmse = baseline_rmse,
              permuted_rmse_mean = mean(permuted_rmse),
              importance_delta_rmse = mean(permuted_rmse) - baseline_rmse,
              importance_relative_rmse = ifelse(
                baseline_rmse > 0, mean(permuted_rmse) / baseline_rmse - 1, NA_real_
              ),
              permutation_sd = stats::sd(permuted_rmse),
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
  }

  message("Completed parameter: ", parameter)
  list(
    predictions = do.call(rbind, prediction_rows),
    tuning = do.call(rbind, tuning_rows),
    importance = do.call(rbind, importance_rows)
  )
}


# Run the nine independent parameter analyses ----------------------------

# It works on macOS or Linus (Unix-style). 9 parameters are distributed on 2 cores to fit
if (.Platform$OS.type == "unix" && n_cores > 1L) {
  parameter_results <- parallel::mclapply(
    seq_along(parameter_names), fit_parameter,
    mc.cores = min(n_cores, length(parameter_names)),
    mc.preschedule = TRUE # R divides the parameter jobs among the chosen cores in advance
  )
} else {
  parameter_results <- lapply(seq_along(parameter_names), fit_parameter)
}
names(parameter_results) <- parameter_names

oof_predictions <- do.call(rbind, lapply(parameter_results, `[[`, "predictions"))
tuning_results <- do.call(rbind, lapply(parameter_results, `[[`, "tuning"))
permutation_by_resample <- do.call(rbind, lapply(parameter_results, `[[`, "importance"))
row.names(oof_predictions) <- NULL
row.names(tuning_results) <- NULL
row.names(permutation_by_resample) <- NULL


# Performance on design and original scales -----------------------------

parameter_bounds <- parameter_dictionary[match(parameter_names, parameter_dictionary$parameter), ]
parameter_bounds$design_lower <- ifelse(
  parameter_bounds$transformation == "log",
  log(parameter_bounds$final_lower), parameter_bounds$final_lower
)
parameter_bounds$design_upper <- ifelse(
  parameter_bounds$transformation == "log",
  log(parameter_bounds$final_upper), parameter_bounds$final_upper
)

summarize_one_performance <- function(x, scale_name) {
  observed_name <- paste0("observed_", scale_name)
  predicted_name <- paste0("predicted_", scale_name)
  observed <- x[[observed_name]]
  predicted <- x[[predicted_name]]
  basic <- metric_values(observed, predicted)
  calibration <- stats::coef(stats::lm(predicted ~ observed))
  c(
    basic,
    calibration_intercept = unname(calibration[1]),
    calibration_slope = unname(calibration[2]),
    bias = mean(predicted - observed),
    observed_range = diff(range(observed)),
    observed_iqr = stats::IQR(observed)
  )
}

# Split it into param(9) * predictor_set(9) * repeat_id(3). e.g.,
# lac_0.All.1, lac_0.All.2, lac_0.All.3, etc.
split_groups <- split(
  oof_predictions,
  interaction(
    oof_predictions$parameter, oof_predictions$predictor_set,
    oof_predictions$repeat_id, drop = TRUE
  )
)

performance_by_repeat <- do.call(rbind, lapply(split_groups, function(x) {
  design_metrics <- summarize_one_performance(x, "design")
  original_metrics <- summarize_one_performance(x, "original")
  rbind(
    data.frame(
      parameter = x$parameter[1], parameter_group = x$parameter_group[1],
      predictor_set = x$predictor_set[1], repeat_id = x$repeat_id[1],
      evaluation_scale = "design", as.list(design_metrics),
      stringsAsFactors = FALSE
    ),
    data.frame(
      parameter = x$parameter[1], parameter_group = x$parameter_group[1],
      predictor_set = x$predictor_set[1], repeat_id = x$repeat_id[1],
      evaluation_scale = "original", as.list(original_metrics),
      stringsAsFactors = FALSE
    )
  )
}))
row.names(performance_by_repeat) <- NULL
performance_by_repeat$nrmse_range <- performance_by_repeat$rmse /
  performance_by_repeat$observed_range
performance_by_repeat$nrmse_iqr <- performance_by_repeat$rmse /
  performance_by_repeat$observed_iqr
performance_by_repeat$nmae_range <- performance_by_repeat$mae /
  performance_by_repeat$observed_range
performance_by_repeat$nmae_iqr <- performance_by_repeat$mae /
  performance_by_repeat$observed_iqr

metric_names <- c(
  "rsq", "rmse", "mae", "nrmse_range", "nrmse_iqr",
  "nmae_range", "nmae_iqr", "spearman", "calibration_intercept",
  "calibration_slope", "bias"
)

performance_groups <- split(
  performance_by_repeat,
  interaction(
    performance_by_repeat$parameter, performance_by_repeat$predictor_set,
    performance_by_repeat$evaluation_scale, drop = TRUE
  )
)

performance_summary <- do.call(rbind, lapply(performance_groups, function(x) {
  out <- data.frame(
    parameter = x$parameter[1], parameter_group = x$parameter_group[1],
    predictor_set = x$predictor_set[1], evaluation_scale = x$evaluation_scale[1],
    n_simulations = nrow(analysis_df), n_repeats = n_outer_repeats,
    stringsAsFactors = FALSE
  )
  for (metric in metric_names) {
    out[[paste0(metric, "_mean")]] <- mean(x[[metric]], na.rm = TRUE)
    out[[paste0(metric, "_sd")]] <- stats::sd(x[[metric]], na.rm = TRUE)
  }
  out
}))
row.names(performance_summary) <- NULL


# Group ablation and summary-group recoverability ------------------------

# This is to check the contribution from a certain group. For example, if remove
# "Richness", how much the model gets improved or worse.
full_repeat <- performance_by_repeat[
  performance_by_repeat$predictor_set == "All" &
    performance_by_repeat$evaluation_scale == "design", , drop = FALSE
]

ablation_rows <- list()
for (summary_group in c("Richness", "Network", "nLTT")) {
  omitted_name <- paste0("All_minus_", summary_group)
  omitted <- performance_by_repeat[
    performance_by_repeat$predictor_set == omitted_name &
      performance_by_repeat$evaluation_scale == "design", , drop = FALSE
  ]
  key_full <- paste(full_repeat$parameter, full_repeat$repeat_id)
  key_omit <- paste(omitted$parameter, omitted$repeat_id)
  omitted <- omitted[match(key_full, key_omit), , drop = FALSE] # 27 obs (9 params * 3 repeats)
  ablation_rows[[summary_group]] <- data.frame(
    parameter = full_repeat$parameter,
    parameter_group = full_repeat$parameter_group,
    summary_group = summary_group,
    repeat_id = full_repeat$repeat_id,
    full_rsq = full_repeat$rsq,
    omitted_rsq = omitted$rsq,
    rsq_loss_when_omitted = full_repeat$rsq - omitted$rsq,
    full_rmse = full_repeat$rmse,
    omitted_rmse = omitted$rmse,
    relative_rmse_increase_when_omitted = omitted$rmse / full_repeat$rmse - 1,
    stringsAsFactors = FALSE
  )
}
group_ablation_by_repeat <- do.call(rbind, ablation_rows)
row.names(group_ablation_by_repeat) <- NULL

ablation_groups <- split(
  group_ablation_by_repeat,
  interaction(# should be 9 params * 3 groups = 81 obs. e.g, lac_0.Network, lac_0.Richness
    group_ablation_by_repeat$parameter,
    group_ablation_by_repeat$summary_group, drop = TRUE
  )
)

# The summary across 3 repeats
group_ablation_summary <- do.call(rbind, lapply(ablation_groups, function(x) {
  data.frame(
    parameter = x$parameter[1],
    parameter_group = x$parameter_group[1],
    summary_group = x$summary_group[1],
    rsq_loss_mean = mean(x$rsq_loss_when_omitted),
    rsq_loss_sd = stats::sd(x$rsq_loss_when_omitted),
    relative_rmse_increase_mean = mean(x$relative_rmse_increase_when_omitted),
    relative_rmse_increase_sd = stats::sd(x$relative_rmse_increase_when_omitted),
    stringsAsFactors = FALSE
  )
}))
row.names(group_ablation_summary) <- NULL

group_only_performance <- performance_summary[
  performance_summary$evaluation_scale == "design" &
    performance_summary$predictor_set %in% c("Richness", "Network", "nLTT"),
  , drop = FALSE
]


# Held-out permutation-importance stability ------------------------------

# summary_group and rank_within_resample has been processed before.
summary_group_lookup <- c(
  setNames(rep("Richness", length(richness_summaries)), richness_summaries),
  setNames(rep("Network", length(network_summaries)), network_summaries),
  setNames(rep("nLTT", length(nltt_summaries)), nltt_summaries)
)
permutation_by_resample$summary_group <- unname(
  summary_group_lookup[permutation_by_resample$summary]
)
permutation_by_resample$rank_within_resample <- ave(
  -permutation_by_resample$importance_delta_rmse,
  interaction(permutation_by_resample$parameter, permutation_by_resample$split_id),
  FUN = function(x) rank(x, ties.method = "average")
)

importance_groups <- split(
  permutation_by_resample,
  interaction(
    permutation_by_resample$parameter,
    permutation_by_resample$summary, drop = TRUE
  )
)

permutation_importance_stability <- do.call(rbind, lapply(importance_groups, function(x) {
  data.frame(
    parameter = x$parameter[1],
    parameter_group = x$parameter_group[1],
    summary = x$summary[1],
    summary_group = x$summary_group[1],
    n_resamples = nrow(x),
    importance_delta_rmse_mean = mean(x$importance_delta_rmse),
    importance_delta_rmse_sd = stats::sd(x$importance_delta_rmse),
    importance_delta_rmse_median = stats::median(x$importance_delta_rmse),
    importance_relative_rmse_mean = mean(x$importance_relative_rmse),
    rank_median = stats::median(x$rank_within_resample),
    rank_iqr = stats::IQR(x$rank_within_resample),
    top3_frequency = mean(x$rank_within_resample <= 3),
    positive_importance_frequency = mean(x$importance_delta_rmse > 0),
    stringsAsFactors = FALSE
  )
}))
row.names(permutation_importance_stability) <- NULL


# Boundary shrinkage and calibration diagnostics ------------------------

full_predictions <- oof_predictions[oof_predictions$predictor_set == "All", , drop = FALSE]
boundary_rows <- list()

# "lower_10_percent", "middle_80_percent", "upper_10_percent"
for (parameter in parameter_names) {
  x <- full_predictions[full_predictions$parameter == parameter, , drop = FALSE]
  for (scale_name in c("design", "original")) {
    observed <- x[[paste0("observed_", scale_name)]]
    predicted <- x[[paste0("predicted_", scale_name)]]
    for (repeat_id in seq_len(n_outer_repeats)) {
      keep_repeat <- x$repeat_id == repeat_id
      obs_repeat <- observed[keep_repeat]
      pred_repeat <- predicted[keep_repeat]
      q <- stats::quantile(obs_repeat, c(0, 0.1, 0.9, 1), type = 8)
      band <- ifelse(
        obs_repeat <= q[2], "lower_10_percent",
        ifelse(obs_repeat >= q[3], "upper_10_percent", "middle_80_percent")
      )
      for (band_name in c("lower_10_percent", "middle_80_percent", "upper_10_percent")) {
        use <- band == band_name
        boundary_rows[[paste(parameter, scale_name, repeat_id, band_name)]] <- data.frame(
          # e.g., lac_0 design 1 lower_10_percent; lac_0 design 1 middle_80_percent
          parameter = parameter,
          parameter_group = unname(parameter_groups[parameter]),
          evaluation_scale = scale_name,
          repeat_id = repeat_id,
          range_band = band_name,
          n = sum(use),
          observed_mean = mean(obs_repeat[use]),
          predicted_mean = mean(pred_repeat[use]),
          bias_predicted_minus_observed = mean(pred_repeat[use] - obs_repeat[use]),
          mae = mean(abs(pred_repeat[use] - obs_repeat[use])),
          stringsAsFactors = FALSE
        )
      }
    }
  }
}
boundary_bias <- do.call(rbind, boundary_rows)
row.names(boundary_bias) <- NULL

# Decile diagnostics use pooled out-of-fold predictions within each repeat.
error_bin_rows <- list()
for (parameter in parameter_names) {
  x <- full_predictions[full_predictions$parameter == parameter, , drop = FALSE]
  for (scale_name in c("design", "original")) {
    for (repeat_id in seq_len(n_outer_repeats)) {
      z <- x[x$repeat_id == repeat_id, , drop = FALSE]
      observed <- z[[paste0("observed_", scale_name)]]
      predicted <- z[[paste0("predicted_", scale_name)]]
      breaks <- unique(stats::quantile(observed, seq(0, 1, 0.1), type = 8))
      bin <- cut(observed, breaks = breaks, include.lowest = TRUE, labels = FALSE)
      for (bin_id in sort(unique(bin))) {
        use <- bin == bin_id
        error_bin_rows[[paste(parameter, scale_name, repeat_id, bin_id)]] <- data.frame(
          parameter = parameter,
          parameter_group = unname(parameter_groups[parameter]),
          evaluation_scale = scale_name,
          repeat_id = repeat_id,
          parameter_decile = bin_id,
          n = sum(use),
          observed_mean = mean(observed[use]),
          predicted_mean = mean(predicted[use]),
          bias_predicted_minus_observed = mean(predicted[use] - observed[use]),
          mae = mean(abs(predicted[use] - observed[use])),
          stringsAsFactors = FALSE
        )
      }
    }
  }
}
error_by_parameter_decile <- do.call(rbind, error_bin_rows)
row.names(error_by_parameter_decile) <- NULL


# Exploratory realized-mechanism diagnostics -----------------------------

# These diagnostics are examined only after primary recoverability has been
# estimated. They never enter a parameter-recovery model.
diagnostics <- read_analysis_csv(diagnostics_path)

# Average the three independent OOF predictions for descriptive error at each
# simulation. This averaging is across validation repeats, not simulations.
full_oof_by_simulation_groups <- split(
  full_predictions,
  interaction(full_predictions$parameter, full_predictions$simulation_id, drop = TRUE)
)
full_oof_by_simulation <- do.call(rbind, lapply(full_oof_by_simulation_groups, function(x) {
  data.frame(
    parameter = x$parameter[1],
    simulation_id = x$simulation_id[1],
    observed_design = x$observed_design[1],
    predicted_design = mean(x$predicted_design),
    observed_original = x$observed_original[1],
    predicted_original = mean(x$predicted_original),
    stringsAsFactors = FALSE
  )
}))
row.names(full_oof_by_simulation) <- NULL
full_oof_by_simulation$absolute_error_design <- abs(
  full_oof_by_simulation$predicted_design - full_oof_by_simulation$observed_design
)

mechanism_specs <- list(
  laa_1 = c(
    "final_D_nonzero_fraction", "final_D_p90", "final_D_max",
    "laa1D_over_laa0_all_mean", "laa1D_over_laa0_all_frac_informative",
    "laa1D_over_laa0_all_frac_strong"
  ),
  mu_1 = c(
    "mu1d_all_mean", "mu1d_all_frac_informative", "mu1d_all_frac_strong"
  ),
  K_1 = c(
    "K1d_over_K0_all_mean", "K1d_over_K0_all_frac_informative",
    "K1d_over_K0_all_frac_strong", "K1d_over_K0_immigration_mean"
  )
)

mechanism_rows <- list()
for (parameter in names(mechanism_specs)) {
  pred <- full_oof_by_simulation[full_oof_by_simulation$parameter == parameter, ]
  joined <- merge(pred, diagnostics, by = "simulation_id", all.x = TRUE, sort = FALSE)
  for (diagnostic in mechanism_specs[[parameter]]) {
    if (!diagnostic %in% names(joined)) next
    mechanism_rows[[paste(parameter, diagnostic)]] <- data.frame(
      parameter = parameter,
      diagnostic = diagnostic,
      n_complete = sum(stats::complete.cases(joined[, c("observed_original", diagnostic)])),
      spearman_raw_parameter_vs_diagnostic = safe_spearman(
        joined$observed_original, joined[[diagnostic]]
      ),
      spearman_abs_recovery_error_vs_diagnostic = safe_spearman(
        joined$absolute_error_design, joined[[diagnostic]]
      ),
      diagnostic_zero_fraction = mean(joined[[diagnostic]] == 0, na.rm = TRUE),
      diagnostic_median = stats::median(joined[[diagnostic]], na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }
}
mechanism_diagnostic_associations <- do.call(rbind, mechanism_rows)
row.names(mechanism_diagnostic_associations) <- NULL

# Stratify recovery by whether each mechanism was realized at all. These are
# descriptive, post-recovery diagnostics and are not used to train a model.
mechanism_strata_specs <- list(
  laa_1 = c(diagnostic = "final_D_nonzero_fraction", label = "nonzero mismatch D"),
  mu_1 = c(diagnostic = "mu1d_all_frac_informative", label = "informative mu_1*d exposure"),
  K_1 = c(diagnostic = "K1d_over_K0_all_frac_informative", label = "informative K_1*d/K_0 exposure")
)
mechanism_strata_rows <- list()
for (parameter in names(mechanism_strata_specs)) {
  specification <- mechanism_strata_specs[[parameter]]
  pred <- full_oof_by_simulation[full_oof_by_simulation$parameter == parameter, ]
  joined <- merge(pred, diagnostics, by = "simulation_id", all.x = TRUE, sort = FALSE)
  diagnostic <- specification[["diagnostic"]]
  joined$exposure_stratum <- ifelse(
    is.na(joined[[diagnostic]]), "undefined",
    ifelse(joined[[diagnostic]] > 0, "realized_above_zero", "zero")
  )
  for (stratum in unique(joined$exposure_stratum)) {
    z <- joined[joined$exposure_stratum == stratum, , drop = FALSE]
    values <- metric_values(z$observed_design, z$predicted_design)
    mechanism_strata_rows[[paste(parameter, stratum)]] <- data.frame(
      parameter = parameter,
      diagnostic = diagnostic,
      diagnostic_label = specification[["label"]],
      exposure_stratum = stratum,
      n = nrow(z),
      rsq_design = values["rsq"],
      rmse_design = values["rmse"],
      mae_design = values["mae"],
      spearman_design = values["spearman"],
      stringsAsFactors = FALSE
    )
  }
}
mechanism_stratified_recovery <- do.call(rbind, mechanism_strata_rows)
row.names(mechanism_stratified_recovery) <- NULL


# Place the full forest beside both null models in one concise table. The
# improvements are positive when the forest has lower error than the null.
primary_performance <- performance_summary[
  performance_summary$predictor_set %in% c("All", "Null_mean", "Null_median"),
  , drop = FALSE
]
full_perf <- primary_performance[primary_performance$predictor_set == "All", ]
mean_perf <- primary_performance[primary_performance$predictor_set == "Null_mean", ]
median_perf <- primary_performance[primary_performance$predictor_set == "Null_median", ]
match_key <- function(x) paste(x$parameter, x$evaluation_scale)
mean_perf <- mean_perf[match(match_key(full_perf), match_key(mean_perf)), ]
median_perf <- median_perf[match(match_key(full_perf), match_key(median_perf)), ]
primary_performance_comparison <- full_perf
primary_performance_comparison$null_mean_rsq <- mean_perf$rsq_mean
primary_performance_comparison$null_mean_rmse <- mean_perf$rmse_mean
primary_performance_comparison$null_mean_mae <- mean_perf$mae_mean
primary_performance_comparison$null_median_rsq <- median_perf$rsq_mean
primary_performance_comparison$null_median_rmse <- median_perf$rmse_mean
primary_performance_comparison$null_median_mae <- median_perf$mae_mean
primary_performance_comparison$rmse_improvement_vs_mean_null <-
  1 - primary_performance_comparison$rmse_mean / mean_perf$rmse_mean
primary_performance_comparison$mae_improvement_vs_mean_null <-
  1 - primary_performance_comparison$mae_mean / mean_perf$mae_mean
primary_performance_comparison$rmse_improvement_vs_median_null <-
  1 - primary_performance_comparison$rmse_mean / median_perf$rmse_mean
primary_performance_comparison$mae_improvement_vs_median_null <-
  1 - primary_performance_comparison$mae_mean / median_perf$mae_mean


# Save reproducible outputs ----------------------------------------------

write.csv(
  oof_predictions, file.path(output_dir, "part1_oof_predictions.csv"),
  row.names = FALSE, na = ""
)
write.csv(
  performance_by_repeat, file.path(output_dir, "part1_performance_by_repeat.csv"),
  row.names = FALSE, na = ""
)
write.csv(
  performance_summary, file.path(output_dir, "part1_performance_all_models.csv"),
  row.names = FALSE, na = ""
)
write.csv(
  primary_performance_comparison,
  file.path(output_dir, "part1_performance_table_nine_parameters.csv"),
  row.names = FALSE, na = ""
)
write.csv(
  group_only_performance,
  file.path(output_dir, "part1_summary_group_recoverability.csv"),
  row.names = FALSE, na = ""
)
write.csv(
  group_ablation_by_repeat,
  file.path(output_dir, "part1_group_ablation_by_repeat.csv"),
  row.names = FALSE, na = ""
)
write.csv(
  group_ablation_summary,
  file.path(output_dir, "part1_group_ablation_summary.csv"),
  row.names = FALSE, na = ""
)
write.csv(
  permutation_by_resample,
  file.path(output_dir, "part1_permutation_importance_by_resample.csv"),
  row.names = FALSE, na = ""
)
write.csv(
  permutation_importance_stability,
  file.path(output_dir, "part1_permutation_importance_stability.csv"),
  row.names = FALSE, na = ""
)
write.csv(
  boundary_bias, file.path(output_dir, "part1_boundary_bias.csv"),
  row.names = FALSE, na = ""
)
write.csv(
  error_by_parameter_decile,
  file.path(output_dir, "part1_error_by_parameter_decile.csv"),
  row.names = FALSE, na = ""
)
write.csv(
  mechanism_diagnostic_associations,
  file.path(output_dir, "part1_realized_mechanism_diagnostics.csv"),
  row.names = FALSE, na = ""
)
write.csv(
  mechanism_stratified_recovery,
  file.path(output_dir, "part1_realized_mechanism_stratified_recovery.csv"),
  row.names = FALSE, na = ""
)
write.csv(
  tuning_results[tuning_results$selected, , drop = FALSE],
  file.path(output_dir, "part1_selected_hyperparameters.csv"),
  row.names = FALSE, na = ""
)

run_manifest <- data.frame(
  setting = c(
    "data_path", "n_simulations", "outer_folds", "outer_repeats",
    "tuning_strategy", "trees", "permutations_per_summary", "cores",
    "outer_seed", "grid_seed", "forest_seed",
    "permutation_seed", "r_version", "ranger_version"
  ),
  value = c(
    data_path, nrow(analysis_df), n_outer_folds, n_outer_repeats,
    "ranger_OOB_within_each_outer_analysis_set", n_trees, n_permutations, n_cores,
    outer_seed, grid_seed, forest_seed, permutation_seed,
    R.version.string, as.character(utils::packageVersion("ranger"))
  ),
  stringsAsFactors = FALSE
)
write.csv(
  run_manifest, file.path(output_dir, "part1_run_manifest.csv"),
  row.names = FALSE
)

saveRDS(
  list(
    run_manifest = run_manifest,
    predictor_sets = predictor_sets,
    parameter_scale = parameter_scale,
    outer_fold_assignments = outer_fold_assignments,
    oof_predictions = oof_predictions,
    performance_by_repeat = performance_by_repeat,
    performance_summary = performance_summary,
    group_ablation_by_repeat = group_ablation_by_repeat,
    group_ablation_summary = group_ablation_summary,
    permutation_by_resample = permutation_by_resample,
    permutation_importance_stability = permutation_importance_stability,
    boundary_bias = boundary_bias,
    error_by_parameter_decile = error_by_parameter_decile,
    mechanism_diagnostic_associations = mechanism_diagnostic_associations,
    mechanism_stratified_recovery = mechanism_stratified_recovery,
    primary_performance_comparison = primary_performance_comparison,
    selected_hyperparameters = tuning_results[tuning_results$selected, , drop = FALSE]
  ),
  file.path(output_dir, "part1_inverse_inference_results.rds"),
  compress = "xz"
)

cat("Part I inverse-inference models completed.\n")
cat("Output directory:", output_dir, "\n")
cat("Out-of-fold prediction rows:", nrow(oof_predictions), "\n")
cat("All primary performance estimates use outer-fold predictions only.\n")
