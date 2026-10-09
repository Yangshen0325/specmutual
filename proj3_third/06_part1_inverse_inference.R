###############################################################################
# Part I: recover nine parameters from 17 observable community summaries.
# Adapted from proj3_second/12_part1_inverse_inference.R.
# Run AFTER 05_collect_stress_rescue_results.R, from the repository root.
#
# * Use available completed weak/intermediate/strong rows; no exact-zero controls.
# * Same seven predictor sets, null models and held-out permutation importance.
# * 3 repeats x 5 outer folds; keep shared intrinsic backgrounds together.
# * Tune a predictor-count-based grid using OOB error within outer training data.
# * RDS for analysis objects; CSV only for compact reporting tables.
#
###############################################################################

rm(list = ls())
source("proj3_third/summary_utils_proj3_third.R")
source("proj3_third/workflow_utils_proj3_third.R")
if (!requireNamespace("ranger", quietly = TRUE) ||
    utils::packageVersion("ranger") < "0.17.0") {
  stop("Use ranger >= 0.17.0 for the na.learn handling used in this analysis.")
}

# Settings ----------------------------------------------------------------
run_dir <- file.path(proj3_third_base_dir(), "proj3_third")

# if run it on cluster, move
generated_dir <- file.path(run_dir, "data", "generated")
input_dir <- file.path(run_dir, "data", "processed")
output_dir <- file.path(run_dir, "part1_inverse_inference")
data_path <- file.path(input_dir, "part1_analysis_ready.rds")

outer_seed <- 43425L
grid_seed <- 20260812L
forest_seed <- 62217L
permutation_seed <- 91831L
n_outer_folds <- 5L
n_outer_repeats <- 3L
n_trees <- proj3_third_env_number("PROJ3_THIRD_PART1_TREES", 500, integer = TRUE)
n_permutations <- proj3_third_env_number("PROJ3_THIRD_PART1_PERMUTATIONS", 5, integer = TRUE)
n_cores <- proj3_third_env_number("PROJ3_THIRD_PART1_CORES", 1, integer = TRUE)

parameter_names <- proj3_third_parameter_names()
parameter_groups <- setNames(c(rep("Intrinsic", 5), rep("Mutualism-related", 4)),
                             parameter_names)
richness_summaries <- c("island_endemic_p", "island_nonendemic_p",
                        "island_endemic_a", "island_nonendemic_a")
network_summaries <- c("connectance", "disconnect_p", "disconnect_a",
                       "largest_component", "n_components", "plant_degree", "animal_degree")
nltt_summaries <- c("nonend_nltt_p", "singleton_nltt_p", "multi_nltt_p",
                    "nonend_nltt_a", "singleton_nltt_a", "multi_nltt_a")
all_summaries <- c(richness_summaries, network_summaries, nltt_summaries)
# Use exactly the same 17 predictors as the previous Part I script. Total
# richness is redundant with endemic + nonendemic richness; internal exposure,
# mismatch, runtime, scenario label and parameter values are never predictors.
predictor_sets <- list(
  All = all_summaries,
  Richness = richness_summaries,
  Network = network_summaries,
  nLTT = nltt_summaries,
  All_minus_Richness = setdiff(all_summaries, richness_summaries),
  All_minus_Network = setdiff(all_summaries, network_summaries),
  All_minus_nLTT = setdiff(all_summaries, nltt_summaries)
)
summary_group_lookup <- c(
  setNames(rep("Richness", length(richness_summaries)), richness_summaries),
  setNames(rep("Network", length(network_summaries)), network_summaries),
  setNames(rep("nLTT", length(nltt_summaries)), nltt_summaries)
)
summary_group_colors <- c(nLTT = "#8C9FC7", Richness = "#F9CF7C", Network = "#E49B8F")

# Read summaries and sampling scales ---------------------------------------
analysis_df <- readRDS(data_path)
analysis_df <- analysis_df[order(analysis_df$simulation_id), , drop = FALSE]
required_columns <- c("simulation_id", "simulation_key", "background_id", "design_group",
                      "analysis_eligible", parameter_names, all_summaries)
if (!all(required_columns %in% names(analysis_df)) ||
    anyDuplicated(analysis_df$simulation_id) || !nrow(analysis_df)) {
  stop("Run collector 05 first; RF input must contain unique, eligible simulation rows.")
}
if (!all(analysis_df$analysis_eligible %in% TRUE) ||
    !all(analysis_df$design_group %in% c("weak", "intermediate", "strong")) ||
    anyNA(analysis_df$background_id)) {
  stop("RF input must contain completed continuous ON rows with known backgrounds.")
}
if (length(unique(analysis_df$background_id)) < n_outer_folds) {
  stop("At least ", n_outer_folds, " distinct backgrounds are needed for the outer folds.")
}

intrinsic_ranges <- read.csv(file.path(generated_dir, "stress_intrinsic_ranges.csv"))
mutualism_ranges <- read.csv(file.path(generated_dir, "mutualism_sampling_ranges.csv"))
range_rows <- rbind(
  intrinsic_ranges[, c("parameter", "lower", "upper", "sampling_scale")],
  mutualism_ranges[, c("parameter", "lower", "upper", "sampling_scale")]
)
parameter_dictionary <- do.call(rbind, lapply(parameter_names, function(parameter) {
  spec <- range_rows[range_rows$parameter == parameter, ]
  if (!nrow(spec) || length(unique(spec$sampling_scale)) != 1L) {
    stop("No unique sampling scale for ", parameter)
  }
  data.frame(parameter = parameter, transformation = spec$sampling_scale[1],
             design_space_lower = min(spec$lower), design_space_upper = max(spec$upper))
}))
parameter_scale <- setNames(parameter_dictionary$transformation, parameter_dictionary$parameter)
for (parameter in parameter_names) {
  spec <- parameter_dictionary[parameter_dictionary$parameter == parameter, ]
  values <- analysis_df[[parameter]]
  if (!all(is.finite(values)) || !spec$transformation %in% c("log", "linear") ||
      any(values < spec$design_space_lower | values > spec$design_space_upper) ||
      (spec$transformation == "log" && any(values <= 0))) {
    stop("Parameter values or design-scale specification are invalid for ", parameter)
  }
  # Natural logs for log-sampled responses; linear responses remain unchanged.
  analysis_df[[paste0(parameter, "__design")]] <- if (spec$transformation == "log")
    log(values) else values
}

# Collection coverage is part of interpretation, not a model predictor.
collection_counts <- read.csv(file.path(input_dir, "stress_rescue_status_counts.csv"))
message("Fitting available ON rows: ", nrow(analysis_df), " from ",
        length(unique(analysis_df$background_id)), " backgrounds.")
print(table(analysis_df$design_group))

# Shared repeated outer resamples -----------------------------------------
make_balanced_folds <- function(ids, v, seed) {
  ids <- sort(unique(ids))
  set.seed(seed)
  shuffled <- sample(ids, length(ids), replace = FALSE)
  setNames(rep(seq_len(v), length.out = length(ids)), as.character(shuffled))
}

# A background is the splitting unit, not a simulation row. Otherwise siblings
# with identical intrinsic parameter responses could occur on both sides of CV.
outer_rows <- list()
for (repeat_id in seq_len(n_outer_repeats)) {
  outer <- make_balanced_folds(analysis_df$background_id, n_outer_folds,
                                outer_seed + repeat_id * 1009L)
  row_fold <- unname(outer[as.character(analysis_df$background_id)])
  outer_rows[[repeat_id]] <- data.frame(
    simulation_id = analysis_df$simulation_id,
    background_id = analysis_df$background_id,
    design_group = analysis_df$design_group,
    repeat_id = repeat_id, outer_fold = row_fold
  )
}
outer_fold_assignments <- do.call(rbind, outer_rows)
row.names(outer_fold_assignments) <- NULL

# Random forest helpers ----------------------------------------------------
safe_spearman <- function(observed, predicted) {
  if (length(observed) < 3L || length(unique(observed)) < 2L ||
      length(unique(predicted)) < 2L) return(NA_real_)
  suppressWarnings(stats::cor(observed, predicted, method = "spearman"))
}
metric_values <- function(observed, predicted) {
  denominator <- sum((observed - mean(observed))^2)
  c(rsq = if (denominator > 0) 1 - sum((observed - predicted)^2) / denominator else NA_real_,
    rmse = sqrt(mean((predicted - observed)^2)),
    mae = mean(abs(predicted - observed)),
    spearman = safe_spearman(observed, predicted))
}
safe_divide <- function(x, denominator) {
  ifelse(is.finite(denominator) & denominator > 0, x / denominator, NA_real_)
}
make_grid <- function(n_predictors, grid_size = 6L) {
  # Same grid construction as proj3_second/12_part1_inverse_inference.R:
  # up to four mtry values crossed with four node sizes, then retain six rows.
  set.seed(grid_seed + n_predictors * 101L)
  mtry_values <- unique(pmax(1L, pmin(n_predictors,
    round(seq(1, n_predictors, length.out = min(4L, n_predictors))))))
  grid <- expand.grid(mtry = mtry_values, min.node.size = c(3L, 7L, 15L, 25L))
  if (nrow(grid) > grid_size) grid <- grid[unique(round(seq(1, nrow(grid), length.out = grid_size))), ]
  row.names(grid) <- NULL
  grid
}
fit_forest <- function(train_data, response, predictors, mtry, min_node_size, seed) {
  # Undefined summaries are not failed simulations. ranger learns missing-value
  # routing using only training data. An entirely unavailable training column
  # cannot be learned, so exclude it for this fit (not based on assessment data).
  usable <- predictors[vapply(train_data[, predictors, drop = FALSE],
                              function(x) any(is.finite(x)), logical(1))]
  if (!length(usable)) {
    return(list(forest = NULL, predictors = usable, constant = mean(train_data[[response]])))
  }
  model_data <- train_data[, c(response, usable), drop = FALSE]
  names(model_data)[1] <- ".outcome"
  forest <- ranger::ranger(
    dependent.variable.name = ".outcome", data = model_data,
    num.trees = n_trees, mtry = min(as.integer(mtry), length(usable)),
    min.node.size = as.integer(min_node_size), splitrule = "variance",
    replace = TRUE, sample.fraction = 0.8, na.action = "na.learn",
    seed = as.integer(seed), num.threads = 1L, importance = "none",
    write.forest = TRUE, oob.error = TRUE
  )
  list(forest = forest, predictors = usable, constant = NULL)
}
predict_forest <- function(model, new_data) {
  if (is.null(model$forest)) return(rep(model$constant, nrow(new_data)))
  prediction <- as.numeric(predict(model$forest,
    data = new_data[, model$predictors, drop = FALSE], num.threads = 1L)$predictions)
  if (any(!is.finite(prediction))) stop("Non-finite forest predictions; do not silently drop them.")
  prediction
}
tune_forest <- function(train_data, response, predictors, task_seed) {
  grid <- make_grid(length(predictors))
  grid$oob_rmse <- NA_real_
  for (grid_id in seq_len(nrow(grid))) {
    # OOB predictions use trees whose bootstrap sample excludes the given row.
    # Only outer training rows participate; outer test data remain untouched.
    model <- fit_forest(train_data, response, predictors,
      grid$mtry[grid_id], grid$min.node.size[grid_id],
      task_seed + grid_id * 1009L)
    if (!is.null(model$forest)) {
      grid$oob_rmse[grid_id] <- sqrt(model$forest$prediction.error)
    }
  }
  # If all summaries are unavailable, fit_forest uses a training-mean fallback;
  # there is no forest to tune, so its OOB score stays NA rather than a fitted error.
  grid <- grid[order(grid$oob_rmse, grid$mtry, grid$min.node.size), ]
  list(best = grid[1, , drop = FALSE], tuning = grid)
}
back_transform <- function(value, parameter) {
  if (parameter_scale[[parameter]] == "log") exp(value) else value
}

# Fit one parameter across all predictor sets ------------------------------
fit_parameter <- function(parameter_index) {
  parameter <- parameter_names[parameter_index]
  response <- paste0(parameter, "__design")
  prediction_rows <- tuning_rows <- importance_rows <- list()
  message("Starting parameter: ", parameter)

  for (repeat_id in seq_len(n_outer_repeats)) {
    membership <- outer_fold_assignments[outer_fold_assignments$repeat_id == repeat_id, ]
    row_fold <- membership$outer_fold[match(analysis_df$simulation_id, membership$simulation_id)]
    for (outer_fold_id in seq_len(n_outer_folds)) {
      split_id <- sprintf("Repeat%02d_Fold%02d", repeat_id, outer_fold_id)
      train_data <- analysis_df[row_fold != outer_fold_id, , drop = FALSE]
      test_data <- analysis_df[row_fold == outer_fold_id, , drop = FALSE]
      observed_design <- test_data[[response]]
      observed_original <- test_data[[parameter]]
      prediction_table <- function(predictor_set, predicted_design, predicted_original) {
        data.frame(parameter = parameter, parameter_group = unname(parameter_groups[parameter]),
          response_scale = unname(parameter_scale[parameter]), predictor_set = predictor_set,
          repeat_id = repeat_id, outer_fold = outer_fold_id, split_id = split_id,
          simulation_id = test_data$simulation_id, background_id = test_data$background_id,
          design_group = test_data$design_group,
          observed_design = observed_design, predicted_design = predicted_design,
          observed_original = observed_original, predicted_original = predicted_original)
      }

      # Fit each null on the outer training data. Original-scale null means are
      # calculated on original values, not exp(mean(log(values))).
      for (null_name in c("Null_mean", "Null_median")) {
        location <- if (null_name == "Null_mean") mean else stats::median
        prediction_rows[[paste(split_id, null_name)]] <- prediction_table(
          null_name, location(train_data[[response]]), location(train_data[[parameter]]))
      }

      for (set_index in seq_along(predictor_sets)) {
        predictor_set <- names(predictor_sets)[set_index]
        predictors <- predictor_sets[[predictor_set]]
        task_seed <- forest_seed + parameter_index * 1000000L +
          repeat_id * 10000L + outer_fold_id * 100L + set_index
        # Match the previous script's grid + OOB tuning. OOB is row-wise, so
        # siblings may occur in its bootstrap samples; it is a tuning score,
        # not the reported performance on held-out background groups.
        tuned <- tune_forest(train_data, response, predictors, task_seed)
        best <- tuned$best
        model <- fit_forest(train_data, response, predictors,
                            best$mtry, best$min.node.size, task_seed + 900001L)
        predicted_design <- predict_forest(model, test_data)
        prediction_rows[[paste(split_id, predictor_set)]] <- prediction_table(
          predictor_set, predicted_design, back_transform(predicted_design, parameter))

        tuning_table <- tuned$tuning
        tuning_table$parameter <- parameter
        tuning_table$predictor_set <- predictor_set
        tuning_table$repeat_id <- repeat_id
        tuning_table$outer_fold <- outer_fold_id
        tuning_table$split_id <- split_id
        tuning_table$selected <- seq_len(nrow(tuning_table)) == 1L
        tuning_table$task_seed <- task_seed
        tuning_table$final_forest_seed <- task_seed + 900001L
        tuning_table$n_usable_predictors <- length(model$predictors)
        tuning_rows[[paste(split_id, predictor_set)]] <- tuning_table

        # Permute each summary only in held-out data. Correlated predictors can
        # substitute for each other, so this is not unique information or causality.
        if (predictor_set == "All") {
          baseline_rmse <- unname(metric_values(observed_design, predicted_design)["rmse"])
          for (summary_index in seq_along(predictors)) {
            summary_name <- predictors[summary_index]
            permuted_rmse <- numeric(n_permutations)
            for (permutation_id in seq_len(n_permutations)) {
              set.seed(permutation_seed + parameter_index * 10000000L +
                repeat_id * 1000000L + outer_fold_id * 100000L +
                summary_index * 1000L + permutation_id)
              permuted_data <- test_data
              permuted_data[[summary_name]] <- sample(permuted_data[[summary_name]],
                                                       nrow(permuted_data), replace = FALSE)
              predicted <- predict_forest(model, permuted_data)
              permuted_rmse[permutation_id] <- metric_values(observed_design, predicted)["rmse"]
            }
            importance_rows[[paste(split_id, summary_name)]] <- data.frame(
              parameter = parameter, parameter_group = unname(parameter_groups[parameter]),
              repeat_id = repeat_id, outer_fold = outer_fold_id, split_id = split_id,
              summary = summary_name, baseline_rmse = baseline_rmse,
              permuted_rmse_mean = mean(permuted_rmse),
              importance_delta_rmse = mean(permuted_rmse) - baseline_rmse,
              importance_relative_rmse = safe_divide(mean(permuted_rmse), baseline_rmse) - 1,
              permutation_sd = stats::sd(permuted_rmse))
          }
        }
      }
    }
  }
  result <- list(predictions = do.call(rbind, prediction_rows),
                 tuning = do.call(rbind, tuning_rows),
                 importance = do.call(rbind, importance_rows))
  # Save this parameter's OOF predictions, tuning scores and importance as soon
  # as it finishes. These are checkpoints, not training predictions or final forests.
  saveRDS(result, file.path(output_dir, paste0("part1_parameter_", parameter, ".rds")))
  message("Completed parameter: ", parameter)
  result
}

# Run the nine parameter analyses ------------------------------------------
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
# Save outer memberships shared by all parameters/predictor sets; no inner folds.
saveRDS(list(outer = outer_fold_assignments),
        file.path(output_dir, "part1_resamples.rds"))
if (.Platform$OS.type == "unix" && n_cores > 1L) {
  parameter_results <- parallel::mclapply(seq_along(parameter_names), fit_parameter,
    mc.cores = min(n_cores, length(parameter_names)), mc.preschedule = TRUE,
    mc.set.seed = FALSE) # every forest and permutation has an explicit seed
} else {
  parameter_results <- lapply(seq_along(parameter_names), fit_parameter)
}
names(parameter_results) <- parameter_names
failed <- vapply(parameter_results, function(x) inherits(x, "try-error") || is.null(x), logical(1))
if (any(failed)) stop("Parameter jobs failed: ", paste(parameter_names[failed], collapse = ", "),
                      ". Check the log; completed parameter checkpoints are retained.")
oof_predictions <- do.call(rbind, lapply(parameter_results, `[[`, "predictions"))
tuning_results <- do.call(rbind, lapply(parameter_results, `[[`, "tuning"))
permutation_by_resample <- do.call(rbind, lapply(parameter_results, `[[`, "importance"))
row.names(oof_predictions) <- row.names(tuning_results) <- row.names(permutation_by_resample) <- NULL

# Performance on design and original scales -------------------------------
# Pool all outer assessment predictions WITHIN a repeat, so each simulation is
# evaluated once per repeat. SD across repeats is descriptive, not a confidence CI.
summarize_one_performance <- function(x, scale_name) {
  observed <- x[[paste0("observed_", scale_name)]]
  predicted <- x[[paste0("predicted_", scale_name)]]
  spec <- parameter_dictionary[parameter_dictionary$parameter == x$parameter[1], ]
  bounds <- c(spec$design_space_lower, spec$design_space_upper)
  if (scale_name == "design" && spec$transformation == "log") bounds <- log(bounds)
  basic <- metric_values(observed, predicted)
  design_range <- diff(bounds)
  observed_iqr <- stats::IQR(observed)
  c(basic, design_range = design_range, observed_iqr = observed_iqr,
    nrmse_range = unname(safe_divide(basic["rmse"], design_range)),
    nmae_range = unname(safe_divide(basic["mae"], design_range)),
    nrmse_iqr = unname(safe_divide(basic["rmse"], observed_iqr)),
    nmae_iqr = unname(safe_divide(basic["mae"], observed_iqr)))
}
performance_table <- function(predictions, by_scenario = FALSE) {
  group_columns <- c("parameter", "predictor_set", "repeat_id",
                      if (by_scenario) "design_group")
  groups <- split(predictions, interaction(predictions[, group_columns], drop = TRUE))
  do.call(rbind, lapply(groups, function(x) {
    do.call(rbind, lapply(c("design", "original"), function(scale_name) {
      data.frame(parameter = x$parameter[1], parameter_group = x$parameter_group[1],
        predictor_set = x$predictor_set[1], repeat_id = x$repeat_id[1],
        design_group = if (by_scenario) x$design_group[1] else "pooled_ON",
        evaluation_scale = scale_name, n_simulations = nrow(x),
        as.list(summarize_one_performance(x, scale_name)))
    }))
  }))
}
performance_by_repeat <- performance_table(oof_predictions)
# Same held-out predictions, evaluated separately by band to distinguish pooled
# recoverability from within-band performance. Models are not refitted here.
performance_by_scenario <- performance_table(oof_predictions, by_scenario = TRUE)
metric_names <- c("rsq", "rmse", "mae", "nrmse_range", "nrmse_iqr",
                   "nmae_range", "nmae_iqr", "spearman")
performance_groups <- split(performance_by_repeat, interaction(
  performance_by_repeat$parameter, performance_by_repeat$predictor_set,
  performance_by_repeat$evaluation_scale, drop = TRUE))
performance_summary <- do.call(rbind, lapply(performance_groups, function(x) {
  out <- data.frame(parameter = x$parameter[1], parameter_group = x$parameter_group[1],
    predictor_set = x$predictor_set[1], evaluation_scale = x$evaluation_scale[1],
    n_simulations = nrow(analysis_df), n_repeats = n_outer_repeats)
  for (metric in metric_names) {
    values <- x[[metric]][is.finite(x[[metric]])]
    out[[paste0(metric, "_mean")]] <- if (length(values)) mean(values) else NA_real_
    out[[paste0(metric, "_sd")]] <- stats::sd(values)
  }
  out
}))

# Group ablation and summary-group recoverability --------------------------
full_repeat <- subset(performance_by_repeat, predictor_set == "All" & evaluation_scale == "design")
group_ablation_by_repeat <- do.call(rbind, lapply(c("Richness", "Network", "nLTT"), function(group) {
  omitted <- subset(performance_by_repeat,
    predictor_set == paste0("All_minus_", group) & evaluation_scale == "design")
  key <- function(x) paste(x$parameter, x$repeat_id)
  omitted <- omitted[match(key(full_repeat), key(omitted)), ]
  data.frame(parameter = full_repeat$parameter, parameter_group = full_repeat$parameter_group,
    summary_group = group, repeat_id = full_repeat$repeat_id,
    full_rsq = full_repeat$rsq, omitted_rsq = omitted$rsq,
    rsq_loss_when_omitted = full_repeat$rsq - omitted$rsq,
    full_rmse = full_repeat$rmse, omitted_rmse = omitted$rmse,
    relative_rmse_increase_when_omitted = safe_divide(omitted$rmse, full_repeat$rmse) - 1)
}))
ablation_groups <- split(group_ablation_by_repeat, interaction(
  group_ablation_by_repeat$parameter, group_ablation_by_repeat$summary_group, drop = TRUE))
group_ablation_summary <- do.call(rbind, lapply(ablation_groups, function(x) {
  data.frame(parameter = x$parameter[1], parameter_group = x$parameter_group[1],
    summary_group = x$summary_group[1],
    rsq_loss_mean = mean(x$rsq_loss_when_omitted), rsq_loss_sd = stats::sd(x$rsq_loss_when_omitted),
    relative_rmse_increase_mean = mean(x$relative_rmse_increase_when_omitted),
    relative_rmse_increase_sd = stats::sd(x$relative_rmse_increase_when_omitted))
}))
group_only_performance <- subset(performance_summary, evaluation_scale == "design" &
                                  predictor_set %in% c("Richness", "Network", "nLTT"))

# Held-out permutation-importance stability -------------------------------
permutation_by_resample$summary_group <- unname(summary_group_lookup[permutation_by_resample$summary])
permutation_by_resample$rank_within_resample <- ave(-permutation_by_resample$importance_delta_rmse,
  interaction(permutation_by_resample$parameter, permutation_by_resample$split_id),
  FUN = function(x) rank(x, ties.method = "average"))
importance_groups <- split(permutation_by_resample, interaction(
  permutation_by_resample$parameter, permutation_by_resample$summary, drop = TRUE))
permutation_importance_stability <- do.call(rbind, lapply(importance_groups, function(x) {
  data.frame(parameter = x$parameter[1], parameter_group = x$parameter_group[1],
    summary = x$summary[1], summary_group = x$summary_group[1], n_resamples = nrow(x),
    importance_delta_rmse_mean = mean(x$importance_delta_rmse),
    importance_delta_rmse_sd = stats::sd(x$importance_delta_rmse),
    importance_delta_rmse_median = stats::median(x$importance_delta_rmse),
    importance_relative_rmse_mean = mean(x$importance_relative_rmse),
    rank_median = stats::median(x$rank_within_resample), rank_iqr = stats::IQR(x$rank_within_resample),
    top3_frequency = mean(x$rank_within_resample <= 3),
    positive_importance_frequency = mean(x$importance_delta_rmse > 0))
}))
# Descriptive correlations help interpret redundant predictors. They are NOT
# used to select predictors before CV; group ablation remains the group comparison.
summary_spearman <- suppressWarnings(stats::cor(analysis_df[, all_summaries],
                                                method = "spearman", use = "pairwise.complete.obs"))
summary_pairwise_n <- crossprod(1L * !is.na(as.matrix(analysis_df[, all_summaries])))

# Full-model performance beside the training-only nulls --------------------
primary_performance_comparison <- subset(performance_summary, predictor_set == "All")
for (null_name in c("Null_mean", "Null_median")) {
  null <- subset(performance_summary, predictor_set == null_name)
  key <- function(x) paste(x$parameter, x$evaluation_scale)
  null <- null[match(key(primary_performance_comparison), key(null)), ]
  prefix <- tolower(null_name)
  for (metric in c("rsq", "rmse", "mae")) {
    primary_performance_comparison[[paste0(prefix, "_", metric)]] <- null[[paste0(metric, "_mean")]]
  }
  for (metric in c("rmse", "mae")) {
    primary_performance_comparison[[paste0(metric, "_improvement_vs_", prefix)]] <- 1 -
      safe_divide(primary_performance_comparison[[paste0(metric, "_mean")]],
                  null[[paste0(metric, "_mean")]])
  }
}

# Save reproducible outputs ------------------------------------------------
run_settings <- list(
  data_path = data_path, input_md5 = tools::md5sum(data_path),
  n_simulations = nrow(analysis_df), n_backgrounds = length(unique(analysis_df$background_id)),
  rows_by_scenario = table(analysis_df$design_group), collection_counts = collection_counts,
  outer_folds = n_outer_folds, outer_repeats = n_outer_repeats,
  tuning_strategy = "ranger_OOB_within_each_outer_analysis_set", trees = n_trees,
  permutations = n_permutations, cores = n_cores,
  seeds = c(outer = outer_seed, grid = grid_seed, forest = forest_seed, permutation = permutation_seed),
  rng_kind = RNGkind(), session_info = capture.output(sessionInfo()),
  note = "Available completed ON subset only; original-scale log-response predictions are exp(design predictions)."
)
# Save all OOF predictions, including repeat, background, scenario and both scales.
saveRDS(oof_predictions, file.path(output_dir, "part1_oof_predictions.rds"))
# Save the complete analysis bundle for later plotting; no plots are generated here.
saveRDS(list(
  run_settings = run_settings, parameter_dictionary = parameter_dictionary,
  predictor_sets = predictor_sets, summary_group_colors = summary_group_colors,
  parameter_scale = parameter_scale, outer_fold_assignments = outer_fold_assignments,
  oof_predictions = oof_predictions, tuning_results = tuning_results,
  performance_by_repeat = performance_by_repeat, performance_by_scenario = performance_by_scenario,
  performance_summary = performance_summary, primary_performance_comparison = primary_performance_comparison,
  group_only_performance = group_only_performance,
  group_ablation_by_repeat = group_ablation_by_repeat, group_ablation_summary = group_ablation_summary,
  permutation_by_resample = permutation_by_resample,
  permutation_importance_stability = permutation_importance_stability,
  summary_spearman = summary_spearman, summary_pairwise_n = summary_pairwise_n
), file.path(output_dir, "part1_inverse_inference_results.rds"), compress = "xz")
# Save the nine-parameter full-RF performance table with mean/median null comparisons.
write.csv(primary_performance_comparison,
          file.path(output_dir, "part1_performance_table_nine_parameters.csv"), row.names = FALSE, na = "")
# Save performance for all predictor sets on design and original scales.
write.csv(performance_summary, file.path(output_dir, "part1_performance_all_models.csv"),
          row.names = FALSE, na = "")
# Save per-band evaluation of the same OOF predictions, not separate fitted models.
write.csv(performance_by_scenario, file.path(output_dir, "part1_performance_by_scenario.csv"),
          row.names = FALSE, na = "")
# Save the summary-group-only recoverability table for later heatmaps.
write.csv(group_only_performance, file.path(output_dir, "part1_summary_group_recoverability.csv"),
          row.names = FALSE, na = "")
# Save the main group-ablation comparison; do not sum individual importances.
write.csv(group_ablation_summary, file.path(output_dir, "part1_group_ablation_summary.csv"),
          row.names = FALSE, na = "")
# Save held-out permutation importance and rank stability, with group labels.
write.csv(permutation_importance_stability,
          file.path(output_dir, "part1_permutation_importance_stability.csv"), row.names = FALSE, na = "")
cat("Part I complete. All performance estimates use held-out predictions.\n")
cat("Output directory:", output_dir, "\n")
