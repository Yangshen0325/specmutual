###############################################################################
# proj3/05a_rf_parameters_to_outcomes.R
#
# Direction A: Simon-style random forest regression.
# Use the 9 simulation parameters as features to predict emergent community
# properties (richness, connectance, NLTT, network structure, etc.).
#
# This mirrors Simon et al. (2023): parameters -> model behavior.
# We quantify variable importance, compute partial dependence, and evaluate
# predictive performance via hold-out cross-validation.
#
# Input:  proj3/dataset_for_rf.rds
# Output: proj3/rf_params_to_outcomes.rds
#         proj3/figures/ (variable importance, observed vs predicted)
#
# Run from specmutual root:
#   Rscript proj3/05a_rf_parameters_to_outcomes.R
###############################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(tidymodels)
  library(ranger)
  library(vip)
  library(ggplot2)
  library(patchwork)
})

# ---------------------------------------------------------------------------
# Settings
# ---------------------------------------------------------------------------
DATA_PATH <- "proj3/dataset_for_rf.rds"
OUT_DIR   <- "proj3/figures"
SAVE_PATH <- "proj3/rf_params_to_outcomes.rds"

if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

# The 9 input parameters (features)
PARAM_FEATURES <- c("lac_0", "mu_0", "gam_0", "laa_0", "K_0",
                    "K_1", "mu_1", "laa_1", "lambda0")

# Community outcomes to predict (targets) — using _mean columns
TARGETS <- c(
  "island_p_mean", "island_a_mean",
  "connectance_mean",
  "largest_cmpnt_mean", "n_components_mean",
  "plant_degree_mean", "animal_degree_mean",
  "nonend_nltt_p_mean", "singleton_nltt_p_mean", "multi_nltt_p_mean",
  "nonend_nltt_a_mean", "singleton_nltt_a_mean", "multi_nltt_a_mean"
)

# ---------------------------------------------------------------------------
# Load data
# ---------------------------------------------------------------------------
if (!file.exists(DATA_PATH)) {
  stop(DATA_PATH, " not found. Run 04_build_dataset.R first.")
}

df_all <- readRDS(DATA_PATH)

# Keep only rows with complete feature data
feature_df <- df_all[, PARAM_FEATURES]
complete_idx <- complete.cases(feature_df)
df <- df_all[complete_idx, ]

cat("Data loaded:", nrow(df), "combos with complete features.\n")
cat("Targets to model:", length(TARGETS), "\n")

# ---------------------------------------------------------------------------
# Helper: fit RF for one target and collect diagnostics
# ---------------------------------------------------------------------------
fit_rf_target <- function(target_name, df, features, test_prop = 0.2) {

  # Drop rows with missing target
  df_sub <- df %>% filter(!is.na(!!sym(target_name)))
  if (nrow(df_sub) < 20) {
    warning("Too few observations for ", target_name, "; skipping.")
    return(NULL)
  }

  # Train/test split (random, not stratified because regression)
  set.seed(42)
  split_obj <- initial_split(df_sub, prop = 1 - test_prop)
  train_df  <- training(split_obj)
  test_df   <- testing(split_obj)

  # Recipe
  rf_recipe <- recipe(as.formula(paste(target_name, "~", paste(features, collapse = " + "))),
                      data = train_df) %>%
    step_zv(all_predictors()) %>%
    step_impute_median(all_numeric_predictors())

  # Model spec (regression)
  rf_spec <- rand_forest(
    mtry  = floor(length(features) / 3),
    trees = 1000,
    min_n = 5
  ) %>%
    set_engine("ranger", importance = "permutation") %>%
    set_mode("regression")

  # Workflow & fit
  rf_wf <- workflow() %>% add_recipe(rf_recipe) %>% add_model(rf_spec)
  rf_fit <- fit(rf_wf, data = train_df)

  # Predict on test
  test_pred <- predict(rf_fit, new_data = test_df) %>%
    bind_cols(test_df %>% select(!!sym(target_name)))

  # Metrics
  rmse_val <- rmse(test_pred, truth = !!sym(target_name), estimate = .pred)$.estimate
  rsq_val  <- rsq(test_pred,  truth = !!sym(target_name), estimate = .pred)$.estimate
  mae_val  <- mae(test_pred,  truth = !!sym(target_name), estimate = .pred)$.estimate

  # Variable importance
  rf_engine <- rf_fit %>% extract_fit_parsnip() %>% purrr::pluck("fit")
  vi_tbl <- vip::vi(rf_engine) %>%
    mutate(Target = target_name)

  list(
    target      = target_name,
    rf_fit      = rf_fit,
    rf_engine   = rf_engine,
    rmse        = rmse_val,
    rsq         = rsq_val,
    mae         = mae_val,
    n_train     = nrow(train_df),
    n_test      = nrow(test_df),
    vi          = vi_tbl,
    test_pred   = test_pred
  )
}

# ---------------------------------------------------------------------------
# Fit models for all targets
# ---------------------------------------------------------------------------
results_list <- list()

for (j in seq_along(TARGETS)) {
  tgt <- TARGETS[j]
  cat("\n[", j, "/", length(TARGETS), "] Fitting RF for:", tgt, "\n")

  res <- tryCatch(
    fit_rf_target(tgt, df, PARAM_FEATURES),
    error = function(e) {
      warning("Error fitting ", tgt, ": ", conditionMessage(e))
      NULL
    }
  )

  if (!is.null(res)) {
    cat("  RMSE =", round(res$rmse, 4),
        "| R2 =", round(res$rsq, 4),
        "| MAE =", round(res$mae, 4), "\n")
    results_list[[tgt]] <- res
  }
}

# ---------------------------------------------------------------------------
# Aggregate performance table
# ---------------------------------------------------------------------------
perf_tbl <- map_dfr(results_list, function(x) {
  tibble(
    Target = x$target,
    RMSE   = x$rmse,
    R2     = x$rsq,
    MAE    = x$mae,
    n_train = x$n_train,
    n_test  = x$n_test
  )
})

cat("\n=== Performance summary ===\n")
print(perf_tbl, n = Inf)

# ---------------------------------------------------------------------------
# Figure 1: Performance bar chart
# ---------------------------------------------------------------------------
p_perf <- ggplot(perf_tbl, aes(x = reorder(Target, R2), y = R2)) +
  geom_col(fill = "steelblue", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", R2)), hjust = -0.1, size = 3) +
  coord_flip(ylim = c(0, 1)) +
  labs(
    title = "RF predictive performance: parameters -> community outcomes",
    subtitle = "Hold-out R-squared",
    x = NULL, y = expression(R^2)
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

ggsave(file.path(OUT_DIR, "05a_rf_performance_r2.pdf"), p_perf,
       width = 8, height = 6, units = "in")

# ---------------------------------------------------------------------------
# Figure 2: Observed vs Predicted (best 6 targets by R2)
# ---------------------------------------------------------------------------
best_targets <- perf_tbl %>%
  arrange(desc(R2)) %>%
  slice_head(n = 6) %>%
  pull(Target)

plot_list <- list()
for (tgt in best_targets) {
  tp <- results_list[[tgt]]$test_pred
  p <- ggplot(tp, aes(x = !!sym(tgt), y = .pred)) +
    geom_point(alpha = 0.5, colour = "steelblue") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "red") +
    labs(
      title = tgt,
      x = "Observed", y = "Predicted"
    ) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold", size = 9))
  plot_list[[tgt]] <- p
}

p_obs_pred <- wrap_plots(plot_list, ncol = 3) +
  plot_annotation(
    title = "Observed vs Predicted (top 6 by R2)",
    theme = theme(plot.title = element_text(face = "bold", hjust = 0.5))
  )

ggsave(file.path(OUT_DIR, "05a_observed_vs_predicted.pdf"), p_obs_pred,
       width = 10, height = 7, units = "in")

# ---------------------------------------------------------------------------
# Figure 3: Variable importance (best 6 targets)
# ---------------------------------------------------------------------------
vi_all <- map_dfr(results_list[best_targets], "vi")

p_vi <- ggplot(vi_all, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(fill = "steelblue", width = 0.7) +
  facet_wrap(~ Target, ncol = 3, scales = "free_x") +
  labs(
    title = "Variable importance: parameters -> community outcomes",
    x = "Permutation importance", y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.major.y = element_blank(),
    strip.text = element_text(face = "bold", size = 9)
  )

ggsave(file.path(OUT_DIR, "05a_variable_importance.pdf"), p_vi,
       width = 12, height = 8, units = "in")

# ---------------------------------------------------------------------------
# Save results
# ---------------------------------------------------------------------------
saveRDS(
  list(
    results_list = results_list,
    performance  = perf_tbl,
    features     = PARAM_FEATURES,
    targets      = TARGETS
  ),
  SAVE_PATH
)

cat("\nSaved results to:", SAVE_PATH, "\n")
cat("Figures saved to:", OUT_DIR, "\n")
cat("\n--- 05a_rf_parameters_to_outcomes.R done ---\n")
