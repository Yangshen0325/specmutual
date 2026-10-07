###############################################################################
# Part I: figures and scientific interpretation for inverse inference.
#
# This script reads the out-of-fold results from
# proj3_second/12_part1_inverse_inference.R. It does not fit or tune models.
#
# Run from the package root:
#   Rscript proj3_second/13_part1_inverse_inference_figures.R
###############################################################################

rm(list = ls())

required_packages <- c("ggplot2", "patchwork")
missing_packages <- required_packages[!vapply(
  required_packages, requireNamespace, logical(1), quietly = TRUE
)]
if (length(missing_packages)) {
  stop("Missing required package(s): ", paste(missing_packages, collapse = ", "))
}


# Settings ----------------------------------------------------------------

output_dir <- file.path("proj3_second", "part1_inverse_inference")
results_path <- file.path(output_dir, "part1_inverse_inference_results.rds")
correlation_path <- file.path(
  "proj3_second", "final_analysis_audit",
  "final_summary_spearman_correlations.csv"
)

if (!file.exists(results_path)) {
  stop(results_path, " not found. Run 12_part1_inverse_inference.R first.")
}

results <- readRDS(results_path)

# `performance`: for each parameter(9) * each predictor_set (plus null model, 9) * design and original (2)
# It's mean, std across 3 repeats.
performance <- results$performance_summary

predictions <- results$oof_predictions
ablation <- results$group_ablation_summary

# baseline_rmse= rmse between observed_design and predicted_design;
# each predictor was permuted 5 times, and get the mean;
# 9 parameters * 5 permuted times * 3 repeats;
# refer to 12_part1_inverse_inference line 655.
importance <- results$permutation_importance_stability

boundary_bias <- results$boundary_bias
mechanism_associations <- results$mechanism_diagnostic_associations
mechanism_strata <- results$mechanism_stratified_recovery
correlations <- read.csv(correlation_path, stringsAsFactors = FALSE)


# Labels and colors -------------------------------------------------------

parameter_names <- c(
  "lac_0", "mu_0", "gam_0", "laa_0", "K_0",
  "K_1", "mu_1", "laa_1", "lambda0"
)

# Keep labels consistent with the old chapter figures.
parameter_labels <- c(
  lac_0 = "lambda[G0]^c",
  mu_0 = "mu[G0]",
  gam_0 = "gamma[G0]",
  laa_0 = "lambda[G0]^a",
  K_0 = "K[G0]",
  K_1 = "K[G1]",
  mu_1 = "mu[G1]",
  laa_1 = "lambda[G1]^a",
  lambda0 = "lambda[G0]"
)

summary_labels <- c(
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

summary_order <- names(summary_labels)
summary_group_colors <- c(
  "nLTT" = "#8C9FC7",
  "Richness" = "#F9CF7C",
  "Network" = "#E49B8F"
)
parameter_group_colors <- c(
  "Intrinsic" = "#7F7F7F",
  "Mutualism-related" = "#D76A5B"
)

summary_group_order <- c("Richness", "Network", "nLTT")

base_theme <- ggplot2::theme_bw(base_size = 12, base_family = "Arial") +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(color = "#E8E8E8", linewidth = 0.3),
    strip.background = ggplot2::element_rect(fill = "white", color = "#B5B5B5"),
    strip.text = ggplot2::element_text(face = "bold"),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold", size = 14),
    plot.margin = ggplot2::margin(8, 8, 8, 8)
  )


# Figure 1: parameter recoverability -------------------------------------

# We use all ("ALL") summaries to predict log(params) "design"
recoverability <- performance[
  performance$predictor_set == "All" & performance$evaluation_scale == "design",
  , drop = FALSE
]

# If we use the raw scale
# recoverability <- performance[
#   performance$predictor_set == "All" & performance$evaluation_scale == "original",
#   , drop = FALSE
# ]



# double check
recoverability <- recoverability[match(parameter_names, recoverability$parameter), ]

recoverability$parameter_label <- factor(
  parameter_labels[recoverability$parameter],
  levels = parameter_labels[recoverability$parameter[order(recoverability$rsq_mean)]]
)

# e.g., rsq_mean is the mean value across 3 CV repeats
p_recoverability <- ggplot2::ggplot(
  recoverability,
  ggplot2::aes(x = rsq_mean, y = parameter_label, fill = parameter_group)
) +
  ggplot2::geom_vline(xintercept = 0, color = "grey35", linewidth = 0.45) +
  ggplot2::geom_col(width = 0.7) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      xmin = rsq_mean - rsq_sd,
      xmax = rsq_mean + rsq_sd
    ),
    width = 0.18, orientation = "y", linewidth = 0.45
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%.2f", rsq_mean)),
    hjust = ifelse(recoverability$rsq_mean >= 0, -0.5, 1.35), size = 3.2
  ) +
  ggplot2::scale_y_discrete(labels = scales::label_parse()) +
  ggplot2::scale_fill_manual(values = parameter_group_colors) +
  ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.08, 0.14))) +
  ggplot2::labs(
    x = expression("Cross-validated " * R^2),
    y = NULL, fill = NULL,
    title = "(a) Recoverability of parameters"
   #title = "Recoverability of model parameters from island community summaries (RAW)"
  ) +
  base_theme +
  theme(plot.title = element_text(hjust = 0.5))


# ggplot2::ggsave(
#   file.path(output_dir, "part1_parameter_recoverability.png"),
#   p_recoverability, width = 7.3, height = 5.3, dpi = 400, bg = "white"
# )
ggplot2::ggsave(
  file.path("proj3_second/figures/part1_parameter_recoverability.pdf"),
  p_recoverability, width = 7.3, height = 5.3,
  device = grDevices::cairo_pdf, bg = "white"
)


# Figure 2: out-of-fold observed versus predicted ------------------------

# Each simulation has one OOF prediction per repeat. Plot their mean only to
# avoid drawing the same observed value three times. This is not simulation
# averaging and no fitted training prediction is used.
full_prediction <- predictions[predictions$predictor_set == "All", ]

plot_groups <- split(
  full_prediction,
  interaction(full_prediction$parameter, full_prediction$simulation_id, drop = TRUE)
) # e.g., lac_0.1, lac_0.2, ..., 9 parameters * 888 simulations = 7992 elements

observed_predicted_plot_data <- do.call(rbind, lapply(plot_groups, function(x) {
  data.frame(
    parameter = x$parameter[1],
    parameter_group = x$parameter_group[1],
    simulation_id = x$simulation_id[1],
    observed_design = x$observed_design[1], # values on logarithmic scale
    predicted_design = mean(x$predicted_design), # mean across 3 CV repeats
    observed_original = x$observed_original[1],
    predicted_original = mean(x$predicted_original),
    stringsAsFactors = FALSE
  )
}))
row.names(observed_predicted_plot_data) <- NULL

observed_predicted_plot_data$parameter_label <- factor(
  parameter_labels[observed_predicted_plot_data$parameter],
  levels = parameter_labels[parameter_names]
)

# write.csv(
#   observed_predicted_plot_data,
#   file.path(output_dir, "part1_oof_predictions_mean_across_repeats_for_plot.csv"),
#   row.names = FALSE
# )

#head(observed_predicted_plot_data)
p_observed_predicted <- ggplot2::ggplot(
  observed_predicted_plot_data,
  ggplot2::aes(x = observed_original, y = predicted_original)
) +
  ggplot2::geom_abline(slope = 1, intercept = 0, color = "grey35", linewidth = 0.55) +
  ggplot2::geom_point(alpha = 0.34, size = 0.85, color = "#DDA0DD") +
  ggplot2::facet_wrap(
    ~parameter_label, scales = "free", ncol = 3,
    labeller = ggplot2::label_parsed
  ) +
  ggplot2::labs(
    x = "Observed parameter",
    y = "Predicted parameter",
    title = "Observed versus predicted model parameters"
  ) +
  theme_bw(base_size = 12, base_family = "Arial") +
  theme(
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(color = "#E8E8E8", linewidth = 0.3),
    strip.background = ggplot2::element_rect(fill = "#B5B5B5", color = "#B5B5B5"),
    strip.text = ggplot2::element_text(face = "bold"),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold", hjust = 0.5)
  )



# ggplot2::ggsave(
#   file.path(output_dir, "part1_observed_vs_predicted_oof.png"),
#   p_observed_predicted, width = 9.2, height = 9.0, dpi = 400, bg = "white"
# )
ggplot2::ggsave(
  file.path(output_dir, "part1_observed_vs_predicted_oof.pdf"),
  p_observed_predicted, width = 9.2, height = 9.0,
  device = grDevices::cairo_pdf, bg = "white"
)


# Figure 1 + picked Figure 2 ----------------------------------------------

p_recoverability_combine <- ggplot2::ggplot(
  recoverability,
  ggplot2::aes(x = rsq_mean, y = parameter_label, fill = parameter_group)
) +
  ggplot2::geom_vline(xintercept = 0, color = "grey35", linewidth = 0.45) +
  ggplot2::geom_col(width = 0.7) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      xmin = rsq_mean - rsq_sd,
      xmax = rsq_mean + rsq_sd
    ),
    width = 0.18, orientation = "y", linewidth = 0.45
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%.2f", rsq_mean)),
    hjust = ifelse(recoverability$rsq_mean >= 0, -0.5, 1.25), size = 2
  ) +
  ggplot2::scale_y_discrete(labels = scales::label_parse()) +
  ggplot2::scale_fill_manual(values = parameter_group_colors) +
  ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.08, 0.14))) +
  ggplot2::labs(
    x = expression("Cross-validated " * R^2),
    y = NULL, fill = NULL,
    title = "(a) Recoverability of parameters"
    #title = "Recoverability of model parameters from island community summaries (RAW)"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))

ggsave(
  file.path(output_dir, "figure1_a.pdf"),
  p_recoverability_combine, width = 9.2, height = 9.0,
  device = grDevices::cairo_pdf, bg = "white"
)

make_parameter_plot <- function(parameter_name, panel_title) {

  plot_data <- subset(
    observed_predicted_plot_data,
    parameter == parameter_name &
      is.finite(observed_original) &
      is.finite(predicted_original)
  )

  axis_limits <- range(
    c(plot_data$observed_original, plot_data$predicted_original)
  )

  ggplot(plot_data, aes(x = observed_original, y = predicted_original)) +
    geom_abline(
      intercept = 0, slope = 1,
      colour = "grey35", linewidth = 0.45
    ) +
    geom_point(
      colour = "#DDA0DD",
      size = 1.25, alpha = 0.5
    ) +
    scale_x_continuous(breaks = scales::breaks_pretty(n = 4)) +
    scale_y_continuous(breaks = scales::breaks_pretty(n = 4)) +
    coord_fixed(
      ratio = 1,
      xlim = axis_limits,
      ylim = axis_limits
    ) +
    labs(
      title = panel_title,
      x = "Observed parameter",
      y = "Predicted parameter"
    ) +
    theme_bw(base_size = 11) +
    theme(
      plot.title = element_text(hjust = 0.5),
      #axis.title = element_text(size = 10),
      axis.text = element_text(colour = "black")
    )
}

# Individual plots with centred titles
p_laa0 <- make_parameter_plot(
  "laa_0", expression("(b)" ~ lambda[G0]^a)
)

p_K1 <- make_parameter_plot(
  "K_1", expression("(c)" ~ K[G1])
)

p_laa1 <- make_parameter_plot(
  "laa_1", expression("(d)" ~ lambda[G1]^a)
)

p_combined <- (
  (p_recoverability_combine | p_laa0) / (p_K1 | p_laa1)
)

p_combined

ggsave(
  file.path(output_dir,"rf_obs_pred_combined.pdf"),
  plot = p_combined,
  width = 8, height = 8, units = "in"
)


ggsave( file.path(output_dir, "figure1_b.pdf"),
        p_laa0, width = 3, height = 3,
        device = grDevices::cairo_pdf, bg = "white")
ggsave( file.path(output_dir, "figure1_c.pdf"),
        p_K1, width = 3, height = 3,
        device = grDevices::cairo_pdf, bg = "white")
ggsave( file.path(output_dir, "figure1_d.pdf"),
        p_laa1, width = 3, height = 3,
        device = grDevices::cairo_pdf, bg = "white")



# Figure 3: parameter-by-summary-group recoverability --------------------

# "Richness" means using richenss metrics only to predict parameters
group_performance <- performance[
  performance$predictor_set %in% c("Richness", "Network", "nLTT") &
    performance$evaluation_scale == "design",
  , drop = FALSE
]
group_performance$parameter_label <- factor(
  parameter_labels[group_performance$parameter],
  levels = rev(parameter_labels[parameter_names])
)
group_performance$predictor_set <- factor(
  group_performance$predictor_set,
  levels = c("Richness", "Network", "nLTT")
)

p_group_heatmap <- ggplot2::ggplot(
  group_performance,
  ggplot2::aes(x = predictor_set, y = parameter_label, fill = rsq_mean)
) +
  ggplot2::geom_tile(color = "white", linewidth = 0.7) +
  ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", rsq_mean)), size = 3.4) +
  ggplot2::scale_y_discrete(labels = scales::label_parse()) +
  ggplot2::scale_fill_gradient2(
    low = "#D6E1EE", mid = "white", high = "#3F739A",
    midpoint = 0, name = expression(R^2),
    limits = c(-0.6, NA)
  ) +
  ggplot2::labs(
    x = "Summary group", y = NULL,
    title = "(a) Prediction using individual summary group"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom",
        plot.title = element_text(face ="bold", hjust = 0.5, size = 11),
        plot.margin = ggplot2::margin(8, 8, 8, 8))

# ggplot2::ggsave(
#   file.path(output_dir, "part1_summary_group_recoverability_heatmap.png"),
#   p_group_heatmap, width = 6.6, height = 5.8, dpi = 400, bg = "white"
# )
# ggplot2::ggsave(
#   file.path(output_dir, "part1_summary_group_recoverability_heatmap.pdf"),
#   p_group_heatmap, width = 6.6, height = 5.8,
#   device = grDevices::cairo_pdf, bg = "white"
# )


# Figure 4: primary group-ablation result --------------------------------

ablation$parameter_label <- factor(
  parameter_labels[ablation$parameter],
  levels = rev(parameter_labels[parameter_names])
)
ablation$summary_group <- factor(
  ablation$summary_group, levels = c("Richness", "Network", "nLTT")
)

p_ablation <- ggplot2::ggplot(
  ablation,
  ggplot2::aes(
    x = rsq_loss_mean, y = parameter_label,
    color = summary_group, shape = summary_group
  )
) +
  ggplot2::geom_vline(xintercept = 0, color = "grey45", linewidth = 0.45) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      xmin = rsq_loss_mean - rsq_loss_sd,
      xmax = rsq_loss_mean + rsq_loss_sd
    ),
    width = 0, orientation = "y", linewidth = 0.5,
    position = ggplot2::position_dodge(width = 0.55)
  ) +
  ggplot2::geom_point(
    size = 2.5, position = ggplot2::position_dodge(width = 0.55)
  ) +
  ggplot2::scale_y_discrete(labels = scales::label_parse()) +
  ggplot2::scale_color_manual(values = summary_group_colors) +
  ggplot2::scale_shape_manual(values = c(Richness = 16, Network = 17, nLTT = 15)) +
  ggplot2::labs(
    x = expression("Loss in out-of-fold " * R^2),
    y = NULL, color = NULL, shape = NULL,
    title = "(b) Change in prediction after omitting each group"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom",
        plot.title = element_text(face ="bold", hjust = 0.5, size = 11),
        plot.margin = ggplot2::margin(8, 8, 8, 8))


p_indivi_omit <- p_group_heatmap + p_ablation

ggplot2::ggsave(
  "proj3_second/figures/p_indivi_omit.pdf",
  p_indivi_omit, width = 8, height = 6, unit = "in", dpi = 300
)

# ggplot2::ggsave(
#   file.path(output_dir, "part1_group_ablation.png"),
#   p_ablation, width = 7.7, height = 5.8, dpi = 400, bg = "white"
# )
# ggplot2::ggsave(
#   file.path(output_dir, "part1_group_ablation.pdf"),
#   p_ablation, width = 7.7, height = 5.8,
#   device = grDevices::cairo_pdf, bg = "white"
# )


# Figure 5: stable held-out permutation importance -----------------------

# Display importance relative to the most important summary for each
# parameter. Colors indicate group membership; group totals are not computed.

# When used "All" to predict parameters and test the importance for each predictors,
# we permuted the each predictor 5 times and computed the average value.
# e.g, importance_delta_rmse = mean(permuted_rmse) - baseline_rmse
# baseline_rmse: between observed_design and predicted_design

# importance_delta_rmse_mean: grouped the importance by parameters and predictors,
# there 15 times resample of a combo of one parameter and one predictor (3 repeats and 5 folds).
# `importance_delta_rmse_mean` does the average.
# importance_relative_rmse_mean: permuted_importance / baseline_importance - 1

importance$importance_nonnegative <- pmax(importance$importance_delta_rmse_mean, 0)

importance_plot <- importance %>%
  filter(!parameter %in% c("laa_1", "mu_1")) %>%
  mutate(
    importance_nonnegative = pmax(
      importance_delta_rmse_mean,
      0
    )
  )

# Maximum importance among the 17 summaries for each parameter
max_by_parameter <- ave(
  importance_plot$importance_nonnegative,
  importance_plot$parameter,
  FUN = max
)

importance_plot <- importance_plot |>
  dplyr::mutate(
    importance_normalized = dplyr::if_else(
      max_by_parameter > 0,
      importance_nonnegative / max_by_parameter,
      0
    ),
    parameter_label = factor(
      parameter_labels[parameter],
      levels = rev(
        parameter_labels[
          parameter_names[
            !parameter_names %in% c("laa_1", "mu_1")
          ]
        ]
      )
    ),
    summary_label = factor(
      summary_labels[summary],
      levels = rev(summary_labels[summary_order])
    )
  )


p_importance <- ggplot2::ggplot(
  importance_plot,
  ggplot2::aes(x = importance_normalized, y = summary_label, fill = summary_group)
) +
  ggplot2::geom_col(width = 0.72) +
  ggplot2::facet_wrap(~parameter_label, ncol = 3, labeller = ggplot2::label_parsed) +
  ggplot2::scale_fill_manual(values = summary_group_colors) +
  ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by=0.25), expand = ggplot2::expansion(mult = c(0, 0.03))) +
  ggplot2::labs(
    x = "Permutation importance (normalized within parameter)",
    y = NULL, fill = NULL,
    title = "Variable importance for each parameter"
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom",
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(color = "#E8E8E8", linewidth = 0.3),
        strip.text = ggplot2::element_text(face = "bold"),
        axis.text.y = ggplot2::element_text(size = 7.2),
        plot.title = element_text(face = "bold", hjust = 0.5),
        panel.spacing.x = unit(1.0, "lines"),
        plot.margin = ggplot2::margin(8, 8, 8, 8)
        )


# ggplot2::ggsave(
#   file.path(output_dir, "part1_permutation_importance.png"),
#   p_importance, width = 11.3, height = 11.0, dpi = 400, bg = "white"
# )
ggplot2::ggsave(
  file.path("proj3_second/figures/part1_permutation_importance.pdf"),
  p_importance, width = 11.3, height = 11.0,
  device = grDevices::cairo_pdf, bg = "white"
)


# plot the heat map

## Pull out r^2 used "All" predictors
performance_rsq_all <- performance %>%
  filter(predictor_set == "All" & performance$evaluation_scale == "design") %>%
  select(parameter, rsq_mean)

# combine it with importance and rearrange the levels of parameter, with higher r^2
# moved forward

heat_df <- importance %>%
  mutate(
    summary_group = factor(
      summary_group,
      levels = summary_group_order
    ),
    summary_label = factor(
      summary_labels[importance$summary],
      levels = rev(summary_labels[summary_order])
    ),
    parameter_label = factor(
      parameter_labels[importance$parameter],
      levels = c("lambda[G0]^a", "gamma[G0]", "mu[G0]", "K[G0]",
                 "lambda[G0]^c", "K[G1]", "lambda[G0]", "mu[G1]", "lambda[G1]^a")
    ),
    importance_relative_rmse_pct =
      100 * importance_relative_rmse_mean
  ) %>%
  left_join(
    performance_rsq_all,
    by = "parameter"
  )

parameter_axis_labels <- heat_df %>%
  distinct(parameter_label, rsq_mean) %>%
  mutate(
    parameter_label = as.character(parameter_label),
    axis_label = sprintf(
      "atop(%s, R^2 == %.2f)",
      parameter_label,
      rsq_mean
    )
  ) %>%
  {
    setNames(.$axis_label, .$parameter_label)
  }

importance_limit <- max(
  abs(heat_df$importance_relative_rmse_mean),
  na.rm = TRUE
)

p_heat_main <- ggplot(heat_df, aes(x = parameter_label, y = summary_label,
                                   fill = importance_relative_rmse_pct)) +
  geom_tile(colour = "white", linewidth = 0.25) +
  scale_x_discrete(
    labels = function(x) {
      parse(text = unname(parameter_axis_labels[x]))
    },
    position = "top",
    drop = FALSE
  ) +
  #scale_x_discrete(labels = scales::label_parse(), position = "top", drop = FALSE) +
  scale_fill_gradient2(
    low = "#4575B4",
    mid = "#FFF7BC",
    high = "#B2182B",
    midpoint = 0,
    #limits = c(-importance_limit, importance_limit),
    #na.value = "grey85",
    name = "Increase in RMSE\nafter permutation (%)"
  ) +
  # scale_fill_gradient(low = "lightyellow", high = "red",
  #                     limits = c(0, 2), name = "Permuted\nimportance") +
  labs(x = NULL, y = NULL) +
  theme_classic(base_size = 9) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 0, vjust = 0),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.position = "right"
  )


group_strip <- heat_df %>%
  distinct(summary_group, summary_label)
# Draw group annotation separately so the main fill scale remains continuous.
p_group <- ggplot(group_strip, aes(x = 1, y = summary_label, fill = summary_group)) +
  geom_tile(width = 0.8, colour = "white", linewidth = 0.25) +
  scale_fill_manual(values = summary_group_colors, name = "Summary group") +
  labs(x = NULL, y = NULL) +
  theme_classic(base_size = 9) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.position = "bottom",
    plot.margin = margin(5.5, 0, 5.5, 5.5)
  )

p_heat <- p_group + p_heat_main +
  plot_layout(widths = c(0.18, 3), guides = "collect") &
  theme(legend.position = "bottom")




# Redundancy screen for interpretation -----------------------------------

# A summary is flagged as strongly redundant when it has |rho| >= 0.80 with
# another summary. This is descriptive; correlated predictors can share or
# exchange permutation importance.
upper_correlations <- correlations[
  match(correlations$variable_x, summary_order) <
    match(correlations$variable_y, summary_order),
  , drop = FALSE
]
redundant_pairs <- upper_correlations[abs(upper_correlations$rho) >= 0.80, ]
redundant_pairs <- redundant_pairs[order(-abs(redundant_pairs$rho)), ]
write.csv(
  redundant_pairs,
  file.path(output_dir, "part1_strongly_correlated_summary_pairs.csv"),
  row.names = FALSE
)


# Concise scientific interpretation -------------------------------------

full_perf <- performance[
  performance$predictor_set == "All" & performance$evaluation_scale == "design",
]
full_perf <- full_perf[match(parameter_names, full_perf$parameter), ]

top_importance <- do.call(rbind, lapply(parameter_names, function(parameter) {
  x <- importance[importance$parameter == parameter, ]
  x <- x[order(-x$importance_delta_rmse_mean), ]
  x[seq_len(min(3L, nrow(x))), ]
}))

interpret_recoverability <- function(rsq, spearman) {
  if (is.na(rsq)) return("could not be assessed")
  if (rsq >= 0.60) return("moderately strong recovery")
  if (rsq >= 0.40) return("moderate recovery")
  if (rsq >= 0.20) return("limited recovery")
  if (rsq > 0) return("weak recovery")
  "no recovery beyond the mean-prediction benchmark"
}

parameter_lines <- character(0)
interpretation_rows <- list()
for (parameter in parameter_names) {
  perf <- full_perf[full_perf$parameter == parameter, ]
  abl <- ablation[ablation$parameter == parameter, ]
  strongest_group <- abl$summary_group[which.max(abl$rsq_loss_mean)]
  strongest_loss <- max(abl$rsq_loss_mean)
  top <- top_importance[top_importance$parameter == parameter, ]
  stable_top <- top[
    top$importance_delta_rmse_mean > 0 &
      top$positive_importance_frequency >= 0.60,
    , drop = FALSE
  ]
  top_text <- if (nrow(stable_top)) {
    paste(summary_labels[stable_top$summary], collapse = ", ")
  } else {
    "none with stable positive held-out importance"
  }
  lower_bias <- boundary_bias[
    boundary_bias$parameter == parameter &
      boundary_bias$evaluation_scale == "design" &
      boundary_bias$range_band == "lower_10_percent", "bias_predicted_minus_observed"
  ]
  upper_bias <- boundary_bias[
    boundary_bias$parameter == parameter &
      boundary_bias$evaluation_scale == "design" &
      boundary_bias$range_band == "upper_10_percent", "bias_predicted_minus_observed"
  ]
  boundary_text <- if (
    mean(lower_bias) > 0 && mean(upper_bias) < 0
  ) {
    "Predictions shrink upward at the lower boundary and downward at the upper boundary."
  } else {
    "Boundary bias is not a simple symmetric shrinkage pattern."
  }

  special_text <- ""
  if (parameter == "laa_1") {
    d_zero <- mechanism_associations[
      mechanism_associations$parameter == "laa_1" &
        mechanism_associations$diagnostic == "final_D_nonzero_fraction",
    ]
    special_text <- paste0(
      " Terminal mismatch D is zero in ",
      sprintf("%.0f%%", 100 * d_zero$diagnostic_zero_fraction),
      " of usable simulations, so the parameter often has no realized substrate on which to act."
    )
  }
  if (parameter == "mu_1") {
    d_info <- mechanism_associations[
      mechanism_associations$parameter == "mu_1" &
        mechanism_associations$diagnostic == "mu1d_all_frac_informative",
    ]
    special_text <- paste0(
      " Raw mu_1 has only a moderate rank association with realized informative exposure (rho = ",
      sprintf("%.2f", d_info$spearman_raw_parameter_vs_diagnostic), ")."
    )
  }
  if (parameter == "K_1") {
    d_info <- mechanism_associations[
      mechanism_associations$parameter == "K_1" &
        mechanism_associations$diagnostic == "K1d_over_K0_immigration_mean",
    ]
    special_text <- paste0(
      " Raw K_1 corresponds more consistently to immigration-network exposure (rho = ",
      sprintf("%.2f", d_info$spearman_raw_parameter_vs_diagnostic),
      "), but this does not make the endpoint summaries sufficient for precise recovery."
    )
  }

  sentence <- paste0(
    "**", parameter, ".** ",
    interpret_recoverability(perf$rsq_mean, perf$spearman_mean),
    " (out-of-fold R-squared = ", sprintf("%.2f", perf$rsq_mean),
    "; Spearman rho = ", sprintf("%.2f", perf$spearman_mean),
    "; calibration slope = ", sprintf("%.2f", perf$calibration_slope_mean),
    "). Omitting ", strongest_group, " caused the largest mean R-squared loss (",
    sprintf("%.2f", strongest_loss), "). The held-out permutation evidence identified ",
    top_text, ". ", boundary_text, special_text
  )
  parameter_lines <- c(parameter_lines, sentence, "")
  interpretation_rows[[parameter]] <- data.frame(
    parameter = parameter,
    parameter_group = perf$parameter_group,
    rsq = perf$rsq_mean,
    spearman = perf$spearman_mean,
    calibration_intercept = perf$calibration_intercept_mean,
    calibration_slope = perf$calibration_slope_mean,
    strongest_ablation_group = strongest_group,
    strongest_ablation_rsq_loss = strongest_loss,
    top_three_summaries = top_text,
    interpretation = gsub("\\*\\*", "", sentence),
    stringsAsFactors = FALSE
  )
}

interpretation_table <- do.call(rbind, interpretation_rows)
row.names(interpretation_table) <- NULL
write.csv(
  interpretation_table,
  file.path(output_dir, "part1_parameter_scientific_interpretation.csv"),
  row.names = FALSE
)

report <- c(
  "# Part I: inverse inference from island community patterns",
  "",
  paste0("Report generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
  "",
  "## Analysis design",
  "",
  "The analysis used the 888 completed continuous Latin-hypercube simulations. Each parameter was modeled separately on its sampling scale: natural log for log-sampled parameters and the original scale for K_0 and laa_1. Three repeats of five-fold outer cross-validation were shared across every parameter and predictor-set comparison. Hyperparameters were selected using random-forest out-of-bag error within each outer training set. All reported performance, calibration, boundary bias, figures, and permutation importance use outer held-out predictions; fitted training predictions were not used.",
  "",
  "Random forests were compared against training-set mean and median null predictors. Structurally undefined network values were handled by ranger's learned missing-value routing rather than complete-case deletion. Model-internal exposure, event, hazard, runtime, status, seed, and safety-cap diagnostics were excluded from primary predictors.",
  "",
  "## Overall result",
  "",
  paste0(
    "Recoverability was highest for laa_0 (R-squared = ",
    sprintf("%.2f", full_perf$rsq_mean[full_perf$parameter == "laa_0"]),
    "), followed by gam_0, mu_0, and K_0. lac_0 was only partly recovered. ",
    "K_1 and lambda0 were weakly recovered, while mu_1 and laa_1 did not outperform the mean-prediction benchmark. Calibration slopes were below one for every parameter, showing systematic shrinkage of out-of-fold predictions toward the center of the sampled range."
  ),
  "",
  "Group ablation is the primary comparison of information content. nLTT supplied the clearest unique information for several intrinsic diversification/demographic rates, whereas Network summaries supplied the main unique information for K_1. Negative ablation values indicate redundancy or finite-sample noise: removing a group slightly improved prediction and should not be read as a negative ecological effect.",
  "",
  "## Parameter-specific interpretation",
  "",
  parameter_lines,
  "## Importance, redundancy, causality, and identifiability",
  "",
  "Held-out permutation importance measures predictive dependence of a fitted model on a summary, not ecological causality. Correlated summaries can substitute for one another, so low individual importance may reflect redundancy rather than absence of information. Conversely, a high importance value does not establish that a summary is sufficient to estimate a parameter. The strong-correlation table should therefore be read beside the importance table.",
  "",
  "Parameter recoverability is predictive performance for one response at a time. It is not exact mechanism identification and does not prove structural parameter identifiability. Poor recovery can arise from stochastic simulation variation, compensating parameter combinations, saturated or inactive mechanisms, and loss of information when complete trajectories are compressed into endpoint summaries. Exact mechanism identification is reserved for the separate structural-anchor analysis.",
  "",
  "## Scope limitation",
  "",
  "These results are conditional on completion of the 888 LHS simulations. Twelve high-richness, parameter-dependent simulations hit the matrix-size safety cap. Therefore, the present analysis does not support claims across the entire originally declared parameter design without successful reruns or an explicitly restricted target domain."
)
writeLines(report, file.path(output_dir, "PART1_INVERSE_INFERENCE_REPORT.md"))


# Figure captions --------------------------------------------------------

captions <- c(
  "Figure 1. Recoverability of nine model parameters from 17 observable island community summaries. Bars show mean out-of-fold R-squared across three repetitions of five-fold cross-validation; error bars show the standard deviation across repetitions. Parameters were modeled on their sampling scales.",
  "",
  "Figure 2. Observed versus out-of-fold predicted parameters on the original biological scales. Each point is one completed continuous-LHS simulation; predictions are averaged across the three independent outer-cross-validation repeats. The diagonal is one-to-one agreement. No fitted training predictions are shown.",
  "",
  "Figure 3. Parameter recoverability from Richness, Network, or nLTT summaries alone. Cell values are mean out-of-fold R-squared across repeated cross-validation using identical folds.",
  "",
  "Figure 4. Group-ablation evidence for unique information content. Points show the loss in out-of-fold R-squared when each complete summary group is omitted from the full 17-summary model; positive values indicate that the omitted group supplied unique predictive information. Error bars show the standard deviation across repeats.",
  "",
  "Figure 5. Held-out permutation importance for the full random-forest models. Importance is the increase in outer-assessment RMSE after permuting one summary and is normalized to the largest nonnegative importance within each parameter. Colors denote Richness, Network, and nLTT groups; individual importance values are not added to create group importance."
)
writeLines(captions, file.path(output_dir, "PART1_FIGURE_CAPTIONS.txt"))

cat("Part I figures and interpretation written to:", output_dir, "\n")
