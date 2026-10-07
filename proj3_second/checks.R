



# Check why nonend_nltt_a has a high importance value  -----------------------------------------------------

# All 15 values are positive and approximately between 40% and 75%.
# The high mean value of laa_0 is therefore not being produced by one extreme split.
#

# read data
# permutaion_by_resample (9 parmas * 17 summaries * 3 repeats * 5 fold = 2295 obs)
permutation_by_resample <- results$permutation_by_resample

laa0_stability <- permutation_by_resample %>%
  filter(parameter == "laa_0") %>% # laa_0 has the largest r-square, we pull it out and check
  mutate(
    importance_relative_rmse_pct =
      importance_relative_rmse * 100
  ) %>%
  group_by(summary) %>%
  summarise(
    n_samples = n(),
    mean_pct = mean(
      importance_relative_rmse_pct,
      na.rm = TRUE
    ),
    median_pct = median(
      importance_relative_rmse_pct,
      na.rm = TRUE
    ),
    sd_between_resamples = sd(
      importance_relative_rmse_pct,
      na.rm = TRUE
    ),
    q10_pct = quantile(
      importance_relative_rmse_pct,
      0.10,
      na.rm = TRUE
    ),
    q90_pct = quantile(
      importance_relative_rmse_pct,
      0.90,
      na.rm = TRUE
    ),
    prop_positive = mean(
      importance_relative_rmse_pct > 0,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_pct))

# the most important predictor? (nonend_nltt_a)
top_summary_laa0 <- laa0_stability %>%
  slice_max(mean_pct, n=1, with_ties = FALSE) %>%
  pull(summary)

top_laa0_values <- permutation_by_resample %>%
  filter(parameter == "laa_0",
         summary == top_summary_laa0) %>%
  mutate(
      importance_relative_rmse_pct =
        100 * importance_relative_rmse,
      resample = paste0(
        "R", repeat_id,
        "-F", outer_fold
      )
   )

# Are they differ in different folds?
# looks okay
ggplot(
  top_laa0_values,
  aes(
    x = resample,
    y = importance_relative_rmse_pct,
    colour = factor(repeat_id)
  )
) +
  geom_hline(
    yintercept = 0,
    colour = "grey60",
    linewidth = 0.4
  ) +
  geom_point(size = 2.3) +
  labs(
    x = "Outer resample",
    y = "Increase in RMSE after permutation (%)",
    colour = "Repeat",
    title = top_laa0_summary
  ) +
  theme_classic(base_size = 10) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

# Or maybe the baseline rmse is too small?
ggplot(
  top_laa0_values,
  aes(
    x = baseline_rmse,
    y = importance_relative_rmse_pct,
    colour = factor(repeat_id)
  )
) +
  geom_point(size = 2.5) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    colour = "grey40",
    linewidth = 0.6
  ) +
  labs(
    x = "Baseline RMSE",
    y = "Increase in RMSE after permutation (%)",
    colour = "Repeat"
  ) +
  theme_classic(base_size = 10)


