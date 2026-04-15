
# Proportion of unconnected species, connectance, and richness across 16 scenarios


# Keep the position as old ones (look at the order of the path). X-axis (scenarios)

# Don't run. Outputs saved ------------------------------------------------


# Proportion scatter plot：
# (total unconnected species / community total species)
# (unconnected plant / plant total)
# (unconnected animal / animal total)

# 01 Deal with data. Read all paths containing data sets
all_paths <- c("results/low_cura/",
               "results/low_cur_high_a/",
               "results/low_cua_high_r/",
               "results/low_cu_high_ra/",
               "results/low_cra_high_u/",
               "results/low_cr_high_ua/",
               "results/low_ca_high_ur/",
               "results/low_c_high_ura/",
               "results/high_c_low_ura/",
               "results/high_ca_low_ur/",
               "results/high_cr_low_ua/",
               "results/high_cra_low_u/",
               "results/high_cu_low_ra/",
               "results/high_cua_low_r/",
               "results/high_cur_low_a/",
               "results/high_cura/")

all_cases <- c("low_cura",
               "low_cur_high_a",
               "low_cua_high_r",
               "low_cu_high_ra",
               "low_cra_high_u",
               "low_cr_high_ua",
               "low_ca_high_ur",
               "low_c_high_ura",
               "high_c_low_ura",
               "high_ca_low_ur",
               "high_cr_low_ua",
               "high_cra_low_u",
               "high_cu_low_ra",
               "high_cua_low_r",
               "high_cur_low_a",
               "high_cura")


# Placeholder for all results
final_results <- list()

for (i in 1:16) {

  # Get the file path and case
  the_path <- all_paths[i]
  the_case <- all_cases[i]

  # Process the data for the current scenario
  # `all_comm_df` is a data frame with columns: Type (None, Medium, High), Com_p, Com_a, disconnect_p, disconnect_a,
  # connectance
  all_comm_df <- get_NLH_sizeConnectance(the_path, the_case)

  # Calculate the the proportion of unconnected total, plant and animal species
  scenario_results <- all_comm_df  |>
    mutate(Proportion = (disconnect_p + disconnect_a) / (Com_p + Com_a),
           Richness = Com_p + Com_a,
           Pro_P = disconnect_p / Com_p,
           Pro_A = disconnect_a / Com_a,
           Scenario = paste0("Scenario_", i))

  # Store results
  final_results[[i]] <- scenario_results
}

# Combine all results into a single data frame
final_results_df <- bind_rows(final_results)

final_results_df$Scenario <- factor(final_results_df$Scenario, levels = paste0("Scenario_", 1:16))


#saveRDS(final_results_df, "results/DiscnectedProRichns.rds")


# Run. Plot ---------------------------------------------------------------
# Changed p1 as boxplot and p3 mean+-95CI (avoid mean+-sd with negative values)

# Load the data
#DiscnectedProRichns <- readRDS("results/DiscnectedProRichns.rds")

# Calculate the average Proportion
DiscnectedProRichns_avg <- DiscnectedProRichns |>
  group_by(Scenario, Type) |>
  summarise(
    mean_Proportion = mean(Proportion, na.rm = TRUE),
    se_Prop = sd(Proportion, na.rm = TRUE) / sqrt(n()), # standard error
    lower = pmax(mean_Proportion - 1.96 * se_Prop, 0), # CI lower bound (no less than 0)
    upper = pmin(mean_Proportion + 1.96 * se_Prop, 1), # CI upper bound (no more than 1)

    mean_Richness = mean(Richness, na.rm = TRUE),
    se_Richness = sd(Richness, na.rm = TRUE) / sqrt(n()), # standard error
    lower_Richness = pmax(mean_Richness - 1.96 * se_Richness, 0), # CI lower bound (no less than 0)
    upper_Richness = mean_Richness + 1.96 * se_Richness, # CI upper bound (no more than 1)

    mean_Pro_P = mean(Pro_P, na.rm = TRUE),
    se_Pro_P = sd(Pro_P, na.rm = TRUE) / sqrt(n()), # standard error
    lower_Pro_P = pmax(mean_Pro_P - 1.96 * se_Pro_P, 0), # CI lower bound (no less than 0)
    upper_Pro_P = pmin(mean_Pro_P + 1.96 * se_Pro_P, 1), # CI upper bound (no more than 1)

    mean_Pro_A = mean(Pro_A, na.rm = TRUE),
    se_Pro_A = sd(Pro_A, na.rm = TRUE) / sqrt(n()), # standard error
    lower_Pro_A = pmax(mean_Pro_A - 1.96 * se_Pro_A, 0), # CI lower bound (no less than 0)
    upper_Pro_A = pmin(mean_Pro_A + 1.96 * se_Pro_A, 1), # CI upper bound (no more than 1)

    mean_Com_p = mean(Com_p, na.rm = TRUE),
    se_Com_p = sd(Com_p, na.rm = TRUE) / sqrt(n()), # standard error
    lower_Com_p = pmax(mean_Com_p - 1.96 * se_Com_p, 0), # CI lower bound (no less than 0)
    upper_Com_p = pmin(mean_Com_p + 1.96 * se_Com_p, 1), # CI upper bound (no more than 1)
  ) |>
  ungroup()



p1 <- ggplot(DiscnectedProRichns, aes(x = Scenario, y = Proportion, fill = Type)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA, na.rm = TRUE) +
  scale_fill_manual(values = c("green3", "blue", "red")) +
  scale_y_continuous(limits = c(0, 1.0), breaks = seq(0, 1, 0.2)) +
  labs(tag = "(a)",
       x = "",
       y = "Proportion of Unconnected Species",
       fill = "Mutualism Effects") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_blank())

p2 <- ggplot(DiscnectedProRichns, aes(x = Scenario, y = connectance, fill = Type)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA, na.rm = TRUE) +
  scale_fill_manual(values = c("green3", "blue", "red")) +
  labs(x = " ",
       y = "Connectance") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        legend.position = "none")

p3 <- ggplot(DiscnectedProRichns_avg, aes(x = mean_Richness, y = mean_Proportion, color = Type)) +
  geom_point(size = 1, alpha = 0.8) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 15) +
  geom_errorbarh(aes(xmin = lower_Richness, xmax = upper_Richness), height = 0.01) +
  scale_y_continuous(limits = c(0, 1.0), breaks = seq(0, 1, 0.2)) +
  scale_color_manual(values = c("green3", "blue", "red")) +
  labs(x = "Species Richness", y = "Proportion of Unconnected Species", color = "Mutualism Effects") +
  theme_bw() +
  theme(legend.position = "none")

p <- p1 + p2 + p3 + patchwork::plot_layout(ncol = 1) & theme(legend.position = "right")


#ggsave("figures/new_P123.pdf", p,
#       width = 6, height = 8, units = "in", dpi = 300)


