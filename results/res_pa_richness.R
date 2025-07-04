
## Plot the species richness on the island across 16 combinations of parameters


# Necessary packages
library(tidyverse)
library(dplyr)



# Don't run. Results saved ------------------------------------------------


all_paths <- c("results/low_cu_high_ra/",
               "results/low_c_high_ura/",
               "results/high_cra_low_u/",
               "results/high_cura/",
               "results/low_cua_high_r/",
               "results/low_ca_high_ur/",
               "results/high_cr_low_ua/",
               "results/high_cur_low_a/",
               "results/low_cur_high_a/",
               "results/low_cr_high_ua/",
               "results/high_ca_low_ur/",
               "results/high_cua_low_r/",
               "results/low_cura/",
               "results/low_cra_high_u/",
               "results/high_c_low_ura/",
               "results/high_cu_low_ra/")

all_cases <- c("low_cu_high_ra",
               "low_c_high_ura",
               "high_cra_low_u",
               "high_cura",
               "low_cua_high_r",
               "low_ca_high_ur",
               "high_cr_low_ua",
               "high_cur_low_a",
               "low_cur_high_a",
               "low_cr_high_ua",
               "high_ca_low_ur",
               "high_cua_low_r",
               "low_cura",
               "low_cra_high_u",
               "high_c_low_ura",
               "high_cu_low_ra")


combined_paths <- paste0(all_paths, all_cases)

all_data <- lapply(combined_paths, function(path) {
  get_sr_all(path) |> mutate(path = path) # Add the path as a column for check
})

# 01 Deal with data. Combine all results into one data frame
final_data <- bind_rows(all_data)

#saveRDS(final_data, "results/PA_richness.rds")


# # 02 Plot ---------------------------------------------------------------

# Read the data
#final_data <- readRDS("results/PA_richness.rds")


 plot_fun <- function(the_path) {

   plot_data <- final_data |> filter(path == the_path)

   plot_data |>
      group_by(Effects) |>
      summarise(Plant = mean(island_p, na.rm = TRUE),
                Animal = mean(island_a, na.rm = TRUE)) |>
      pivot_longer(cols = c(Plant, Animal), names_to = "Species", values_to = "Richness") |>
      mutate(Species = factor(Species, levels = c("Plant", "Animal"))) |>
      ggplot(aes(x = Effects, y = Richness + 1, fill = Species)) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
      scale_fill_manual(values = c("darkgreen", "orange")) +
      scale_y_continuous(
       limits = c(1, 1500),
       trans = "log10"
     ) +
      labs(
        x = "Mutualism Effects",
        y = "No.species (+1)",
        fill = "Species"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        aspect.ratio = 3/4,  # Aspect ratio of each subplot
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)  # Frames around subplots
      )
  }

all_plots <- lapply(combined_paths, plot_fun)

for (i in 1:16) {
  if (i %% 4 != 1) {
    all_plots[[i]] <- all_plots[[i]] + theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  }
  if (i <= 12) {
    all_plots[[i]] <- all_plots[[i]] + theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  }
}

p <- wrap_plots(all_plots, ncol = 4, nrow = 4) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")


# ggsave("results/combo16PA.png", p,
#        width = 10, height = 10, units = "in", dpi = 300)

# ggsave("figures/combo16PA.pdf", p,
#        width = 10, height = 10, units = "in", dpi = 300)



# compute the correlation between plant and animal ------------------------
# across different mutualism levels
library(dplyr)
library(broom)
# Read data
PA_richness <- readRDS("results/PA_richness.rds")

# Compute correlation between `island_p` and `island_a` across different mutualism levels
test <- PA_richness |>
  group_by(Effects) |>
  do({
    model <- lm(island_a ~island_p, data = .)
    tidied <- tidy(model, conf.int = TRUE)  # Get p-values, F-values, and other statistics
    tidied |>
      filter(term == "island_p") |>  # Focus on the effect of island_p
      mutate(Effects = unique(.$Effects)) |>  # Add the 'Effects' column back
      select(Effects, term, p.value, statistic)  # Select the relevant columns
  })

# > test
# # A tibble: 3 × 4
# # Groups:   Effects [3]
# Effects term       p.value statistic
# <fct>   <chr>        <dbl>     <dbl>
#   1 None    island_p 1.81e-181      32.9
# 2 Medium  island_p 0             103.
# 3 High    island_p 0             103.

# (1) 0 is not exactly the 0 value, the true value is extremly small so reported as 0 by Rstudio.
# (2) small values of p-value indicate that the correlation is significant.
# (3) high values of statistic (F-value) indicate that the correlation is strong.




