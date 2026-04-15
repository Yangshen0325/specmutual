


### For each replicate, determine the number of species in each degree bin
### then take the average of across these replicates, and the 95% CI.


# get the data, saved, don't run ------------------------------------------


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


plant_deg_bin_list <- list()
animal_deg_bin_list <- list()

for ( i in seq_along(all_paths)) {

  scenario <- paste0("scenario_", i)

  the_path <- all_paths[i]
  the_case <- all_cases[i]

  all_deg_bin_df <- get_NLH_deg_bin(the_path, the_case)

  plant_deg_bin <- all_deg_bin_df$plant_deg_bin
  plant_deg_bin$scenario <- scenario
  plant_deg_bin_list[[i]] <- plant_deg_bin

  animal_deg_bin <- all_deg_bin_df$animal_deg_bin
  animal_deg_bin$scenario <- scenario
  animal_deg_bin_list[[i]] <- animal_deg_bin

}

# Bind data together
plant_deg_bin_data <- bind_rows(plant_deg_bin_list)
animal_deg_bin_data <- bind_rows(animal_deg_bin_list)

#saveRDS(plant_deg_bin_data, "results/plant_deg_bin_data.rds")
#saveRDS(animal_deg_bin_data, "results/animal_deg_bin_data.rds")

# Plot --------------------------------------------------------------------

rm(list = ls())
library(tidyverse)
library(patchwork)
library(scales)

# Read data
#plant_deg_bin_data <- readRDS("results/plant_deg_bin_data.rds")
#animal_deg_bin_data <- readRDS("results/animal_deg_bin_data.rds")

# plot function
plot_deg_bin <- function(data){

  data <- data |>
    mutate(
     # sd_prop = ifelse(is.na(sd_prop), 0, sd_prop),
      degree_bin = factor(degree_bin, levels = c(
        "[0]", "[1]", "[2]", "[3-4]", "[5-8]", "[9-16]", "[17-32]",
        "[33-64]", "[65-128]", "[129-256]", "[257-512]", "[513-1024]", "[1025-]"
      ))
    ) |>
    tidyr::complete(degree_bin, Effects, fill = list(mean_prop = 0, lower_ci = 0, upper_ci = 0))

    p <- ggplot(data, aes(x = degree_bin, y = mean_prop, fill = Effects, color = Effects)) +
      geom_col(position = position_dodge(width = 0.9), linewidth = 0.2) +
      geom_errorbar(
        aes(ymin =lower_ci, ymax = upper_ci),
        position = position_dodge(width = 0.9),
        width = 0.25,
        linewidth = 0.6
      ) +
      scale_y_continuous(limits = c(0, 1)) +
      scale_fill_manual(values = c("None" = "green3", "Medium" = "blue", "High" = "red")) +
      scale_color_manual(values = c("None" = "green3", "Medium" = "blue", "High" = "red")) +
      labs(
        x = "Degree",
        y = "Proportion of Richness",
        fill = "Mutualism Level",
        color = "Mutualism Level"
      ) +
      theme_bw(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top",
        aspect.ratio = 3/4
      )

  return(p)
}


# Plant

all_plots <- list()
for (i in 1:16){

  data <- plant_deg_bin_data |>  filter(scenario == paste0("scenario_", i))
  all_plots[[i]] <- plot_deg_bin(data)

}


for (i in 1:16) {

  if(i %% 4 != 1) {

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

#ggsave("figures/deg_bin_p.pdf", p,
      # width = 10, height = 10, units = "in", dpi = 100)

# Animal
all_plots <- list()
for (i in 1:16){

  data <- animal_deg_bin_data |>  filter(scenario == paste0("scenario_", i))
  all_plots[[i]] <- plot_deg_bin(data)

}

for(i in 1:16){
  if(i %% 4 != 1) {

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

p2 <- wrap_plots(all_plots, ncol = 4, nrow = 4) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

#ggsave("figures/deg_bin_a.pdf", p2,
#       width = 10, height = 10, units = "in", dpi = 100)

