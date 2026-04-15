

# Load necessary libraries
library(ggplot2)
library(dplyr)


plot_data <- function(data, plot_the) {

  if(data == "from_island_parts") {

    the_data <-  island_parts

    if (plot_the == "stt_pa") {
      # Data processing for stt plot for plant, animal separately
      cladeinfo <- specmutual:::format_island_mutual_pa(island_replicates = the_data,
                                                        total_time = total_time,
                                                        sample_freq = sample_freq,
                                                        M0 = M0,
                                                        verbose = FALSE)

      # Get `Median` `Q0.25` and `Q0.75` for plant and animal species, separately
      infolist_plant <- get_endemism_list(cladeinfo = cladeinfo,
                                          species_type = "plant",
                                          sample_freq = sample_freq)

      infolist_animal <- get_endemism_list(cladeinfo = cladeinfo,
                                           species_type = "animal",
                                           sample_freq = sample_freq)


      plant_stt_plot <- Stt_pa_plot(infolist_plant, title = "Plant STT")
      animal_stt_plot <- Stt_pa_plot(infolist_animal, title = "Animal STT")

      pa_separate <- plant_stt_plot + animal_stt_plot + plot_layout(guides = "collect")
      return(pa_separate)

    } else if (plot_the == "stt_all") {
      # Data processing for  stt plot for all species
      cladeinfo_all <- specmutual:::format_island_mutual_all(island_replicates = the_data,
                                                             total_time = total_time,
                                                             sample_freq = sample_freq,
                                                             M0 = M0,
                                                             verbose = FALSE)

      # Get `Median` `Q0.25` and `Q0.75` for plant and animal species, together
      infolist_all <- get_endemism_list(cladeinfo = cladeinfo_all,
                                        species_type = "all",
                                        sample_freq = sample_freq)


      pa_stt_all <- Stt_pa_plot(infolist_all, title = "Plant and Animal STT")
      return(pa_stt_all)

    } else if (plot_the == "event_frequency") {

      evt_freq <- get_freq(island_replicates = the_data)

      # P1: average event frequency across replicates
      avg_freq <- evt_freq |>
        group_by(event_is)  |>
        summarize(avg_frequency = mean(frequency, na.rm = TRUE))

      p_aver_freq <- ggplot(avg_freq, aes(x = event_is, y= avg_frequency + 1)) +
        geom_bar(stat = "identity", fill = "skyblue") +
        scale_y_continuous(trans = "log10") +
        labs(title = "Average Event Frequency",
             x = "Event Type",
             y = "Average Frequency") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      p_freq_point <- ggplot(evt_freq, aes(x = event_is, y = frequency + 1)) +
        geom_jitter(width = 0.2, alpha = 0.6, color = "blue") +  # jitter for better visualization
        scale_y_continuous(trans = "log10") +
        labs(title = "Frequency of Each Event",
             x = "Event Type",
             y = "Frequency") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability


      p_evt_freq <- p_aver_freq + p_freq_point

      return(p_evt_freq)

    } else if (plot_the == "links_over_time") {

      linksOverTime <- get_linksOverTime(data = the_data, total_time = total_time)

      p_linksoverTime <- ggplot(linksOverTime, aes(x = Time, y = Links_median + 1)) +
        geom_line(linewidth = 1) +
        geom_ribbon(aes(ymin = Links_q25 + 1, ymax = Links_q75 + 1), alpha = 0.2, color = NA) +
        scale_x_reverse()+
        scale_y_continuous(trans = "log10") +
        labs(title = "Links over Time",
             x = "Time to present",
             y = "Number of links") +
        theme_minimal()

      return(p_linksoverTime)

    }
    else {
      stop("Invalid plot type. Please use 'stt_pa', 'stt_all', 'event_frequency',
           or 'links_over_time' in data from the island parts.")
    }

  }

  if(data == "from_additional_parts") {

    the_data <- additional_parts

    # Define standardized time points
    standard_time <- rev(seq(from = 0, to = total_time, length.out = sample_freq + 1))

    # Apply processing to each replicate and store results in a list
    all_replicates_data <- lapply(the_data, get_rate_ptn_dynmc, standard_time = standard_time)

    # Combine all replicates and calculate mean across each time point
    combined_data <- do.call(rbind, all_replicates_data)
    combined_data$replicate <- rep(1:length(all_replicates_data), each = nrow(all_replicates_data[[1]]))

    # Average across replicates
    average_data <- combined_data  |>
      group_by(timeval)  |>
      summarize(across(-replicate, mean, na.rm = TRUE))

    if (plot_the == "rates_over_time") {
      columns_to_plot <- average_data  |>
        select(-richness_p, -richness_a)  |>
        select(-timeval, everything())

      # Reshape the data to long format for easier plotting
      data_long <- columns_to_plot  |>
        pivot_longer(cols = -timeval, names_to = "variable", values_to = "value")

      p_rates_dynamc <- ggplot(data_long, aes(x = timeval, y = value, color = variable)) +
        geom_line(linewidth = 1) +
        scale_y_continuous(limits = c(0, 10)) +
        labs(title = "Rates Over Time",
             x = "Time to present",
             y = "Rate Value") +
        scale_x_reverse() +
        theme_minimal() +
        theme(legend.title = element_blank())

      return(p_rates_dynamc)

    } else if(plot_the == "partners_over_time") {

      partners_columns <- average_data |>
        select(timeval,sum_partners_p, sum_partners_a)

      partners_long <- partners_columns |>
        pivot_longer(cols = -timeval,
                     names_to = "variable",
                     values_to = "value")

      p_partners <- ggplot(partners_long, aes(x = timeval, y = value, color = variable)) +
        geom_line(linewidth = 1) +
        labs(title = "Partners Over Time",
             x = "Time to present",
             y = "Number of partners") +
        scale_x_reverse() +
        theme_minimal() +
        theme(legend.title = element_blank())

      return(p_partners)

    }
    else {
      stop("Invalid plot type. Please use 'rates_over_time', or 'partners_over_time'
           in data from additional parts.")
    }
  }
}
