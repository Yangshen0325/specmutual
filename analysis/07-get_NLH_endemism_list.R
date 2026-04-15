
## Function to get endemism data across None, Low, and High mutualism effects.
## `species_type` options: "all", "plant", "animal"
# "all" includes both plant and animal species.
# "plant" includes only plant species.
# "animal" includes only animal species.

get_NLH_endemism_list <- function(the_path,
                                  the_case,
                                  species_type = species_type,
                                  M0,
                                  total_time,
                                  sample_freq)  {

  # Data source
  file_name <- c(paste0(the_path, the_case, "_none.rds"),
                 paste0(the_path, the_case, "_medium.rds"),
                 paste0(the_path, the_case, "_high.rds"))

  # Assign group labels
  group_labels <- c("None", "Medium", "High")

  # Initialize a list to store all results
  sp_endemism_list <- list()

  for (the_file in seq_along(file_name)) {

    # Read data
    the_data <- readRDS(file_name[the_file])

    # Species endemism for plant and animal species together
    if (species_type == "all") {

      # Data processing for stt plot for all species
      cladeinfo_all <- specmutual:::format_island_mutual_all(island_replicates = the_data,
                                                             total_time = total_time,
                                                             sample_freq = sample_freq,
                                                             M0 = M0,
                                                             verbose = FALSE)

      # Get `Median` `Q0.25` and `Q0.75` for plant and animal species, together
      infolist_all <- get_endemism_list(cladeinfo = cladeinfo_all,
                                        species_type = species_type,
                                        sample_freq = sample_freq)

      sp_endemism <- bind_rows(infolist_all, .id = 'Type')
      sp_endemism <- sp_endemism |> mutate(Group = group_labels[the_file])
    } else if (species_type == "plant") {

      cladeinfo <- specmutual:::format_island_mutual_pa(island_replicates = the_data,
                                                        total_time = total_time,
                                                        sample_freq = sample_freq,
                                                        M0 = M0,
                                                        verbose = FALSE)

      # Get `Median` `Q0.25` and `Q0.75` for plant species
      infolist_plant <- get_endemism_list(cladeinfo = cladeinfo,
                                          species_type = species_type,
                                          sample_freq = sample_freq)

      sp_endemism <- bind_rows(infolist_plant, .id = 'Type')
      sp_endemism <- sp_endemism |> mutate(Group = group_labels[the_file])
    } else if (species_type == "animal") {

      cladeinfo <- specmutual:::format_island_mutual_pa(island_replicates = the_data,
                                                        total_time = total_time,
                                                        sample_freq = sample_freq,
                                                        M0 = M0,
                                                        verbose = FALSE)

      # Get `Median` `Q0.25` and `Q0.75` for animal species
      infolist_animal <- get_endemism_list(cladeinfo = cladeinfo,
                                           species_type = species_type,
                                           sample_freq = sample_freq)

      sp_endemism <- bind_rows(infolist_animal, .id = 'Type')
      sp_endemism <- sp_endemism |> mutate(Group = group_labels[the_file])
    }

    sp_endemism_list[[the_file]] <- sp_endemism
  }

  sp_endemism_list <- bind_rows(sp_endemism_list)
  sp_endemism_list$Group <- factor(sp_endemism_list$Group, levels = c("None", "Medium", "High"))

  return(sp_endemism_list)
}
