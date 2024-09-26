

#### Format "raw" simulation output into standard DAISIE format, plant and animal are together ####


# `island_replicates` is the "raw" simulation output

format_island_mutual_all <- function(island_replicates,
                                     total_time,
                                     sample_freq,
                                     M0,
                                     verbose) {
  if (is.infinite(sample_freq)) {
    several_islands <- format_full_stt_all( # without `sample_freq`, full table
      island_replicates = island_replicates,
      total_time = total_time,
      M0 = M0,
      verbose = verbose
    )
  } else {
    several_islands <- format_sampled_stt_all(
      island_replicates = island_replicates,
      total_time = total_time,
      sample_freq = sample_freq,
      M0 = M0,
      verbose = verbose
    )
  }
  return(several_islands)
}


# For a full table, without `sample_freq` ---------------------------------

# Format plant and animal species together without `sample_freq`, so it's a
# full table containing all information

format_full_stt_all <- function(island_replicates,
                                total_time,
                                M0,
                                verbose) {

  several_islands <- list()
  for (rep in 1:length(island_replicates)) {
    the_island <- island_replicates[[rep]][["island"]]
    the_stt <- the_island$stt_table
    clades_info_plant <- the_island$clades_info_plant
    clades_info_animal <- the_island$clades_info_animal
    clades_info_total <- c(clades_info_plant, clades_info_animal)

    island_list <- list()
    if (sum(the_stt[nrow(the_stt), 2:7]) == 0) {
      island_list[[1]] <- list(
        island_age = total_time,
        not_present = nrow(M0) + ncol(M0),
        stt_all = the_stt
      )
    } else {
      island_list[[1]] <- list(
        island_age = total_time,
        not_present = nrow(M0) + ncol(M0) - length(clades_info_total),
        stt_all = the_stt
      )
      for (y in 1:length(clades_info_total)) {
        island_list[[y + 1]] <- clades_info_total[[y]]
      }
    }
    island_list <- DAISIE:::add_brt_table(island_list)
    several_islands[[rep]] <- island_list
    if (verbose == TRUE) {
      message(
        "Island being formatted: ", rep, "/", length(island_replicates)
      )
    }
  }
  return(several_islands)
}












# For a sampled table, with `sample_freq`  --------------------------------

# Format plant and animal species together with `sample_freq`, so it's ready to plot

format_sampled_stt_all <- function(island_replicates,
                                   total_time,
                                   sample_freq,
                                   M0,
                                   verbose) {

  several_islands <- list()
  for (rep in 1:length(island_replicates)) {
    the_island <- island_replicates[[rep]][["island"]]
    the_stt <- the_island$stt_table
    clades_info_plant <- the_island$clades_info_plant
    clades_info_animal <- the_island$clades_info_animal
    clades_info_total <- c(clades_info_plant, clades_info_animal)

    stt_all <- matrix(ncol = 7, nrow = sample_freq + 1)
    colnames(stt_all) <- c("Time", "nIp", "nAp", "nCp", "nIa", "nAa", "nCa")
    stt_all[, "Time"] <- rev(seq(
      from = 0,
      to = total_time,
      length.out = sample_freq + 1
    ))
    stt_all[1, 2:7] <- c(0, 0, 0, 0, 0, 0)
    for (j in 2:nrow(stt_all)) {
      the_age <- stt_all[j, "Time"]
      stt_all[j, 2:7] <- the_stt[max(which(the_stt[, "Time"] >= the_age)), 2:7]
    }

    island_list <- list()
    if (sum(the_stt[nrow(the_stt), 2:7]) == 0) {
      island_list[[1]] <- list(
        island_age = total_time,
        not_present = nrow(M0) + ncol(M0),
        stt_all = stt_all
      )
    } else {
      island_list[[1]] <- list(
        island_age = total_time,
        not_present = nrow(M0) + ncol(M0) - length(clades_info_total),
        stt_all = stt_all
      )
      for (y in 1:length(clades_info_total)) {
        island_list[[y + 1]] <- clades_info_total[[y]]
      }
    }

    island_list <- DAISIE:::add_brt_table(
      island = island_list,
      full_table = FALSE
    )
    several_islands[[rep]] <- island_list
    if (verbose == TRUE) {
      message(
        "Island being formatted: ", rep, "/", length(island_replicates)
      )
    }
  }
  return(several_islands = several_islands)
}






