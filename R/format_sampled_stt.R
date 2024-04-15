# format the simulation output into standard DAISIE list output
# with sampled stt, and it's plant and animal species seperately.

format_sampled_stt <- function(island_replicates,
                               total_time,
                               sample_freq,
                               mutualism_pars,
                               verbose) {
  M0 <- mutualism_pars$M0
  several_islands_plant <- list()
  several_islands_animal <- list()
  for (rep in 1:length(island_replicates)) {
    the_island <- island_replicates[[rep]][["island"]]
    the_stt <- the_island$stt_table
    clades_info_plant <- the_island$clades_info_plant
    clades_info_animal <- the_island$clades_info_animal

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

    island_list_plant <- list()
    island_list_animal <- list()
    if (sum(the_stt[nrow(the_stt), 2:4]) == 0) {
      island_list_plant[[1]] <- list(
        island_age = total_time,
        not_present_p = nrow(M0),
        stt_plant = stt_all[, 1:4]
      )
    } else {
      island_list_plant[[1]] <- list(
        island_age = total_time,
        not_present_p = nrow(M0) - length(clades_info_plant),
        stt_plant = stt_all[, 1:4]
      )
      for (y in 1:length(clades_info_plant)) {
        island_list_plant[[y + 1]] <- clades_info_plant[[y]]
      }
    }
    if (sum(the_stt[nrow(the_stt), 5:7]) == 0) {
      island_list_animal[[1]] <- list(
        island_age = total_time,
        not_present_a = ncol(M0),
        stt_animal = stt_all[, c(1, 5:7)]
      )
    } else {
      island_list_animal[[1]] <- list(
        island_age = total_time,
        not_present_a = ncol(M0) - length(clades_info_animal),
        stt_animal = stt_all[, c(1, 5:7)]
      )
      for (y in 1:length(clades_info_animal)) {
        island_list_animal[[y + 1]] <- clades_info_animal[[y]]
      }
    }

    island_list_plant <- DAISIE:::add_brt_table(island = island_list_plant)
    island_list_animal <- DAISIE:::add_brt_table(island = island_list_animal)
    several_islands_plant[[rep]] <- island_list_plant
    several_islands_animal[[rep]] <- island_list_animal
    if (verbose == TRUE) {
      message(
        "Island being formatted: ", rep, "/", length(island_replicates)
      )
    }
  }
  return(list(
    several_islands_plant = several_islands_plant,
    several_islands_animal = several_islands_animal
  ))
}
