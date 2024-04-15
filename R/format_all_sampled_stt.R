# put plant and animal together, sampled=25
format_all_sampled_stt <- function(island_replicates,
                                   total_time,
                                   sample_freq,
                                   mutualism_pars,
                                   verbose) {
  M0 <- mutualism_pars$M0
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
