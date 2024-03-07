# when `sample_freq = Inf`

format_full_stt <- function(island_replicates,
                             total_time,
                             sample_freq,
                             mutualism_pars,
                             verbose = verbose) {

  M0 <- mutualism_pars$M0

  several_islands_plant <- list()
  several_islands_animal <- list()

  for (rep in 1:length(island_replicates)) {
    the_island <- island_replicates[[rep]][["island"]]
    stt_all <- the_island$stt_table

    island_list_plant <- list()
    island_list_animal <- list()

    # deal with plant species
    if (sum(stt_all[nrow(stt_all), 2:4]) == 0) {
      island_list_plant[[1]] <- list(
        island_age = total_time,
        not_present_p = nrow(M0),
        stt_all_plant = stt_all[, 1:4]
      )
    } else {
      plant_size <- length(the_island$clades_info_plant)
      island_list_plant[[1]] <- list(
        island_age = total_time,
        not_present_p = nrow(M0) - plant_size,
        stt_all_plant = stt_all[, 1:4]
      )
      if (plant_size != 0) {
        for (y in seq_len(plant_size)) {
          island_list_plant[[ y+1 ]] <- the_island$clades_info_plant[[y]]
        }
      }
    }
      island_list_plant <- DAISIE:::add_brt_table(island = island_list_plant,
                                                  full_table = TRUE)
      several_islands_plant[[rep]] <- island_list_plant

    # deal with animal species
    if (sum(stt_all[nrow(stt_all), 5:7]) == 0) {
      island_list_animal[[1]] <- list(
        island_age = total_time,
        not_present_a = ncol(M0),
        stt_all_animal = stt_all[, c(1, 5:7)]
      )
    } else {
      animal_size <- length(the_island$clades_info_animal)
      island_list_animal[[1]] <- list(
        island_age = total_time,
        not_present_a = ncol(M0) - animal_size,
        stt_all_animal = stt_all[, c(1, 5:7)]
      )
      if (animal_size != 0) {
        for (y in seq_len(animal_size)) {
          island_list_animal[[ y+1 ]] <- the_island$clades_info_animal[[y]]
        }
      }
    }
      island_list_animal <- DAISIE:::add_brt_table(island = island_list_animal,
                                                   full_table = TRUE)
      several_islands_animal[[rep]] <- island_list_animal

      if (verbose == TRUE) {
        message(
          "Island being formatted: ", rep, "/", length(island_replicates)
        )
      }
  }
  return(list(several_islands_plant = several_islands_plant,
              several_islands_animal = several_islands_animal))
}
