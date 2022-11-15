# format island
sim_steptwo_mutual <- function(total_time,
                               mutualism_pars,
                               replicates,
                               sample_freq,
                               verbose = TRUE) {
  sim_stepone_mutual <- list()
  plant_in_island <- list()
  animal_in_island <- list()
  for (i in 1:replicates) {
    sim_stepone_mutual[[i]] <- sim_stepone_mutual(total_time = total_time,
                                           mutualism_pars = mutualism_pars)
    plant_in_island[[i]] <- sim_stepone_mutual[[i]]$plant_in_island
    animal_in_island[[i]] <- sim_stepone_mutual[[i]]$animal_in_island
    if (verbose == TRUE) {
      print(paste("Island replicate ", i, sep = ""))
    }
  }
  M0 <- mutualism_pars$M0
  M_totalspec <- nrow(M0) + ncol(M0)

  plant_replicates <- DAISIE:::DAISIE_format_IW(
    island_replicates = plant_in_island,
    time = total_time,
    M = M_totalspec,
    sample_freq = sample_freq,
    verbose = verbose)

  animal_replicates <- DAISIE:::DAISIE_format_IW(
    island_replicates = animal_in_island,
    time = total_time,
    M = M_totalspec,
    sample_freq = sample_freq,
    verbose = verbose)

  return(list(plant_replicates = plant_replicates,
              animal_replicates = animal_replicates))

}
