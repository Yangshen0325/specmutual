#' @title sim_mutualism
#' @export sim_mutualism
sim_mutualism <- function(total_time,
                          replicates,
                          mutualism_pars,
                          sample_freq,
                          cond_p,
                          cond_a,
                          verbose) {
  island_replicates <- list()
  for (rep in 1:replicates) {
    if (cond_p == 0 & cond_a == 0) {
      number_present_p <- -1
      number_present_a <- -1
    } else {
      number_present_p <- 0
      number_present_a <- 0
    }
    while (number_present_p < cond_p | number_present_a < cond_a) {
      island_replicates[[rep]] <- sim_core_mutualism(
        total_time = total_time,
        mutualism_pars = mutualism_pars
      )
      temp_island <- island_replicates[[rep]][["island"]]
      clades_plant <- temp_island[["clades_info_plant"]]
      clades_animal <- temp_island[["clades_info_animal"]]
      stac_vec_p <- unlist(clades_plant)[which(names(unlist(clades_plant)) == "stac")]
      stac_vec_a <- unlist(clades_animal)[which(names(unlist(clades_animal)) == "stac")]
      present_p <- which(stac_vec_p != 0)
      present_a <- which(stac_vec_a != 0)
      number_present_p <- length(present_p)
      number_present_a <- length(present_a)
    }
    if (verbose == TRUE) {
      print(paste("Island replicate ", rep, sep = ""))
    }
  }
  # Format data, plant and animal are separated
  island_segments <- format_island_mutual_pa(
    island_replicates = island_replicates,
    total_time = total_time,
    sample_freq = sample_freq,
    mutualism_pars = mutualism_pars,
    verbose = verbose
  )

  # Format data, plant and animal are together
  island_total <- format_island_mutual_all(
    island_replicates = island_replicates,
    total_time = total_time,
    sample_freq = sample_freq,
    mutualism_pars = mutualism_pars,
    verbose = verbose
  )

  return(list(
    island_replicates = island_replicates,
    island_segments = island_segments,
    island_total = island_total
  ))
}
