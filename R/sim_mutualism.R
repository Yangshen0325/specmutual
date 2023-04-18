#' @title sim_mutualism

sim_mutualism <- function(total_time,
                          replicates,
                          mutualism_pars,
                          sample_freq,
                          verbose = TRUE) {
  island_replicates <- list()
  for (rep in 1:replicates) {

    island_replicates[[rep]] <- sim_core_mutualism(total_time = total_time,
                                                   mutualism_pars = mutualism_pars)
    if (verbose == TRUE) {
    print(paste("Island replicate ", rep, sep = ""))
    }
  }

  island_segments <- format_island_mutual(island_replicates = island_replicates,
                                            total_time = total_time,
                                            sample_freq = sample_freq,
                                            mutualism_pars = mutualism_pars)
  island_total <- format_island_mutual_total(island_replicates = island_replicates,
                                             total_time = total_time,
                                             sample_freq = sample_freq,
                                             mutualism_pars = mutualism_pars)

  return (list(island_replicates = island_replicates,
               island_segments = island_segments,
               island_total = island_total))
}

