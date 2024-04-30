#' @title sim_condition_mutualism
#' @export sim_condition_mutualism
sim_condition_mutualism <- function(total_time,
                          replicates,
                          mutualism_pars,
                          sample_freq,
                          web_cond_p,
                          web_cond_a,
                          verbose) {

  web_p <- 0
  web_a <- 0

  island_replicates <- list()

  for (rep in 1:replicates) {

    while (web_p < web_cond_p | web_a < web_cond_a) {
      island_replicates[[rep]] <- sim_core_mutualism(
        total_time = total_time,
        mutualism_pars = mutualism_pars
      )
      Mt <- island_replicates[[rep]][["Mt"]]
      status_p <- island_replicates[[rep]][["status_p"]]
      status_a <- island_replicates[[rep]][["status_a"]]
      Mt_island <- Mt[status_p == 1, status_a == 1]
      Mt_web_island <- Mt_island[rowSums(Mt_island) != 0, colSums(Mt_island) != 0, drop = FALSE]
      web_p <- dim(Mt_web_island)[1]
      web_a <- dim(Mt_web_island)[2]
    }
    if (verbose == TRUE) {
      print(paste("Island replicate ", rep, sep = ""))
    }
  }
  island_segments <- format_island_mutual(
    island_replicates = island_replicates,
    total_time = total_time,
    sample_freq = sample_freq,
    mutualism_pars = mutualism_pars,
    verbose = verbose
  )
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
