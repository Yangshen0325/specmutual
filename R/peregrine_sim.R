
#' @export
peregrine_sim <- function(total_time,
                          replicates,
                          mutualism_pars,
                          verbose = TRUE) {
  island_replicates <- list()
  for (rep in 1:replicates) {
   island_replicates[[rep]] <- sim_core_mutualism(total_time = total_time,
                                                 mutualism_pars = mutualism_pars)
  if (verbose == TRUE) {
    print(paste("Island replicate ", rep, sep = ""))
  }
 }
}


