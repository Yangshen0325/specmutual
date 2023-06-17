

#' Running it in cluster
#'
#' @param total_time simulation time
#' @param replicates replicates
#' @param mutualism_pars muutalism-related stuff
#' @param verbose
#'
#' @return a list
#' @export
#'
peregrine_sim <- function(total_time,
                          replicates,
                          mutualism_pars,
                          verbose = TRUE) {
  outputs <- list()
  island_replicates <- list()
  for (rep in 1:replicates) {
   island_replicates[[rep]] <- sim_core_mutualism(total_time = total_time,
                                                 mutualism_pars = mutualism_pars)
  if (verbose == TRUE) {
    print(paste("Island replicate ", rep, sep = ""))
  }
   if (rep %% 200 == 0) {
     outputs[[rep/200]] <- island_replicates
   }
   saveRDS(outputs, file = "~/specmutual/result/par1.rds")
  }
}


