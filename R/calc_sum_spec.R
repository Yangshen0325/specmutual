# Obtain the number of species in all clades

calc_sum_spec <- function(the_novel_sim) {
  novel_sim_clades <- the_novel_sim
  novel_sim_clades[[1]][[1]] <- NULL
  num_spec <- c()
  for (i in seq_along(novel_sim_clades[[1]])) {
    num_spec[i] <- length(novel_sim_clades[[1]][[i]]$branching_times) - 1
  }
  sum_spec_clade <- sum(num_spec)

  testit::assert(
    is.numeric(sum_spec_clade) && all(is.finite(sum_spec_clade))
  )
  return(sum_spec_clade)
}
