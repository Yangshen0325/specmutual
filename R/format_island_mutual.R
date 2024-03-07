####  format the simulation output into standard DAISIE list output
# including `sample_freq = Inf` and sampled stt.
format_island_mutual <- function(island_replicates,
                                 total_time,
                                 sample_freq,
                                 mutualism_pars,
                                 verbose = verbose) {
  if (is.infinite(sample_freq)) {
    several_islands <- format_full_stt(
      island_replicates = island_replicates,
      total_time = total_time,
      sample_freq = sample_freq,
      mutualism_pars = mutualism_pars,
      verbose = verbose
    )
  } else {
      several_islands <- format_sampled_stt(
        island_replicates = island_replicates,
        total_time = total_time,
        sample_freq = sample_freq,
        mutualism_pars = mutualism_pars,
        verbose = verbose
      )
    }
  return(several_islands)
}
