#### format the simulaiton output into standard DAISIE output
# plant and animal species together

format_island_mutual_all <- function(island_replicates,
                                     total_time,
                                     sample_freq,
                                     mutualism_pars,
                                     verbose) {
  if (is.infinite(sample_freq)) {
    several_islands <- format_all_full_stt (
      island_replicates = island_replicates,
      total_time = total_time,
      sample_freq = sample_freq,
      mutualism_pars = mutualism_pars,
      verbose = verbose
    )
  } else {
    several_islands <- format_all_sampled_stt (
      island_replicates = island_replicates,
      total_time = total_time,
      sample_freq = sample_freq,
      mutualism_pars = mutualism_pars,
      verbose = verbose)
  }
  return(several_islands)
}
