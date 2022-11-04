sim_steptwo_mutual <- function(mutualism_pars,
                               simtime,
                               stt_plant,
                               stt_animal,
                               island_plant,
                               island_animal) {
  M0 <- mutualism_pars$M0
  mainland_n <- nrow(M0) + ncol(M0)
  island_plant <- DAISIE:::DAISIE_create_island(stt_table = stt_plant,
                                          total_time = simtime,
                                          island_spec = island_plant,
                                          mainland_n = mainland_n)
  island_animal <- DAISIE:::DAISIE_create_island(stt_table = stt_animal,
                                                 total_time = simtime,
                                                 island_spec = island_animal,
                                                 mainland_n = mainland_n)

  return(list(island_plant = island_plant,
              island_animal = island_animal))

}
