# single simulation
sim_stepone_mutual <- function(total_time, mutualism_pars) {
  testit::assert(are_mutualism_pars(mutualism_pars))

  #### Initialization ####
  timeval <- 0
  M0 <- mutualism_pars$M0
  mainland_n <- nrow(M0) + ncol(M0)
  Mt <- M0
  ## plant species
  maxplantID <- nrow(M0)
  status_p <- matrix(0, nrow = nrow(M0), ncol = 1)
  island_plant <- c()
  stt_plant <- matrix(ncol = 4)
  colnames(stt_plant) <- c("Time", "nIp", "nAp", "nCp")
  stt_plant[1, ] <- c(total_time, 0, 0, 0)
  ## animal speceis
  maxanimalID <- ncol(M0)
  status_a <- matrix(0, nrow = ncol(M0), ncol = 1)
  island_animal <- c()
  stt_animal <- matrix(ncol = 4)
  colnames(stt_animal) <- c("Time", "nIa", "nAa", "nCa")
  stt_animal[1, ] <- c(total_time, 0, 0, 0)
  ## initial parameters
  lac_pars <- mutualism_pars$lac_pars
  mu_pars <- mutualism_pars$mu_pars
  K_pars <-  mutualism_pars$K_pars
  gam_pars <-  mutualism_pars$gam_pars
  laa_pars <-  mutualism_pars$laa_pars
  qgain <-  mutualism_pars$qgain
  qloss <-  mutualism_pars$qloss
  lambda0 <-  mutualism_pars$lambda0
  transprob <-  mutualism_pars$transprob

  #### Start Monte Carlo iterations ####
  while (timeval < total_time) {
    rates <- update_rates_mutual(M0 = M0,
                                 Mt = Mt,
                                 status_p = status_p,
                                 status_a = status_a,
                                 lac_pars = lac_pars,
                                 mu_pars = mu_pars,
                                 K_pars = K_pars,
                                 gam_pars = gam_pars,
                                 laa_pars = laa_pars,
                                 qgain = qgain,
                                 qloss = qloss,
                                 lambda0 = lambda0,
                                 transprob = transprob,
                                 island_plant = island_plant,
                                 island_animal = island_animal)
    testit::assert(are_rates(rates))
    # next time
    timeval_and_dt <- sample_time_mutual(rates = rates, timeval = timeval)
    timeval <- timeval_and_dt$timeval

    if (timeval <= total_time) {
      # next event
      possible_event <- sample_event_mutual(rates = rates)
      # update all states on the island
      update_states <- update_states_mutual(M0 = M0,
                                           Mt = Mt,
                                           status_p = status_p,
                                           status_a = status_a,
                                           maxplantID = maxplantID,
                                           maxanimalID = maxanimalID,
                                           timeval = timeval,
                                           total_time = total_time,
                                           rates = rates,
                                           possible_event = possible_event,
                                           island_plant = island_plant,
                                           island_animal = island_animal,
                                           stt_plant = stt_plant,
                                           stt_animal = stt_animal,
                                           transprob =transprob)
      Mt <- update_states$Mt
      status_p <- update_states$status_p
      status_a <- update_states$status_a
      maxplantID <- update_states$maxplantID
      maxanimalID <- update_states$maxanimalID
      island_plant <- update_states$island_plant
      island_animal <- update_states$island_animal
      stt_plant <- update_states$stt_plant
      stt_animal <- update_states$stt_animal
    }
  }

  #### Finalize STT ####
  stt_plant <- rbind(stt_plant,
                     c(0, stt_plant[nrow(stt_plant), 2:4]))
  stt_animal <- rbind(stt_animal,
                      c(0, stt_animal[nrow(stt_animal), 2:4]))

  plant_in_island <- DAISIE:::DAISIE_create_island(stt_table = stt_plant,
                                                   totaltime = total_time,
                                                   island_spec = island_plant,
                                                   mainland_n = mainland_n)
  animal_in_island <- DAISIE:::DAISIE_create_island(stt_table = stt_animal,
                                                    totaltime = total_time,
                                                    island_spec = island_animal,
                                                    mainland_n = mainland_n)


  return(list(Mt = Mt,
              status_p = status_p,
              status_a = status_a,
              island_plant = island_plant,
              island_animal = island_animal,
              stt_plant = stt_plant,
              stt_animal = stt_animal,
              plant_in_island = plant_in_island,
              animal_in_island = animal_in_island))
}
