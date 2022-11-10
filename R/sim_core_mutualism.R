# simulation
# sim_mutualism(simtime = 1.5, mutualism_pars = mutualism_pars)
# profvis::profvis({sim_mutualism(simtime = 1, mutualism_pars = mutualism_pars)})
#' Internal function of simulation
#' @return a named list with island information

sim_core_mutualism <- function(total_time, mutualism_pars){
  #### Initialization ####
  testit::assert(are_mutualism_pars(mutualism_pars))
  timeval <- 0
  M0 <- mutualism_pars$M0
  Mt <- M0
  maxplantID <- nrow(M0)
  maxanimalID <- ncol(M0)
  status_p <- matrix(0, nrow = nrow(M0), ncol = 1)
  status_a <- matrix(0, nrow = ncol(M0), ncol = 1)

  island_spec <- c()
  stt_table <- matrix(ncol = 7)
  colnames(stt_table) <- c("Time", "nIp", "nAp", "nCp", "nIa", "nAa", "nCa")
  stt_table[1, ] <- c(total_time, 0, 0, 0, 0, 0, 0)

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
  while (timeval < total_time){
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
                                 island_spec = island_spec)
    testit::assert(are_rates(rates))
    # next time
    timeval_and_dt <- sample_time_mutual(rates = rates, timeval = timeval)
    timeval <- timeval_and_dt$timeval

    if (timeval <= total_time){
      # next event
      possible_event <- sample_event_mutual(rates = rates)
      # next state based on event
      updated_state <- update_states_mutual(M0 = M0,
                                            Mt = Mt,
                                            status_p = status_p,
                                            status_a = status_a,
                                            maxplantID = maxplantID,
                                            maxanimalID = maxanimalID,
                                            timeval = timeval,
                                            total_time = total_time,
                                            rates = rates,
                                            possible_event = possible_event,
                                            island_spec = island_spec,
                                            stt_table = stt_table,
                                            transprob = transprob)
      Mt <- updated_state$Mt
      status_p <- updated_state$status_p
      status_a <- updated_state$status_a
      maxplantID <- updated_state$maxplantID
      maxanimalID <- updated_state$maxanimalID
      island_spec <- updated_state$island_spec
      stt_table <- updated_state$stt_table
    }
  }
 #### Finalize STT ####
  stt_table <- rbind(stt_table,
                     c(0, stt_table[nrow(stt_table), 2:7]))

  #### Finalize island_spec ####
  cnames <- c("Species",
              "Mainland Ancestor",
              "Colonisation time (BP)",
              "Species type",
              "branch_code",
              "branching time (BP)",
              "Anagenetic_origin",
              "Species state" )
  colnames(island_spec) <- cnames
  ### set ages as counting backwards from present
  island_spec[, "branching time (BP)"] <- simtime -
    as.numeric(island_spec[, "branching time (BP)"])
  island_spec[, "Colonisation time (BP)"] <- simtime -
    as.numeric(island_spec[, "Colonisation time (BP)"])


island <- create_island_mutualism(stt_table = stt_table,
                                  simtime = simtime,
                                  island_spec = island_spec,
                                    M0 = M0)
  return(list(state_list = state_list,
            island = island))
}
