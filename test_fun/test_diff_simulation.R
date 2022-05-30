# simulation
set.seed(123)
 result1 <- sim_mutualism_test(simtime = 1.5, mutualism_pars = mutualism_pars_plant)
 set.seed(123)
 result2 <- sim_mutualism_test(simtime = 1.5, mutualism_pars = mutualism_pars_animal)

sim_mutualism_test <- function(simtime, mutualism_pars){
  #### Initialization ####
  timeval <- 0
  M0 <- mutualism_pars$M0
  Mt <- M0
  maxplantID <- NROW(M0)
  maxanimalID <- NCOL(M0)
  status_p <- matrix(0, nrow = NROW(M0), ncol = 1)
  status_a <- matrix(0, nrow = NCOL(M0), ncol = 1)

  island_spec <- c()
  stt_table <- matrix(ncol = 7)
  colnames(stt_table) <- c("Time", "nIp", "nAp", "nCp", "nIa", "nAa", "nCa")
  stt_table[1, ] <- c(simtime, 0, 0, 0, 0, 0, 0)

  # record
  rates_list <- list()
  timeval_list <- list()
  possible_event_list <- list()
  updated_state_list <- list()
  #### Start Monte Carlo iterations ####
  while (timeval < simtime){
    rates <- update_rates_mutualism(Mt = Mt,
                                    status_p = status_p,
                                    status_a = status_a,
                                    mutualism_pars = mutualism_pars,
                                    island_spec = island_spec)
    rates_list[[length(rates_list) + 1]] <- rates
    # next time
    timeval_and_dt <- calc_next_timeval_mutualism(rates = rates, timeval = timeval)
    timeval <- timeval_and_dt$timeval
    timeval_list[[length(timeval_list) + 1]] <- timeval

    if (timeval <= simtime){
      # next event
      possible_event <- sample_event_mutualism(rates = rates)
      possible_event_list[[length(possible_event_list) + 1]] <- possible_event
      # next state based on event
      updated_state <- sim_update_state_mutualism(timeval = timeval,
                                                  simtime = simtime,
                                                  possible_event = possible_event,
                                                  Mt = Mt,
                                                  status_p = status_p,
                                                  status_a = status_a,
                                                  maxplantID = maxplantID,
                                                  maxanimalID = maxanimalID,
                                                  island_spec = island_spec,
                                                  stt_table = stt_table,
                                                  mutualism_pars = mutualism_pars)
      Mt <- updated_state$Mt
      status_p <- updated_state$status_p
      status_a <- updated_state$status_a
      maxplantID <- updated_state$maxplantID
      maxanimalID <- updated_state$maxanimalID
      island_spec <- updated_state$island_spec
      stt_table <- updated_state$stt_table

      updated_state_list[[length(updated_state_list) + 1]] <- updated_state
    }
  }
  return(list(rates_list = rates_list,
              timeval_list = timeval_list,
              possible_event_list = possible_event_list,
              updated_state_list = updated_state_list))
}
