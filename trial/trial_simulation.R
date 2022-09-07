load("X:/YSPhD_Aca/specmutual/trial/M0.RData")
mutualism_pars <- create_mutualism_pars(
  lac_pars = c(0.5, 0.5), # c(lac_plant, lac_animal)
  mu_pars = c(0.2, 0.2, 1.0, 1.0), # c(mu_P0, mu_A0, mu_P1, mu_A1)
  K_pars = c(Inf, Inf, 1.0, 1.0), # c(K_P0, K_A0, K_P1, K_A1)
  gam_pars = c(0.1, 0.1), # (gam_plant, gam_animal)
  qgain = 1.0,
  qloss = 1.0,
  laa_pars = c(0.3, 0.3, 1.0, 1.0), # c(laa_P0, laa_A0, laa_P1, laa_A1)
  lambda0 = 1.0,
  M0 = M0,
  transprob = 1.0) # the possibility to inherit links from parents

trial_sim <- function(simtime, mutualism_pars){
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

  rates_list <- list()
  timeval_list <- list(0)
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
      # print(c(possible_event$event, possible_event$rate))
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
    }
  }
  timeval_list[[length(timeval_list)]] <- NULL
  return(list(updated_state = updated_state,
              rates_list = rates_list,
              timeval_list = timeval_list))

}
results <- trial_sim(simtime = 5, mutualism_pars = mutualism_pars)

rates_list <- results[["rates_list"]]
timeval_list <- results[["timeval_list"]]


for (i in 1:length(rates_list)){
  rep <- rates_list[[1]]
  immig_p <- rep[["immig_p"]]
  immig_a <- rep[["immig_a"]]
  ana_p <- rep[["ana_p"]]
  ana_a <- rep[["ana_a"]]
  ext_p <- rep[["ext_p"]]
  ext_a <- rep[["ext_a"]]
  clado_p <- rep[["clado_p"]]
  clado_a <- rep[["clado_a"]]
}

