
sim_ana_pp <- function(simtime, mutualism_pars){
  #### Initialization ####
  timeval <- 0
  M0 <- mutualism_pars$M0
  Mt <- M0
  maxplantID <- NROW(M0)
  maxanimalID <- NCOL(M0)
  status_p <- matrix(1, nrow = NROW(M0), ncol = 1) #all plant are present and immigrants
  status_a <- matrix(1, nrow = NCOL(M0), ncol = 1) #all animals are on the island

  load("X:/YSPhD_Aca/specmutual/trial/ext_trial.RData")
  island_spec <- island_spec
  timeval_list <- list()
  ana_rs <- list()
  stt_table <- matrix(ncol = 7)
  colnames(stt_table) <- c("Time", "nIp", "nAp", "nCp", "nIa", "nAa", "nCa")
  stt_table[1, ] <- c(simtime, 0, 0, 0, 0, 0, 0)
  #### Start Monte Carlo iterations ####
  while (timeval < simtime){
    rates <- update_rates_mutualism(Mt = Mt,
                                    status_p = status_p,
                                    status_a = status_a,
                                    mutualism_pars = mutualism_pars,
                                    island_spec = island_spec)
    ana_rs <- rates[["ana_p"]]
    ana_rs[[length(ana_rs) + 1 ]] <- ana_p
    # next time
    timeval_and_dt <- calc_next_timeval_mutualism(rates = rates, timeval = timeval)
    timeval <- timeval_and_dt$timeval
    timeval_list[[length(timeval_list) + 1 ]] <- timeval

    if (timeval <= simtime){
      # next event
      possible_event <- sample_event_mutualism(rates = rates)
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
  return(list(ext_rs = ext_rs,
              timeval_list = timeval_list))
}

M0 <- matrix(sample(c(0, 1), 40, replace = TRUE), nrow = 4, ncol = 10)
qgain_list <- seq(0, 2, 0.5)
for (x in 1:5){
mutualism_pars <- list(
  lac_pars = c(0, 0),
  mu_pars = c(0, 0, 0, 0),
  K_pars = c(Inf, 100, Inf, 0.5),
  gam_pars = c(0, 0), # so focus on extinction rates
  qgain = qgain_list[x],
  qloss = 0,
  laa_pars = c(0.3, 0, 1.0, 0),
  lambda0 = 0,
  M0 = M0,
  transprob = 0)
results[[x]] <- sim_ana_pp()
}

Mt <- M0
for (x in 1:10){
  M[x, 1:x] <- 1 -  M0[x, 1:x]
}


