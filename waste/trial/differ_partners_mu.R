
sim_ext_pp <- function(simtime, mutualism_pars){
  #### Initialization ####
  timeval <- 0
  M0 <- mutualism_pars$M0
  Mt <- M0
  maxplantID <- NROW(M0)
  maxanimalID <- NCOL(M0)
  status_p <- matrix(1, nrow = NROW(M0), ncol = 1)
  status_a <- matrix(1, nrow = NCOL(M0), ncol = 1) #all animals are on the island

  load("X:/YSPhD_Aca/specmutual/trial/ext_trial.RData")
  island_spec <- island_spec
  timeval_list <- list()
  ext_rs <- list()
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
    ext_p <- rates[["ext_p"]]
    ext_rs[[length(ext_rs) + 1 ]] <- ext_p
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

M0 <- matrix(1, nrow = 11, ncol = 10)
M0[lower.tri(M0)] <- 0 #plant11 has no partners on the island

mutualism_pars <- list(
  lac_pars = c(0, 0),
  mu_pars = c(0.3, 0, 0.01, 0.5),#0.01 because 0.3/11
  K_pars = c(Inf, 100, Inf, 0.5), #immigration is always gamma0
  gam_pars = c(00, 0), # so focus on extinction rates
  qgain = 0,
  qloss = 0,
  laa_pars = c(0, 0, 0, 0),
  lambda0 = 0,
  M0 = M0,
  transprob = 0)
results <- sim_ext_pp(simtime = 5, mutualism_pars = mutualism_pars)

ext_rs <- results[["ext_rs"]]
pp_rates <- matrix(unlist(ext_rs), nrow = 11, ncol = length(ext_rs))
timeval <- unlist(results[["timeval_list"]])
plot(timeval, pp_rates[11, ], type = "l", lwd = 3,
     xlim = c(0, 5), ylim = c(0, 0.6),
     xlab = "Timeval", ylab = "Plant Extinction Rates")

cl <- rainbow(10)
for (x in 1:10){
  lines(timeval, pp_rates[x, ], type = "l", lwd = 3, col = cl[x])
}
legend(-0.2,0.6,
       legend = c("plant11","plant1","plant2","plant3","plant4",
                  "plant5","plant6","plant7","plant8","plant9","plant10"),
       col = c("black", cl),
       bty = "n",
       cex = 1,
       lwd = 3,
       ncol = 5)
title("11*10, differ in mutualism partners, mu")




