
sim_mutual_pp <- function(simtime, mutualism_pars){
  #### Initialization ####
  timeval <- 0
  M0 <- mutualism_pars$M0
  Mt <- M0
  maxplantID <- NROW(M0)
  maxanimalID <- NCOL(M0)
  status_p <- matrix(0, nrow = NROW(M0), ncol = 1)
  status_a <- matrix(1, nrow = NCOL(M0), ncol = 1) #all animals are on the island

  island_spec <- c()
  timeval_list <- list()
  plant_rs <- list()
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
    immig_p <- rates[["immig_p"]]
    plant_rs[[length(plant_rs) + 1 ]] <- immig_p
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
  return(list(plant_rs = plant_rs,
              timeval_list = timeval_list))
}

M0 <- matrix(1, nrow = 10, ncol = 10)
M0[lower.tri(M0)] <- 0
# status_p <- matrix(1, nrow = 10, ncol = 1)
# status_a <- matrix(1, nrow = 10, ncol = 1)
# pans_cmps_list <- get_pans_cmps(Mt = Mt,
#                                 status_p = status_p,
#                                 status_a = status_a)
# NK <- pans_cmps_list[["cmps_p"]]/pans_cmps_list[["pans_p"]]
#
# plot(1:10, NK)
mutualism_pars <- list(
  lac_pars = c(0, 0),
  mu_pars = c(0, 0, 0.5, 0.5),
  K_pars = c(Inf, 100, 1, 0.5), #it doesn't matter K_A0, K_A1 are
  #but we set K_P1 <- 1 as a constant
  gam_pars = c(0.3, 0), # make no animal immigrates
  qgain = 0,
  qloss = 0,
  laa_pars = c(0, 0, 0, 0),
  lambda0 = 0,
  M0 = M0,
  transprob = 0)
results <- sim_mutual_pp(simtime = 5, mutualism_pars = mutualism_pars)

plant_rs <- results[["plant_rs"]]
pp_rates <- matrix(unlist(plant_rs), nrow = 10, ncol = length(plant_rs))
timeval <- unlist(results[["timeval_list"]])
plot(timeval, pp_rates[1, ], type = "l", lwd = 3,
     xlim = c(0, 5), ylim = c(0, 0.5),
     xlab = "Timeval", ylab = "Plant Species Rates")

cl <- rainbow(9)
for (x in 2:10){
  lines(timeval, pp_rates[x, ], type = "l", lwd = 3, col = cl[x-1])
}
legend(x = "topright",
       legend = c("plant1","plant2","plant3","plant4",
                  "plant5","plant6","plant7","plant8","plant9","plant10"),
       col = c("black", cl),
       bty = "n",
       cex = 0.6,
       lwd = 3)
title("10*10, differ in mutualism partners")




