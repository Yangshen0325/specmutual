###I want to plot immigration rates for plant species over timeval
sim_rates_time <- function(simtime, mutualism_pars){
  #### Initialization ####
  timeval <- 0
  M0 <- mutualism_pars$M0
  Mt <- M0
  maxplantID <- NROW(M0)
  maxanimalID <- NCOL(M0)
  status_p <- matrix(0, nrow = NROW(M0), ncol = 1)
  status_a <- matrix(0, nrow = NCOL(M0), ncol = 1)

  island_spec <- c()
  timeval_list <- list()
  rates_list <- list()
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
    rates_list[[length(rates_list) + 1 ]] <- rates
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
  return(list(rates_list = rates_list,
              timeval_list = timeval_list))
}

# row1 <- matrix(1, nrow = 1, ncol = 10)
# row2 <- matrix(c(1,0,0,0,1,1,1,1,1,1), nrow = 1, ncol = 10)
# row3 <- matrix(c(1,1,0,0,0,1,1,0,1,0), nrow = 1, ncol = 10)
# row4 <- matrix(c(0,1,0,0,0,1,1,0,0,0), nrow = 1, ncol = 10)
# M0 <- rbind(row1, row2, row3, row4)

# set1 to set7: K_P0 <- c(Inf, 50, 40, 30, 20, 10, 5)
results <- list()
K_P0 <- c(Inf, 50, 40, 30, 20, 10, 5)
M0 <- matrix(sample(c(0, 1), 2000, replace = TRUE), nrow = 40, ncol = 50)
for (x in 1:7){
  mutualism_pars <- list(
    lac_pars = c(0, 0.1), # doesn't matter lac_animal is
    mu_pars = c(0, 0.2, 0.5, 0.5),
    K_pars = c(K_P0[x], 100, 0.5, 0.5), #it doesn't matter K_A0, K_P1, K_A1 are
    gam_pars = c(0.3, 0), # make no animal immigrates
    qgain = 0.5,
    qloss = 0.5,
    laa_pars = c(0, 0.2, 0.5, 0.5),
    lambda0 = 0.3,
    M0 = M0,
    transprob = 0.5)
  results[[x]] <- sim_rates_time(simtime = 3, mutualism_pars = mutualism_pars)
}

rates_list <- results[[1]][["rates_list"]]
timeval <- unlist(results[[1]][["timeval_list"]])
mean_immig_p <- c()
for (y in 1:length(rates_list)){
  immig_p <- rates_list[[y]][["immig_p"]]
  mean_immig_p[y] <- mean(immig_p)
}
plot(timeval, mean_immig_p, type = "l", lwd = 3)

cl <- rainbow(6)
for (x in 2:7){
  rates_list <- results[[x]][["rates_list"]]
  timeval <- unlist(results[[x]][["timeval_list"]])
  mean_immig_p <- c()
  for (y in 1:length(rates_list)){
    immig_p <- rates_list[[y]][["immig_p"]]
    mean_immig_p[y] <- mean(immig_p)
  }
  lines(timeval, mean_immig_p, type = "l", lwd = 3, col = cl[x-1])
}
# red yellow green lightblue blue rosered
legend(x = "topright",
       legend = c("K_P0 = Inf","K_P0 = 50","K_P0 = 40","K_P0 = 30",
                  "K_P0 = 20","K_P0 = 10","K_P0 = 5"),
       col = c("black", cl),
       bty = "n",
       cex = 0.8,
       lwd = 3)
title("40*50, differ in K_P0")


