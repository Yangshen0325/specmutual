# I set four types of rate only for plant with lac=2, mu=2, gam=1, laa=3 and without mutualism, DD,
# and set the same only for animal species, expecting sampled the same ID.
# M0 is random generated but have to make nrow(M0)=ncol(M0).
test_that("same stt_stable for system only has plant rates or animal rates", {
mutualism_pars1 = create_mutualism_pars(
  lac_pars = c(2, 0),
  mu_pars = c(2, 0, 0, 0),
  K_pars = c(Inf, Inf, Inf, Inf),
  gam_pars = c(1, 0),
  laa_pars = c(3, 0, 0, 0),
  qgain = 0,
  qloss = 0,
  lambda0 = 0,
  M0 = matrix(sample(c(0, 1), 25, replace = TRUE), ncol=5, nrow=5),
  transprob = 0)

mutualism_pars2 = create_mutualism_pars(
  lac_pars = c(0, 2),
  mu_pars = c(0, 2, 0, 0),
  K_pars = c(Inf, Inf, Inf, Inf),
  gam_pars = c(0, 1),
  laa_pars = c(0, 3, 0, 0),
  qgain = 0,
  qloss = 0,
  lambda0 = 0,
  M0 = matrix(sample(c(0, 1), 25, replace = TRUE), ncol=5, nrow=5),
  transprob = 0)

sim1_mutualism <- function(simtime, mutualism_pars){
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

  #### Start Monte Carlo iterations ####
  while (timeval < simtime){
    rates <- update_rates_mutualism(Mt = Mt,
                                    status_p = status_p,
                                    status_a = status_a,
                                    mutualism_pars = mutualism_pars,
                                    island_spec = island_spec)
    # next time
    timeval_and_dt <- calc_next_timeval_mutualism(rates = rates, timeval = timeval)
    timeval <- timeval_and_dt$timeval

    if (timeval <= simtime){
      # next event
      testrates <- rates[-c(5:11)]
      output <- reshape2::melt(setNames(rates, seq_along(rates)))
      cnames <- c("plant", "animal", "rate", "event")
      colnames(output) <- cnames
      output$event <-as.integer(output$event)
      onlyplant <- output[which(output$event < 5), ]
      x <- sample(1:dim(onlyplant)[1],
                  size = 1,
                  replace = FALSE,
                  prob = unlist(testrates))
      possible_event <- onlyplant[x,]
      #print(c(possible_event$event, possible_event$plant))
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
      #print(island_spec)
      stt_table <- updated_state$stt_table
      #print(stt_table)
    }
  }
  #### Finalize STT ####
  stt_table <- rbind(stt_table,
                     c(0,
                       stt_table[nrow(stt_table), 2],
                       stt_table[nrow(stt_table), 3],
                       stt_table[nrow(stt_table), 4],
                       stt_table[nrow(stt_table), 5],
                       stt_table[nrow(stt_table), 6],
                       stt_table[nrow(stt_table), 7]))
  return(stt_table)
}
sim2_mutualism <- function(simtime, mutualism_pars){
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
  stt_table[1, ] <- c(1, 0, 0, 0, 0, 0, 0)

  #### Start Monte Carlo iterations ####
  while (timeval < simtime){
    rates <- update_rates_mutualism(Mt = Mt,
                                    status_p = status_p,
                                    status_a = status_a,
                                    mutualism_pars = mutualism_pars,
                                    island_spec = island_spec)
    # next time
    timeval_and_dt <- calc_next_timeval_mutualism(rates = rates, timeval = timeval)
    timeval <- timeval_and_dt$timeval

    if (timeval <= simtime){
      # next event
      testrates <- rates[c(5:8)]
      output <- reshape2::melt(setNames(rates, seq_along(rates)))
      cnames <- c("plant", "animal", "rate", "event")
      colnames(output) <- cnames
      output$event <-as.integer(output$event)
      onlyanimal <- output[which(output$event > 4 & output$event < 9), ]
      onlyanimal$animal <- onlyanimal$plant
      x <- sample(1:dim(onlyanimal)[1],
                  size = 1,
                  replace = FALSE,
                  prob = unlist(testrates))
      possible_event <- onlyanimal[x, ]
      #print(c(possible_event$event, possible_event$animal))
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
      #print(island_spec)
      stt_table <- updated_state$stt_table
      #print(stt_table)
    }
  }
  #### Finalize STT ####
  stt_table <- rbind(stt_table,
                     c(0,
                       stt_table[nrow(stt_table), 2],
                       stt_table[nrow(stt_table), 3],
                       stt_table[nrow(stt_table), 4],
                       stt_table[nrow(stt_table), 5],
                       stt_table[nrow(stt_table), 6],
                       stt_table[nrow(stt_table), 7]))
  return(stt_table)
}
set.seed(12)
stt_table1 <- sim1_mutualism(simtime = 1, mutualism_pars = mutualism_pars1)
set.seed(12)
stt_table2 <- sim2_mutualism(simtime = 1, mutualism_pars = mutualism_pars2)
print(stt_table1)
print(stt_table2)
expect_equal(stt_table1[, 1], stt_table2[, 1])
expect_equal(stt_table1[, 2], stt_table2[, 5])
expect_equal(stt_table1[, 3], stt_table2[, 6])
expect_equal(stt_table1[, 4], stt_table2[, 7])
})

