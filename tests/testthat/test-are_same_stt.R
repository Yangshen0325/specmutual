# I set four types of rate only for plant with lac=2, mu=2, gam=1, laa=3 and without mutualism, DD,
# and set the same only for animal species, expecting the same stt_table.

test_that("same stt_stable for system only has plant rates or animal rates", {
  mutualism_pars1 <- create_mutual_pars(
    lac_pars = c(2, 0),
    mu_pars = c(2, 0, 0, 0),
    K_pars = c(Inf, Inf, Inf, Inf),
    gam_pars = c(1, 0),
    laa_pars = c(3, 0, 0, 0),
    qgain = 0,
    qloss = 0,
    lambda0 = 0,
    M0 = {
      set.seed(123)
      matrix(sample(c(0, 1), 25, replace = TRUE), ncol = 5, nrow = 5)
    },
    transprob = 0
  )

  mutualism_pars2 <- create_mutual_pars(
    lac_pars = c(0, 2),
    mu_pars = c(0, 2, 0, 0),
    K_pars = c(Inf, Inf, Inf, Inf),
    gam_pars = c(0, 1),
    laa_pars = c(0, 3, 0, 0),
    qgain = 0,
    qloss = 0,
    lambda0 = 0,
    M0 = {
      set.seed(123)
      matrix(sample(c(0, 1), 25, replace = TRUE), ncol = 5, nrow = 5)
    },
    transprob = 0
  )

  simulation_test <- function(simtime, mutualism_pars1, mutualism_pars2) {
    #### Initialization ####
    timeval1 <- 0
    timeval2 <- 0
    M0_1 <- mutualism_pars1$M0
    M0_2 <- mutualism_pars2$M0
    Mt_1 <- M0_1
    Mt_2 <- M0_2
    maxplantID_1 <- NROW(M0_1)
    maxanimalID_1 <- NCOL(M0_1)
    maxplantID_2 <- NROW(M0_2)
    maxanimalID_2 <- NCOL(M0_2)
    status_p1 <- matrix(0, nrow = NROW(M0_1), ncol = 1)
    status_a1 <- matrix(0, nrow = NCOL(M0_1), ncol = 1)
    status_p2 <- matrix(0, nrow = NROW(M0_2), ncol = 1)
    status_a2 <- matrix(0, nrow = NCOL(M0_2), ncol = 1)

    island_spec1 <- c()
    island_spec2 <- c()
    stt_table1 <- matrix(ncol = 7)
    stt_table2 <- matrix(ncol = 7)
    colnames(stt_table1) <- c("Time", "nIp", "nAp", "nCp", "nIa", "nAa", "nCa")
    colnames(stt_table2) <- c("Time", "nIp", "nAp", "nCp", "nIa", "nAa", "nCa")
    stt_table1[1, ] <- c(simtime, 0, 0, 0, 0, 0, 0)
    stt_table2[1, ] <- c(simtime, 0, 0, 0, 0, 0, 0)

    #### Start Monte Carlo iterations ####
    while (timeval1 < simtime & timeval2 < simtime) {
      rates1 <- update_rates_mutualism(
        Mt = Mt_1,
        status_p = status_p1,
        status_a = status_a1,
        mutualism_pars = mutualism_pars1,
        island_spec = island_spec1
      )
      rates2 <- update_rates_mutualism(
        Mt = Mt_2,
        status_p = status_p2,
        status_a = status_a2,
        mutualism_pars = mutualism_pars2,
        island_spec = island_spec2
      )
      # next time
      set.seed(123)
      timeval_and_dt1 <- calc_next_timeval_mutualism(rates = rates1, timeval = timeval1)
      set.seed(123)
      timeval_and_dt2 <- calc_next_timeval_mutualism(rates = rates2, timeval = timeval2)
      timeval1 <- timeval_and_dt1$timeval
      timeval2 <- timeval_and_dt2$timeval

      if (timeval1 <= simtime & timeval2 <= simtime) {
        # next event: only has plant
        testrates1 <- rates1[-c(5:11)]
        output1 <- reshape2::melt(setNames(rates1, seq_along(rates1)))
        cnames <- c("plant", "animal", "rate", "event")
        colnames(output1) <- cnames
        output1$event <- as.integer(output1$event)
        onlyplant <- output1[which(output1$event < 5), ]
        set.seed(456)
        x <- sample(1:dim(onlyplant)[1],
          size = 1,
          replace = FALSE,
          prob = unlist(testrates1)
        )
        possible_event1 <- onlyplant[x, ]
        # print(c(possible_event1$event, possible_event1$plant))
        # next event
        testrates2 <- rates2[c(5:8)]
        output2 <- reshape2::melt(setNames(rates2, seq_along(rates2)))
        cnames <- c("plant", "animal", "rate", "event")
        colnames(output2) <- cnames
        output2$event <- as.integer(output2$event)
        onlyanimal <- output2[which(output2$event > 4 & output2$event < 9), ]
        onlyanimal$animal <- onlyanimal$plant
        set.seed(456)
        y <- sample(1:dim(onlyanimal)[1],
          size = 1,
          replace = FALSE,
          prob = unlist(testrates2)
        )
        possible_event2 <- onlyanimal[y, ]
        # print(c(possible_event2$event, possible_event2$animal))
        # next state based on event, only has plant
        updated_state1 <- sim_update_state_mutualism(
          timeval = timeval1,
          simtime = simtime,
          possible_event = possible_event1,
          Mt = Mt_1,
          status_p = status_p1,
          status_a = status_a1,
          maxplantID = maxplantID_1,
          maxanimalID = maxanimalID_1,
          island_spec = island_spec1,
          stt_table = stt_table1,
          mutualism_pars = mutualism_pars1
        )
        Mt_1 <- updated_state1$Mt
        status_p1 <- updated_state1$status_p
        status_a1 <- updated_state1$status_a
        maxplantID_1 <- updated_state1$maxplantID
        maxanimalID_1 <- updated_state1$maxanimalID
        island_spec1 <- updated_state1$island_spec
        stt_table1 <- updated_state1$stt_table
        # next state based on event, only has animal
        updated_state2 <- sim_update_state_mutualism(
          timeval = timeval2,
          simtime = simtime,
          possible_event = possible_event2,
          Mt = Mt_2,
          status_p = status_p2,
          status_a = status_a2,
          maxplantID = maxplantID_2,
          maxanimalID = maxanimalID_2,
          island_spec = island_spec2,
          stt_table = stt_table2,
          mutualism_pars = mutualism_pars2
        )
        Mt_2 <- updated_state2$Mt
        status_p2 <- updated_state2$status_p
        status_a2 <- updated_state2$status_a
        maxplantID_2 <- updated_state2$maxplantID
        maxanimalID_2 <- updated_state2$maxanimalID
        island_spec2 <- updated_state2$island_spec
        stt_table2 <- updated_state2$stt_table
      }
    }
    #### Finalize STT ####
    stt_table1 <- rbind(
      stt_table1,
      c(0, stt_table1[nrow(stt_table1), 2:7])
    )
    stt_table2 <- rbind(
      stt_table2,
      c(0, stt_table2[nrow(stt_table2), 2:7])
    )

    stt_list <- list(
      stt_table1 = stt_table1,
      stt_table2 = stt_table2
    )
    return(stt_list)
  }
  stt_list <- simulation_test(
    simtime = 1,
    mutualism_pars1 = mutualism_pars1,
    mutualism_pars2 = mutualism_pars2
  )
  stt_table1 <- stt_list[[1]]
  stt_table2 <- stt_list[[2]]
  expect_equal(stt_table1[, 1], stt_table2[, 1])
  expect_equal(stt_table1[, 2], stt_table2[, 5])
  expect_equal(stt_table1[, 3], stt_table2[, 6])
  expect_equal(stt_table1[, 4], stt_table2[, 7])
})
