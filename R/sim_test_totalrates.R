# Simulation for testing rates stuff, also benefit checking parameters. It's
# the same code as `sim_core_mutualism`, but for saving time and storage, delete
# something but only print the sum of each kind of rates, the sum of all rates,
# the sum rate for plant and animal,respectively and the event happening on the island.
#
#' @title For checking parameters
#' @return a named list with island information (not this)
#'
sim_test_totalrates <- function(total_time, mutualism_pars) {

  #### Initialization ####

  # test if the structure of `mutualism_pars`
  testit::assert(are_mutualism_pars(mutualism_pars))

  timeval <- 0
  M0 <- mutualism_pars$M0
  Mt <- M0

  # `alphaa` is alpha in equation, to differentiate with transparency alpha
  alphaa <- mutualism_pars$alphaa
  maxplantID <- nrow(M0)
  maxanimalID <- ncol(M0)
  status_p <- matrix(0, nrow = nrow(M0), ncol = 1)
  status_a <- matrix(0, nrow = ncol(M0), ncol = 1)

  island_spec <- c()
  stt_table <- matrix(ncol = 7)
  # species through time table, `~p` stands for plant species, `~a` animal species
  colnames(stt_table) <- c("Time", "nIp", "nAp", "nCp", "nIa", "nAa", "nCa")
  stt_table[1, ] <- c(total_time, 0, 0, 0, 0, 0, 0)

  lac_pars <- mutualism_pars$lac_pars
  mu_pars <- mutualism_pars$mu_pars
  K_pars <- mutualism_pars$K_pars
  gam_pars <- mutualism_pars$gam_pars
  laa_pars <- mutualism_pars$laa_pars
  qgain <- mutualism_pars$qgain
  qloss <- mutualism_pars$qloss
  lambda0 <- mutualism_pars$lambda0
  transprob <- mutualism_pars$transprob

  # M_true_list <- list()
  # measure_interval <- 0.5
  # measure_time <- measure_interval

  # if (sum(gam_pars) == 0) {
  #   stop("Island has no species and the rate of
  #   colonisation is zero. Island cannot be colonised.")
  # }

  # evolution table, with the first element represents what event is happenning
  # ar what time, and sencond element represents
  # evo_table <- list(c(), NULL)

  #### Start Monte Carlo iterations ####

  while (timeval < total_time) {
    rates <- update_rates_mutual(
      M0 = M0,
      Mt = Mt,
      alphaa = alphaa,
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
      island_spec = island_spec
    )

    testit::assert(are_rates(rates))

    print(unlist(lapply(rates, sum))) # the the sum for each kind of rates
    print(do.call(sum, rates)) # the sum for all rates
    print(c(sum(status_p), sum(status_a))) # the sum rates for all plant n animal

    # next time
    timeval_and_dt <- sample_time_mutual(rates = rates, timeval = timeval)
    timeval <- timeval_and_dt$timeval

    # save matrix on island every 0.5 time step
    # if (timeval > measure_time &&
    #     timeval - timeval_and_dt$dt < measure_time) {
    #   M_true <- Mt[which(status_p == 1), which(status_a == 1)]
    #
    #   store_index <- floor(timeval / measure_interval)
    #   M_true_list[[store_index]] <- M_true
    #   # print(store_index)
    #   measure_time <- (store_index + 1) * measure_interval
    # }

    if (timeval <= total_time) {
      # next event
      possible_event <- sample_event_mutual(rates = rates)
      print(possible_event) # the event happening on the island

      # evo_table[[1]] <- rbind(evo_table[[1]], c(timeval, possible_event))

      # next state based on the event
      updated_states <- update_states_mutual(
        M0 = M0,
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
        transprob = transprob
      )
      Mt <- updated_states$Mt
      status_p <- updated_states$status_p
      status_a <- updated_states$status_a
      maxplantID <- updated_states$maxplantID
      maxanimalID <- updated_states$maxanimalID
      island_spec <- updated_states$island_spec
      stt_table <- updated_states$stt_table
    }
  }

  #### Finalize STT ####
  # stt_table <- rbind(
  #   stt_table,
  #   c(0, stt_table[nrow(stt_table), 2:7])
  # )
  # evo_table[[2]] <- stt_table[nrow(stt_table), ]

  #### Finalize island_spec ####
  # if (length(island_spec) != 0) {
  #   cnames <- c(
  #     "Species",
  #     "Mainland Ancestor",
  #     "Colonisation time (BP)",
  #     "Species type",
  #     "branch_code",
  #     "branching time (BP)",
  #     "Anagenetic_origin",
  #     "Species state"
  #   )
  #   colnames(island_spec) <- cnames
  #   ### set ages as counting backwards from present
  #   island_spec[, "branching time (BP)"] <- total_time -
  #     as.numeric(island_spec[, "branching time (BP)"])
  #   island_spec[, "Colonisation time (BP)"] <- total_time -
  #     as.numeric(island_spec[, "Colonisation time (BP)"])
  # }
  #
  # island <- create_island_mutual(
  #   stt_table = stt_table,
  #   total_time = total_time,
  #   island_spec = island_spec
  # )
  #
  #
  # return(list(
  #   Mt = Mt,
  #   M_true_list = M_true_list,
  #   status_p = status_p,
  #   status_a = status_a,
  #   island_spec = island_spec,
  #   island = island,
  #   evo_table = evo_table
  # ))

}
