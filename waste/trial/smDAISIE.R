mutualism_pars <- create_mutualism_pars(
  lac_pars = c(0.5, 0.5),
  mu_pars = c(0.2, 0.2, 0, 0),
  K_pars = c(Inf, Inf, Inf, Inf),
  gam_pars = c(0.05, 0),
  laa_pars = c(1.0, 1.0, 0, 0),
  qgain = 0,
  qloss = 0,
  lambda0 = 0,
  M0 = matrix(sample(c(0, 1), 1000, replace = TRUE), ncol = 10, nrow = 100),
  transprob = 1)
# a different way to calculate immigration rates & cladogenesis rates

# calculate algorithm rates
# get immigration rates
get_immig_rate_l <- function(Mt,
                           status_p,
                           status_a,
                           mutualism_pars){
  M0 <- mutualism_pars$M0
  gam_pars <- mutualism_pars$gam_pars

  nk_list <- get_nk(Mt = Mt,
                    status_p = status_p,
                    status_a = status_a,
                    mutualism_pars = mutualism_pars)
  immig_p <- gam_pars[1] * (1 - log(nk_list[[1]]))
  immig_a <- gam_pars[2] * (1 - log(nk_list[[2]]))

  immig_list <- list(immig_p = as.matrix(immig_p[1:nrow(M0)]),
                     immig_a = as.matrix(immig_a[1:ncol(M0)]))
  return(immig_list)
}

# get cladogenetic rates
get_clado_rate_l <- function(Mt,
                           status_p,
                           status_a,
                           mutualism_pars){
  lac_pars <- mutualism_pars$lac_pars
  nk_list <- get_nk(Mt = Mt,
                    status_p = status_p,
                    status_a = status_a,
                    mutualism_pars = mutualism_pars)
  clado_p <- lac_pars[1] * (1 - log(nk_list[[1]])) * status_p #mind the conflict, status is a matrix
  clado_a <- lac_pars[2] * (1 - log(nk_list[[2]])) * status_a

  clado_list <- list(clado_p = clado_p,
                     clado_a = clado_a)
  return(clado_list)
}
update_rates_mutualism_l <- function(Mt,
                                   status_p,
                                   status_a,
                                   mutualism_pars,
                                   island_spec){
  testit::assert(are_mutualism_pars(mutualism_pars))

  immig_rate <- get_immig_rate_l(
    Mt = Mt,
    status_p = status_p,
    status_a = status_a,
    mutualism_pars = mutualism_pars
  )

  ext_rate <- get_ext_rate(
    Mt = Mt,
    status_p = status_p,
    status_a = status_a,
    mutualism_pars = mutualism_pars)

  ana_rate <- get_ana_rate(
    Mt = Mt,
    status_p = status_p,
    status_a = status_a,
    mutualism_pars = mutualism_pars,
    island_spec = island_spec
  )

  clado_rate <- get_clado_rate_l(
    Mt = Mt,
    status_p = status_p,
    status_a = status_a,
    mutualism_pars = mutualism_pars
  )

  cospec_rate <- get_cospec_rate(
    Mt = Mt,
    status_p = status_p,
    status_a = status_a,
    mutualism_pars = mutualism_pars
  )

  gain_rate <- get_gain_rate(
    Mt = Mt,
    status_p = status_p,
    status_a = status_a,
    mutualism_pars = mutualism_pars
  )

  loss_rate <- get_loss_rate(
    Mt = Mt,
    status_p = status_p,
    status_a = status_a,
    mutualism_pars = mutualism_pars
  )

  rates <- list(
    immig_p = immig_rate$immig_p,
    ext_p = ext_rate$ext_p,
    clado_p = clado_rate$clado_p,
    ana_p = ana_rate$ana_p,
    immig_a = immig_rate$immig_a,
    ext_a = ext_rate$ext_a,
    clado_a = clado_rate$clado_a,
    ana_a = ana_rate$ana_a,
    cospec_rate = cospec_rate,
    gain_rate = gain_rate,
    loss_rate = loss_rate
  )
  return(rates)
}


sim_core_mutualism_l <- function(simtime, mutualism_pars){
  #### Initialization ####
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
  stt_table[1, ] <- c(simtime, 0, 0, 0, 0, 0, 0)
  state_list <- list()
  #### Start Monte Carlo iterations ####

  while (timeval < simtime){
    rates <- update_rates_mutualism_l(Mt = Mt,
                                    status_p = status_p,
                                    status_a = status_a,
                                    mutualism_pars = mutualism_pars,
                                    island_spec = island_spec)
    testit::assert(are_rates(rates))
    # next time
    timeval_and_dt <- calc_next_timeval_mutualism(rates = rates, timeval = timeval)
    timeval <- timeval_and_dt$timeval

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
      state_list <- list(M0 = M0,
                         updated_state = updated_state)
    }
  }
  #### Finalize STT ####
  stt_table <- rbind(stt_table,
                     c(0, stt_table[nrow(stt_table), 2:7]))

  #return(stt_table)

  island <- create_island_mutualism(stt_table = stt_table,
                                    simtime = simtime,
                                    island_spec = island_spec,
                                    M0 = M0)
  return(list(state_list = state_list,
              island = island))
}

sim_mutualism_l <- function(simtime,
                          replicates,
                          mutualism_pars,
                          sample_freq,
                          plot_sims = TRUE){
  island_replicates <- list()
  for (rep in 1:replicates){
    print(rep)
    island_replicates[[rep]] <- sim_core_mutualism_l(
      simtime = simtime,
      mutualism_pars = mutualism_pars
    )
  }
  several_islands <- format_island(island_replicates = island_replicates,
                                   simtime = simtime,
                                   sample_freq = sample_freq,
                                   mutualism_pars = mutualism_pars)
  if (plot_sims == TRUE){
    plot_stt(several_islands = several_islands,
             simtime = simtime)
  }
  return (list(island_replicates = island_replicates,
               several_islands = several_islands))
}





set.seed(1)
results <- sim_mutualism_l(simtime = 1,
              replicates = 2,
              mutualism_pars = mutualism_pars,
              sample_freq = 25,
              plot_sims = TRUE)
