# simulation
# profvis::profvis({sim_mutualism(total_time = 1, mutualism_pars = mutualism_pars)})
#' Internal function of simulation
#' @return a named list with island information
#'
sim_test_dynamicsrates <- function(total_time, mutualism_pars) {
  #### Initialization ####
  testit::assert(are_mutualism_pars(mutualism_pars))
  timeval <- 0
  M0 <- mutualism_pars$M0
  Mt <- M0
  alphaa <- mutualism_pars$alphaa
  M_true_list <- list()
  maxplantID <- nrow(M0)
  maxanimalID <- ncol(M0)
  status_p <- matrix(0, nrow = nrow(M0), ncol = 1)
  status_a <- matrix(0, nrow = ncol(M0), ncol = 1)

  island_spec <- c()
  stt_table <- matrix(ncol = 7)
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

  immig_p_list <- list()
  ext_p_list <- list()
  clado_p_list <- list()
  ana_p_list <- list()
  immig_a_list <- list()
  ext_a_list <- list()
  clado_a_list <- list()
  ana_a_list <- list()
  cospec_list <-  list()
  gain_list <- list()
  loss_list <- list()

  time_list <- list()


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

    sum_rates <- lapply(rates, sum)
    immig_p_list[[length(immig_p_list) + 1]] <- sum_rates$immig_p
    ext_p_list[[length(ext_p_list) + 1]] <- sum_rates$ext_p
    clado_p_list[[length(clado_p_list) + 1]] <- sum_rates$clado_p
    ana_p_list[[length(ana_p_list) + 1]] <- sum_rates$ana_p
    immig_a_list[[length(immig_a_list) + 1]] <- sum_rates$immig_a
    ext_a_list[[length(ext_a_list) + 1]] <- sum_rates$ext_a
    clado_a_list[[length(clado_a_list) + 1]] <- sum_rates$clado_a
    ana_a_list[[length(ana_a_list) +1 ]] <- sum_rates$ana_a
    cospec_list[[length(cospec_list) + 1]] <- sum_rates$cospec_rate
    gain_list[[length(gain_list) + 1]] <- sum_rates$gain_rate
    loss_list[[length(loss_list) + 1]] <- sum_rates$loss_rate

    # next time
    timeval_and_dt <- sample_time_mutual(rates = rates, timeval = timeval)
    timeval <- timeval_and_dt$timeval

    time_list[[length(time_list) + 1]] <- timeval

    if (timeval <= total_time) {
      # next event
      possible_event <- sample_event_mutual(rates = rates)

      # next state based on event
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

  immig_p_list[[length(immig_p_list)]] <- NULL
  ext_p_list[[length(ext_p_list)]] <- NULL
  clado_p_list[[length(clado_p_list)]] <- NULL
  ana_p_list[[length(ana_p_list)]] <- NULL
  immig_a_list[[length(immig_a_list)]] <- NULL
  ext_a_list[[length(ext_a_list)]] <- NULL
  clado_a_list[[length(clado_a_list)]] <- NULL
  ana_a_list[[length(ana_a_list)]] <- NULL
  cospec_list[[length(cospec_list)]] <-  NULL
  gain_list[[length(gain_list)]] <- NULL
  loss_list[[length(loss_list)]] <- NULL
  time_list[[length(time_list)]] <- NULL


  return(list(
    immig_p_list = immig_p_list,
    ext_p_list = ext_p_list,
    clado_p_list = clado_p_list,
    ana_p_list = ana_p_list,
    immig_a_list = immig_a_list,
    ext_a_list = ext_a_list,
    clado_a_list = clado_a_list,
    ana_a_list = ana_a_list,
    cospec_list = cospec_list,
    gain_list = gain_list,
    loss_list = loss_list,
    time_list = time_list
  ))
}
