
trial_update_state <- function(M0,
                               status_p,
                               status_a,
                               maxplantID,
                               maxanimalID,
                               timeval,
                               total_time,
                               rates,
                               possible_event,
                               island_spec) {

  if (possible_event != 1 && possible_event != 5) {
    stop("can only be immigration event" )
  }

  ## [1] plant species: Immigration
  if(possible_event == 1) {
    immig_p <- rates$immig_p
    colonist <- DDD:::sample2(1:length(immig_p),
                              size = 1,
                              replace = FALSE,
                              prob = immig_p)

    status_p[colonist] <- 1

    island_spec <- rbind(island_spec, c(colonist, colonist, timeval, "I",
                                          NA, NA, NA, "plant"))
  }

  ## [5] animal species: Immigration
  if (possible_event == 5) {
    immig_a <- rates$immig_a
    colonist <- DDD:::sample2(1:length(immig_a),
                              size = 1,
                              replace = FALSE,
                              prob = immig_a)

    status_a[colonist] <- 1

    island_spec <- rbind(island_spec, c(colonist, colonist, timeval, "I",
                                          NA, NA, NA, "animal"))
    }

  updated_states <- list(
                         status_p = status_p,
                         status_a = status_a,
                         maxplantID = maxplantID,
                         maxanimalID = maxanimalID,
                         island_spec = island_spec)
  return(updated_states)
}



trial_sim <- function(total_time, mutualism_pars){
  #### Initialization ####
  testit::assert(are_mutualism_pars(mutualism_pars))
  timeval <- 0
  M0 <- mutualism_pars$M0
  Mt <- M0
  maxplantID <- nrow(M0)
  maxanimalID <- ncol(M0)
  status_p <- matrix(0, nrow = nrow(M0), ncol = 1)
  status_a <- matrix(0, nrow = ncol(M0), ncol = 1)

  island_spec <- c()

  lac_pars <- mutualism_pars$lac_pars
  mu_pars <- mutualism_pars$mu_pars
  K_pars <-  mutualism_pars$K_pars
  gam_pars <-  mutualism_pars$gam_pars
  laa_pars <-  mutualism_pars$laa_pars
  qgain <-  mutualism_pars$qgain
  qloss <-  mutualism_pars$qloss
  lambda0 <-  mutualism_pars$lambda0
  transprob <- mutualism_pars$transprob

  if (sum(gam_pars) == 0) {
    stop("Island has no species and the rate of
    colonisation is zero. Island cannot be colonised.")
  }

  #### Start Monte Carlo iterations ####
  while (timeval < total_time){
    rates <- update_rates_mutual(M0 = M0,
                                 Mt = Mt,
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
                                 island_spec = island_spec)
    testit::assert(are_rates(rates))
    # next time
    timeval_and_dt <- sample_time_mutual(rates = rates, timeval = timeval)
    timeval <- timeval_and_dt$timeval

    if (timeval <= total_time){
      # next event
      possible_event <- sample_event_mutual(rates = rates)
      # next state based on event
      updated_states <- trial_update_state(M0 = M0,
                                           status_p = status_p,
                                             status_a = status_a,
                                             maxplantID = maxplantID,
                                             maxanimalID = maxanimalID,
                                             timeval = timeval,
                                             total_time = total_time,
                                             rates = rates,
                                             possible_event = possible_event,
                                             island_spec = island_spec)
      status_p <- updated_states$status_p
      status_a <- updated_states$status_a
      maxplantID <- updated_states$maxplantID
      maxanimalID <- updated_states$maxanimalID
      island_spec <- updated_states$island_spec
    }
  }

  #### Finalize island_spec ####
  if (length(island_spec) != 0) {
    cnames <- c("Species",
                "Mainland Ancestor",
                "Colonisation time (BP)",
                "Species type",
                "branch_code",
                "branching time (BP)",
                "Anagenetic_origin",
                "Species state" )
    colnames(island_spec) <- cnames
    ### set ages as counting backwards from present
    island_spec[, "branching time (BP)"] <- total_time -
      as.numeric(island_spec[, "branching time (BP)"])
    island_spec[, "Colonisation time (BP)"] <- total_time -
      as.numeric(island_spec[, "Colonisation time (BP)"])
  }

  return(island_spec = island_spec)
}
