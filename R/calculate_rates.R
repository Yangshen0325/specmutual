# calculate algorithm rates
update_rates_mutualism <- function(Mt,
                                   status_p,
                                   status_a,
                                   mutualism_pars,
                                   island_spec){
  immig_rate <- get_immig_rate(
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

  clado_rate <- get_clado_rate(
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
# get immigration rates
get_immig_rate <- function(Mt,
                           status_p,
                           status_a,
                           mutualism_pars){
  M0 <- mutualism_pars$M0
  gam_pars <- mutualism_pars$gam_pars

  nk_list <- get_nk(Mt = Mt,
                    status_p = status_p,
                    status_a = status_a,
                    mutualism_pars = mutualism_pars)
  immig_p <- gam_pars[1] * nk_list[[1]]
  immig_a <- gam_pars[2] * nk_list[[2]]

  immig_list <- list(immig_p = as.matrix(immig_p[1:NROW(M0)]),
                     immig_a = as.matrix(immig_a[1:NCOL(M0)]))
  return(immig_list)
}

# get extinction rates
get_ext_rate <- function(Mt,
                         status_p,
                         status_a,
                         mutualism_pars){
  mu_pars <- mutualism_pars$mu_pars
  pans_cmps_list <- get_pans_cmps(Mt = Mt,
                                  status_p = status_p,
                                  status_a = status_a)
  ext_p <- pmax(0, mu_pars[1] - mu_pars[3] * pans_cmps_list[[1]]) * status_p
  ext_a <- pmax(0, mu_pars[2] - mu_pars[4] * pans_cmps_list[[2]]) * status_a
  ext_list <- list(ext_p = ext_p,
                   ext_a = ext_a)
  return(ext_list)
}

# get anagenetic rates
get_ana_rate <- function(Mt,
                         status_p,
                         status_a,
                         mutualism_pars,
                         island_spec){
  laa_pars <- mutualism_pars$laa_pars
  M0 <- mutualism_pars$M0

  possible_ana_p <- matrix(0, nrow = NROW(M0), ncol = 1)
  possible_ana_a <- matrix(0, nrow = NCOL(M0), ncol = 1)
  index_p <- as.numeric(island_spec[intersect(which(island_spec[ ,4] == "I"),
                                              which(island_spec[ ,8] == "plant")),1])
  index_a <- as.numeric(island_spec[intersect(which(island_spec[ ,4] == "I"),
                                              which(island_spec[ ,8] == "animal")),1])
  possible_ana_p[index_p] <- 1
  possible_ana_a[index_a] <- 1

  ana_p <- (laa_pars[1] + laa_pars[3] * abs(Mt[1:NROW(M0), 1:NCOL(M0)] - M0)
                      %*% status_a[1:NCOL(M0)]) * status_p[1:NROW(M0)] * possible_ana_p
  ana_a <- (laa_pars[2] + laa_pars[4] * t(abs(Mt[1:NROW(M0), 1:NCOL(M0)] - M0))
            %*% status_p[1:NROW(M0)]) * status_a[1:NCOL(M0)] * possible_ana_a

  ana_list <- list(ana_p = ana_p,
                   ana_a = ana_a)
  return(ana_list)
}

# get cladogenetic rates
get_clado_rate <- function(Mt,
                           status_p,
                           status_a,
                           mutualism_pars){
  lac_pars <- mutualism_pars$lac_pars
  nk_list <- get_nk(Mt = Mt,
                    status_p = status_p,
                    status_a = status_a,
                    mutualism_pars = mutualism_pars)
  clado_p <- lac_pars[1] * nk_list[[1]] * status_p #mind the conflict, status is a matrix
  clado_a <- lac_pars[2] * nk_list[[2]] * status_a

  clado_list <- list(clado_p = clado_p,
                     clado_a = clado_a)
  return(clado_list)
}

# get cospeciation rates
get_cospec_rate <- function(Mt,
                            status_p,
                            status_a,
                            mutualism_pars){
  lambda0 <- mutualism_pars$lambda0
  nk_list <- get_nk(Mt = Mt,
                    status_p = status_p,
                    status_a = status_a,
                    mutualism_pars = mutualism_pars)
  expd_status_list <- get_expd_status(Mt = Mt,
                                      status_p = status_p,
                                      status_a = status_a)
  cospec_rate <-
    lambda0 * Mt * expd_status_list[[1]] * expd_status_list[[2]] *
    do.call("cbind", rep(list(nk_list[[1]]), NCOL(Mt))) *
    do.call("rbind", rep(list(nk_list[[2]]), NROW(Mt)))
  return(cospec_rate)
}

# get gain rates
get_gain_rate <- function(Mt,
                          status_p,
                          status_a,
                          mutualism_pars){
  qgain <- mutualism_pars$qgain
  expd_status_list <- get_expd_status(Mt = Mt,
                                      status_p = status_p,
                                      status_a = status_a)
  gain_rate <- qgain * (1 - Mt) * expd_status_list[[1]] * expd_status_list[[2]]
  return(gain_rate)
}

# get loss rates
get_loss_rate <- function(Mt,
                          status_p,
                          status_a,
                          mutualism_pars){
  qloss <- mutualism_pars$qloss
  expd_status_list <- get_expd_status(Mt = Mt,
                                      status_p = status_p,
                                      status_a = status_a)
  loss_rate <- qloss * Mt * (expd_status_list[[1]] * expd_status_list[[2]]
                             + (1 - expd_status_list[[1]]) * expd_status_list[[2]]
                             + (1 - expd_status_list[[2]]) * expd_status_list[[1]])
  return(loss_rate)
}

# test if object rates are rates
are_rates <- function(rates){
  if (!all(sapply(rates, is.numeric))) return (FALSE)
  if (!"immig_p" %in% names(rates)) return (FALSE)
  if (!"ext_p" %in% names(rates)) return (FALSE)
  if (!"ana_p" %in% names(rates)) return (FALSE)
  if (!"clado_p" %in% names(rates)) return (FALSE)
  if (!"immig_a" %in% names(rates)) return (FALSE)
  if (!"ext_a" %in% names(rates)) return (FALSE)
  if (!"ana_a" %in% names(rates)) return (FALSE)
  if (!"clado_a" %in% names(rates)) return (FALSE)
  if (!"cospec_rate" %in% names(rates)) return (FALSE)
  if (!"gain_rate" %in% names(rates)) return (FALSE)
  if (!"loss_rate" %in% names(rates)) return (FALSE)
  if (prod(rates$immig_p) < 0.0) return (FALSE)
  if (prod(rates$ext_p) < 0.0) return (FALSE)
  if (prod(rates$ana_p) < 0.0) return (FALSE)
  if (prod(rates$clado_p) < 0.0) return (FALSE)
  if (prod(rates$immig_a) < 0.0) return (FALSE)
  if (prod(rates$ext_a) < 0.0) return (FALSE)
  if (prod(rates$ana_a) < 0.0) return (FALSE)
  if (prod(rates$clado_a) < 0.0) return (FALSE)
  if (prod(rates$cospec_rate) < 0.0) return (FALSE)
  if (prod(rates$gain_rate) < 0.0) return (FALSE)
  if (prod(rates$loss_rate) < 0.0) return (FALSE)
  return (TRUE)
}

