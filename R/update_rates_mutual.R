#' Calculates algorithm rates
#'
#' @description Internal function that updates the all the rates at time t.
#'
#' @return a named list with the updated rates.
#'
update_rates_mutual <- function(M0,
                                Mt,
                                alphaa,
                                status_p,
                                status_a,
                                lac_pars,
                                mu_pars,
                                K_pars,
                                gam_pars,
                                laa_pars,
                                qgain,
                                qloss,
                                lambda0,
                                transprob,
                                island_spec) {
  partners_list <- get_partners(
    Mt = Mt,
    status_p = status_p,
    status_a = status_a
  )

  wrates_list <- get_wrates(
    alphaa = alphaa,
    status_p = status_p,
    status_a = status_a,
    K_pars = K_pars,
    partners_list = partners_list
  )

  immig_rate <- get_immig_rate(
    M0 = M0,
    wrates_list = wrates_list,
    gam_pars = gam_pars
  )

  ext_rate <- get_ext_rate(
    partners_list = partners_list,
    status_p = status_p,
    status_a = status_a,
    mu_pars = mu_pars
  )

  ana_rate <- get_ana_rate(
    M0 = M0,
    Mt = Mt,
    status_p = status_p,
    status_a = status_a,
    laa_pars = laa_pars,
    island_spec = island_spec
  )

  clado_rate <- get_clado_rate(
    wrates_list = wrates_list,
    status_p = status_p,
    status_a = status_a,
    lac_pars = lac_pars
  )

  t_status_a <- t(status_a)

  pa_table <- get_pa_table(
    status_p = status_p,
    status_a = status_a,
    t_status_a = t_status_a
  )

  cospec_rate <- get_cospec_rate(
    Mt = Mt,
    wrates_list = wrates_list,
    pa_table = pa_table,
    lambda0 = lambda0
  )

  gain_rate <- get_gain_rate(
    Mt = Mt,
    pa_table = pa_table,
    qgain = qgain
  )

  loss_rate <- get_loss_rate(
    Mt = Mt,
    pa_table = pa_table,
    qloss = qloss
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


check_neg <- function(v) {
  number_neg <- sum(v < 0)
  if (number_neg %% 2 == 1) return(TRUE)

  return(FALSE)
}

# test if object rates are rates
are_rates <- function(rates) {
  if (!all(sapply(rates, is.numeric))) {
    return(FALSE)
  }
  if (!"immig_p" %in% names(rates)) {
    return(FALSE)
  }
  if (!"ext_p" %in% names(rates)) {
    return(FALSE)
  }
  if (!"ana_p" %in% names(rates)) {
    return(FALSE)
  }
  if (!"clado_p" %in% names(rates)) {
    return(FALSE)
  }
  if (!"immig_a" %in% names(rates)) {
    return(FALSE)
  }
  if (!"ext_a" %in% names(rates)) {
    return(FALSE)
  }
  if (!"ana_a" %in% names(rates)) {
    return(FALSE)
  }
  if (!"clado_a" %in% names(rates)) {
    return(FALSE)
  }
  if (!"cospec_rate" %in% names(rates)) {
    return(FALSE)
  }
  if (!"gain_rate" %in% names(rates)) {
    return(FALSE)
  }
  if (!"loss_rate" %in% names(rates)) {
    return(FALSE)
  }

  if(check_neg(rates$immig_p) == TRUE) {
    return(FLASE)
  }
  if(check_neg(rates$ext_p) == TRUE) {
    return(FLASE)
  }
  if(check_neg(rates$ana_p) == TRUE) {
    return(FLASE)
  }
  if(check_neg(rates$clado_p) == TRUE) {
    return(FLASE)
  }
  if(check_neg(rates$immig_a) == TRUE) {
    return(FLASE)
  }
  if(check_neg(rates$ext_a) == TRUE) {
    return(FLASE)
  }
  if(check_neg(rates$ana_a) == TRUE) {
    return(FLASE)
  }
  if(check_neg(rates$clado_a) == TRUE) {
    return(FLASE)
  }
  if(check_neg(rates$cospec_rate) == TRUE) {
    return(FLASE)
  }
  if(check_neg(rates$gain_rate) == TRUE) {
    return(FLASE)
  }
  if(check_neg(rates$loss_rate) == TRUE) {
    return(FLASE)
  }
  return(TRUE)
}
