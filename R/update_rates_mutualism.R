# calculate algorithm rates
update_rates_mutualism <- function(Mt,
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
                                   M0,
                                   transprob,
                                   island_spec){
  testit::assert(are_mutualism_pars(mutualism_pars))

  immig_rate <- get_immig_rate(
    M0 = M0,
    Mt = Mt,
    status_p = status_p,
    status_a = status_a,
    gam_pars = gam_pars
  )

  ext_rate <- get_ext_rate(
    Mt = Mt,
    status_p = status_p,
    status_a = status_a,
    mu_pars = mu_pars)

  ana_rate <- get_ana_rate(
    M0 = M0,
    Mt = Mt,
    status_p = status_p,
    status_a = status_a,
    laa_pars = laa_pars,
    island_spec = island_spec
  )

  clado_rate <- get_clado_rate(
    Mt = Mt,
    status_p = status_p,
    status_a = status_a,
    lac_pars = lac_pars
  )

  cospec_rate <- get_cospec_rate(
    Mt = Mt,
    status_p = status_p,
    status_a = status_a,
    lambda0 = lambda0
  )

  gain_rate <- get_gain_rate(
    Mt = Mt,
    status_p = status_p,
    status_a = status_a,
    qgain = qgain
  )

  loss_rate <- get_loss_rate(
    Mt = Mt,
    status_p = status_p,
    status_a = status_a,
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

