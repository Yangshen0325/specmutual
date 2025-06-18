#' Update Rates at Time t
#'
#' Internal function that calculates and returns all updated rates
#' for plants and animals at a given time step, considering mutualistic interactions and species status.
#'
#' @param M0 A matrix. Mainland interaction matrix, where rows represent plant species and columns represent animal species.
#' @param Mt A matrix. Representing the "false" matrix on the island, including extinct and non-immigrated species occupying
#'            their original indices from the mainland.
#' @param alpha A numeric value. Coefficient to mediate rates.
#' @param status_p A one column matrix indicating the presence of the plant species
#'   on the island (Presence (1) or absence (0)).
#' @param status_a A one column matrix indicating the presence of the animal species
#'   on the island (Presence (1) or absence (0)).
#' @param lac_pars Numeric vector of length 2. Cladogenesis rates: \code{c(lac_plant, lac_animal)}.
#' @param mu_pars Numeric vector of length 4. Extinction rates: \code{c(mu_P0, mu_A0, mu_P1, mu_A1)}.
#' @param K_pars Numeric vector of length 4. Carrying capacities: \code{c(K_P0, K_A0, K_P1, K_A1)}.
#' @param gam_pars Numeric vector of length 2. Immigration rates: \code{c(gam_plant, gam_animal)}.
#' @param laa_pars Numeric vector of length 4. Anagenesis rates: \code{c(laa_P0, laa_A0, laa_P1, laa_A1)}.
#' @param qgain Numeric. Rate of gaining a new link.
#' @param qloss Numeric. Rate of losing a link.
#' @param lambda0 A numeric value. Intrinsic cospeciation rate.
#' @param transprob  A numeric value. Probability for daughter species to inherit the link from the mainland ancestors.
#' @param partners_list A list. Each element contains the indices of mutualistic partners for a species.
#' @param island_spec A data frame. Current island species composition and their status.
#'
#' @return A named list containing the updated rates.
#'
#'
#' @export
update_rates_mutual <- function(M0,
                                Mt,
                                alpha,
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
                                partners_list,
                                island_spec) {


  # Get the mutualism effects for immigration and cladogenesis
  mutualism_effect_list <- calculate_mutualism_effect(
  alpha = alpha,
  status_p = status_p,
  status_a = status_a,
  K_pars = K_pars,
  partners_list = partners_list
)

  immig_rate <- get_immig_rate(
    M0 = M0,
    mutualism_effect_list =  mutualism_effect_list,
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
    mutualism_effect_list =  mutualism_effect_list,
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
    pa_table = pa_table,
    lambda0 = lambda0,
    alpha = alpha,
    K_pars = K_pars,
    status_p = status_p,
    status_a = status_a
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
