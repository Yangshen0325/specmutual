#' Create a Mutualism-Related Parameter List for Simulation
#'
#' This function creates a named list of parameters related to mutualism used in the model.
#' It bundles all required rates and constants into a single list.
#'
#' @param lac_pars A numeric vector of length 2. Intrinsic cladogenesis rates: \code{c(lac_plant, lac_animal)}.
#' @param mu_pars A numeric vector of length 4. Intrinsic extinction rates: \code{c(mu_P0, mu_A0, mu_P1, mu_A1)}.
#' @param K_pars A numeric vector of length 4. Intrinsic carrying capacities + coefficient: \code{c(K_P0, K_A0, K_P1, K_A1)}.
#' @param gam_pars A numeric vector of length 2. Intrinsic immigration rates: \code{c(gam_plant, gam_animal)}.
#' @param laa_pars A numeric vector of length 4. Intrinsic anagenesis rates + coefficient: \code{c(laa_P0, laa_A0, laa_P1, laa_A1)}.
#' @param qgain A numeric value. Rate of a mutualistic link gain.
#' @param qloss A numeric value. Rate of a mutualistic link loss.
#' @param lambda0 A numeric value. Intrinsic cospeciation rate.
#' @param M0 A matrix. Mainland interaction matrix, where rows represent plant species and columns represent animal species.
#' @param transprob A numeric value. Probability for daughter species to inherit the link from the mainland ancestors.
#' @param alpha A numeric value. Coefficient to mediate rates.
#'
#' @return A named list containing all input parameters, structured for use in simulation.
#'
#' @examples
#'mutualism_pars <- create_mutual_pars(
#'       lac_pars = c(0.3, 0.3),
#'       mu_pars = c(0.05, 0.05, 0.01, 0.01),
#'       K_pars = c(50, 50, 100, 100),
#'       gam_pars = c(0.04, 0.04),
#'       laa_pars = c(0.01, 0.01, 0.01, 0.01),
#'       qgain = 0.001,
#'       qloss = 0.001,
#'       lambda0 = 0.5,
#'       M0 = {set.seed(123); matrix(runif(100, 0, 1), nrow = 10, ncol = 10)},
#'       transprob = 1,
#'       alpha = 100
#'   )
#'
#' @export create_mutual_pars
create_mutual_pars <- function(lac_pars,
                                  mu_pars,
                                  K_pars,
                                  gam_pars,
                                  laa_pars,
                                  qgain,
                                  qloss,
                                  lambda0,
                                  M0,
                                  transprob,
                                  alpha) {
  testit::assert(is.numeric(lac_pars))
  testit::assert(is.numeric(mu_pars))
  testit::assert(is.numeric(K_pars))
  testit::assert(is.numeric(gam_pars))
  testit::assert(is.numeric(laa_pars))
  testit::assert(is.numeric(qgain))
  testit::assert(is.numeric(qloss))
  testit::assert(is.numeric(lambda0))
  testit::assert(is.numeric(alpha))
  testit::assert(is.matrix(M0))
  testit::assert(is.numeric(transprob))
  testit::assert(lac_pars >= 0.0)
  testit::assert(mu_pars >= 0.0)
  testit::assert(K_pars >= 0.0)
  testit::assert(gam_pars >= 0.0)
  testit::assert(laa_pars >= 0.0)
  testit::assert(qgain >= 0.0)
  testit::assert(qloss >= 0.0)
  testit::assert(lambda0 >= 0.0)
  testit::assert(alpha >= 0.0)
  testit::assert(transprob >= 0.0 & transprob <= 1.0)
  list(
    lac_pars = lac_pars,
    mu_pars = mu_pars,
    K_pars = K_pars,
    gam_pars = gam_pars,
    laa_pars = laa_pars,
    qgain = qgain,
    qloss = qloss,
    lambda0 = lambda0,
    M0 = M0,
    transprob = transprob,
    alpha = alpha
  )
}

# test if a list has mutualism parameters
are_mutualism_pars <- function(mutualism_pars) {
  if (class(mutualism_pars) != class(list())) {
    return(FALSE)
  }
  if (!"lac_pars" %in% names(mutualism_pars)) {
    return(FALSE)
  }
  if (!"mu_pars" %in% names(mutualism_pars)) {
    return(FALSE)
  }
  if (!"K_pars" %in% names(mutualism_pars)) {
    return(FALSE)
  }
  if (!"gam_pars" %in% names(mutualism_pars)) {
    return(FALSE)
  }
  if (!"laa_pars" %in% names(mutualism_pars)) {
    return(FALSE)
  }
  if (!"qgain" %in% names(mutualism_pars)) {
    return(FALSE)
  }
  if (!"qloss" %in% names(mutualism_pars)) {
    return(FALSE)
  }
  if (!"lambda0" %in% names(mutualism_pars)) {
    return(FALSE)
  }
  if (!"M0" %in% names(mutualism_pars)) {
    return(FALSE)
  }
  if (!"transprob" %in% names(mutualism_pars)) {
    return(FALSE)
  }
  if (!"alpha" %in% names(mutualism_pars)) {
    return(FALSE)
  }
  if (any(mutualism_pars$lac_pars < 0.0)) {
    return(FALSE)
  }
  if (any(mutualism_pars$mu_pars < 0.0)) {
    return(FALSE)
  }
  if (any(mutualism_pars$K_pars < 0.0)) {
    return(FALSE)
  }
  if (any(mutualism_pars$gam_pars < 0.0)) {
    return(FALSE)
  }
  if (any(mutualism_pars$laa_pars < 0.0)) {
    return(FALSE)
  }
  if (mutualism_pars$qgain < 0.0) {
    return(FALSE)
  }
  if (mutualism_pars$qloss < 0.0) {
    return(FALSE)
  }
  if (mutualism_pars$lambda0 < 0.0) {
    return(FALSE)
  }
  if (!is.array(mutualism_pars$M0) | !is.matrix(mutualism_pars$M0)) {
    return(FALSE)
  }
  if (mutualism_pars$transprob < 0.0) {
    return(FALSE)
  }
  if (mutualism_pars$alpha < 0.0) {
    return(FALSE)
  }
  return(TRUE)
}
