# create parameters
# mutualism_pars <- list(lac_pars,mu_pars,K_pars,gam_pars,laa_pars,qgain,qloss,lambda0, M0, pro)
# lac_pars <- c(lac_plant, lac_animal)
# mu_pars <- c(mu_P0, mu_A0, mu_P1, mu_A1)
# K_pars <- c(K_P0, K_A0, K_P1, K_A1)
# gam_pars <- c(gam_plant, gam_animal)
# laa_pars <- c(laa_P0, laa_A0, laa_P1, laa_A1)

#' Title
#' @export create_mutualism_pars
create_mutualism_pars <- function(lac_pars,
                                  mu_pars,
                                  K_pars,
                                  gam_pars,
                                  laa_pars,
                                  qgain,
                                  qloss,
                                  lambda0,
                                  M0,
                                  transprob) {
  testit::assert(is.numeric(lac_pars))
  testit::assert(is.numeric(mu_pars))
  testit::assert(is.numeric(K_pars))
  testit::assert(is.numeric(gam_pars))
  testit::assert(is.numeric(laa_pars))
  testit::assert(is.numeric(qgain))
  testit::assert(is.numeric(qloss))
  testit::assert(is.numeric(lambda0))
  testit::assert(is.matrix(M0))
  testit::assert(is.numeric(transprob))
  testit::assert(lac_pars >= 0.0)
  testit::assert(mu_pars >= 0.0)
  testit::assert(K_pars[1] > 0.0)
  testit::assert(K_pars[2] > 0.0)
  testit::assert(K_pars[3] >= 0.0)
  testit::assert(K_pars[4] >= 0.0)
  testit::assert(gam_pars >= 0.0)
  testit::assert(laa_pars >= 0.0)
  testit::assert(qgain >= 0.0)
  testit::assert(qloss >= 0.0)
  testit::assert(lambda0 >= 0.0)
  testit::assert(transprob >= 0.0 & transprob <= 1.0)
  list(lac_pars = lac_pars,
       mu_pars = mu_pars,
       K_pars = K_pars,
       gam_pars = gam_pars,
       laa_pars = laa_pars,
       qgain = qgain,
       qloss = qloss,
       lambda0 = lambda0,
       M0 = M0,
       transprob = transprob)}

# test if a list has mutualism parameters
are_mutualism_pars <- function(mutualism_pars){
  if (class(mutualism_pars) != class(list())) return (FALSE)
  if (!"lac_pars" %in% names(mutualism_pars)) return (FALSE)
  if (!"mu_pars" %in% names(mutualism_pars)) return (FALSE)
  if (!"K_pars" %in% names(mutualism_pars)) return (FALSE)
  if (!"gam_pars" %in% names(mutualism_pars)) return (FALSE)
  if (!"laa_pars" %in% names(mutualism_pars)) return (FALSE)
  if (!"qgain" %in% names(mutualism_pars)) return (FALSE)
  if (!"qloss" %in% names(mutualism_pars)) return (FALSE)
  if (!"lambda0" %in% names(mutualism_pars)) return (FALSE)
  if (!"M0" %in% names(mutualism_pars)) return (FALSE)
  if (!"transprob" %in% names(mutualism_pars)) return (FALSE)
  if (any(mutualism_pars$lac_pars < 0.0)) return (FALSE)
  if (any(mutualism_pars$mu_pars < 0.0)) return (FALSE)
  if (any(mutualism_pars$K_pars < 0.0)) return (FALSE)
  if (any(mutualism_pars$gam_pars < 0.0)) return (FALSE)
  if (any(mutualism_pars$laa_pars < 0.0)) return (FALSE)
  if (mutualism_pars$qgain < 0.0) return (FALSE)
  if (mutualism_pars$qloss < 0.0) return (FALSE)
  if (mutualism_pars$lambda0 < 0.0) return (FALSE)
  if (!is.array(mutualism_pars$M0) | !is.matrix(mutualism_pars$M0)) return (FALSE)
  if (mutualism_pars$transprob < 0.0) return (FALSE)
  return(TRUE)
}



