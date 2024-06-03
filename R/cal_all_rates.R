# Functions to calculate all kinds of rates


# Immigration rates -------------------------------------------------------

get_immig_rate <- function(M0,
                           wrates_list,
                           gam_pars) {
  immig_p <- gam_pars[1] * wrates_list[[1]]
  immig_a <- gam_pars[2] * wrates_list[[2]]

  # immigration rates only apply to mainland species
  immig_list <- list(
    immig_p = as.matrix(immig_p[1:nrow(M0)]),
    immig_a = as.matrix(immig_a[1:ncol(M0)])
  )

  return(immig_list)
}

# Extinction rates --------------------------------------------------------

get_ext_rate <- function(partners_list,
                         status_p,
                         status_a,
                         mu_pars) {
  # extinction rates only apply to island species
  ext_p <- pmax(0, mu_pars[1] - mu_pars[3] * partners_list[[1]]) * status_p
  ext_a <- pmax(0, mu_pars[2] - mu_pars[4] * partners_list[[2]]) * status_a

  ext_list <- list(
    ext_p = ext_p,
    ext_a = ext_a
  )

  return(ext_list)
}

# Anagenesis rates --------------------------------------------------------

get_ana_rate <- function(M0,
                         Mt,
                         status_p,
                         status_a,
                         laa_pars,
                         island_spec) {
  # initialise
  possible_ana_p <- matrix(0, nrow = nrow(M0), ncol = 1)
  possible_ana_a <- matrix(0, nrow = ncol(M0), ncol = 1)

  # only mainland acestors on islands can go through anagenesis
  index_p <- as.numeric(island_spec[intersect(
    which(island_spec[, 4] == "I"),
    which(island_spec[, 8] == "plant")
  ), 1])
  index_a <- as.numeric(island_spec[intersect(
    which(island_spec[, 4] == "I"),
    which(island_spec[, 8] == "animal")
  ), 1])
  possible_ana_p[index_p] <- 1
  possible_ana_a[index_a] <- 1

  # the difference between the species on islands and in the mainland
  precomp_mt <- abs(Mt[1:nrow(M0), 1:ncol(M0)] - M0)

  ana_p <- (laa_pars[1] + laa_pars[3] * precomp_mt %*% status_a[1:ncol(M0)]) *
    status_p[1:nrow(M0)] * possible_ana_p # whether the plant is on the island and whether is
  # possible to anagenesis
  ana_a <- (laa_pars[2] + laa_pars[4] * t(precomp_mt) %*% status_p[1:nrow(M0)]) *
    status_a[1:ncol(M0)] * possible_ana_a # whether the animal is on the island and whether is
  # possible to anagenesis

  ana_list <- list(
    ana_p = ana_p,
    ana_a = ana_a
  )
  return(ana_list)
}

# Cladogenesis rates ------------------------------------------------------

get_clado_rate <- function(wrates_list,
                           status_p,
                           status_a,
                           lac_pars) {
  # cladogenesis rates only apply to species on islands
  clado_p <- lac_pars[1] * wrates_list[[1]] * status_p
  clado_a <- lac_pars[2] * wrates_list[[2]] * status_a

  clado_list <- list(
    clado_p = clado_p,
    clado_a = clado_a
  )
  return(clado_list)
}

# Co-speciation rates -----------------------------------------------------

get_cospec_rate <- function(Mt,
                            wrates_list,
                            pa_table,
                            lambda0) {
  cospec_rate <- lambda0 * Mt * pa_table[[1]] *
    (wrates_list[[1]] %*% t(wrates_list[[2]]))

  return(cospec_rate)
}

# Gain links rates --------------------------------------------------------

get_gain_rate <- function(Mt,
                          pa_table,
                          qgain) {
  # both_shown <- (status_p %*% t(status_a)
  gain_rate <- qgain * ((1 - Mt) * pa_table[[1]])

  return(gain_rate)
}

# Lose links rates --------------------------------------------------------

get_loss_rate <- function(Mt,
                          pa_table,
                          qloss) {
  loss_rate <- qloss * Mt * (pa_table[[1]] + pa_table[[2]] + pa_table[[3]])

  return(loss_rate)
}
