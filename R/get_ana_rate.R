# get anagenetic rates
get_ana_rate <- function(M0,
                         Mt,
                         status_p,
                         status_a,
                         laa_pars,
                         island_spec) {

  possible_ana_p <- matrix(0, nrow = nrow(M0), ncol = 1)
  possible_ana_a <- matrix(0, nrow = ncol(M0), ncol = 1)
  index_p <- as.numeric(island_spec[intersect(which(island_spec[, 4] == "I"),
                                              which(island_spec[, 8] == "plant")), 1])
  index_a <- as.numeric(island_spec[intersect(which(island_spec[, 4] == "I"),
                                              which(island_spec[, 8] == "animal")), 1])
  possible_ana_p[index_p] <- 1
  possible_ana_a[index_a] <- 1

  precomp_mt <- abs(Mt[1:nrow(M0), 1:ncol(M0)] - M0)
  ana_p <- (laa_pars[1] + laa_pars[3] * precomp_mt %*% status_a[1:ncol(M0)]) * status_p[1:nrow(M0)] * possible_ana_p
  ana_a <- (laa_pars[2] + laa_pars[4] * t(precomp_mt) %*% status_p[1:nrow(M0)]) * status_a[1:ncol(M0)] * possible_ana_a


  ana_list <- list(ana_p = ana_p,
                   ana_a = ana_a)
  return(ana_list)
}
