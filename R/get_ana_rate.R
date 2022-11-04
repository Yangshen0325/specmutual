# get anagenetic rates
get_ana_rate <- function(M0,
                         Mt,
                         status_p,
                         status_a,
                         laa_pars,
                         island_plant,
                         island_animal){

  possible_ana_p <- matrix(0, nrow = nrow(M0), ncol = 1)
  possible_ana_a <- matrix(0, nrow = ncol(M0), ncol = 1)
  index_p <- as.numeric(island_plant[which(island_plant[, 4] == "I"), 1])
  index_a <- as.numeric(island_animal[which(island_animal[, 4] == "I"), 1])
  possible_ana_p[index_p] <- 1
  possible_ana_a[index_a] <- 1

  ana_p <- (laa_pars[1] + laa_pars[3] * abs(Mt[1:nrow(M0), 1:ncol(M0)] - M0)
            %*% status_a[1:ncol(M0)]) * status_p[1:nrow(M0)] * possible_ana_p
  ana_a <- (laa_pars[2] + laa_pars[4] * t(abs(Mt[1:nrow(M0), 1:ncol(M0)] - M0))
            %*% status_p[1:nrow(M0)]) * status_a[1:ncol(M0)] * possible_ana_a

  ana_list <- list(ana_p = ana_p,
                   ana_a = ana_a)
  return(ana_list)
}
