# get immigration rates
get_immig_rate <- function(M0,
                           Mt,
                           K_pars,
                           status_p,
                           status_a,
                           gam_pars){

  nk_list <- get_nk(Mt = Mt,
                    status_p = status_p,
                    status_a = status_a,
                    K_pars = K_pars)
  immig_p <- gam_pars[1] * (1 - nk_list[[1]])
  immig_a <- gam_pars[2] * (1 - nk_list[[2]])

  immig_list <- list(immig_p = as.matrix(immig_p[1:nrow(M0)]),
                     immig_a = as.matrix(immig_a[1:ncol(M0)]))
  return(immig_list)
}
