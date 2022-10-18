# get cospeciation rates
get_cospec_rate <- function(Mt,
                            status_p,
                            status_a,
                            lambda0,
                            K_pars){
  nk_list <- get_nk(Mt = Mt,
                    status_p = status_p,
                    status_a = status_a,
                    K_pars = K_pars)

  cospec_rate <-
    lambda0 * Mt * (status_p %*% t(status_a)) * (nk_list[[1]] %*% t(nk_list[[2]]))
  return(cospec_rate)
}
