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
  expd_status_list <- get_expd_status(Mt = Mt,
                                      status_p = status_p,
                                      status_a = status_a)
  cospec_rate <-
    lambda0 * Mt * expd_status_list[[1]] * expd_status_list[[2]] *
    do.call("cbind", rep(list(nk_list[[1]]), ncol(Mt))) *
    do.call("rbind", rep(list(nk_list[[2]]), nrow(Mt)))
  return(cospec_rate)
}
