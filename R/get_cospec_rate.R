# get cospeciation rates
get_cospec_rate <- function(Mt,
                            nk_list,
                            status_p,
                            status_a,
                            lambda0) {
  cospec_rate <-
    lambda0 * Mt * (status_p %*% t(status_a)) * ((1 - nk_list[[1]])
                                                 %*% t(1 - nk_list[[2]]))
  return(cospec_rate)
}
