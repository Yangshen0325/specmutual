# get cospeciation rates
get_cospec_rate <- function(Mt,
                            wrates_list,
                            status_p,
                            status_a,
                            lambda0) {
  cospec_rate <-
    lambda0 * Mt * (status_p %*% t(status_a)) * (wrates_list[[1]] %*% t(wrates_list[[2]]))
  return(cospec_rate)
}
