# get cospeciation rates
get_cospec_rate <- function(Mt,
                            wrates_list,
                            status_p,
                            t_status_a,
                            lambda0) {
  return(lambda0 * Mt *
           (status_p %*% t_status_a) *
           (wrates_list[[1]] %*% t(wrates_list[[2]])))
}
