# get gain rates
get_gain_rate <- function(Mt,
                          status_p,
                          status_a,
                          qgain){
  expd_status_list <- get_expd_status(Mt = Mt,
                                      status_p = status_p,
                                      status_a = status_a)
  gain_rate <- qgain * (1 - Mt) * expd_status_list[[1]] * expd_status_list[[2]]
  return(gain_rate)
}
