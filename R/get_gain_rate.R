# get gain rates
get_gain_rate <- function(Mt,
                          status_p,
                          status_a,
                          qgain){
  both_shown <- status_p %*% t(status_a)
  gain_rate <- qgain * (1 - Mt) * both_shown
  return(gain_rate)
}
