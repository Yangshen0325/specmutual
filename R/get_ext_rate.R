# get extinction rates
get_ext_rate <- function(Mt,
                         status_p,
                         status_a,
                         mu_pars){
  pans_cmps_list <- get_pans_cmps(Mt = Mt,
                                  status_p = status_p,
                                  status_a = status_a)
  ext_p <- pmax(0, mu_pars[1] - mu_pars[3] * pans_cmps_list[[1]]) * status_p
  ext_a <- pmax(0, mu_pars[2] - mu_pars[4] * pans_cmps_list[[2]]) * status_a
  ext_list <- list(ext_p = ext_p,
                   ext_a = ext_a)
  return(ext_list)
}
