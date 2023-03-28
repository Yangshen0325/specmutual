# get cladogenetic rates
get_clado_rate <- function(nk_list,
                           status_p,
                           status_a,
                           lac_pars) {

  clado_p <- lac_pars[1] * pmax(0, (1 - nk_list[[1]])) * status_p #note the conflict, status is a matrix
  clado_a <- lac_pars[2] * pmax(0, (1 - nk_list[[2]])) * status_a

  clado_list <- list(clado_p = clado_p,
                     clado_a = clado_a)
  return(clado_list)
}
