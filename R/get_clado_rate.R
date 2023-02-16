# get cladogenetic rates
get_clado_rate <- function(Mt,
                           status_p,
                           status_a,
                           lac_pars,
                           K_pars){
  nk_list <- get_nk(Mt = Mt,
                    status_p = status_p,
                    status_a = status_a,
                    K_pars = K_pars)
  clado_p <- lac_pars[1] * (1 - nk_list[[1]]) * status_p #mind the conflict, status is a matrix
  clado_a <- lac_pars[2] * (1 - nk_list[[2]]) * status_a

  clado_list <- list(clado_p = clado_p,
                     clado_a = clado_a)
  return(clado_list)
}
