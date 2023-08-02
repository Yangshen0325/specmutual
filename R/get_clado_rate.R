# get cladogenetic rates
get_clado_rate <- function(wrates_list,
                           status_p,
                           status_a,
                           lac_pars) {

  clado_p <- lac_pars[1] * wrates_list[[1]] * status_p
  clado_a <- lac_pars[2] * wrates_list[[2]] * status_a

  clado_list <- list(clado_p = clado_p,
                     clado_a = clado_a)
  return(clado_list)
}
