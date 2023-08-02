# get immigration rates
get_immig_rate <- function(M0,
                           wrates_list,
                           gam_pars) {
  immig_p <- gam_pars[1] * wrates_list[[1]]
  immig_a <- gam_pars[2] * wrates_list[[2]]

  immig_list <- list(immig_p = as.matrix(immig_p[1:nrow(M0)]),
                     immig_a = as.matrix(immig_a[1:ncol(M0)]))
  return(immig_list)
}
