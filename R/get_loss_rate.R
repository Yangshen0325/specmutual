# get loss rates
get_loss_rate <- function(Mt,
                          status_p,
                          status_a,
                          qloss){
  expd_status_list <- get_expd_status(Mt = Mt,
                                      status_p = status_p,
                                      status_a = status_a)
  loss_rate <- qloss * Mt * (expd_status_list[[1]] * expd_status_list[[2]]
                             + (1 - expd_status_list[[1]]) * expd_status_list[[2]]
                             + (1 - expd_status_list[[2]]) * expd_status_list[[1]])
  return(loss_rate)
}
