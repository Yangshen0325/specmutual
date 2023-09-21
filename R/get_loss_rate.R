# get loss rates
 get_loss_rate <- function(Mt,
                          status_p,
                          status_a,
                          t_status_a,
                          qloss) {
  loss_rate <- qloss * Mt * (status_p %*% t_status_a +
               (1 - status_p) %*% t_status_a +
               status_p %*% t(1 - status_a))
  return(loss_rate)
}
