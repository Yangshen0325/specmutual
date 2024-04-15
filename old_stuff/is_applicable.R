# Test if the network is applicable

is_applicable <- function(net) {
  if (!"matrix" %in% class(net)) {
    return(FALSE)
  }
  if (nrow(net) == 0) {
    return(FALSE)
  }
  if (ncol(net) == 0) {
    return(FALSE)
  }
  if (nrow(net) < 2) {
    return(FALSE)
  }
  if (ncol(net) < 2) {
    return(FALSE)
  }
  if (all(net == 1)) {
    return(FALSE)
  }

  return(TRUE)
}
