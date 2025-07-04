# Function to calculate connectance
cal_C <- function(x) {

  dims <- dim(x)
  if(any(dims < 2)) { # we don't think it's worth to trust the connectance from very small networks
    return(NA)
  } else {
    the_connectance <- sum(x) / prod(dim(x))
    return(the_connectance)
  }
}
