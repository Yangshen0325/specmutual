

#### end_ltt fucntion for mutualism version
end_ltt_mutual <- function(clades_info, brt) {

  stac <- unlist(lapply(clades_info, "[[", "stac"))

  # non_end
  nonend_brt <- c(unique(sort(unlist(brt[which(stac ==4)]),
                              decreasing = TRUE)), 0)
  if(length(nonend_brt) == 1) {
    nonend_brt <- 0
    n_nonend <- 0
  } else {
    n_nonend <- c(seq(0, length(nonend_brt)-2), length(nonend_brt)-2)
  }
  nonend_ltt <- data.frame(nonend_brt, n_nonend)

  #singleton (anagnesis)

  brt_length <- unlist(lapply(brt, length))

  singleton_brt <-c(unique(sort(unlist(brt[which(stac == 2 & brt_length == 2 )]),
                                decreasing = TRUE)), 0)
  if(length(singleton_brt) == 1) {
    singleton_brt <- 0
    n_singleton <- 0
  } else {
    n_singleton <- c(seq(0, length(singleton_brt)-2), length(singleton_brt)-2)
  }
  singleton_ltt <- data.frame(singleton_brt, n_singleton)

  # multi ltt (cladogenesis)
  multi_brt <-c(unique(sort(unlist(brt[which((stac ==2 & brt_length >2) | stac ==3)]),
                            decreasing = TRUE)), 0)
  if(length(multi_brt) == 1) {
    multi_brt <- 0
    n_multi <- 0
  } else {
    n_multi <- c(seq(0, length(multi_brt) - 2), length(multi_brt) - 2)
  }
  multi_ltt <- data.frame(multi_brt, n_multi)

  end_ltt_mutual <- list(nonend_ltt = nonend_ltt,
                    singleton_ltt = singleton_ltt,
                    multi_ltt = multi_ltt)

}
