# get mutualistic partners and competitors for plant and animal species respectively

get_pans_cmps <- function(Mt,
                          status_p,
                          status_a){
  tMt <- t(Mt)
  pans_p <- Mt %*% status_a
  pans_a <- tMt %*% status_p

  cmps_p <- c()
  cmps_a <- c()
  for (x in seq(NROW(Mt))){
    cmps_p[x] <- sum((colSums(tMt[, x] * tMt[, -x] * as.numeric(status_a)) >= 1)
                     * status_p[-x, ])
  }
  for (x in seq(NCOL(Mt))){
    cmps_a[x] <- sum((colSums(Mt[, x] * Mt[, -x] * as.numeric(status_p)) >= 1)
                     * status_a[-x, ])
  }

  pans_cmps_list <- list(pans_p = as.numeric(pans_p),
                         pans_a = as.numeric(pans_a),
                         cmps_p = cmps_p,
                         cmps_a = cmps_a)
  return(pans_cmps_list)
}
# test the format
test_format_pans_cmps <- function(pans_cmps_list,
                             status_p,
                             status_a) {
  if (!all(sapply(pans_cmps_list, is.numeric))) return(FALSE)
  if (!"pans_p" %in% names(pans_cmps_list)) return(FALSE)
  if (!"pans_a" %in% names(pans_cmps_list)) return(FALSE)
  if (!"cmps_p" %in% names(pans_cmps_list)) return(FALSE)
  if (!"cmps_a" %in% names(pans_cmps_list)) return(FALSE)
  if (any(pans_cmps_list$pans_p < 0.0)) return(FALSE)
  if (any(pans_cmps_list$pans_a < 0.0)) return(FALSE)
  if (any(pans_cmps_list$cmps_p < 0.0)) return(FALSE)
  if (any(pans_cmps_list$cmps_a < 0.0)) return(FALSE)
  if (length(pans_cmps_list$pans_p) != length(status_p)) return(FALSE)
  if (length(pans_cmps_list$pans_a) != length(status_a)) return(FALSE)
  if (length(pans_cmps_list$cmps_p) != length(status_p)) return(FALSE)
  if (length(pans_cmps_list$cmps_a) != length(status_a)) return(FALSE)
  return(TRUE)
}

# get N/K
get_nk <- function(Mt,
                   status_p,
                   status_a,
                   mutualism_pars){
  pans_cmps_list <- get_pans_cmps(Mt = Mt,
                                  status_p = status_p,
                                  status_a = status_a)
  K_pars <- mutualism_pars$K_pars
  nk_p <- exp(-(sum(status_p) / K_pars[1] + pans_cmps_list[[3]] / (K_pars[3] * pans_cmps_list[[1]])))
  nk_a <- exp(-(sum(status_a) / K_pars[2] + pans_cmps_list[[4]] / (K_pars[4] * pans_cmps_list[[2]])))
  # if there is no mutualistic partners on the island, N/K should be exp(-N/K).
  nk_p[which(pans_cmps_list[[1]] == 0)] <- exp(-(sum(status_p) / K_pars[1]))
  nk_a[which(pans_cmps_list[[2]] == 0)] <- exp(-(sum(status_a) / K_pars[2]))

  nk_list <- list(nk_p = nk_p,
                  nk_a = nk_a)
  return(nk_list)
}
# test the format
test_format_nk <- function(nk_list,
                           status_p,
                           status_a) {
  if (!all(sapply(nk_list, is.numeric))) return(FALSE)
  if (!"nk_p" %in% names(nk_list)) return(FALSE)
  if (!"nk_a" %in% names(nk_list)) return(FALSE)
  if (any(nk_list$nk_p < 0.0)) return(FALSE)
  if (any(nk_list$nk_a < 0.0)) return(FALSE)
  if (length(nk_list$nk_p) != length(status_p)) return(FALSE)
  if (length(nk_list$nk_a) != length(status_a)) return(FALSE)
  return(TRUE)
}

# get a new matrix when cladogenesis happens
newMt_clado <- function(M,
                        possible_event,
                        mutualism_pars){
  transprob <- mutualism_pars$transprob
  x <- possible_event$plant
  newrows <- list()
  possible_output <- list(c(1,1), c(1,0), c(0,1))
  newrows[which(M[x, ] == 0)] <- list(c(0,0))
  newrows[which(M[x, ] == 1)] <- sample(possible_output,
                                         size = length(which(M[x, ] == 1)),
                                         replace = TRUE,
                                         prob = c(transprob,
                                                 (1-transprob) / 2,
                                                 (1-transprob) / 2))
  newrows <- matrix(unlist(newrows), nrow = 2, ncol = NCOL(M))
  M <- rbind(M, newrows)
  return(M)
}

# get a new matrix when anagenesis happens
newMt_ana <- function(M,
                      possible_event,
                      mutualism_pars){
  transprob <- mutualism_pars$transprob
  x <- possible_event$plant
  newrows <- list()
  newrows[which(M[x, ] == 0)] <- 0
  newrows[which(M[x, ] == 1)] <- sample(c(1, 0),
                                        size = length(which(M[x, ] == 1)),
                                        replace = TRUE,
                                        prob = c(transprob,
                                                 1-transprob))
  newrows <- matrix(unlist(newrows), nrow = 1, ncol = NCOL(M))
  M <- rbind(M, newrows)
  return(M)
}

# get a new matrix if cospeciation happens
newMt_cospec <- function(M,
                         possible_event,
                         mutualism_pars){
  transprob <- mutualism_pars$transprob
  x <- possible_event$plant
  y <- possible_event$animal
  newrows <- list()
  newcols <- list()
  possible_output <- list(c(1,1), c(1,0), c(0,1))

  newrows[which(M[x, ] == 0)] <- list(c(0,0))
  newrows[which(M[x, ] == 1)] <- sample(possible_output,
                                        size = length(which(M[x, ] == 1)),
                                        replace = TRUE,
                                        prob = c(transprob,
                                                 (1-transprob) / 2,
                                                 (1-transprob) / 2))
  newrows <- matrix(unlist(newrows), nrow = 2, ncol = NCOL(M))
  newrows <- cbind(newrows, diag(1,2,2))

  newcols[which(M[ ,y] == 0)] <- list(c(0,0))
  newcols[which(M[ ,y] == 1)] <- sample(possible_output,
                                        size = length(which(M[ ,y] == 1)),
                                        replace = TRUE,
                                        prob = c(transprob,
                                                 (1-transprob) / 2,
                                                 (1-transprob) / 2))
  newcols <- t(matrix(unlist(newcols), nrow = 2, ncol = NROW(M)))
  M <- rbind(cbind(M, newcols), newrows)
  return(M)
}

# get status_p and status_a expanded
get_expd_status <- function(Mt,
                            status_p,
                            status_a){
  expd_status_p <- do.call("cbind", rep(list(status_p), NCOL(Mt)))
  expd_status_a <- do.call("rbind", rep(list(t(status_a)), NROW(Mt)))

  expd_status_list <- list(expd_status_p = expd_status_p,
                           expd_status_a = expd_status_a)

  return(expd_status_list)
}
