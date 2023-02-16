#' @title Calculating mutualistic partners and competitors
#'
#' @param Mt Matrix with plant species on the row and animal species on the column.
#' Mt is the matrix on the island having the same size as \code{M0} at \code{
#' timeval = 0 }, yet it constantly keeps changing as events (immigration, extinction
#' and speciation) happening.
#' @param status_p,status_a Matrix indicating species status on the island, with 0
#' meaning absent, 1 meaning present.
#'
#' @return A list containing the number of reciprocal partners and competitors:
#'   \itemize{
#'     \item{\code{$pans_p}: A numeric vector about the number of partners of
#'     plant species}
#'     \item{\code{$pans_a}: A numeric vector about the number of partners of
#'     plant species}
#'     \item{\code{cmps_p}: A numeric vector about the number of competitors of
#'     plant species}
#'     \item{\code{cmps_a}: A numeric vector about the number of competitors of
#'     animal species}
#'     }

get_pans_cmps <- function(Mt,
                          status_p,
                          status_a){
  tMt <- t(Mt)
  pans_p <- Mt %*% status_a
  pans_a <- tMt %*% status_p

  cmps_p <- c()
  cmps_a <- c()
  for (x in seq(nrow(Mt))){
    copartner <- tMt[, x] * tMt[, -x] * as.numeric(status_a) # think of 2*2 network
    if (is.null(dim(copartner))){
      cmps_p[x] <- sum((copartner >= 1) * status_p[-x, ])
    } else {
      cmps_p[x] <- sum((colSums(copartner) >= 1) * status_p[-x, ])
    }
  }
  for (x in seq(ncol(Mt))){
    copartner <- Mt[, x] * Mt[, -x] * as.numeric(status_p) # think of 2*2 network
    if (is.null(dim(copartner))){
      cmps_a[x] <- sum((copartner >= 1) * status_a[-x, ])
    } else {
      cmps_a[x] <- sum((colSums(copartner) >= 1) * status_a[-x, ])
    }
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
                   K_pars){
  pans_cmps_list <- get_pans_cmps(Mt = Mt,
                                  status_p = status_p,
                                  status_a = status_a)

  nk_p <- (sum(status_p) + pans_cmps_list[[3]]) /
    (K_pars[1] + K_pars[3] * pans_cmps_list[[1]])

  nk_a <- (sum(status_a) + pans_cmps_list[[4]]) /
    (K_pars[2] + K_pars[4] * pans_cmps_list[[2]])

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


newMt_clado <- function(M,
                        tosplit,
                        transprob) {
  newrows <- list()
  possible_output <- list(c(1,1), c(1,0), c(0,1))
  newrows[which(M[tosplit, ] == 0)] <- list(c(0,0))
  newrows[which(M[tosplit, ] == 1)] <- sample(possible_output,
                                              size = length(which(M[tosplit, ] == 1)),
                                              replace = TRUE,
                                              prob = c(transprob,
                                                       (1-transprob) / 2,
                                                       (1-transprob) / 2))
  newrows <- matrix(unlist(newrows), nrow = 2, ncol = ncol(M))
  M <- rbind(M, newrows)
  return(M)
}

newMt_ana <- function(M,
                      anagenesis,
                      transprob) {
  newrows <- list()
  newrows[which(M[anagenesis, ] == 0)] <- 0
  newrows[which(M[anagenesis, ] == 1)] <- sample(c(1, 0),
                                                 size = length(which(M[anagenesis, ] == 1)),
                                                 replace = TRUE,
                                                 prob = c(transprob, 1 - transprob))
  newrows <- matrix(unlist(newrows), nrow = 1, ncol = ncol(M))
  M <- rbind(M, newrows)
  return(M)
}

newMt_cospec <- function(M,
                         cospec_plant,
                         cospec_animal,
                         transprob) {
  newrows <- list()
  newcols <- list()
  possible_output <- list(c(1,1), c(1,0), c(0,1))

  newrows[which(M[cospec_plant, ] == 0)] <- list(c(0,0))
  newrows[which(M[cospec_plant, ] == 1)] <- sample(possible_output,
                                                   size = length(which(M[cospec_plant, ] == 1)),
                                                   replace = TRUE,
                                                   prob = c(transprob,
                                                            (1-transprob) / 2,
                                                            (1-transprob) / 2))
  newrows <- matrix(unlist(newrows), nrow = 2, ncol = ncol(M))
  newrows <- cbind(newrows, diag(1,2,2))

  newcols[which(M[, cospec_animal] == 0)] <- list(c(0,0))
  newcols[which(M[, cospec_animal] == 1)] <- sample(possible_output,
                                                    size = length(which(M[, cospec_animal] == 1)),
                                                    replace = TRUE,
                                                    prob = c(transprob,
                                                             (1-transprob) / 2,
                                                             (1-transprob) / 2))
  newcols <- t(matrix(unlist(newcols), nrow = 2, ncol = nrow(M)))
  M <- rbind(cbind(M, newcols), newrows)
  return(M)
}

add_brt_table <- function(island, full_table = FALSE) {
  island_age <- island[[1]]$island_age
  island_top <- island[[1]]
  if (length(island) == 1) {
    brts_table <- matrix(ncol = 5, nrow = 1)
    brts_table[1, ] <-  c(island_age, 0, 0, NA, NA)
    island[[1]]$brts_table <- brts_table
  } else {
    island_top <- island[[1]]
    island[[1]] <- NULL
    btimes <- list()
    for (i in 1:length(island)) {
      btimes[[i]] <- island[[i]]$branching_times[-1]
    }
    island <- island[rev(order(sapply(btimes, "[", 1)))]
    il <- unlist(island)
    stac1s <- which(il[which(names(il) == "stac")] == "1")
    stac5s <- which(il[which(names(il) == "stac")] == "5")
    stac1_5s <- sort(c(stac1s, stac5s))
    if (length(stac1_5s) != 0) {
      if (length(stac1_5s) == length(island)) {
        brts_table <- matrix(ncol = 5, nrow = 1)
        brts_table[1, ] <- c(island_age, 0, 0, NA, NA)
        island_no_stac1or5 <- NULL
      } else {
        island_no_stac1or5 <- island[-stac1_5s]
      }
    }
    if (length(stac1_5s) == 0) {
      island_no_stac1or5 <- island
    }
    if (length(island_no_stac1or5) != 0) {
      btimes <- list()
      for (i in 1:length(island_no_stac1or5)) {
        btimes[[i]] <- island_no_stac1or5[[i]]$branching_times[-1]
      }
      brts <- rev(sort(unlist(btimes)))
      brts_IWK <- NULL
      pos1 <- 0
      j <- 1
      for (i in 1:length(btimes)) {
        the_brts <- btimes[[i]]
        the_stac <- island_no_stac1or5[[i]]$stac
        pos2 <- pos1 + length(the_brts)
        ff <- matrix(ncol = 5, nrow = pos2 - pos1)
        ff[1:(pos2 - pos1), 1] <- the_brts
        ff[1:(pos2 - pos1), 2] <- i
        ff[1:(pos2 - pos1), 3] <- seq(1, length(the_brts))
        ff[1:(pos2 - pos1), 4] <- (the_stac == 2) +
          (the_stac == 3) + (the_stac == 4) * 0
        ff[1:(pos2 - pos1), 5] <- NA
        brts_IWK <- rbind(brts_IWK,ff)
        pos1 <- pos2
        j <- j + 1
        if( !is.null(island[[i]]$all_colonisations) & full_table == TRUE) {
          for (k in 1:length(island[[i]]$all_colonisations)) {
            the_brts <- island[[i]]$all_colonisations[[k]]$event_times[-1]
            pos2 <- pos1 + length(the_brts)
            ff <- matrix(ncol = 5, nrow = pos2 - pos1 + 1)
            ff[1:(pos2 - pos1), 1] <- the_brts
            ff[1:(pos2 - pos1), 2] <- j
            ff[1:(pos2 - pos1), 3] <- seq(1, length(the_brts))
            ff[1:(pos2 - pos1), 4] <- NA
            ff[1:(pos2 - pos1), 5] <- j - 1
            brts_IWK <- rbind(brts_IWK,ff)
            pos1 <- pos2
            j <- j + 1
          }
        }
      }
      brts_table <- brts_IWK[rev(order(brts_IWK[, 1])), ]
      brts_table <- rbind(c(island_age, 0, 0, NA, NA), brts_table)
    }
    island_top$brts_table <- brts_table
    if (length(stac1_5s) != 0) {
      for (i in 1:length(stac1_5s)) {
        island[[length(island) + 1]] <- island[[stac1_5s[i]]]
        island[[stac1_5s[i]]] <- NULL
        stac1_5s <- stac1_5s - 1
      }
    }
    island <- append(list(island_top), island)
  }
  colnames(island[[1]]$brts_table) <- c("brt", "clade", "event", "endemic", "col")
  return(island)
}
