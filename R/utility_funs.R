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

# get_pans_cmps <- function(Mt,
#                           status_p,
#                           status_a){
#   tMt <- t(Mt)
#   pans_p <- Mt %*% status_a
#   pans_a <- tMt %*% status_p
#
#   to_apply_p <- function(x) {
#     copartner <- tMt[, x] * tMt[, -x] * as.numeric(status_a) # think of 2*2 network
#     if (is.null(dim(copartner))) {
#       return(sum((copartner >= 1) * status_p[-x, ]))
#     } else {
#       return(sum((colSums(copartner) >= 1) * status_p[-x, ]))
#     }
#   }
#
#   to_apply_a <- function(x) {
#     copartner <- Mt[, x] * Mt[, -x] * as.numeric(status_p)
#     if (is.null(dim(copartner))) {
#       return(sum((copartner >= 1) * status_a[-x, ]))
#     } else {
#       return(sum((colSums(copartner) >= 1) * status_a[-x, ]))
#     }
#   }
#
#   cmps_p <- sapply(seq_len(nrow(Mt)), to_apply_p)
#   cmps_a <- sapply(seq_len(ncol(Mt)), to_apply_a)
#
#   pans_cmps_list <- list(pans_p = as.numeric(pans_p),
#                          pans_a = as.numeric(pans_a),
#                          cmps_p = cmps_p,
#                          cmps_a = cmps_a)
#   return(pans_cmps_list)
# }
# # test the format
# test_format_pans_cmps <- function(pans_cmps_list,
#                                   status_p,
#                                   status_a) {
#   if (!all(sapply(pans_cmps_list, is.numeric))) return(FALSE)
#   if (!"pans_p" %in% names(pans_cmps_list)) return(FALSE)
#   if (!"pans_a" %in% names(pans_cmps_list)) return(FALSE)
#   if (!"cmps_p" %in% names(pans_cmps_list)) return(FALSE)
#   if (!"cmps_a" %in% names(pans_cmps_list)) return(FALSE)
#   if (any(pans_cmps_list$pans_p < 0.0)) return(FALSE)
#   if (any(pans_cmps_list$pans_a < 0.0)) return(FALSE)
#   if (any(pans_cmps_list$cmps_p < 0.0)) return(FALSE)
#   if (any(pans_cmps_list$cmps_a < 0.0)) return(FALSE)
#   if (length(pans_cmps_list$pans_p) != length(status_p)) return(FALSE)
#   if (length(pans_cmps_list$pans_a) != length(status_a)) return(FALSE)
#   if (length(pans_cmps_list$cmps_p) != length(status_p)) return(FALSE)
#   if (length(pans_cmps_list$cmps_a) != length(status_a)) return(FALSE)
#   return(TRUE)
# }
#
# # get N/K
# get_nk <- function(Mt,
#                    status_p,
#                    status_a,
#                    K_pars,
#                    pans_cmps_list) {
#
#   nk_p <- pmin(1, (sum(status_p) + pans_cmps_list[[3]]) /
#     (K_pars[1] + K_pars[3] * pans_cmps_list[[1]]))
#
#   nk_a <- pmin(1, (sum(status_a) + pans_cmps_list[[4]]) /
#     (K_pars[2] + K_pars[4] * pans_cmps_list[[2]]))
#
#   nk_list <- list(nk_p = nk_p,
#                   nk_a = nk_a)
#   return(nk_list)
# }
#
# # test the format
# test_format_nk <- function(nk_list,
#                            status_p,
#                            status_a) {
#   if (!all(sapply(nk_list, is.numeric))) return(FALSE)
#   if (!"nk_p" %in% names(nk_list)) return(FALSE)
#   if (!"nk_a" %in% names(nk_list)) return(FALSE)
#   if (any(nk_list$nk_p < 0.0)) return(FALSE)
#   if (any(nk_list$nk_a < 0.0)) return(FALSE)
#   if (length(nk_list$nk_p) != length(status_p)) return(FALSE)
#   if (length(nk_list$nk_a) != length(status_a)) return(FALSE)
#   return(TRUE)
# }


newMt_clado <- function(M,
                        tosplit,
                        transprob) {
  newrows <- list()
  possible_output <- list(c(1, 1),
                          c(1, 0),
                          c(0, 1))
  newrows[which(M[tosplit, ] == 0)] <- list(c(0, 0))

  matches <- which(M[tosplit, ] == 1)

  newrows[matches] <- sample(possible_output,
                             size = length(matches),
                             replace = TRUE,
                             prob = c(transprob,
                                      (1 - transprob) / 2,
                                      (1 - transprob) / 2))
  newrows <- matrix(unlist(newrows), nrow = 2, ncol = ncol(M))
  return(rbind(M, newrows))
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


 #### calculate the partners of plant and animal species
get_pans <- function(Mt, status_p, status_a) {

    pans_p <- Mt %*% status_a
    pans_a <- t(Mt) %*% status_p

    pans_list <- list(pans_p = pans_p,
                      pans_a = pans_a)

    return(pans_list)
}

#### get N/K
# get_nk <- function(Mt,
#                    status_p,
#                    status_a,
#                    K_pars,
#                    pans_list) {
#
#   nk_p <- sum(status_p) /
#     (K_pars[1] + K_pars[3] * pans_list[[1]])
#
#   nk_a <- sum(status_a) /
#     (K_pars[2] + K_pars[4] * pans_list[[2]])
#
#   nk_list <- list(nk_p = nk_p,
#                   nk_a = nk_a)
#   return(nk_list)
# }

#### get_wrates ####
get_wrates <- function(alphaa,
                       status_p,
                       status_a,
                       K_pars,
                       pans_list) {

  wp_rates <- exp(-alphaa / (pmax(0, K_pars[1] + K_pars[3] * pans_list[[1]] -  sum(status_p))))
  wa_rates <-  exp(-alphaa / (pmax(0, K_pars[2] + K_pars[4] * pans_list[[2]] -  sum(status_a))))

  wrates_list <- list(wp_rates = wp_rates,
                      wa_rates = wa_rates)
  return(wrates_list)

}

# generate M0
## construct the original mutualistic network in mainland, refering to
## Luis Santamatia et. al (2007) Linkage rules for plant-pollinator networks:
# trait complementary of exploitation barriers?
## model1: Lognormal neutral model
## flower <- round(runif(1, min = 10, max = 160))
## P = 45.573 - 0.8082 * F + 0.047 * F*F
# flower <- 20
# pollinator <- round(45.573-0.8082* flower + 0.047 * flower^2)
## vi and wj are independent random variates with lognormal distribution
# wj <- rlnorm(flower)
# vi <- rlnorm(pollinator)
# link <- (vi %*% t(wj)) / max(vi %*% t(wj))
# nonlink <- 1 - link
# network <- matrix(nrow = pollinator, ncol = flower)
# for (i in 1:pollinator){
#   for (j in 1:flower){
#     network[i, j] <- sample(c(0, 1),
#                             size = 1,
#                             replace = FALSE,
#                             prob = c(nonlink[i, j], link[i, j]))
#   }
# }
#
# network <- network[which(rowSums(network) != 0), which(colSums(network) != 0)]
# M0 <- t(network)

## model2: Mixed model, mixed narrow-complementarity model with barrier model
## flower <- round(runif(1, min = 10, max = 160))

#' Title
#'
#' @param plant
#'
#' @return

get_M0 <- function(plant){
  animal <- round(45.573-0.8082* plant + 0.047 * plant^2)
  vi1 <- runif(animal, 0, 1)
  vi2 <- runif(animal, 0, 1)
  vi3 <- runif(animal, 0, 1)
  vi4 <- runif(animal, 0, 1)
  wj1 <- runif(plant, 0, 1)
  wj2 <- runif(plant, 0, 1)
  wj3 <- runif(plant, 0, 1)
  wj4 <- runif(plant, 0, 1)
  delta_vi1 <- runif(animal, 0, 0.25)
  delta_vi2 <- runif(animal, 0, 0.25)
  delta_wj1 <- runif(plant, 0, 0.25)
  delta_wj2 <- runif(plant, 0, 0.25)
  complemd_k1 <- matrix(nrow = animal, ncol = plant)
  complemd_k2 <- matrix(nrow = animal, ncol = plant)
  barriermd_k3 <- matrix(nrow = animal, ncol = plant)
  barriermd_k4 <- matrix(nrow = animal, ncol = plant)
  for (i in 1:animal){
    for (j in 1:plant){
      complemd_k1[i, j] <- abs(vi1[i] - wj1[j]) - 0.5 * (delta_vi1[i] + delta_wj1[j])
      complemd_k2[i, j] <- abs(vi2[i] - wj2[j]) - 0.5 * (delta_vi2[i] + delta_wj2[j])
      barriermd_k3[i, j] <- vi3[i] -wj3[j]
      barriermd_k4[i, j] <- vi4[i] -wj4[j]
    }
  }
  complemd_k1 <- complemd_k1 < 0
  complemd_k2 <- complemd_k2 < 0
  barriermd_k3 <- barriermd_k3 > 0
  barriermd_k4 <- barriermd_k4 > 0

  network <- matrix(nrow = dim(complemd_k1)[1], ncol = dim(complemd_k1)[2])
  network[which(complemd_k1 + complemd_k2 + barriermd_k3 + barriermd_k4 == 4)] <- 1
  network[which(complemd_k1 + complemd_k2 + barriermd_k3 + barriermd_k4 != 4)] <- 0
  network <- network[which(rowSums(network) != 0), which(colSums(network) != 0)]
  M0 <- t(network)
  return(M0)
}
##set.seed(12)
## save(M0, file = "D:/PhD_Yang/specmutual/trial/M0.RData")














