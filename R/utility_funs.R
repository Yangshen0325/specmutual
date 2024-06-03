
#' Some functions used for simulation
#'
#'


# new matrix on island due to cladogenesis ------------------------------------------

newMt_clado <- function(M,
                        tosplit,
                        transprob) {
  newrows <- list()

  # for the two new cladogenesis species, it has three scenarios for inheriting links from parental species
  possible_output <- list(
    c(1, 1),
    c(1, 0),
    c(0, 1)
  )

  # when the parental link is 0, keep 0 for the two new cladogenesis
  newrows[which(M[tosplit, ] == 0)] <- list(c(0, 0))

  matches <- which(M[tosplit, ] == 1)

  newrows[matches] <- sample(possible_output,
    size = length(matches),
    replace = TRUE,
    prob = c(
      transprob,
      (1 - transprob) / 2,
      (1 - transprob) / 2
    )
  )

  # append the new rows
  newrows <- matrix(unlist(newrows), nrow = 2, ncol = ncol(M))
  return(rbind(M, newrows))
}

# new matrix on island due to anagenesis ----------------------------------

newMt_ana <- function(M,
                      anagenesis,
                      transprob) {
  newrows <- list()

  # we keep the new anagensis species inherit 0 from parental species, while with
  # `transprob` probability to inherit 1 (i.e. 1-`tranprob` to have 0)
  newrows[which(M[anagenesis, ] == 0)] <- 0
  newrows[which(M[anagenesis, ] == 1)] <- sample(c(1, 0),
    size = length(which(M[anagenesis, ] == 1)),
    replace = TRUE,
    prob = c(transprob, 1 - transprob)
  )

  # append the new row
  newrows <- matrix(unlist(newrows), nrow = 1, ncol = ncol(M))
  M <- rbind(M, newrows)
  return(M)
}

# new matrix on island due to co-speciation -------------------------------

newMt_cospec <- function(M,
                         cospec_plant,
                         cospec_animal,
                         transprob) {
  newrows <- list()
  newcols <- list()

  possible_output <- list(c(1, 1), c(1, 0), c(0, 1))

  newrows[which(M[cospec_plant, ] == 0)] <- list(c(0, 0))
  newrows[which(M[cospec_plant, ] == 1)] <- sample(possible_output,
    size = length(which(M[cospec_plant, ] == 1)),
    replace = TRUE,
    prob = c(
      transprob,
      (1 - transprob) / 2,
      (1 - transprob) / 2
    )
  )

  newrows <- matrix(unlist(newrows), nrow = 2, ncol = ncol(M))
  newrows <- cbind(newrows, diag(1, 2, 2))

  newcols[which(M[, cospec_animal] == 0)] <- list(c(0, 0))
  newcols[which(M[, cospec_animal] == 1)] <- sample(possible_output,
    size = length(which(M[, cospec_animal] == 1)),
    replace = TRUE,
    prob = c(
      transprob,
      (1 - transprob) / 2,
      (1 - transprob) / 2
    )
  )

  newcols <- t(matrix(unlist(newcols), nrow = 2, ncol = nrow(M)))
  M <- rbind(cbind(M, newcols), newrows)
  return(M)
}


# get the number of interacting partners ----------------------


get_partners <- function(Mt, status_p, status_a) {
  partners_p <- Mt %*% status_a
  partners_a <- t(Mt) %*% status_p

  partners_list <- list(
    partners_p = partners_p,
    partners_a = partners_a
  )

  return(partners_list)
}

# get exp(-alpha / max(0, K0+K1*partners - N_present))-----------------------
# Here I call it `wrates`, how much mutualism weight for the original rates

get_wrates <- function(alphaa,
                       status_p,
                       status_a,
                       K_pars,
                       partners_list) {
  wp_rates <- exp(-alphaa / (pmax(
    0,
    K_pars[1] + K_pars[3] * partners_list[[1]] -
      sum(status_p)
  )))
  wa_rates <- exp(-alphaa / (pmax(
    0,
    K_pars[2] + K_pars[4] * partners_list[[2]] -
      sum(status_a)
  )))

  wrates_list <- list(
    wp_rates = wp_rates,
    wa_rates = wa_rates
  )

  return(wrates_list)
}


# Generate matrix in mainland, M0 -----------------------------------------

#### The code to generate species interaction binary matrix on the mainland.
#### (refer to "doi: 10.1371/journal.pbio.0050031")

# vi/wj: the central trait value of animal i/plant j, uniform distribution (0, 1).
# delta_vi/wj: the range of variablity, uniform distribution (0, 1).
# Mixed-model: two complementarity traits model with narrow-range variablity and
# .             two barrier traits model


get_M0 <- function(plant) {
  # according to the real-world communnities plant-animal relationship
  animal <- round(45.573 - 0.8082 * plant + 0.047 * plant^2)

  # two complementarity traits: vi1, vi2, delata_vi1, delta_vi2 (animal),
  # wj1, wj2, delata_wj1, delta_wj2 (plant)
  # two barrier traits: vi3, vi4 (animal), wj3, wj4 (plant)
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

  for (i in 1:animal) {
    for (j in 1:plant) {
      complemd_k1[i, j] <- abs(vi1[i] - wj1[j]) - 0.5 * (delta_vi1[i] + delta_wj1[j])
      complemd_k2[i, j] <- abs(vi2[i] - wj2[j]) - 0.5 * (delta_vi2[i] + delta_wj2[j])
      barriermd_k3[i, j] <- vi3[i] - wj3[j]
      barriermd_k4[i, j] <- vi4[i] - wj4[j]
    }
  }
  complemd_k1 <- complemd_k1 < 0
  complemd_k2 <- complemd_k2 < 0
  barriermd_k3 <- barriermd_k3 > 0
  barriermd_k4 <- barriermd_k4 > 0

  network <- matrix(nrow = dim(complemd_k1)[1], ncol = dim(complemd_k1)[2])
  network[which(complemd_k1 + complemd_k2 + barriermd_k3 + barriermd_k4 == 4)] <- 1
  network[which(complemd_k1 + complemd_k2 + barriermd_k3 + barriermd_k4 != 4)] <- 0

  # remove all species that have no interactions
  network <- network[which(rowSums(network) != 0), which(colSums(network) != 0)]

  # We usually make plant species as rows
  M0 <- t(network)
  return(M0)
}


# M0 used in the paper ----------------------------------------------------

# set.seed(12)
# M0 <- get_M0(plant = 150)
# saveRDS(M0, file = "/Users/yangshen/Downloads/phd_yang/chapter2/code/M0.rds")


# P %*% A -------------------------------------------------------------------

get_pa_table <- function(status_p,
                         status_a,
                         t_status_a) {

  PA_both <- status_p %*% t_status_a
  Pno_A <-  (1 - status_p) %*% t_status_a
  P_Ano <- status_p %*% (1 - t_status_a)

  pa_table = list(PA_both = PA_both,
                  Pno_A = Pno_A,
                  P_Ano = P_Ano)

  return(pa_table)
}















