#' Some functions used for simulation
#'
#'


# new matrix on island due to cladogenesis ------------------------------------------

newMt_clado <- function(M,
                        tosplit,
                        transprob) {

  # Validate input
  if (tosplit <= 0 || tosplit > nrow(M)) {
    stop("Invalid index for tosplit.")
  }
  if (transprob < 0 || transprob > 1) {
    stop("transprob must be between 0 and 1.")
  }

  # Initialize new rows for the two new cladogenesis species
  newrows <- matrix(0, nrow = 2, ncol = ncol(M))

  # For the links where the parental species has connections
  matches <- which(M[tosplit, ] == 1)
  # Inherit interactions based on the defined probabilities
  if (length(matches) > 0) {
    newrows[1, matches] <- sample(c(1, 0),
                                  size = length(matches),
                                  replace = TRUE,
                                  prob = c(transprob, 1 - transprob)
    )
    newrows[2, matches] <- sample(c(1, 0),
                                  size = length(matches),
                                  replace = TRUE,
                                  prob = c(transprob, 1 - transprob)
    )
  }

  # Append the new rows to the original matrix
  M <- rbind(M, newrows)
  return(M)
}

# new matrix on island due to anagenesis ----------------------------------

newMt_ana <- function(M,
                      anagenesis,
                      transprob) {

  # Validate input
  if (anagenesis <= 0 || anagenesis > nrow(M)) {
    stop("Invalid index for anagenesis.")
  }
  if (transprob < 0 || transprob > 1) {
    stop("transprob must be between 0 and 1.")
  }

  # Initialize a new row for the anagenesis species
  newrow <- numeric(ncol(M))  # Start with a row of zeros

  # Inherit interactions with specified probability
  if (any(M[anagenesis, ] == 1)) {
    newrow[M[anagenesis, ] == 1] <- sample(c(1, 0),
                                           size = sum(M[anagenesis, ] == 1),
                                           replace = TRUE,
                                           prob = c(transprob, 1 - transprob)
    )
  }

  # Append the new row to the original matrix
  M <- rbind(M, newrow)

  return(M)
}

# new matrix on island due to co-speciation -------------------------------

newMt_cospec <- function(M,
                         cospec_plant,
                         cospec_animal,
                         transprob) {
  # Initialize lists for new rows and columns
  newrows <- list()
  newcols <- list()

  # Possible outputs for inheriting links
  possible_output <- list(c(1, 1), c(1, 0), c(0, 1))

  # Handle the new rows from cospeciation of plants
  newrows[which(M[cospec_plant, ] == 0)] <- list(c(0, 0))
  newrows[which(M[cospec_plant, ] == 1)] <- sample(possible_output,
    size = sum(M[cospec_plant, ] == 1),
    replace = TRUE,
    prob = c(
      transprob,
      (1 - transprob) / 2,
      (1 - transprob) / 2
    )
  )

  # Create the new rows matrix
  newrows <- matrix(unlist(newrows), nrow = 2, ncol = ncol(M))
  newrows <- cbind(newrows, diag(1, 2, 2))

  # Handle the new columns from cospeciation of animals
  newcols[which(M[, cospec_animal] == 0)] <- list(c(0, 0))
  newcols[which(M[, cospec_animal] == 1)] <- sample(possible_output,
    size = sum(M[, cospec_animal] == 1),
    replace = TRUE,
    prob = c(
      transprob,
      (1 - transprob) / 2,
      (1 - transprob) / 2
    )
  )

  # Create the new columns matrix
  newcols <- t(matrix(unlist(newcols), nrow = 2, ncol = nrow(M)))
  M <- rbind(cbind(M, newcols), newrows)
  return(M)
}

# Get the number of interacting partners ----------------------

# partners_list[[1]] = sum_{j=1}{N_A} M_{ij} * A_j, plant partners present
# partners_list[[2]] = sum_{i=1}{N_P} M_{ji} * P_i, animal partners present

get_partners <- function(Mt, status_p, status_a) {
  partners_p <- Mt %*% status_a
  partners_a <- t(Mt) %*% status_p

  partners_list <- list(
    partners_p = partners_p,
    partners_a = partners_a
  )

  return(partners_list)
}

# Get exp(-alpha / max(0, K0 + K1 * partners - N_present))-----------------------
# It defines how much mutualism affect the original rates, especially immigration and cladogenesis

calculate_mutualism_effect <- function(alpha,
                                       status_p,
                                       status_a,
                                       K_pars,
                                       partners_list) {
  # Calculate mutualism effect for plants
  denominator_p <- K_pars[1] + K_pars[3] * partners_list[[1]] - sum(status_p)
  mutualism_effect_p <- ifelse(denominator_p > 0,
    exp(-alpha / denominator_p),
    0
  )

  # Calculate mutualism effect for animals
  denominator_a <- K_pars[2] + K_pars[4] * partners_list[[2]] - sum(status_a)
  mutualism_effect_a <- ifelse(denominator_a > 0,
    exp(-alpha / denominator_a),
    0
  )

  # Create the result list
  mutualism_effect_list <- list(
    mutualism_effect_p = mutualism_effect_p,
    mutualism_effect_a = mutualism_effect_a
  )

  return(mutualism_effect_list)
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
  PA_both <- status_p %*% t_status_a # P_i* A_j
  Pno_A <- (1 - status_p) %*% t_status_a # (1 - P_i) * A_j
  P_Ano <- status_p %*% (1 - t_status_a) # P_i * (1 - A_j)

  pa_table <- list(
    PA_both = PA_both,
    Pno_A = Pno_A,
    P_Ano = P_Ano
  )

  return(pa_table)
}


# Function to check and create folder if not exists -----------------------


check_and_create_folder <- function(the_path, folder_name) {
  if (!dir.exists(paste0(the_path, "/", folder_name))) {
    dir.create(paste0(the_path, "/", folder_name))
  }
}


# Function to save parameters to file -------------------------------------


save_parameters <- function(out, the_path, folder_name, effect) {
  file_name <- paste0(the_path, "/", folder_name, "/", folder_name, "_", effect, ".rds")
  saveRDS(out, file = file_name)
}
