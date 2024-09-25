#' Convert simulation output into island species interaction data
#'
#' @description This function processes the output of a simulation to generate a comprehensive overview of the species
#' present on the island. It compiles the state of the ecosystem over time, detailing the branching events
#' of extant species and their current statuses. The output includes information for both plant and animal
#' species, facilitating further analyses of island biodiversity and interactions.
#'
#' @return A list containing the following elements:
#' \describe{
#'   \item{stt_table}{A table showing species through time.}
#'   \item{clades_info_plant}{A structured dataset with information on plant species clades, including:
#'     branching times of extant species, current status on the island, and the number of missing species.}
#'   \item{clades_info_animal}{Similar to `clades_info_plant`, this dataset contains information on animal species clades,
#'     including branching times, current status, and number of missing species.}
#' }

create_island_mutual <- function(stt_table,
                                 total_time,
                                 island_spec) {
  ### if there is no species on the island branching_times = island_age,
  ### stac = 0, missing_species = 0
  if (length(island_spec[, 1]) == 0) {
    island <- list(
      stt_table = stt_table,
      branching_times = total_time,
      stac = 0,
      missing_species = 0
    )
  } else {
    ### number of colonists present, plant species
    island_spec_plant <- island_spec[which(island_spec[, 8] == "plant"), ]
    if (!is.matrix(island_spec_plant)) {
      island_spec_plant <- rbind(island_spec_plant[1:8])
      colnames(island_spec_plant) <- colnames(island_spec)
    }
    present_plant <- sort(as.numeric(unique(island_spec_plant[, "Mainland Ancestor"])))
    num_present_plant <- length(present_plant)

    ### number of colonists present, animal species
    island_spec_animal <- island_spec[which(island_spec[, 8] == "animal"), ]
    if (!is.matrix(island_spec_animal)) {
      island_spec_animal <- rbind(island_spec_animal[1:8])
      colnames(island_spec_animal) <- colnames(island_spec)
    }
    present_animal <- sort(as.numeric(unique(island_spec_animal[, "Mainland Ancestor"])))
    num_present_animal <- length(present_animal)

    ### clades information, plant species
    island_clades_info_plant <- list()
    if (num_present_plant >= 1) {
      for (i in 1:num_present_plant) {
        subset_island_plant <- island_spec_plant[which(island_spec_plant[, "Mainland Ancestor"] ==
          present_plant[i]), ]
        if (!is.matrix(subset_island_plant)) {
          subset_island_plant <- rbind(subset_island_plant[1:8])
          colnames(subset_island_plant) <- colnames(island_spec)
        }
        island_clades_info_plant[[i]] <- DAISIE:::DAISIE_ONEcolonist(
          time = total_time,
          island_spec = subset_island_plant,
          stt_table = NULL
        )
        island_clades_info_plant[[i]]$stt_table <- NULL
      }
    } else {
      island_clades_info_plant <- NULL
    }

    ### clades information, animal species
    island_clades_info_animal <- list()
    if (num_present_animal >= 1) {
      for (i in 1:num_present_animal) {
        subset_island_animal <- island_spec_animal[which(island_spec_animal[, "Mainland Ancestor"] ==
          present_animal[i]), ]
        if (!is.matrix(subset_island_animal)) {
          subset_island_animal <- rbind(subset_island_animal[1:8])
          colnames(subset_island_animal) <- colnames(island_spec)
        }
        island_clades_info_animal[[i]] <- DAISIE:::DAISIE_ONEcolonist(
          time = total_time,
          island_spec = subset_island_animal,
          stt_table = NULL
        )
        island_clades_info_animal[[i]]$stt_table <- NULL
      }
    } else {
      island_clades_info_animal <- NULL
    }

    island <- list(
      stt_table = stt_table,
      clades_info_plant = island_clades_info_plant,
      clades_info_animal = island_clades_info_animal
    )
  }
  return(island)
}
