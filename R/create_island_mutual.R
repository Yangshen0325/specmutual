#' @title Converts simulation output into island output
#'
#' @inheritParams default_params_doc
#'
#' @return list with the island information, composed stt table,
#' branching times of extant species, status of species on
#' the island and number of missing species.

create_island_mutual <- function(stt_table,
                                 total_time,
                                 island_spec) {
  ### if there are no species on the island branching_times = island_age,
  ### stac = 0, missing_species = 0
  if(length(island_spec[, 1]) == 0) {
    island <- list(stt_table = stt_table,
                   branching_times = total_time,
                   stac = 0,
                   missing_species = 0)
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
          stt_table = NULL)
          island_clades_info_plant[[i]]$stt_table <- NULL
      }
    } else { island_clades_info_plant <- NULL }

    ### clades information, animal species
    island_clades_info_animal <- list()
    if (num_present_animal >= 1){
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
          stt_table = NULL)
        island_clades_info_animal[[i]]$stt_table <- NULL
      }
    } else { island_clades_info_animal <- NULL }
  }

  island <- list(stt_table = stt_table,
                 clades_info_plant = island_clades_info_plant,
                 clades_info_animal = island_clades_info_animal)

  return(island)
}

