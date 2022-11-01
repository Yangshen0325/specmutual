# update the state on the island
update_state_v2 <- function(M0,
                            status_p,
                            status_a,
                            rates,
                            possible_event) {

  ## [1] plant species: Immigration
  if(possible_event == 1) {
    immig_p <- rates$immig_p
    colonist <- sample(1:nrow(M0),
                       size = 1,
                       replace = FALSE,
                       prob = immig_p)
    status_p[colonist] <- 1

    if (length(island_plant[, 1]) != 0){
      isitthere <- which(island_plant[, 1] == colonist)
    } else { isitthere <- c() }

    if (length(isitthere) == 0){
      island_plant <- rbind(island_plant, c(colonist, colonist, timeval, "I",
                                          NA, NA, NA))
    } else {
      island_plant[isitthere, ] <- c(colonist, colonist, timeval, "I", NA, NA, NA)
      Mt[colonist, which(M0[colonist, ] == 1)] <- M0[colonist, which(M0[colonist, ] == 1)]
    }
  }
  # if the colonist recolonize, only use the most recent time for the same colonist
  # any links linked with the colonist should be restored.

  ## [2] plant species: Extinction
  if(possible_event == 2) {
    if (length(island_plant[, 1]) == 1) {
      extinct <- as.integer(island_plant[, 1])
      status_p[extinct] <- 0
      island_plant <- c()
    } else {
      ext_p <- rates$ext_p
      ext_pool <- which(status_p == 1, arr.ind = TRUE)[, 1]
      ext_pool_rates <- ext_p[ext_pool]
      testit::assert(length(as.integer(island_plant[, 1])) == length(ext_pool))
      extinct <- sample(ext_pool,
                        size = 1,
                        replace = FALSE,
                        prob = ext_pool_rates)
      ind <- which(island_plant[, 1] == extinct)
      status_p[extinct] <- 0
      typeofspecies <- island_plant[ind, 4]

      if (typeofspecies == "I" | typeofspecies == "A"){
      island_plant <- island_plant[-ind, ]
      }
      if (typeofspecies == "C"){
      # first find species with same ancestor and arrival time
      sisters <- intersect(which(island_plant[, 2] == island_plant[ind, 2]),
                           which(island_plant[, 3] == island_plant[ind, 3]))
      survivors <- sisters[which(sisters != ind)]

      if (length(sisters) == 2){# survivors status becomes anagenetic
        island_plant[survivors, 4] <- "A"
        island_plant[survivors, c(5, 6)] <- c(NA, NA)
        island_plant[survivors, 7] <- "Clado_extinct"
        island_plant <- island_plant[-ind, ]
      }
      if (length(sisters) >= 3){
        numberofsplits <- nchar(island_plant[ind, 5])
        mostrecentspl <- substring(island_plant[ind, 5], numberofsplits)

        if (mostrecentspl == "A") {
          sistermostrecentspl < "B"
        }
        if (mostrecentspl == "B") {
          sistermostrecentspl < "A"
        }

        motiftoind <- paste(substring(island_plant[ind, 5], 1, numberofsplits - 1),
                            sistermostrecentspl, sep = "")
        possiblesister <- survivors[which(substring(island_plant[survivors, 5], 1,
                                                    numberofsplits) == motiftoind)]
        if (mostrecentspl == "A") {
          tochange <- possiblesister[which(island_plant[possiblesister, 6] ==
                                             min(as.numeric(island_plant[possiblesister, 6])))]
          island_plant[tochange, 6] <- island_plant[ind, 6]
        }
        island_plant[possiblesister, 5] <- paste(substring(island_plant[possiblesister, 5],
                                                          1, numberofsplits - 1),
                                                substring(island_plant[possiblesister, 5],
                                                          numberofsplits + 1,
                                                          nchar(island_plant[possiblesister, 5])),
                                                sep = "")
        island_plant <- island_plant[-ind, ]
      }
    }
      island_plant <- rbind(island_plant)
    }
  }

  ## [3] plant species: Anagenesis
  if (possible_event == 3) {
    immi_specs <- which(island_plant[, 4] == "I")
    if(length(immi_specs) == 1) {
      ind <- immi_specs
    } else {
      ind <- DDD::sample2(immi_specs, 1)
    }

    anagenesis <- as.integer(island_plant[ind, 1])
    status_p[anagenesis] <- 0
    status_p <- rbind(status_p, 1)
    Mt <- newMt_ana(M = Mt,
                    possible_event = possible_event,
                    transprob = transprob)

    island_plant[ind, 1] <- maxplantID + 1
    island_plant[ind, 4] <- "A"
    island_plant[ind, 7] <- "Immig_parent"
    maxplantID <- maxplantID + 1
  }

  ## [4] plant species: Cladogenesis
  if (possible_event == 4) {
    ind <- DDD::sample2(1:length(island_plant[, 1]), 1)
    tosplit <- as.integer(island_plant[ind, 1], 1)
  }







}
