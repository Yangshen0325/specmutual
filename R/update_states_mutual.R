# update the state on the island
update_states_mutual <- function(M0,
                                 Mt,
                                 status_p,
                                 status_a,
                                 maxplantID,
                                 maxanimalID,
                                 timeval,
                                 total_time,
                                 rates,
                                 possible_event,
                                 island_plant,
                                 island_animal,
                                 stt_plant,
                                 stt_animal,
                                 transprob) {

  ## [1] plant species: Immigration
  if(possible_event == 1) {
    immig_p <- rates$immig_p
    colonist <- sample(1:length(immig_p),
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
      ext_p <- rates$ext_p
      extinct <- sample(1:length(ext_p),
                        size = 1,
                        replace = FALSE,
                        prob = ext_p)
      status_p[extinct] <- 0
      ind <- which(island_plant[, 1] == extinct)

      typeofspecies <- island_plant[ind, 4]
      if (typeofspecies == "I" | typeofspecies == "A"){
      island_plant <- island_plant[-ind, ]
      }
      if (typeofspecies == "C"){
      # first find species with same ancestor and arrival time
      sisters <- intersect(which(island_plant[, 2] == island_plant[ind, 2]),
                           which(island_plant[, 3] == island_plant[ind, 3]))
      survivors <- sisters[which(sisters != ind)]

      if (length(sisters) == 2) {# survivors status becomes anagenetic
        island_plant[survivors, 4] <- "A"
        island_plant[survivors, c(5, 6)] <- c(NA, NA)
        island_plant[survivors, 7] <- "Clado_extinct"
        island_plant <- island_plant[-ind, ]
      }
      if (length(sisters) >= 3) {
        numberofsplits <- nchar(island_plant[ind, 5])
        mostrecentspl <- substring(island_plant[ind, 5], numberofsplits)

        if (mostrecentspl == "A") {
          sistermostrecentspl <- "B"
        }
        if (mostrecentspl == "B") {
          sistermostrecentspl <- "A"
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

  ## [3] plant species: Cladogenesis
  if (possible_event == 3) {
    clado_p <- rates$clado_p
    tosplit <- sample(1:length(clado_p),
                      size = 1,
                      replace = FALSE,
                      prob = clado_p)
    status_p[tosplit] <- 0
    status_p <- rbind(status_p, 1 ,1)
    Mt <- newMt_clado(M = Mt,
                      tosplit = tosplit,
                      transprob = transprob)
    ind <- which(island_plant[, 1] == tosplit)

    if (island_plant[ind, 4] == "C"){
      # for daughter A
      island_plant[ind, 1] <- maxplantID + 1
      oldstatus <- island_plant[ind, 5]
      island_plant[ind, 5] <- paste(oldstatus, "A", sep = "")
      island_plant[ind, 7] <- NA
      # for daughter B
      island_plant <- rbind(island_plant, c(maxplantID + 2, island_plant[ind, 2],
                            island_plant[ind, 3], "C", paste(oldstatus, "B", sep = ""),
                            timeval, NA))
      maxplantID <- maxplantID + 2
    } else {
      # for daughter A
      island_plant[ind, 1] <- maxplantID + 1
      island_plant[ind, 4] <- "C"
      island_plant[ind, 5] <- "A"
      island_plant[ind, 6] <- island_plant[ind, 3]
      island_plant[ind, 7] <- NA
      # for daughter B
      island_plant <- rbind(island_plant, c(maxplantID + 2, island_plant[ind, 2],
                            island_plant[ind, 3], "C", "B", timeval, NA))
      maxplantID <- maxplantID + 2
    }
  }

  ## [4] plant species: Anagenesis
  if (possible_event == 4) {
    ana_p <- rates$ana_p
    anagenesis <- sample(1:length(ana_p),
                  size = 1,
                  replace = FALSE,
                  prob = ana_p)
    status_p[anagenesis] <- 0
    status_p <- rbind(status_p, 1)
    Mt <- newMt_ana(M = Mt,
                    anagenesis = anagenesis,
                    transprob = transprob)
    ind <- which(island_plant[, 1] == anagenesis)

    island_plant[ind, 1] <- maxplantID + 1
    island_plant[ind, 4] <- "A"
    island_plant[ind, 7] <- "Immig_parent"
    maxplantID <- maxplantID + 1
  }

  ## [5] animal species: Immigration
  if (possible_event == 5) {
    immig_a <- rates$immig_a
    colonist <- sample(1:length(immig_a),
                       size = 1,
                       replace = FALSE,
                       prob = immig_a)
    status_a[colonist] <- 1

    if (length(island_animal[, 1]) != 0){
      isitthere <- which(island_animal[, 1] == colonist)
    } else { isitthere <- c() }

    if (length(isitthere) == 0){
      island_animal <- rbind(island_animal, c(colonist, colonist, timeval, "I",
                                            NA, NA, NA))
    } else {
      island_animal[isitthere, ] <- c(colonist, colonist, timeval, "I", NA, NA, NA)
      Mt[which(M0[, colonist] == 1), colonist] <- M0[which(M0[, colonist] == 1), colonist]
    }
  }

  ## [6] animal species: Extinction
  if (possible_event == 6) {
    ext_a <- rates$ext_a
    extinct <- sample(1:length(ext_a),
                      size = 1,
                      replace = FALSE,
                      prob = ext_a)
    status_a[extinct] <- 0
    ind <- which(island_animal[, 1] == extinct)

    typeofspecies <- island_animal[ind, 4]
    if (typeofspecies == "I" | typeofspecies == "A"){
      island_animal <- island_animal[-ind, ]
    }
    if (typeofspecies == "C"){
      # first find species with same ancestor and arrival time
      sisters <- intersect(which(island_animal[, 2] == island_animal[ind, 2]),
                           which(island_animal[, 3] == island_animal[ind, 3]))
      survivors <- sisters[which(sisters != ind)]
      if (length(sisters) == 2) { # survivors status becomes anagenetic
        island_animal[survivors, 4] <- "A"
        island_animal[survivors, c(5, 6)] <- c(NA, NA)
        island_animal[survivors, 7] <- "Clado_extinct"
        island_animal <- island_animal[-ind, ]
      }
      if (length(sisters) >= 3) {
        numberofsplits <- nchar(island_animal[ind, 5])
        mostrecentspl <- substring(island_animal[ind, 5], numberofsplits)

        if (mostrecentspl == "A") {
          sistermostrecentspl <- "B"
        }
        if (mostrecentspl == "B") {
          sistermostrecentspl <- "A"
        }

        motiftoind <- paste(substring(island_animal[ind, 5], 1, numberofsplits - 1),
                            sistermostrecentspl, sep = "")
        possiblesister <- survivors[which(substring(island_animal[survivors, 5], 1,
                                                    numberofsplits) == motiftoind)]
        if (mostrecentspl == "A"){
          # change the splitting data of the sister species so that it inherits the early
          # splitting that used to belong to A
          tochange <- possiblesister[which(island_animal[possiblesister, 6] ==
                                             min(as.numeric(island_animal[possiblesister, 6])))]
          island_animal[tochange, 6] <- island_animal[ind, 6]
        }
        island_animal[possiblesister, 5] <- paste(substring(island_animal[possiblesister, 5],
                                                          1, numberofsplits - 1),
                                                substring(island_animal[possiblesister, 5],
                                                          numberofsplits + 1,
                                                          nchar(island_animal[possiblesister, 5])),
                                                sep = "")
        island_animal <- island_animal[-ind, ]
      }
    }
    island_animal <- rbind(island_animal)
  }

  ## [7] animal species: Cladogenesis
  if (possible_event == 7) {
    clado_a <- rates$clado_a
    tosplit <- sample(1:length(clado_a),
                      size = 1,
                      replace = FALSE,
                      prob = clado_a)
    status_a[tosplit] <- 0
    status_a <- rbind(status_a, 1 ,1)
    Mt <- t(newMt_clado(M = t(Mt),
                        tosplit = tosplit,
                        transprob = transprob))
    ind <- which(island_animal[, 1] == tosplit)

    if (island_animal[ind, 4] == "C"){
      # for daughter A
      island_animal[ind, 1] <- maxanimalID + 1
      oldstatus <- island_animal[ind, 5]
      island_animal[ind, 5] <- paste(oldstatus, "A", sep = "")
      island_animal[ind, 7] <- NA
      # for daughter B
      island_animal <- rbind(island_animal, c(maxanimalID + 2, island_animal[ind, 2],
                             island_animal[ind, 3], "C", paste(oldstatus, "B", sep = ""),
                                          timeval, NA))
      maxanimalID <- maxanimalID + 2
    } else {
      # for daughter A
      island_animal[ind, 1] <- maxanimalID + 1
      island_animal[ind, 4] <- "C"
      island_animal[ind, 5] <- "A"
      island_animal[ind, 6] <- island_animal[ind, 3]
      island_animal[ind, 7] <- NA
      # for daughter B
      island_animal <- rbind(island_animal, c(maxanimalID + 2, island_animal[ind, 2],
                             island_animal[ind, 3], "C", "B", timeval, NA))
      maxanimalID <- maxanimalID + 2
    }
  }

  ## [8] animal species: Anagenesis
  if (possible_event == 8) {
    ana_a <- rates$ana_a
    anagenesis <- sample(1:length(ana_a),
                         size = 1,
                         replace = FALSE,
                         prob = ana_a)
    status_a[anagenesis] <- 0
    status_a <- rbind(status_a, 1)
    Mt <- t(newMt_ana(M = t(Mt),
                      anagenesis = anagenesis,
                      transprob = transprob))
    ind <- which(island_animal[, 1] == anagenesis)

    island_animal[ind, 1] <- maxanimalID + 1
    island_animal[ind, 4] <- "A"
    island_animal[ind, 7] <- "Immig_parent"
    maxanimalID <- maxanimalID + 1
  }

  ## [9] Cospeciation
  if (possible_event == 9) {
    cospec_rate <- rates$cospec_rate
    copairs <- sample(1:length(cospec_rate),
                      size = 1,
                      replace = FALSE,
                      prob = cospec_rate)
    cospec_plant <- 1 + (copairs - 1) %% nrow(cospec_rate)
    cospec_animal <- 1 + floor((copairs - 1) / nrow(cospec_rate))
    status_p[cospec_plant] <- 0
    status_a[cospec_animal] <- 0
    status_p <- rbind(status_p, 1, 1)
    status_a <- rbind(status_a, 1, 1)
    Mt <- newMt_cospec(M = Mt,
                       cospec_plant = cospec_plant,
                       cospec_animal = cospec_animal,
                       transprob = transprob)

    ind1 <- which(island_plant[, 1] == cospec_plant)
    ind2 <- which(island_animal[, 1] == cospec_animal)

    # for plant species
    if (island_plant[ind1, 4] == "C") {
      # for daughter A
      island_plant[ind1, 1] <- maxplantID + 1
      oldstatus <- island_plant[ind1, 5]
      island_plant[ind1, 5] <- paste(oldstatus, "A", sep = "")
      island_plant[ind1, 7] <- NA
      # for daughter B
      island_plant <- rbind(island_plant, c(maxplantID + 2, island_plant[ind1, 2],
                            island_plant[ind1, 3], "C", paste(oldstatus, "B", sep = ""),
                            timeval, NA))
      maxplantID <- maxplantID + 2
    } else {
      # for daughter A
      island_plant[ind1, 1] <- maxplantID + 1
      island_plant[ind1, 4] <- "C"
      island_plant[ind1, 5] <- "A"
      island_plant[ind1, 6] <- island_plant[ind1, 3]
      island_plant[ind1, 7] <- NA
      # for daughter B
      island_plant <- rbind(island_plant, c(maxplantID + 2, island_plant[ind1, 2],
                            island_plant[ind1, 3], "C", "B", timeval, NA))

      maxplantID <- maxplantID + 2
    }
    # for animal species
    if (island_animal[ind2, 4] == "C") {
      # for daughter A
      island_animal[ind2, 1] <- maxanimalID + 1
      oldstatus <- island_animal[ind2, 5]
      island_animal[ind2, 5] <- paste(oldstatus, "A", sep = "")
      island_animal[ind2, 7] <- NA
      # for daughter B
      island_animal <- rbind(island_animal, c(maxanimalID + 2, island_animal[ind2, 2],
                             island_animal[ind2, 3], "C", paste(oldstatus, "B", sep = ""),
                             timeval, NA))
      maxanimalID <- maxanimalID + 2
    } else {
      # for daughter A
      island_animal[ind2, 1] <- maxanimalID + 1
      island_animal[ind2, 4] <- "C"
      island_animal[ind2, 5] <- "A"
      island_animal[ind2, 6] <- island_animal[ind2, 3]
      island_animal[ind2, 7] <- NA
      # for daughter B
      island_animal <- rbind(island_animal, c(maxanimalID + 2, island_animal[ind2, 2],
                             island_animal[ind2, 3], "C", "B", timeval, NA))
      maxanimalID <- maxanimalID + 2
    }
  }

  ## [10] Gain links
  if (possible_event == 10){
    gain_rate <- rates$gain_rate
    gainpairs <- sample(1:length(gain_rate),
                        size = 1,
                        replace = FALSE,
                        prob = gain_rate)
    togain_plant <- 1 + (gainpairs - 1) %% nrow(gain_rate)
    togain_animal <- 1 + floor((gainpairs - 1) / nrow(gain_rate))
    Mt[togain_plant, togain_animal] <- 1
  }

  ## [11] Lose links
  if (possible_event == 11){
    loss_rate <- rates$loss_rate
    losspairs <- sample(1:length(loss_rate),
                        size = 1,
                        replace = FALSE,
                        prob = loss_rate)
    tolose_plant <- 1 + (losspairs - 1) %% nrow(loss_rate)
    tolose_animal <- 1 + floor((losspairs - 1) / nrow(loss_rate))
    Mt[tolose_plant, tolose_animal] <- 0
  }

  stt_plant <- rbind(stt_plant,
                     c(total_time - timeval,
                     length(which(island_plant[, 4] == "I")),
                     length(which(island_plant[, 4] == "A")),
                     length(which(island_plant[, 4] == "C"))
                     )
   )

   stt_animal <- rbind(stt_animal,
                       c(total_time - timeval,
                         length(which(island_animal[, 4] == "I")),
                         length(which(island_animal[, 4] == "A")),
                         length(which(island_animal[, 4] == "C")))

   )

  update_states <- list(Mt = Mt,
                        status_p = status_p,
                        status_a = status_a,
                        maxplantID = maxplantID,
                        maxanimalID = maxanimalID,
                        island_plant = island_plant,
                        island_animal = island_animal,
                        stt_plant = stt_plant,
                        stt_animal = stt_animal)
  return(update_states)
}


