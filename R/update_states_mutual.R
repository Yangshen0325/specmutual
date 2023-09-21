# update the state on the island
# [1]: immigration event with plant species
# [2]: extinction event with plant species
# [3]: cladogenesis event with plant species
# [4]: anagenesis event with plant species
# [5]: immigration event with animal species
# [6]: extinction event with animal species
# [7]: cladogenesis event with animal species
# [8]: anagenesis event with animal species
# [9]: cospeciation event between pairs
# [10]: gain links event between pairs
# [11]: loss links event between pairs
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
                                 island_spec,
                                 stt_table,
                                 transprob) {

  ## [1] plant species: Immigration
  if (possible_event == 1) {
    immig_p <- rates$immig_p
    colonist <- DDD:::sample2(1:length(immig_p),
                              size = 1,
                              replace = FALSE,
                              prob = immig_p)

    status_p[colonist] <- 1

    if (length(island_spec[, 1]) != 0) {
      isitthere <- intersect(which(island_spec[, 1] == colonist),
                             which(island_spec[, 8] == "plant"))
    } else { isitthere <- c() }

    if (length(isitthere) == 0) {
      island_spec <- rbind(island_spec, c(colonist, colonist, timeval, "I",
                                          NA, NA, NA, "plant"))
    } else {
      island_spec[isitthere, ] <- c(colonist, colonist, timeval, "I", NA, NA, NA, "plant")
      Mt[colonist, which(M0[colonist, ] == 1)] <- M0[colonist, which(M0[colonist, ] == 1)]
    }
  } else
  # if the colonist recolonize, only use the most recent time for the same colonist
  # any links linked with the colonist should be restored.

  ## [2] plant species: Extinction
  if (possible_event == 2) {
      ext_p <- rates$ext_p
      extinct <- DDD:::sample2(1:length(ext_p),
                               size = 1,
                               replace = FALSE,
                               prob = ext_p)

      status_p[extinct] <- 0
      ind <- intersect(which(island_spec[, 1] == extinct),
                       which(island_spec[, 8] == "plant"))

      typeofspecies <- island_spec[ind, 4]
      if (typeofspecies == "I" | typeofspecies == "A") {
      island_spec <- island_spec[-ind, ]
      }
      if (typeofspecies == "C") {
      # first find species with same ancestor and arrival time
      sisters <- intersect(which(island_spec[, 2] == island_spec[ind, 2]),
                           which(island_spec[, 3] == island_spec[ind, 3]))
      survivors <- sisters[which(sisters != ind)]

      if (length(sisters) == 2) {# survivors status becomes anagenetic
        island_spec[survivors, 4] <- "A"
        island_spec[survivors, c(5, 6)] <- c(NA, NA)
        island_spec[survivors, 7] <- "Clado_extinct"
        island_spec <- island_spec[-ind, ]
      }
      if (length(sisters) >= 3) {
        numberofsplits <- nchar(island_spec[ind, 5])
        mostrecentspl <- substring(island_spec[ind, 5], numberofsplits)

        if (mostrecentspl == "A") {
          sistermostrecentspl <- "B"
        }
        if (mostrecentspl == "B") {
          sistermostrecentspl <- "A"
        }

        motiftoind <- paste(substring(island_spec[ind, 5], 1, numberofsplits - 1),
                            sistermostrecentspl, sep = "")
        possiblesister <- survivors[which(substring(island_spec[survivors, 5], 1,
                                                    numberofsplits) == motiftoind)]
        if (mostrecentspl == "A") {
          tochange <- possiblesister[which(island_spec[possiblesister, 6] ==
                                             min(as.numeric(island_spec[possiblesister, 6])))]
          island_spec[tochange, 6] <- island_spec[ind, 6]
        }
        island_spec[possiblesister, 5] <- paste(substring(island_spec[possiblesister, 5],
                                                          1, numberofsplits - 1),
                                                substring(island_spec[possiblesister, 5],
                                                          numberofsplits + 1,
                                                          nchar(island_spec[possiblesister, 5])),
                                                sep = "")
        island_spec <- island_spec[-ind, ]
      }
    }
      island_spec <- rbind(island_spec)
  } else

  ## [3] plant species: Cladogenesis
  if (possible_event == 3) {
    clado_p <- rates$clado_p
    tosplit <- DDD:::sample2(1:length(clado_p),
                             size = 1,
                             replace = FALSE,
                             prob = clado_p)

    status_p[tosplit] <- 0
    status_p <- rbind(status_p, 1 ,1)
    Mt <- newMt_clado(M = Mt,
                      tosplit = tosplit,
                      transprob = transprob)
    ind <- intersect(which(island_spec[, 1] == tosplit),
                     which(island_spec[, 8] == "plant"))

    if (island_spec[ind, 4] == "C"){
      # for daughter A
      island_spec[ind, 1] <- maxplantID + 1
      oldstatus <- island_spec[ind, 5]
      island_spec[ind, 5] <- paste(oldstatus, "A", sep = "")
      island_spec[ind, 7] <- NA
      # for daughter B
      island_spec <- rbind(island_spec, c(maxplantID + 2, island_spec[ind, 2],
                           island_spec[ind, 3], "C", paste(oldstatus, "B", sep = ""),
                           timeval, NA, "plant"))
      maxplantID <- maxplantID + 2
    } else {
      # for daughter A
      island_spec[ind, 1] <- maxplantID + 1
      island_spec[ind, 4] <- "C"
      island_spec[ind, 5] <- "A"
      island_spec[ind, 6] <- island_spec[ind, 3]
      island_spec[ind, 7] <- NA
      # for daughter B
      island_spec <- rbind(island_spec, c(maxplantID + 2, island_spec[ind, 2],
                           island_spec[ind, 3], "C", "B", timeval, NA, "plant"))
      maxplantID <- maxplantID + 2
    }
  } else

  ## [4] plant species: Anagenesis
  if (possible_event == 4) {
    ana_p <- rates$ana_p
    anagenesis <- DDD:::sample2(1:length(ana_p),
                                size = 1,
                                replace = FALSE,
                                prob = ana_p)

    status_p[anagenesis] <- 0
    status_p <- rbind(status_p, 1)
    Mt <- newMt_ana(M = Mt,
                    anagenesis = anagenesis,
                    transprob = transprob)
    ind <- intersect(which(island_spec[, 1] == anagenesis),
                     which(island_spec[, 8] == "plant"))

    island_spec[ind, 1] <- maxplantID + 1
    island_spec[ind, 4] <- "A"
    island_spec[ind, 7] <- "Immig_parent"
    maxplantID <- maxplantID + 1
  } else

  ## [5] animal species: Immigration
  if (possible_event == 5) {
    immig_a <- rates$immig_a
    colonist <- DDD:::sample2(1:length(immig_a),
                              size = 1,
                              replace = FALSE,
                              prob = immig_a)

    status_a[colonist] <- 1

    if (length(island_spec[, 1]) != 0){
      isitthere <- intersect(which(island_spec[, 1] == colonist),
                             which(island_spec[, 8] == "animal"))
    } else { isitthere <- c() }

    if (length(isitthere) == 0){
      island_spec <- rbind(island_spec, c(colonist, colonist, timeval, "I",
                                            NA, NA, NA, "animal"))
    } else {
      island_spec[isitthere, ] <- c(colonist, colonist, timeval, "I", NA, NA, NA, "animal")
      Mt[which(M0[, colonist] == 1), colonist] <- M0[which(M0[, colonist] == 1), colonist]
    }
  } else

  ## [6] animal species: Extinction
  if (possible_event == 6) {
    ext_a <- rates$ext_a
    extinct <- DDD:::sample2(1:length(ext_a),
                             size = 1,
                             replace = FALSE,
                             prob = ext_a)

    status_a[extinct] <- 0
    ind <- intersect(which(island_spec[, 1] == extinct),
                     which(island_spec[, 8] == "animal"))

    typeofspecies <- island_spec[ind, 4]
    if (typeofspecies == "I" | typeofspecies == "A"){
      island_spec <- island_spec[-ind, ]
    }
    if (typeofspecies == "C"){
      # first find species with same ancestor and arrival time
      sisters <- intersect(which(island_spec[, 2] == island_spec[ind, 2]),
                           which(island_spec[, 3] == island_spec[ind, 3]))
      survivors <- sisters[which(sisters != ind)]
      if (length(sisters) == 2) { # survivors status becomes anagenetic
        island_spec[survivors, 4] <- "A"
        island_spec[survivors, c(5, 6)] <- c(NA, NA)
        island_spec[survivors, 7] <- "Clado_extinct"
        island_spec <- island_spec[-ind, ]
      }
      if (length(sisters) >= 3) {
        numberofsplits <- nchar(island_spec[ind, 5])
        mostrecentspl <- substring(island_spec[ind, 5], numberofsplits)

        if (mostrecentspl == "A") {
          sistermostrecentspl <- "B"
        }
        if (mostrecentspl == "B") {
          sistermostrecentspl <- "A"
        }

        motiftoind <- paste(substring(island_spec[ind, 5], 1, numberofsplits - 1),
                            sistermostrecentspl, sep = "")
        possiblesister <- survivors[which(substring(island_spec[survivors, 5], 1,
                                                    numberofsplits) == motiftoind)]
        if (mostrecentspl == "A"){
          # change the splitting data of the sister species so that it inherits the early
          # splitting that used to belong to A
          tochange <- possiblesister[which(island_spec[possiblesister, 6] ==
                                             min(as.numeric(island_spec[possiblesister, 6])))]
          island_spec[tochange, 6] <- island_spec[ind, 6]
        }
        island_spec[possiblesister, 5] <- paste(substring(island_spec[possiblesister, 5],
                                                          1, numberofsplits - 1),
                                                substring(island_spec[possiblesister, 5],
                                                          numberofsplits + 1,
                                                          nchar(island_spec[possiblesister, 5])),
                                                sep = "")
        island_spec <- island_spec[-ind, ]
      }
    }
    island_spec <- rbind(island_spec)
  } else

  ## [7] animal species: Cladogenesis
  if (possible_event == 7) {
    clado_a <- rates$clado_a
    tosplit <- DDD:::sample2(1:length(clado_a),
                             size = 1,
                             replace = FALSE,
                             prob = clado_a)

    status_a[tosplit] <- 0
    status_a <- rbind(status_a, 1 ,1)
    Mt <- t(newMt_clado(M = t(Mt),
                        tosplit = tosplit,
                        transprob = transprob))
    ind <- intersect(which(island_spec[, 1] == tosplit),
                     which(island_spec[, 8] == "animal"))

    if (island_spec[ind, 4] == "C"){
      # for daughter A
      island_spec[ind, 1] <- maxanimalID + 1
      oldstatus <- island_spec[ind, 5]
      island_spec[ind, 5] <- paste(oldstatus, "A", sep = "")
      island_spec[ind, 7] <- NA
      # for daughter B
      island_spec <- rbind(island_spec, c(maxanimalID + 2, island_spec[ind, 2],
                           island_spec[ind, 3], "C", paste(oldstatus, "B", sep = ""),
                                          timeval, NA, "animal"))
      maxanimalID <- maxanimalID + 2
    } else {
      # for daughter A
      island_spec[ind, 1] <- maxanimalID + 1
      island_spec[ind, 4] <- "C"
      island_spec[ind, 5] <- "A"
      island_spec[ind, 6] <- island_spec[ind, 3]
      island_spec[ind, 7] <- NA
      # for daughter B
      island_spec <- rbind(island_spec, c(maxanimalID + 2, island_spec[ind, 2],
                           island_spec[ind, 3], "C", "B", timeval, NA, "animal"))
      maxanimalID <- maxanimalID + 2
    }
  } else

  ## [8] animal species: Anagenesis
  if (possible_event == 8) {
    ana_a <- rates$ana_a
    anagenesis <- DDD:::sample2(1:length(ana_a),
                                size = 1,
                                replace = FALSE,
                                prob = ana_a)

    status_a[anagenesis] <- 0
    status_a <- rbind(status_a, 1)
    Mt <- t(newMt_ana(M = t(Mt),
                      anagenesis = anagenesis,
                      transprob = transprob))
    ind <- intersect(which(island_spec[, 1] == anagenesis),
                     which(island_spec[, 8] == "animal"))

    island_spec[ind, 1] <- maxanimalID + 1
    island_spec[ind, 4] <- "A"
    island_spec[ind, 7] <- "Immig_parent"
    maxanimalID <- maxanimalID + 1
  } else

  ## [9] Cospeciation
  if (possible_event == 9) {
    cospec_rate <- rates$cospec_rate
    copairs <- DDD:::sample2(1:length(cospec_rate),
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

    ind1 <- intersect(which(island_spec[, 1] == cospec_plant),
                      which(island_spec[, 8] == "plant"))
    ind2 <- intersect(which(island_spec[, 1] == cospec_animal),
                      which(island_spec[, 8] == "animal"))

    # for plant species
    if (island_spec[ind1, 4] == "C") {
      # for daughter A
      island_spec[ind1, 1] <- maxplantID + 1
      oldstatus <- island_spec[ind1, 5]
      island_spec[ind1, 5] <- paste(oldstatus, "A", sep = "")
      island_spec[ind1, 7] <- NA
      # for daughter B
      island_spec <- rbind(island_spec, c(maxplantID + 2, island_spec[ind1, 2],
                            island_spec[ind1, 3], "C", paste(oldstatus, "B", sep = ""),
                            timeval, NA, "plant"))
      maxplantID <- maxplantID + 2
    } else {
      # for daughter A
      island_spec[ind1, 1] <- maxplantID + 1
      island_spec[ind1, 4] <- "C"
      island_spec[ind1, 5] <- "A"
      island_spec[ind1, 6] <- island_spec[ind1, 3]
      island_spec[ind1, 7] <- NA
      # for daughter B
      island_spec <- rbind(island_spec, c(maxplantID + 2, island_spec[ind1, 2],
                                          island_spec[ind1, 3], "C", "B",
                                          timeval, NA, "plant") )

      maxplantID <- maxplantID + 2
    }
    # for animal species
    if (island_spec[ind2, 4] == "C") {
      # for daughter A
      island_spec[ind2, 1] <- maxanimalID + 1
      oldstatus <- island_spec[ind2, 5]
      island_spec[ind2, 5] <- paste(oldstatus, "A", sep = "")
      island_spec[ind2, 7] <- NA
      # for daughter B
      island_spec <- rbind(island_spec, c(maxanimalID + 2, island_spec[ind2, 2],
                                          island_spec[ind2, 3], "C", paste(oldstatus, "B", sep = ""),
                                          timeval, NA, "animal"))
      maxanimalID <- maxanimalID + 2
    } else {
      # for daughter A
      island_spec[ind2, 1] <- maxanimalID + 1
      island_spec[ind2, 4] <- "C"
      island_spec[ind2, 5] <- "A"
      island_spec[ind2, 6] <- island_spec[ind2, 3]
      island_spec[ind2, 7] <- NA
      # for daughter B
      island_spec <- rbind(island_spec, c(maxanimalID + 2, island_spec[ind2, 2],
                                          island_spec[ind2, 3], "C", "B",
                                          timeval, NA, "animal") )
      maxanimalID <- maxanimalID + 2
    }
  } else

  ## [10] Gain links
  if (possible_event == 10) {
    gain_rate <- rates$gain_rate
    gainpairs <- DDD:::sample2(1:length(gain_rate),
                               size = 1,
                               replace = FALSE,
                               prob = gain_rate)

    togain_plant <- 1 + (gainpairs - 1) %% nrow(gain_rate)
    togain_animal <- 1 + floor((gainpairs - 1) / nrow(gain_rate))
    Mt[togain_plant, togain_animal] <- 1
  } else

  ## [11] Lose links
  if (possible_event == 11) {
    loss_rate <- rates$loss_rate
    losspairs <- DDD:::sample2(1:length(loss_rate),
                               size = 1,
                               replace = FALSE,
                               prob = loss_rate)

    tolose_plant <- 1 + (losspairs - 1) %% nrow(loss_rate)
    tolose_animal <- 1 + floor((losspairs - 1) / nrow(loss_rate))
    Mt[tolose_plant, tolose_animal] <- 0
  }

  stt_table <- rbind(stt_table,
                     c(total_time - timeval,
                       length(intersect(which(island_spec[, 4] == "I"),
                                        which(island_spec[, 8] == "plant"))),
                       length(intersect(which(island_spec[, 4] == "A"),
                                        which(island_spec[, 8] == "plant"))),
                       length(intersect(which(island_spec[, 4] == "C"),
                                        which(island_spec[, 8] == "plant"))),
                       length(intersect(which(island_spec[, 4] == "I"),
                                        which(island_spec[, 8] == "animal"))),
                       length(intersect(which(island_spec[, 4] == "A"),
                                        which(island_spec[, 8] == "animal"))),
                       length(intersect(which(island_spec[, 4] == "C"),
                                        which(island_spec[, 8] == "animal")))))

  updated_states <- list(Mt = Mt,
                        status_p = status_p,
                        status_a = status_a,
                        maxplantID = maxplantID,
                        maxanimalID = maxanimalID,
                        island_spec = island_spec,
                        stt_table = stt_table)
  return(updated_states)
}


