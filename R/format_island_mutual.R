# load("D:/PhD_Yang/specmutual/result/out_5.RData")
# load("X:/YSPhD_Aca/specmutual/result/out_5.RData")

# load("X:/YSPhD_Aca/specmutual/trial/M0.RData")
# load("D:/PhD_Yang/specmutual/trial/M0.RData")
# mutualism_pars <- create_mutualism_pars(
#   lac_pars = c(0.5, 0.5), # c(lac_plant, lac_animal)
#   mu_pars = c(0.2, 0.2, 1.0, 1.0), # c(mu_P0, mu_A0, mu_P1, mu_A1)
#   K_pars = c(Inf, Inf, 1.0, 1.0), # c(K_P0, K_A0, K_P1, K_A1)
#   gam_pars = c(0.1, 0.1), # (gam_plant, gam_animal)
#   qgain = 1.0,
#   qloss = 1.0,
#   laa_pars = c(0.3, 0.3, 1.0, 1.0), # c(laa_P0, laa_A0, laa_P1, laa_A1)
#   lambda0 = 1.0,
#   M0 = M0,
#   transprob = 1.0) # the possibility to inherit links from parents
#  load("X:/YSPhD_Aca/specmutual/trial/island_replicates.RData")
#  load("D:/PhD_Yang/specmutual/trial/island_replicates.RData")

format_island_mutual <- function(island_replicates,
                                 total_time,
                                 sample_freq,
                                 mutualism_pars) {

  M0 <- mutualism_pars$M0
  Mtotal <- nrow(M0) + ncol(M0)
  several_islands <- list()
  for (rep in 1:length(island_replicates)) {

    the_island <- island_replicates[[rep]]
    the_stt <- the_island$stt_table
    clades_info_plant <- the_island$clades_info_plant
    clades_info_animal <- the_island$clades_info_animal
    taxon_list <- c(clades_info_plant, clades_info_animal)

    stt_all <- matrix(ncol = 7, nrow = sample_freq + 1)
    colnames(stt_all) <- c("Time", "nIp", "nAp", "nCp", "nIa", "nAa", "nCa")
    stt_all[, "Time"] <- rev(seq(from = 0,
                                 to = total_time,
                                 length.out = sample_freq + 1))
    stt_all[1, 2:7] <- c(0, 0, 0, 0, 0, 0)
    for (j in 2:nrow(stt_all)) {
      the_age <- stt_all[j, "Time"]
      stt_all[j, 2:7] <- the_stt[max(which(the_stt[, "Time"] >= the_age)), 2:7]
    }


    #island_list[[1]] <- list(island_age = simtime,
                             #stt_all = stt_all)
    island_list_plant <- list()
    island_list_animal <- list()
    if(sum(the_stt[nrow(the_stt), 2:7]) == 0) {
      island_list[[1]] <- list(island_age = total_time,
                               not_present = Mtotal,
                               stt_all = stt_all)
    } else {
      island_list_plant[[1]] <- list(
        island_age = total_time,
        not_present_p = nrow(M0) - length(clades_info_plant),
        stt_all = stt_all)
      island_list_animal[[1]] <- list(
        island_age = total_time,
        not_present_a = ncol(M0) - length(clades_info_animal),
        stt_all = stt_all)

      for (y in 1:length(clades_info_plant)) {
        island_list_plant[[y + 1]] = clades_info_plant[[y]]
      }
      for (y in 1:length(clades_info_animal)) {
        island_list_animal[[y + 1]] = clades_info_animal[[y]]
      }
    }

    island_list_plant <- add_brt_table(island = island_list_plant)
    several_islands[[rep]] <- island_list
  }
  return(several_islands)
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


