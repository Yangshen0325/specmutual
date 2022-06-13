# load()
# islands <- out_5


format_stt <- function(islands,
                       simtime,
                       sample_freq){
  several_islands <- list()
  for (i in 1:length(islands)){
    the_island <- islands[[i]]
    the_stt <- the_island[["island"]][["stt_table"]]
    stt_final <- matrix(ncol = 7, nrow = sample_freq + 1)
    colnames(stt_final) <- c("Time", "nIp", "nAp", "nCp", "nIa", "nAa", "nCa")
    stt_final[, "Time"] <- rev(seq(from = 0,
                                   to = total_time,
                                   length.out = sample_freq + 1))
    stt_final[1, 2:7] <- c(0, 0, 0, 0, 0, 0)

    for (j in 2:nrow(stt_final)) {
      the_age <- stt_final[j, "Time"]
      stt_final[j, 2:7] <- the_stt[max(which(the_stt[, "Time"] >= the_age)), 2:7]
    }

    island_list <- list() # I didn't write `not_present`
    island_list[[1]] <- list(island_age = simtime,
                             stt_final = stt_final)

    taxon_list_size <- length(the_island[["island"]][["taxon_list"]])
      if (taxon_list_size != 0) {
        for (y in seq_len(taxon_list_size)) {
          island_list[[y + 1]] <-the_island[["island"]][["taxon_list"]][[y]]
        }
      }
    island_list <- DAISIE:::add_brt_table(island = island_list)
    several_islands[[i]] <- island_list
  }
  return(several_islands)
}

