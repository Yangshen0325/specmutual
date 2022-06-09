# plot stt_table

  # stt_table <- island$stt_table
  # par(mfrow = c(2, 3))
  # plot(stt_table[, 1], stt_table[, 2], type = "l",
  #      xlab = "Time", ylab = "richness", main = "number of plant immigrants")
  # plot(stt_table[, 1], stt_table[, 3], type = "l",
  #      xlab = "Time", ylab = "richness", main = "number of plant anagenesis")
  # plot(stt_table[, 1], stt_table[, 4], type = "l",
  #      xlab = "Time", ylab = "richness", main = "number of plant cladogenesis")
  # plot(stt_table[, 1], stt_table[, 5], type = "l",
  #      xlab = "Time", ylab = "richness", main = "number of animal immigrants")
  # plot(stt_table[, 1], stt_table[, 6], type = "l",
  #      xlab = "Time", ylab = "richness", main = "number of animal anagenesis")
  # plot(stt_table[, 1], stt_table[, 7], type = "l",
  #      xlab = "Time", ylab = "richness", main = "number of animal cladogenesis")
  #
  # par(mfrow = c(1, 1))
  #

# sim_out <- list(out_1, out_2, out_3, out_4)
# species_mean <-  matrix(nrow = 4, ncol = 6)
# colnames(species_mean) <- c("nIp", "nAp", "nCp", "nIa", "nAa", "nCa")
# for (j in 1:4){
#   for (i in 1:20){
#     stt_table <- sim_out[[j]][[i]][["island"]][["stt_table"]]
#     nIp <- floor(mean(c(nIp, stt_table[nrow(stt_table), 2])))
#     nAp <- floor(mean(c(nAp, stt_table[nrow(stt_table), 3])))
#     nCp <- floor(mean(c(nCp, stt_table[nrow(stt_table), 4])))
#     nIa <- floor(mean(c(nIa, stt_table[nrow(stt_table), 5])))
#     nAa <- floor(mean(c(nAa, stt_table[nrow(stt_table), 6])))
#     nCa <- floor(mean(c(nCa, stt_table[nrow(stt_table), 7])))
#   }
#   species_mean[j, ] <- c(nIp, nAp, nCp, nIa, nAa, nCa)
# }


