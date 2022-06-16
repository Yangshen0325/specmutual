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

# sim_out <- list(out_5, out_6, out_7, out_8,
#                 out_9, out_10, out_11, out_12, out_13)
# nIp <- nAp <- nCp <- nIa <- nAa <- nCa <- NULL
# species_mean <-  matrix(nrow = 9, ncol = 6)
# colnames(species_mean) <- c("nIp", "nAp", "nCp", "nIa", "nAa", "nCa")
# for (j in 1:9){ # have 9 sets
#   for (i in 1:50){# 50 reps
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
#
#
# out <- out_13
# par(mfrow = c(2, 3))
# #### number of plant immigrants ####
# plot(0, 0, xlim = c(0,5), ylim = c(0,30), type = "l",
#      xlab = "Time", ylab = "richness", main = "number of plant immigrants")
# cl <- rainbow(50)
# for (i in 1:50){
#   stt_table <- out[[i]][["island"]][["stt_table"]]
#   lines(stt_table[, 1], stt_table[, 2], col = cl[i], type = 'l')
# }
# #### number of plant anagenesis ####
# plot(0, 0, xlim = c(0,5), ylim = c(0,30), type = "l",
#      xlab = "Time", ylab = "richness", main = "number of plant anagenesis")
# cl <- rainbow(50)
# for (i in 1:50){
#   stt_table <- out[[i]][["island"]][["stt_table"]]
#   lines(stt_table[, 1], stt_table[, 3], col = cl[i], type = 'l')
# }
# #### number of plant cladogenesis ####
# plot(0, 0, xlim = c(0,5), ylim = c(0,30), type = "l",
#      xlab = "Time", ylab = "richness", main = "number of plant cladogenesis")
# cl <- rainbow(50)
# for (i in 1:50){
#   stt_table <- out[[i]][["island"]][["stt_table"]]
#   lines(stt_table[, 1], stt_table[, 4], col = cl[i], type = 'l')
# }
# #### number of animal immigrants ####
# plot(0, 0, xlim = c(0,5), ylim = c(0,30), type = "l",
#      xlab = "Time", ylab = "richness", main = "number of animal immigrants")
# cl <- rainbow(50)
# for (i in 1:50){
#   stt_table <- out[[i]][["island"]][["stt_table"]]
#   lines(stt_table[, 1], stt_table[, 5], col = cl[i], type = 'l')
# }
# #### number of animal anagenesis ####
# plot(0, 0, xlim = c(0,5), ylim = c(0,30), type = "l",
#      xlab = "Time", ylab = "richness", main = "number of animal anagenesis")
# cl <- rainbow(50)
# for (i in 1:50){
#   stt_table <- out[[i]][["island"]][["stt_table"]]
#   lines(stt_table[, 1], stt_table[, 6], col = cl[i], type = 'l')
# }
# #### number of animal cladogenesis ####
# plot(0, 0, xlim = c(0,5), ylim = c(0,30), type = "l",
#      xlab = "Time", ylab = "richness", main = "number of animal cladogenesis")
# cl <- rainbow(50)
# for (i in 1:50){
#   stt_table <- out[[i]][["island"]][["stt_table"]]
#   lines(stt_table[, 1], stt_table[, 7], col = cl[i], type = 'l')
# }
#
# par(mfrow = c(1, 1))

#  write.csv(net1, "D:/Desktop/net1.csv")

# Mt <- out_5[[1]][["state_list"]][[2]][["Mt"]]
# status_p <- out_5[[1]][["state_list"]][[2]][["status_p"]]
# status_a <- out_5[[1]][["state_list"]][[2]][["status_a"]]
# delete_p <- which(status_p == 0)
# delete_a <- which(status_a == 0)
# Mt <- Mt[-delete_p, -delete_a]
# net1 <- as.data.frame(Mt)

# out <- out_5
# network <- data.frame()
# for (i in 1:50){
#   M0 <- out[[i]][["state_list"]][[1]]
#   Mt <- out[[i]][["state_list"]][[2]][["Mt"]]
#   status_p <- out[[i]][["state_list"]][[2]][["status_p"]]
#   status_a <- out[[i]][["state_list"]][[2]][["status_a"]]
#
#   links_p0 <- rowSums(M0)
#   links_a0 <- colSums(M0)
#
#   links_p <- rowSums(Mt)
#   links_p[which(status_p == 0)] <- NA
#   links_a <- colSums(Mt)
#   links_a[which(status_a == 0)] <- NA
#
#   links_all <- qpcR:::cbind.na(links_p0, links_p, links_a0, links_a)
#   net <- data.frame(links_all)
#   network <- (network, net)
#   write.csv(net, "D:/Desktop/network.csv", row.names = FALSE)
# }


# set.seed(12)
# M0 = matrix(
#   sample(c(0, 1), 25, replace = TRUE),
#   ncol = 5,
#   nrow = 5)
# M0[4, ]
## M0[4, ] whould be
##  0 1 0 1 1
# set.seed(2)
# Mt = matrix(
#   sample(c(0, 1), 48, replace = TRUE),
#   ncol = 6,
#   nrow = 8)
# Mt[4, ]
## Mt[4, ] would be
##  1 0 1 0 1 1

#### explaination
## M0[4, ]: 0 1 0 1 1
## Mt[4, ]: 1 0 1 0 1 1
## M_41, M_43 gained links, but M_42 and M_44 lost links because of random loss
## event (plant species 4 was there, but because of "qloss*Mij*Pi*Aj". Here Mij
## intends to M[1:nrow(M0), 1:ncol(M0)] as it's immigration event). Suppose it
## colonizes again, colonization time should be refreshed but also links with it
## should still be the same as the original, which means M_42 = 1 and M_44 = 1.
## The idea behind it is...

