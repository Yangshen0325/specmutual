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

###I want to plot immigration rates for each plant species (differ in
### the number of partners) over timeval
sim_dynamics_rates <- function(simtime, mutualism_pars){
  #### Initialization ####
  timeval <- 0
  M0 <- mutualism_pars$M0
  Mt <- M0
  maxplantID <- NROW(M0)
  maxanimalID <- NCOL(M0)
  status_p <- matrix(0, nrow = NROW(M0), ncol = 1)
  status_a <- matrix(0, nrow = NCOL(M0), ncol = 1)

  island_spec <- c()
  timeval_list <- list()
  rates_list <- list()
  stt_table <- matrix(ncol = 7)
  colnames(stt_table) <- c("Time", "nIp", "nAp", "nCp", "nIa", "nAa", "nCa")
  stt_table[1, ] <- c(simtime, 0, 0, 0, 0, 0, 0)
  #### Start Monte Carlo iterations ####
  while (timeval < simtime){
    rates <- update_rates_mutualism(Mt = Mt,
                                    status_p = status_p,
                                    status_a = status_a,
                                    mutualism_pars = mutualism_pars,
                                    island_spec = island_spec)
    rates_list[[length(rates_list) + 1 ]] <- rates
    # next time
    timeval_and_dt <- calc_next_timeval_mutualism(rates = rates, timeval = timeval)
    timeval <- timeval_and_dt$timeval
    timeval_list[[length(timeval_list) + 1 ]] <- timeval

    if (timeval <= simtime){
      # next event
      possible_event <- sample_event_mutualism(rates = rates)
      # next state based on event
      updated_state <- sim_update_state_mutualism(timeval = timeval,
                                                  simtime = simtime,
                                                  possible_event = possible_event,
                                                  Mt = Mt,
                                                  status_p = status_p,
                                                  status_a = status_a,
                                                  maxplantID = maxplantID,
                                                  maxanimalID = maxanimalID,
                                                  island_spec = island_spec,
                                                  stt_table = stt_table,
                                                  mutualism_pars = mutualism_pars)
      Mt <- updated_state$Mt
      status_p <- updated_state$status_p
      status_a <- updated_state$status_a
      maxplantID <- updated_state$maxplantID
      maxanimalID <- updated_state$maxanimalID
      island_spec <- updated_state$island_spec
      stt_table <- updated_state$stt_table
    }
  }

  return(list(rates_list = rates_list,
              timeval_list = timeval_list))
}
row1 <- matrix(1, nrow = 1, ncol = 10)
row2 <- matrix(c(1,0,0,0,1,1,1,1,1,1), nrow = 1, ncol = 10)
row3 <- matrix(c(1,1,0,0,0,1,1,0,1,0), nrow = 1, ncol = 10)
row4 <- matrix(c(0,1,0,0,0,1,1,0,0,0), nrow = 1, ncol = 10)
M0 <- rbind(row1, row2, row3, row4)

# with this set, the dynamics of every plant species should be constant with 0.5
mutualism_pars_set1 <- list(
  lac_pars = c(0, 0),
  mu_pars = c(0, 0, 0, 0),
  K_pars = c(Inf, 50, Inf, 0.1),
  gam_pars = c(0.5, 0),
  laa_pars = c(0, 0, 0, 0),
  qgain = 0,
  qloss = 0,
  lambda0 = 0,
  M0 = M0,
  transprob = 0
)
results1 <- sim_dynamics_rates(simtime = 5, mutualism_pars = mutualism_pars_set1)
plant1 <- c()
plant2 <- c()
plant3 <- c()
plant4 <- c()
for (i in 1:length(results1[[1]])){
immig_p <- results1[["rates_list"]][[i]][["immig_p"]]
plant1[i] <- immig_p[1, ]
plant2[i] <- immig_p[2, ]
plant3[i] <- immig_p[3, ]
plant4[i] <- immig_p[4, ]
}
timeval1 <- results1[[2]]
plot(unlist(timeval1), plant1, col = "red")
lines(unlist(timeval1), plant2, col = "blue")
lines(unlist(timeval1), plant3, col = "green")
lines(unlist(timeval1), plant4, col = "black")
legend("topleft", legend=c("plant1", "plant2", "plant3", "plant4"),
       col=c("red", "blue", "green", "black"), lty = 1:2, cex=0.8)

# with this set, I am expecting plant1 has high immigration rates
# no, because no animals there on the island, N~/K is 0.
mutualism_pars_set2 <- list(
  lac_pars = c(0, 0),
  mu_pars = c(0, 0, 0, 0),
  K_pars = c(Inf, 50, 0.1, 0.1),
  gam_pars = c(0.5, 0),
  laa_pars = c(0, 0, 0, 0),
  qgain = 0,
  qloss = 0,
  lambda0 = 0,
  M0 = M0,
  transprob = 0
)
mutualism_pars_set3 <- list(
  lac_pars = c(0, 0),
  mu_pars = c(0, 0, 0, 0),
  K_pars = c(50, 50, 0.1, 0.1),
  gam_pars = c(0.5, 0),
  laa_pars = c(0, 0, 0, 0),
  qgain = 0,
  qloss = 0,
  lambda0 = 0,
  M0 = M0,
  transprob = 0
)
mutualism_pars_set4 <- list(
  lac_pars = c(0, 0),
  mu_pars = c(0, 0, 0, 0),
  K_pars = c(20, 50, 0.1, 0.1),
  gam_pars = c(0.5, 0),
  laa_pars = c(0, 0, 0, 0),
  qgain = 0,
  qloss = 0,
  lambda0 = 0,
  M0 = M0,
  transprob = 0
)
results2 <- sim_dynamics_rates(simtime = 5, mutualism_pars = mutualism_pars_set2)
results3 <- sim_dynamics_rates(simtime = 5, mutualism_pars = mutualism_pars_set3)
results4 <- sim_dynamics_rates(simtime = 5, mutualism_pars = mutualism_pars_set4)

plant1 <- c()
plant2 <- c()
plant3 <- c()
plant4 <- c()
for (i in 1:length(results2[[1]])){
  immig_p <- results2[["rates_list"]][[i]][["immig_p"]]
  plant1[i] <- immig_p[1, ]
  plant2[i] <- immig_p[2, ]
  plant3[i] <- immig_p[3, ]
  plant4[i] <- immig_p[4, ]
}
timeval2 <- results2[[2]]

plant11 <- c()
plant2 <- c()
plant3 <- c()
plant4 <- c()
for (i in 1:length(results3[[1]])){
  immig_p <- results3[["rates_list"]][[i]][["immig_p"]]
  plant11[i] <- immig_p[1, ]
  plant2[i] <- immig_p[2, ]
  plant3[i] <- immig_p[3, ]
  plant4[i] <- immig_p[4, ]
}
timeval3 <- results3[[2]]

plant111 <- c()
plant2 <- c()
plant3 <- c()
plant4 <- c()
for (i in 1:length(results4[[1]])){
  immig_p <- results4[["rates_list"]][[i]][["immig_p"]]
  plant111[i] <- immig_p[1, ]
  plant2[i] <- immig_p[2, ]
  plant3[i] <- immig_p[3, ]
  plant4[i] <- immig_p[4, ]
}
timeval4 <- results4[[2]]

par(mfrow = c(1, 3))
plot(unlist(timeval2), plant1, col = "red", type = "l")
plot(unlist(timeval3), plant11, col = "red", type = "l")
plot(unlist(timeval4), plant111, col = "red", type = "l")







