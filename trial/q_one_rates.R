#complete_arr_all <- array(dim = c(10, 11, rep))
sim_mutual_pp <- function(total_time,
                          replicates,
                          mutualism_pars) {
  plant_arr <- array(dim = c(20, 11, replicates))
  animal_arr <- array(dim = c(20, 11, replicates))
  for (rep in 1:replicates) {
    set.seed(rep)
  testit::assert(are_mutualism_pars(mutualism_pars))
  timeval <- 0
  M0 <- mutualism_pars$M0
  Mt <- M0
  #M_true_list <- list()
  maxplantID <- nrow(M0)
  maxanimalID <- ncol(M0)
  status_p <- matrix(0, nrow = nrow(M0), ncol = 1)
  status_a <- matrix(0, nrow = ncol(M0), ncol = 1)

  island_spec <- c()
  stt_table <- matrix(ncol = 7)
  colnames(stt_table) <- c("Time", "nIp", "nAp", "nCp", "nIa", "nAa", "nCa")
  stt_table[1, ] <- c(total_time, 0, 0, 0, 0, 0, 0)
  plant_rs <- list()
  animal_rs <- list()
  timeval_list <- list()

  lac_pars <- mutualism_pars$lac_pars
  mu_pars <- mutualism_pars$mu_pars
  K_pars <-  mutualism_pars$K_pars
  gam_pars <-  mutualism_pars$gam_pars
  laa_pars <-  mutualism_pars$laa_pars
  qgain <-  mutualism_pars$qgain
  qloss <-  mutualism_pars$qloss
  lambda0 <-  mutualism_pars$lambda0
  transprob <-  mutualism_pars$transprob

  if (sum(gam_pars) == 0) {
    stop("Island has no species and the rate of
    colonisation is zero. Island cannot be colonised.")
  }

  #### Start Monte Carlo iterations ####
  while (timeval < total_time){
    rates <- update_rates_mutual(M0 = M0,
                                 Mt = Mt,
                                 status_p = status_p,
                                 status_a = status_a,
                                 lac_pars = lac_pars,
                                 mu_pars = mu_pars,
                                 K_pars = K_pars,
                                 gam_pars = gam_pars,
                                 laa_pars = laa_pars,
                                 qgain = qgain,
                                 qloss = qloss,
                                 lambda0 = lambda0,
                                 transprob = transprob,
                                 island_spec = island_spec)
    testit::assert(are_rates(rates))
    immig_p <- rates[["immig_p"]]
    immig_a <- rates[["immig_a"]]
    plant_rs[[length(plant_rs) + 1 ]] <- immig_p
    animal_rs[[length(animal_rs) + 1 ]] <- immig_a
    # next time
    timeval_and_dt <- sample_time_mutual(rates = rates, timeval = timeval)
    timeval <- timeval_and_dt$timeval
    timeval_list[[length(timeval_list) + 1 ]] <- timeval

    if (timeval <= total_time){
      # next event
      possible_event <- sample_event_mutual(rates = rates)
      # next state based on event
      updated_states <- update_states_mutual(M0 = M0,
                                             Mt = Mt,
                                             status_p = status_p,
                                             status_a = status_a,
                                             maxplantID = maxplantID,
                                             maxanimalID = maxanimalID,
                                             timeval = timeval,
                                             total_time = total_time,
                                             rates = rates,
                                             possible_event = possible_event,
                                             island_spec = island_spec,
                                             stt_table = stt_table,
                                             transprob = transprob)
      Mt <- updated_states$Mt
      status_p <- updated_states$status_p
      status_a <- updated_states$status_a
      maxplantID <- updated_states$maxplantID
      maxanimalID <- updated_states$maxanimalID
      island_spec <- updated_states$island_spec
      stt_table <- updated_states$stt_table
      #M_true <- Mt[which(status_p == 1), which(status_a == 1)]
      #M_true_list[[length(M_true_list) + 1]] <- M_true
    }
  }
  a <- seq(0, 5, 0.5)
  b <-list()
  b[1] <- 1
  for (i in 2:length(a)) {
    b[i] <- min(which(timeval_list >= a[i]))
  }
  plant_rs <- matrix(unlist(plant_rs[unlist(b)]), nrow = 20)
  animal_rs <- matrix(unlist(animal_rs[unlist(b)]), nrow = 20)
  plant_arr[, , rep] <- plant_rs
  animal_arr[, , rep] <- animal_rs
}

  return(list(plant_arr = plant_arr,
              animal_arr = animal_arr))
}


M0 <- matrix(1, ncol = 20, nrow = 20)
M0[lower.tri(M0)] <-0
M0<-M0[,c(20:1)]

set <- create_mutual_pars(
  lac_pars = c(0.0, 0.0),
  mu_pars = c(0.0, 0.0, 0.0, 0.0),
  K_pars = c(100, 100, 1.0, 1.0),
  gam_pars = c(0.5, 0.5),
  laa_pars = c(0.0, 0.0, 0.0, 0.0),
  qgain = 0.0,
  qloss = 0.0,
  lambda0 = 0.0,
  M0 = M0,
  transprob = 0.0
)
results <- sim_mutual_pp(total_time = 5,
                         replicates = 1000,
                         mutualism_pars = set)
plant_arr <- results[["plant_arr"]]
animal_arr <- results[["animal_arr"]]
mean_plant <- apply(plant_arr, c(1, 2), mean)
mean_animal <- apply(animal_arr, c(1, 2), mean)
timeval <- seq(0, 5, 0.5)


#plot plant
png(file="D:/YSPhD_Aca/specmutual/trial/rates_p_set4.png")

plot(timeval, mean_plant[1, ], type = "l", lwd = 3,
     xlim = c(0, 5), ylim = c(0.3, 0.5),
     xlab = "Timeval", ylab = "Plant Immig Rates")

cl <- rainbow(9)
for (x in 2:10){
  lines(timeval, mean_plant[x, ], type = "l", lwd = 3, col = cl[x-1])
}
legend(x = "topright",
       legend = c("plant1","plant2","plant3","plant4",
                  "plant5","plant6","plant7","plant8","plant9","plant10"),
       col = c("black", cl),
       bty = "n",
       cex = 0.3,
       lwd = 3)

dev.off()


#plot animal
png(file="D:/YSPhD_Aca/specmutual/trial/rates_a_set4.png")

plot(timeval, mean_animal[1, ], type = "l", lwd = 3,
     xlim = c(0, 5), ylim = c(0.3, 0.5),
     xlab = "Timeval", ylab = "Animal Rates")

cl <- rainbow(9)
for (x in 2:10){
  lines(timeval, mean_animal[x, ], type = "l", lwd = 3, col = cl[x-1])
}
legend(x = "topright",
       legend = c("animal1","animal2","animal3","animal4",
                  "animal5","animal6","animal7","animal8","animal9","animal10"),
       col = c("black", cl),
       bty = "n",
       cex = 0.3,
       lwd = 3)

dev.off()


