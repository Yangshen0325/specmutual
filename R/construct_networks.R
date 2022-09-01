# construct the original mutualistic network in mainland, refering to
# Luis Santamatia et. al (2007) Linkage rules for plant-pollinator networks:
# trait complementary of exploitation barriers?
# model1: Lognormal neutral model
# flower <- round(runif(1, min = 10, max = 160))
# P = 45.573 - 0.8082 * F + 0.047 * F*F
flower <- 20
pollinator <- round(45.573-0.8082* flower + 0.047 * flower^2)
# vi and wj are independent random variates with lognormal distribution
wj <- rlnorm(flower)
vi <- rlnorm(pollinator)
link <- (vi %*% t(wj)) / max(vi %*% t(wj))
nonlink <- 1 - link
network <- matrix(nrow = pollinator, ncol = flower)
for (i in 1:pollinator){
  for (j in 1:flower){
    network[i, j] <- sample(c(0, 1),
                            size = 1,
                            replace = FALSE,
                            prob = c(nonlink[i, j], link[i, j]))
  }
}

network <- network[which(rowSums(network) != 0), which(colSums(network) != 0)]
M0 <- t(network)

# model2: Mixed model, mixed narrow-complementarity model with barrier model
# flower <- round(runif(1, min = 10, max = 160))
flower <- 20
pollinator <- round(45.573-0.8082* flower + 0.047 * flower^2)
vi1 <- runif(pollinator, 0, 1)
vi2 <- runif(pollinator, 0, 1)
vi3 <- runif(pollinator, 0, 1)
vi4 <- runif(pollinator, 0, 1)
wj1 <- runif(flower, 0, 1)
wj2 <- runif(flower, 0, 1)
wj3 <- runif(flower, 0, 1)
wj4 <- runif(flower, 0, 1)
delta_vi1 <- runif(pollinator, 0, 0.25)
delta_vi2 <- runif(pollinator, 0, 0.25)
delta_wj1 <- runif(flower, 0, 0.25)
delta_wj2 <- runif(flower, 0, 0.25)
complemd_k1 <- matrix(nrow = pollinator, ncol = flower)
complemd_k2 <- matrix(nrow = pollinator, ncol = flower)
barriermd_k3 <- matrix(nrow = pollinator, ncol = flower)
barriermd_k4 <- matrix(nrow = pollinator, ncol = flower)
for (i in 1:pollinator){
  for (j in 1:flower){
    complemd_k1[i, j] <- abs(vi1[i] - wj1[j]) - 0.5 * (delta_vi1[i] + delta_wj1[j])
    complemd_k2[i, j] <- abs(vi2[i] - wj2[j]) - 0.5 * (delta_vi2[i] + delta_wj2[j])
    barriermd_k3[i, j] <- vi3[i] -wj3[j]
    barriermd_k4[i, j] <- vi4[i] -wj4[j]
  }
}
complemd_k1 <- complemd_k1 < 0
complemd_k2 <- complemd_k2 < 0
barriermd_k3 <- barriermd_k3 > 0
barriermd_k4 <- barriermd_k4 > 0

network <- matrix(nrow = dim(complemd_k1)[1], ncol = dim(complemd_k1)[2])
network[which(complemd_k1 + complemd_k2 + barriermd_k3 + barriermd_k4 == 4)] <- 1
network[which(complemd_k1 + complemd_k2 + barriermd_k3 + barriermd_k4 != 4)] <- 0
network <- network[which(rowSums(network) != 0), which(colSums(network) != 0)]
M0 <- t(network)
# save(M0, file = "X:/YSPhD_Aca/specmutual/trial/M0.RData")









