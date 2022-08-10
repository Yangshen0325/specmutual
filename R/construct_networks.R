# construct the original mutualistic network in mainland, refering to
# Luis Santamatia et. al (2007) Linkage rules for plant-pollinator networks:
# trait complementary of exploitation barriers?
# model1: Lognormal neutral model
flower <- round(runif(1, min = 10, max = 160))
# P = 45.573 - 0.8082 * F + 0.047 * F*F
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
M0 <- t(network)

# model2: Mixed model, mixed narrow-complementarity model with barrier model
flower <- round(runif(1, min = 10, max = 160))
pollinator <- round(45.573-0.8082* flower + 0.047 * flower^2)
vi1 <- runif(pollinator, 0, 1)
vi2 <- runif(pollinator, 0, 1)
wj1 <- runif(flower, 0, 1)
wj2 <- runif(flower, 0, 1)
delta_vi1 <- runif(pollinator, 0, 0.25)
delta_vi2 <- runif(pollinator, 0, 0.25)
delta_wj1 <- runif(flower, 0, 0.25)
delta_wj2 <- runif(flower, 0, 0.25)

for (i in 1:pollinator){
  for (j in 1:flower){
    complemd_k1[i, j] <- abs(vi1[i] - wj1[j]) - 0.5 * (delta_vi1 + delta_wj1)

  }
}

















