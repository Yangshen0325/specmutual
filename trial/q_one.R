###2002_23 set2
M0 <- matrix(1, ncol = 10, nrow = 10)
M0[lower.tri(M0)] <-0
M0<-M0[,c(10:1)]

set2 <- create_mutualism_pars(
  lac_pars = c(0.0, 0.0),
  mu_pars = c(0.0, 0.0, 0.0, 0.0),
  K_pars = c(20, 20, 3.0, 3.0),
  gam_pars = c(0.05, 0.05),
  laa_pars = c(0.0, 0.0, 0.0, 0.0),
  qgain = 0.0,
  qloss = 0.0,
  lambda0 = 0.0,
  M0 = M0,
  transprob = 0.0
)
## run the simulation 1000 times
out_list <- list()
for (loop in 1:1000){
  print(loop)
  set.seed(loop)
  r2 <- sim_core_mutualism(total_time = 5, mutualism_pars = set2)
  island_spec <- r2[["island_spec"]]
  #if (is.null(island_spec)) stop("here it is")
  out_list[[length(out_list) + 1]] <- island_spec
}
save(out_list, file = "D:/YSPhD_Aca/specmutual/trial/out_list.RData")
## deal with data
library(tidyverse)
data <- map2_dfr(out_list, seq(out_list), ~ .x %>%
                   as_tibble %>%
                   mutate(sim_id = .y))

data %>%
  group_by(`Species state`, Species) %>%
  summarize(n = n()) %>%
  View
