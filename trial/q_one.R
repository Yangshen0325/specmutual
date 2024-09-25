###2002_23 set2
M0 <- matrix(1, ncol = 10, nrow = 10)
M0[lower.tri(M0)] <-0
M0<-M0[,c(10:1)]

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
## run the simulation 1000 times
out_list <- list()
for (loop in 1:1000){
  print(loop)
  set.seed(loop)
  island_spec <- trial_sim(total_time = 5, mutualism_pars = set)

  #if (is.null(island_spec)) stop("here it is")
  out_list[[length(out_list) + 1]] <- island_spec
}
# save(out_list, file = "D:/YSPhD_Aca/specmutual/trial/out_list.RData")
## deal with data
library(tidyverse)
data <- map2_dfr(out_list, seq(out_list), ~ .x %>%
                   as_tibble %>%
                   mutate(sim_id = .y))

freq_out <- data %>%
  group_by(`Species state`, Species) %>%
  summarize(n = n()) %>%
  View
# freq_out_p <- freq_out[1:10, ]
# freq_out_a <- freq_out[11:20, ]
#
# ggplot(freq_out_p,
#        aes(x = Species, y= n)) +
#   geom_bar(stat = 'identity')
#
# ggplot(freq_out_a,
#        aes(x = Species, y= n)) +
#   geom_bar(stat = 'identity')
freq_out$Species <- factor(freq_out$Species,
                           levels = as.character(c(1:10)))
ggplot(freq_out,
       aes(x=`Species state`,y=n,fill=Species)) +
  geom_col(position = 'dodge')

ggsave("D:/YSPhD_Aca/specmutual/trial/out_list.tiff",
       width = 4, height = 4, dpi = 300)
dev.off()
