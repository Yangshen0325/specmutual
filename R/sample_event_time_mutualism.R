# Samples what event to happen next

# [1]: immigration event with plant species
# [2]: extinction event with plant species
# [3]: cladogenesis event with plant species
# [4]: anagenesis event with plant species
# [5]: immigration event with animal species
# [6]: extinction event with animal species
# [7]: cladogenesis event with animal species
# [8]: anagenesis event with animal species
# [9]: cospeciation event between pairs
# [10]: gain links event between pairs
# [11]: loss links event between pairs

#library(reshape2)
sample_event_mutualism <- function(rates){

  output <- reshape2::melt(setNames(rates, seq_along(rates)))
  cnames <- c("plant", "animal", "rate", "event")
  colnames(output) <- cnames
  output$event <-as.integer(output$event)
  output$animal[output$event<9 & output$event > 4] <-
    output$plant[output$event<9 & output$event > 4]
  output$plant[output$event<9 & output$event > 4] <- 1

  x <- sample(1:dim(output)[1],
              size = 1,
              replace = FALSE,
              prob = unlist(rates))
  possible_event <- output[x, ]

  return(possible_event)
}

# Calculates when the next timestep will be.

calc_next_timeval_mutualism <- function(rates, timeval){

  output <- reshape2::melt(setNames(rates,seq_along(rates)))
  cnames <- c("plant", "animal", "rate", "event")
  colnames(output) <- cnames

  totalrate <- sum(output$rate)
  dt <- stats::rexp(1, totalrate)
  timeval <- timeval + dt

  return(list(timeval = timeval, dt = dt))
}


