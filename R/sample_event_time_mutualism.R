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

#' Samples what event will happen next
#'
#' @param rates a list with every event rates
#'
#' @return a dataframe indicating which species undergoing which event

sample_event_mutualism <- function(rates){

  testit::assert(are_rates(rates))

  output <- reshape2::melt(setNames(rates, seq_along(rates)))
  cnames <- c("plant", "animal", "rate", "event")
  colnames(output) <- cnames
  output$event <-as.integer(output$event)
  #event 5,7,6,8 related to animal
  output$animal[output$event < 9 & output$event > 4] <-
    output$plant[output$event < 9 & output$event > 4]
  output$plant[output$event < 9 & output$event > 4] <- 1

  x <- sample(1:dim(output)[1],
              size = 1,
              replace = FALSE,
              prob = output$rate)
  possible_event <- output[x, ]

  return(possible_event)
}

#' Calculates when the next timestep will be
#'
#' @param rates a list with every event rates
#' @param timeval current time of simulation
#'
#' @return named list with numeric vector containing the time of the next
#' timestep and the change in time.

calc_next_timeval_mutualism <- function(rates, timeval){

  testit::assert(are_rates(rates))

  output <- reshape2::melt(setNames(rates,seq_along(rates)))
  cnames <- c("plant", "animal", "rate", "event")
  colnames(output) <- cnames

  totalrate <- sum(output$rate)
  dt <- stats::rexp(1, totalrate)
  timeval <- timeval + dt

  return(list(timeval = timeval, dt = dt))
}


