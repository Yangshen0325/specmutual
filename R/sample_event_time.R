
#' Samples what event to happen next
#'
#' @return Numeric indicating what event will happen
#' \itemize{
#'   \item{[1]: immigration event with plant species}
#'   \item{[2]: extinction event with plant species}
#'   \item{[3]: cladogenesis event with plant species}
#'   \item{[4]: anagenesis event with plant species}
#'   \item{[5]: immigration event with animal species}
#'   \item{[6]: extinction event with animal species}
#'   \item{[7]: cladogenesis event with animal species}
#'   \item{[8]: anagenesis event with animal species}
#'   \item{[9]: cospeciation event between pairs}
#'   \item{[10]: gain links event between pairs}
#'   \item{[11]: loss links event between pairs}
#' }

sample_event_mutual <- function(rates) {
  testit::assert(are_rates(rates))

  possible_event <- sample(
    x = 1:11,
    size = 1,
    replace = FALSE,
    prob = c(
      sum(rates$immig_p),
      sum(rates$ext_p),
      sum(rates$clado_p),
      sum(rates$ana_p),
      sum(rates$immig_a),
      sum(rates$ext_a),
      sum(rates$clado_a),
      sum(rates$ana_a),
      sum(rates$cospec_rate),
      sum(rates$gain_rate),
      sum(rates$loss_rate)
    )
  )

  testit::assert(is.numeric(possible_event))
  testit::assert(possible_event >= 1)
  return(possible_event)
}


#' Samples the next time value
#'
#' @param timeval Current time of simulation
#' @param rates A named list of all kinds of rates as returned by
#' \code{\link{update_rates_mutual}}.
#'
#' @return A named list with numeric vector containing the time of the next
#' timestep and the change in time.
#'
sample_time_mutual <- function(rates, timeval) {
  totalrate <- do.call(sum, rates)

  dt <- stats::rexp(1, totalrate)
  timeval <- timeval + dt

  return(list(
    timeval = timeval,
    dt = dt
  ))
}
