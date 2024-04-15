# sample the possible event
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
# sample next time value
sample_time_mutual <- function(rates, timeval) {
  totalrate <- do.call(sum, rates)

  dt <- stats::rexp(1, totalrate)
  timeval <- timeval + dt

  return(list(
    timeval = timeval,
    dt = dt
  ))
}
