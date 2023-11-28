#' Interval for plot breaks, determine a "nice" length of seq(by = Interval)
#'
#' @param values              numeric values for an axis
#' @param num_intervals       rough desired number of intervals
#' @param val_leading_digits  leading digits to consider (in ascending order, end with 10)
#'
#' @return interval for breaks; if \code{length(values) == 1}, then return \code{values}
#' @export
#'
#' @examples
#' e_plot_calc_break_interval(1:250)
#' e_plot_calc_break_interval(1:1000)
#' e_plot_calc_break_interval(-1:1)
e_plot_calc_break_interval <-
  function(
    values
  , num_intervals     = 10
  , val_leading_digits = c(1, 2, 5, 10)   # always end with 10 to assure a number is returned
  ) {

  if (length(values) == 1) {
    message("e_plot_calc_break_interval: values is length 1, returning values for break_interval")
    return(values)
  }

  # https://stackoverflow.com/questions/237220/tickmark-algorithm-for-a-graph-axis

  values_range  <- values |> range() |> diff()
  num_digits    <- values_range |> log10() |> floor()
  order_mag     <- 10 ^ num_digits

  for (i_val in val_leading_digits) {
    if (values_range / (order_mag / i_val) >= num_intervals) {
      break_interval <- order_mag / i_val
      return( break_interval )
    }
  }

  # this won't run if last val_leading_digits is 10
  break_interval <- order_mag / 10
  return( break_interval )

} # e_plot_calc_break_interval

