#' Convert from Probability to Odds Ratio
#'
#' @param p          list of probabilities
#' @param sw_percent T/F input as percent, instead?
#'
#' @return or        list of odds ratios
#' @export
#'
#' @examples
#' p <- c(0, 0.1, 0.2, 1/3, 1/2, 2/3, 0.8, 0.9)
#' e_convert_probability_to_oddsratio(p)
e_convert_probability_to_oddsratio <-
  function(
    p          = NULL
  , sw_percent = FALSE
  ) {

  if(sw_percent) {
    p <- p / 100
  }

  or <- p / (1 - p)

  return(or)
}
