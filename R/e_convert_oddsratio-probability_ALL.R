#' Convert from Odds Ratio to Probability
#'
#' @param or         list of odds ratios
#' @param sw_percent T/F return as percent, instead?
#'
#' @return p         list of probabilities
#' @export
#'
#' @examples
#' or <- c(0, 1/9, 1/4, 1/2, 1, 2, 4, 9)
#' e_convert_oddsratio_to_probability(or)
e_convert_oddsratio_to_probability <-
  function(
    or         = NULL
  , sw_percent = FALSE
  ) {

  p <- or / (or + 1)

  if(sw_percent) {
    p <- p * 100
  }

  return(p)
}


#' Convert from Probability to Odds Ratio
#'
#' @param p          list of probabilities
#' @param sw_percent T/F input is percent
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
