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
