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


#' Logit: Convert from Probability to log(Odds Ratio)
#'
#' See \code{car::logit} for a better implementation.
#'
#' @param p          list of probabilities
#' @param sw_percent T/F input is percent
#'
#' @return z         list of log(odds ratios) logit-scale values
#' @export
#'
#' @examples
#' p <- seq(0, 1, by = 0.1)
#' e_convert_logit(p)
e_convert_logit <-
  function(
    p          = NULL
  , sw_percent = FALSE
  ) {

  if(sw_percent) {
    p <- p / 100
  }

  z <- log(p / (1 - p))

  return(z)
}


#' Logistic: Convert from log(Odds Ratio) to Probability
#'
#' @param z          list of log(odds ratios) logit-scale values
#' @param sw_percent T/F output is percent
#'
#' @return p         list of probabilities
#' @export
#'
#' @examples
#' z <- qnorm(seq(0, 1, by = 0.1))
#' e_convert_logistic(z)
e_convert_logistic <-
  function(
    z          = NULL
  , sw_percent = FALSE
  ) {

  p <- exp(z) / (1 + exp(z))

  if(sw_percent) {
    p <- p * 100
  }

  return(p)
}
