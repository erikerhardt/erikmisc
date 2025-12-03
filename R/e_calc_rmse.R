#' Root Mean Squared Error
#'
#' @param obs     list of observed values
#' @param pred    list of predicted values
#'
#' @return  rmse
#' @export
#'
#' @examples
#' e_calc_rmse(obs = 1:5 + rnorm(n=5), pred = 1:5)
e_calc_rmse <-
  function(
    obs
  , pred
  ) {

  rmse <- sqrt(mean((obs - pred)^2, rm.na = TRUE))
  return(rmse)
} # e_calc_rmse
