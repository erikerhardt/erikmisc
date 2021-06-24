#' Root Mean Squared Error
#'
#' @param obs
#' @param pred
#'
#' @return
#' @export
#'
#' @examples
f_rmse <- function(obs, pred) {

  rmse <- sqrt(mean((obs - pred)^2))
  return(rmse)
}
