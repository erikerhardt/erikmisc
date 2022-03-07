#' Return star symbols for ranges between a set of p-values, or any set of numbers
#'
#' Default text for caption: "p-value stars:  (blank): not sig.;   -: p > 0.10;   *: p <= 0.05;   **: p <= 0.01;   *** : p <= 0.001;   **** : p <= 0.0001"
#'
#' @param p_values  list of p-values
#' @param cutpoints list of p-value cutpoints, should include 0 and 1 at extremes
#' @param symbols   list of symbols with length one fewer than cutpoints to indicate which two cutpoints the p-value was between
#'
#' @return a list of symbols of the same length as p_values
#' @importFrom stats symnum
#' @export
#'
#' @examples
#' e_pval_stars(c(0.049, 0.050, 0.051, NA))
e_pval_stars <-
  function(
    p_values  = NULL
  , cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 0.1, 1)
  , symbols   = c("****", "***", "**", "*", "-", " ")
  ) {

  pval_stars <-
    unclass(
      stats::symnum(
        x         = p_values
      , corr      = FALSE
      , na        = FALSE
      , cutpoints = cutpoints
      , symbols   = symbols
      )
    )

  return(pval_stars)

} # e_pval_stars
