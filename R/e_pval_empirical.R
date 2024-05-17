#' Calculate empirical tail probabilities (p-values), typically of bootstrap resamples
#'
#' @param x           vector of values
#' @param obs         observed value to calculate tail probabilities
#' @param prefix_pval prefix for p-value named list
#' @param suffix_tail suffixes for p-value named list
#'
#' @return out        a named vector of 3 (pval_lower, pval_upper, pval_twoside)
#' @export
#'
#' @examples
#' out = e_pval_empirical(x = rnorm(1000), obs = 2, prefix_pval = "this_pval_")
#' out |> print()
e_pval_empirical <-
  function(
    x           = NULL
  , obs         = 0
  , prefix_pval = "pval_"
  , suffix_tail = c("lower", "upper", "twoside")
  ) {
  # x = rnorm(1000)
  # obs = 2

  pval_lower    <- sum(obs <= x) / length(x)
  pval_upper    <- sum(obs >= x) / length(x)
  pval_twoside  <- min(2 * min(c(pval_lower, pval_upper)), 1)

  out <-
    c(
      pval_lower
    , pval_upper
    , pval_twoside
    )

  names(out) <- paste0(prefix_pval, suffix_tail)

  return(out)
} # e_pval_empirical
