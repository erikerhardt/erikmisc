#' Extract confidence interval limits from vector (of bootstrap resamples)
#'
#' @param x                            vector of values
#' @param conf_level                   central proportion coverage of interval
#' @param percentile_lower             \code{NULL}, or lower percentile (0.025) of interval if \code{conf_level} not used
#' @param percentile_upper             \code{NULL}, or upper percentile (0.975) of interval if \code{conf_level} not used
#' @param sw_return_exact_CI_coverage  T/F, return exact coverage of interval or not
#'
#' @return out a named vector of 2 (lower, upper) or 3 (and coverage) values
#' @export
#'
#' @examples
#' x = seq(0, 1, length = 100001) |> sample(replace = TRUE)
#' e_CI_limits(x)
#' e_CI_limits(x, sw_return_exact_CI_coverage = TRUE)
#' e_CI_limits(x, conf_level = 0.80, sw_return_exact_CI_coverage = TRUE)
#' e_CI_limits(x, percentile_lower = 0.01, percentile_upper = 0.90, sw_return_exact_CI_coverage = TRUE)
e_CI_limits <-
  function(
    x
  , conf_level = 0.95
  , percentile_lower = NULL # 0.025
  , percentile_upper = NULL # 0.975
  , sw_return_exact_CI_coverage = FALSE
  ) {
  ## x = seq(0, 1, length = 1001) |> sample(replace = TRUE)

  if(!is.null(percentile_lower) & !is.null(percentile_upper)) {
    warning("e_CI_limits:  using percentile limits for CI")

    prob_CI <-
      c(
        lower = percentile_lower
      , upper = percentile_upper
      )
  } else {
    # calculate equal limits based on central conf_level
    prob_CI <-
      c(
        lower =     (1 - conf_level) / 2
      , upper = 1 - (1 - conf_level) / 2
      )
  }


  ind_CI <- prob_CI * length(x)
  ind_CI[1] <- ind_CI[1] |> ceiling()
  if (ind_CI[1] == 0) { ind_CI[1] <- 1 }
  ind_CI[2] <- ind_CI[2] |> floor()

  CI_limits <- sort(x)[ind_CI]

  out <-
    c(
      lower = CI_limits[1]
    , upper = CI_limits[2]
    )


  if (sw_return_exact_CI_coverage) {
    CI_coverage = (diff(as.numeric(ind_CI))+1) / length(x)

    out <-
      c(
        out
      , coverage = CI_coverage
      )
  }

  return(out)
}
