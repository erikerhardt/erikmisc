#' Cross-correlation function, maximum over lags
#'
#' @param x1        time series object
#' @param x2        time series object
#' @param lag.max   range of lags, passed to \code{stats::ccf}
#' @param cor_goal  \code{"max"} for maximum (typically positive) correlation, \code{"min"} for minimum (typically negative) correlation, \code{"abs"} for maximum absolute correlation (largest positive or negative)
#'
#' @return list cor = maximum correlation, lag = lag to shift x2 to align with x1 for maximum correlation
#' @export
#'
#' @examples
#' n_shift = 4
#' n_obs <- n_shift + length(datasets::AirPassengers)
#'
#' dat_ex <-
#'   tibble::tibble(
#'     id = seq_len(n_obs)
#'   , x1 = c(datasets::AirPassengers, rep(NA, n_shift))
#'   , x2 = c(rep(NA, n_shift), datasets::AirPassengers)
#'   ) |>
#'   dplyr::mutate(
#'     x2 = x2 + rnorm(n = length(x2), mean = 0, sd = sd(x2, na.rm = TRUE) / 3)
#'   )
#'
#' e_ccf_max(
#'     x1        = dat_ex$x1
#'   , x2        = dat_ex$x2
#'   , lag.max   = 10
#'   , cor_goal  = c("max", "min", "abs")[1]
#'   )
#'
e_ccf_max <-
  function(
    x1        = NULL
  , x2        = NULL
  , lag.max   = 10
  , cor_goal  = c("max", "min", "abs")[1]
  ) {
  ## x1 = dat_ex$x1
  ## x2 = dat_ex$x2

  out_ccf <-
    stats::ccf(
      x         = x1
    , y         = x2
    , lag.max   = lag.max
    , type      = c("correlation", "covariance")[1]
    , plot      = FALSE
    , na.action = na.pass
    )

  if (cor_goal  == c("max", "min", "abs")[1]) {
    ind_max <- which.max(out_ccf$acf)
  }
  if (cor_goal  == c("max", "min", "abs")[2]) {
    ind_max <- which.min(out_ccf$acf)
  }
  if (cor_goal  == c("max", "min", "abs")[3]) {
    ind_max <- which.max(abs(out_ccf$acf))
  }

  out <-
    list(
      cor = out_ccf$acf[ind_max]
    , lag = out_ccf$lag[ind_max]
    )

  return(out)

} # e_ccf_max

