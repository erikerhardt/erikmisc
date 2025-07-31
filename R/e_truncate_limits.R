#' Truncate limits of a vector to specified min/max values
#'
#' @param dat_vec   numeric vector
#' @param min_max   min and max values which to truncate values
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' dat_vec = c(NA, -Inf, -10:10, +Inf, NA)
#' e_truncate_limits(dat_vec, min_max = c(-Inf, Inf) )
#' e_truncate_limits(dat_vec, min_max = c(-5, +5)    )
#' e_truncate_limits(dat_vec, min_max = c(-5, Inf)   )
#' e_truncate_limits(dat_vec, min_max = c(-Inf, +5)  )
e_truncate_limits <-
  function(
    dat_vec = NULL
  , min_max = c(-Inf, Inf)
  ) {
  ## dat_vec = c(NA, -Inf, -10:10, +Inf, NA)
  ## min_max = c(-5, +5)
  ## min_max = c(-Inf, Inf)
  ## min_max = c(-5, Inf)
  ## min_max = c(-Inf, +5)

  # min value
  dat_vec <-
    pmax(
      min_max[1]
    , pmax(
        min_max[1]
      , dat_vec
      )
    )

  # max value
  dat_vec <-
    pmin(
      min_max[2]
    , pmin(
        min_max[2]
      , dat_vec
      )
    )

  return(dat_vec)

} # e_truncate_limits
