#' Log2 nonnegative numbers after offsetting 0 by minimum non-zero value.
#'
#' Default \code{min_add} values:
#'   If specified, use that.
#'   All positive, add 0.
#'   Min is 0, add the minimum non-0 value.
#'   Min is negative and max is positive, add the minimum non-0 value (so minimum is now the previous non-0 value).
#'   Max is negative, add the minimum value plus 1 so \code{log2(x)=0} is the minimum value.
#'
#' @param x         numeric vector
#' @param min_add   value to add to x before taking log, if not specified and min(x) <= 0, then this is determined automatically
#' @param base_log  base for log
#'
#' @return numeric vector with attributes
#' @export
#'
#' @examples
#'
#' e_log_shift(x = c(0, 1, 2, 3), min_add = 1)
#' e_log_shift(x = c(0, 10, 100, 1000), base = 10)
#' e_log_shift(x = c(-4, -2, 0, 2, 4))
e_log_shift <-
  function(
    x
  , min_add   = NULL
  , base_log  = 2
  ) {
  # if passed as a column from a tibble, need to make it a list
  x <- as.numeric(unlist(x))

  # if specified min_add, then do it
  if(!is.null(min_add)) {
    x_log <-
      log(
        x + min_add
      , base = base_log
      )
    attr(x_log, "e_log_shift") <- c(min_add = min_add, base_log = base_log)

    return(x_log)
  }

  # all positive
  # add 0
  if (min(x, na.rm = TRUE) > 0) {
    min_add <- 0
  }

  # min is 0
  # add the minimum non-zero value
  if (min(x, na.rm = TRUE) == 0) {
    min_add <- min(x[(x > 0)], na.rm = TRUE)
  }

  # min is negative, max is positive
  # add the minimum non-zero value minus the minimum value
  #   (make the minimum value equal the minimum non-zero value)
  if ((min(x, na.rm = TRUE) < 0) & (max(x, na.rm = TRUE) > 0)) {
    min_add <- min(x[(x > 0)], na.rm = TRUE) - min(x, na.rm = TRUE)
  }

  # max is negative
  # add the minimum value plus 1 so log2(x)=0 is the minimum value
  if (max(x, na.rm = TRUE) <= 0) {
    min_add <- -min(x[(x > 0)], na.rm = TRUE) + 1
  }

  x_log <-
    log(
      x + min_add
    , base = base_log
    )
  attr(x_log, "e_log_shift") <- c(min_add = min_add, base_log = base_log)

  return(x_log)
}
