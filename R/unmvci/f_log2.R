#' Log2 nonnegative numbers after offsetting 0 by minimum non-zero value.
#'
#' @param x numeric vector
#'
#' @return
#' @export
#'
#' @examples
f_log2 <- function(x) {
  x <- as.numeric(unlist(x))

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

  x_log2 <- log2(x + min_add)

  return(x_log2)
}
