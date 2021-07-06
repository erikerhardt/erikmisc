#' check a function and if there's an error return an indication that we can ifelse() to either run for real or skip
#'
#' @param f report result of a \code{try()}
#'
#' @return
#' @export
#'
#' @examples
#' sw_try_ok <-
#'   !e_is_error(
#'     try(
#'       # try what you want, check for error
#'       log(NULL)
#'     )
#'   )
#' if(sw_try_ok) {
#'   # no error, do what you intended to do
#' } else {
#'   # error, do something else
#' }
e_is_error <-
  function(
    f
  ) {
  # https://rdrr.io/cran/BBmisc/src/R/is_error.R
  inherits(f, c("try-error", "error"))
} # e_is_error
