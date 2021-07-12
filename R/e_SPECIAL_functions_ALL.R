# modified https://github.com/daattali/rsalad/blob/master/R/notIn.R
# because of Check warnings from:
#  `%notin%` <- Negate(`%in%`)
#' Boolean operator "not in"
#'
#' Negation of the \code{\%in\%} operator.
#' Determine if values in the first argument don't exist in the second argument.
#'
#' \code{\%notin\%} is the functional inverse of \code{\%in\%}.
#'
#' \code{lhs \%notin\% rhs} is equivalent to \code{notin(lhs, rhs)}.
#'
#' @param  x Vector or NULL: the values to be matched.
#' @param  y Vector or NULL: the values to be matched against.
#' @return A logical vector of the same length as \code{x}.
#'
#'   For every element in \code{x}, return \code{FALSE} if the value exists
#'   in \code{y}, and \code{TRUE} otherwise.
#' @examples
#' "a" %notin% letters
#' "a" %notin% LETTERS
#' c("a", "A") %notin% letters
#' letters %notin% c("a", "b", "c")
#' notin("A", letters)
#' @name notin
NULL

#' @export
#' @rdname notin
"%notin%" <- function(x, y) {
  notin(x, y)
}

#' @export
#' @rdname notin
notin <- function(x, y) {
  stopifnot(
    (is.vector(x) || is.null(x)) &&
    (is.vector(y) || is.null(y))
  )
  !(x %in% y)
}



#' All Determine Duplicate Elements
#'
#' \code{e_duplicated_all()} determines which elements of a vector or data frame are duplicates, and returns a logical vector indicating which elements (rows) are duplicates.
#'
#' @param dat list or data.frame
#'
#' @return logical for all duplicated values
#' @export
#'
#' @examples
#' duplicated      (c(1, 2, 2, 3, 4, 5, 2))
#' e_duplicated_all(c(1, 2, 2, 3, 4, 5, 2))
#'
#' dat <- data.frame(a = c(1,2,1,2,1,2), b = c(3,3,4,4,3,4))
#' dat
#' dat[e_duplicated_all(dat), ]
e_duplicated_all <-
  function(
    dat
  ) {
    # https://stat.ethz.ch/pipermail/r-help/2011-October/291383.html

    duplicated(dat) | duplicated(dat, fromLast = TRUE)
}
