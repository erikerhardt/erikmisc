# Index for this file
# notin
# duplicated
# extract numbers

# ------------------------------------------------------------------------------
# because of Check warnings from:
#  `%notin%` <- Negate(`%in%`)
# This is the version in use below
# https://peter.solymos.org/code/2016/11/26/how-to-write-and-document-special-functions-in-r.html

#' Boolean operator "not in"
#'
#' Negation of the \code{\%in\%} operator, see \code{?match}.
#' \code{\%notin\%} is the negation of \code{\link{\%in\%}}, which returns a logical vector indicating if there is a non-match or not for its left operand.
#'
#' @param  x      vector or \code{NULL}: the values to be matched.
#' @param  table  vector or \code{NULL}: the values to be matched against.
#' @return A logical vector, indicating if a non-match was located for each element of \code{x}: thus the values are \code{TRUE} or \code{FALSE} and never \code{NA}.
#'
#' @usage       x \%notin\% table
#' @name        %notin%
#' @rdname      notin
#' @export
#' @examples
#' "a" %notin% letters
#' "a" %notin% LETTERS
#' c("a", "A") %notin% letters
#' letters %notin% c("a", "b", "c")
"%notin%" <- function(x, table) !(match(x, table, nomatch = 0) > 0)



# ------------------------------------------------------------------------------
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

    base::duplicated(dat) | base::duplicated(dat, fromLast = TRUE)
}



#' Extract numbers from a string
#'
#' @param vec list of strings
#'
#' @return numeric list
#' @importFrom stringr str_extract_all
#' @export
#'
#' @examples
#' # note difference in last two items
#' vec <- c(NA, 1, "a", "a1a", "@%^#@0.23asdf", ")(&*.2&*", ")(&*0.2&*")
#' e_extract_numbers_from_string(vec)
#' vec <- c(NA, "1a2")
#' e_extract_numbers_from_string(vec)
e_extract_numbers_from_string <-
  function (
    vec
  ) {
  # https://stackoverflow.com/questions/14543627/extracting-numbers-from-vectors-of-strings
  # pattern is by finding a set of numbers in the start and capturing them
  # This approach makes the decimal point and decimal fraction optional and allows multiple numbers to be extracted:
  # The concern about negative numbers can be address with optional perl style look-ahead:
    # EBE: removed unlist() in order to keep NAs for blank values
  # out <-
  #   as.numeric(
  #     regmatches(
  #       vec
  #     , gregexpr(
  #         "(?>-)*[[:digit:]]+\\.*[[:digit:]]*"
  #       , vec
  #       , perl = TRUE
  #       )
  #     )
  #   )

  out <-
    stringr::str_extract_all(
      string    = vec
    , pattern   = "(?>-)*[[:digit:]]+\\.*[[:digit:]]*"
    , simplify  = TRUE
    )

  # if only one column, then return it as a list
  if (ncol(out) == 1) {
    out <- out[, 1]
  }
  # otherwise, return it as a table and need to later determine what to do with it

  return(out)
} # e_extract_numbers_from_string

