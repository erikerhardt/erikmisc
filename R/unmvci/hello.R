# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'



#' Prints 'Hello, world!' with "x".
#'
#' \code{hello} returns two strings pasted together
#'
#' @param x A string
#' @param sep A string separator passed to \code{paste()}
#'
#' @return NULL, side effect is printing string.
#' @export
#'
#' @examples
#' hello()
#' hello("Additional string.")
hello <- function(x = NULL, sep = " ") {
  message(trimws(paste("Hello, world!", as.character(x), sep = sep), which = "right"))
  invisible(NULL)
}
