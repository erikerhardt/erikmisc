#' Substring last n characters
#'
#' Uses \code{stringr::str_sub(string, start = -n)}.
#'
#' @param string  list of strings
#' @param last_n  number of characters from the end to keep
#'
#' @return        same list of strings but only up to last n characters from each
#' @importFrom stringr str_sub
#' @export
#'
#' @examples
#'
#' c("abcdefghij", "This has a space", "abc", NA) |>
#'  e_substr_last_n(last_n = 6)
e_substr_last_n <-
  function(
    string  = NULL
  , last_n  = 1
  ) {

  return(stringr::str_sub(string, start = -last_n) )
}
