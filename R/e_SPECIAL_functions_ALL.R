#' Boolean operator "not in"
#'
#' Negation of the \code{\%in\%} operator.
#'
#' @param
#'
#' @return
#' @export
#'
#' @examples
#' letters %notin% c("a", "b", "c")
`%notin%` <- Negate(`%in%`)

