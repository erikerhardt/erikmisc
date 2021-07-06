#' Boolean operator "not in"
#'
#' Negation of the \code{\%in\%} operator.
#'
#' @return
#' @export
#'
#' @examples
#' letters %notin% c("a", "b", "c")
`%notin%` <- Negate(`%in%`)

