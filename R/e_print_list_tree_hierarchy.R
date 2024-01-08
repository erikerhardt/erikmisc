#' Print list tree hierarchy
#'
#' runs \code{data.tree::FromListSimple()}
#'
#' @param x   list of lists
#'
#' @return list_tree Tree as a data.frame
#'
#' @importFrom data.tree FromListSimple
#' @export
#'
#' @examples
#' x <- list()
#' x$aa <- list()
#' x$aa <- list(x = list(xx = 1), y = list(yy = 2), z = list(zz = 3))
#' x$bb <- list()
#' x$bb <- list(xx = 4, yy = 5, zz = 6)
#' e_print_list_tree_hierarchy(x)
e_print_list_tree_hierarchy <-
  function(
    x = NULL
  ) {

  list_tree =
    x |>
    data.tree::FromListSimple() |>
    as.data.frame()

  return(list_tree)

  } # e_print_list_tree_hierarchy
