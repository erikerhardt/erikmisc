#' List of filenames as the first or last n by filename or file time matching a regular expression within a path
#'
#' @param name            filename regular expression to search for
#' @param path            path
#' @param sw_new_old      newest or oldest files
#' @param n_file          number of filenames to return
#' @param sw_sort         sorty by filename, or time to use from \code{file.inf}, file modification, last status change, or last access time.
#' @param sw_oldest_first sort oldest first or reverse for newest first?
#' @param ...             additional arguments passed to list.files in addition to \code{path} and \code{pattern}
#'
#' @return fn_out         filename list with path
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr slice_head
#' @importFrom dplyr slice_tail
#' @importFrom rlang sym
#' @importFrom tibble as_tibble
#' @importFrom tidyr drop_na
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # last filename by name in getwd()
#' e_file_newest_oldest()
#' # newest 3 files in getwd()
#' e_file_newest_oldest(n_file = 3, sw_sort = "mtime")
#'
#' }
e_file_newest_oldest <-
  function(
    name        = NULL
  , path        = "."
  , sw_new_old  = c("new", "old")[1]
  , n_file      = 1
  , sw_sort     = c("filename", "mtime", "ctime", "atime")[1]
  , sw_oldest_first = c(TRUE, FALSE)[1]
  , ...
  ) {

  fn_df <-
    list.files(path = path, pattern = name, ...) |>
    file.info(full.names = T) |>
    tibble::as_tibble(rownames = "filename") |>
    dplyr::filter(
      !isdir
    ) |>
    tidyr::drop_na(
      !!rlang::sym(sw_sort)
    ) |>
    dplyr::arrange(
      !!rlang::sym(sw_sort)
    )

  if ( n_file <= 0 | is.infinite(n_file) | !is.numeric(n_file)) {
    n_file = nrow(fn_df)
  }
  if ( n_file > nrow(fn_df) ) {
    n_file = nrow(fn_df)
  }

  if ( sw_new_old == c("new", "old")[1] ) {
    fn_df <-
      fn_df |>
      dplyr::slice_tail(
        n = n_file
      )
  }
  if ( sw_new_old == c("new", "old")[2] ) {
    fn_df <-
      fn_df |>
      dplyr::slice_head(
        n = n_file
      )
  }

  fn_out <-
    file.path(
      path
    , fn_df$filename
    )

  if ( sw_oldest_first ) {
    fn_out <-
      fn_out |>
      rev()
  }

  return(fn_out)
} # e_file_newest_oldest
