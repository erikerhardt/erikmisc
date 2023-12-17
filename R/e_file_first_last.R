#' List of filenames as the first or last n by filename or file time matching a regular expression within a path
#'
#' @param name            filename regular expression for which to search
#' @param path            path
#' @param sw_first_last   first (oldest) or last (newest) set of files
#' @param n_file          number of filenames to return
#' @param sw_sort         sort by filename, or by time from \code{file.inf}, file modification, last status change, or last access time
#' @param sw_rev_order    default sort starting with first (oldest) or reverse to start with last (newest)?
#' @param ...             additional arguments passed to \code{list.files} in addition to \code{path} and \code{pattern}
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
#' e_file_first_last()
#' # newest 3 files in getwd()
#' e_file_first_last(n_file = 3, sw_sort = "mtime")
#'
#' }
e_file_first_last <-
  function(
    name        = NULL
  , path        = "."
  , sw_first_last  = c("first", "last")[2]
  , n_file      = 1
  , sw_sort     = c("filename", "mtime", "ctime", "atime")[1]
  , sw_rev_order = c(TRUE, FALSE)[1]
  , ...
  ) {

  fn_df <-
    list.files(path = path, pattern = name, ...) |>
    file.info(full.names = TRUE) |>
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

  if ( sw_first_last == c("first", "last")[2] ) {
    fn_df <-
      fn_df |>
      dplyr::slice_tail(
        n = n_file
      )
  }
  if ( sw_first_last == c("first", "last")[1] ) {
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

  if ( sw_rev_order ) {
    fn_out <-
      fn_out |>
      rev()
  }

  return(fn_out)
} # e_file_first_last
