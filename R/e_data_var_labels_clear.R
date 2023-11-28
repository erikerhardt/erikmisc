#' Clear labels from variables in data frames
#'
#' @param dat  a data.frame or list of data.frames with attributes "label" to remove
#'
#' @return dat the same object with attribute "label" removed (set to \code{NULL})
#' @export
#'
#' @examples
#' # a single data.frame
#' dat <- dat_mtcars_e
#' dat |> str()
#' dat |> e_data_var_labels_clear() |> str()
#' # a list of data.frames
#' dat <- list(dat_mtcars_e, dat_mtcars_e)
#' dat |> str()
#' dat |> e_data_var_labels_clear() |> str()
e_data_var_labels_clear <-
  function(
    dat
  ) {
  if(inherits(dat, "list")) {
    for(i_list in 1:length(dat)) {
      class(dat[[ i_list ]]) <- base::setdiff(class(dat[[ i_list ]]), "labelled")
      attr(dat[[ i_list ]], "label") <- NULL
      # remove "labelled" from class
      class(dat[[ i_list ]]) <- base::setdiff(class(dat[[ i_list ]]), "labelled")
      # remove label from data.frame
      attr(dat[[ i_list ]], "label") <- NULL
      # remove labels from variables
      for (i_col in 1:ncol(dat[[ i_list ]])) {
        attr(dat[[ i_list ]][[ i_col ]], "label") <- NULL
      }
    }
  } else {
    # remove "labelled" from class
    class(dat) <- base::setdiff(class(dat), "labelled")
    # remove label from data.frame
    attr(dat, "label") <- NULL
    # remove labels from variables
    for (i_col in 1:ncol(dat)) {
      attr(dat[[ i_col ]], "label") <- NULL
    }
  }

  return(dat)
} # e_data_var_labels_clear

