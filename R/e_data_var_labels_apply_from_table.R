#' Apply labels to a dataset from a table of Var and Label
#'
#' @param dat  a data.frame
#' @param var_labels a data.frame with columns \code{Var} and \code{Label}, where \code{Var} are column names that may appear in \code{dat}
#'
#' @return dat the same data.frame with attribute "label" added
#' @export
#'
#' @examples
#' dat <- tibble::tibble(a = 1:10, b = 11:20, c = 21:30)
#' var_labels <-
#'   tibble::tribble(
#'     ~Var, ~Label
#'   , "a" , "Var a Label"
#'   , "b" , "Var b Label"
#'   , "c" , "Var c Label"
#'   )
#' dat |> str()
#' dat |> e_data_var_labels_apply_from_table(var_labels) |> str()
e_data_var_labels_apply_from_table <-
  function(
    dat
  , var_labels
  ) {

  for (i_var in seq_len(nrow(var_labels))) {
    ## i_var = 1
    if (var_labels$Var[i_var] %in% names(dat))
      labelled::var_label(dat[,var_labels$Var[i_var]]) <-
        var_labels$Label[i_var]
  } # i_var

  return(dat)
} # e_data_var_labels_apply_from_table

