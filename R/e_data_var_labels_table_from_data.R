#' Create table of labels from a labelled dataset, columns Var and Label
#'
#' @param dat  a data.frame with attribute "label" for columns
#'
#' @return var_labels a data.frame with columns \code{Var} and \code{Label}, where \code{Var} are column names that may appear in \code{dat}
#' @export
#'
#' @examples
#' # remove two variable labels
#' labelled::var_label(dat_mtcars_e[,"vs"]) <- NULL
#' labelled::var_label(dat_mtcars_e[,"am"]) <- NULL
#' # get table of labels
#' var_labels <- dat_mtcars_e |> e_data_var_labels_table_from_data()
#' var_labels |> print()
e_data_var_labels_table_from_data <-
  function(
    dat
  ) {
  ## dat = dat_mtcars_e

  n_var <- ncol(dat)

  var_labels <-
    dplyr::left_join(
      tibble::tibble(
        Var   = dat |> names()
      #, Label_temp = dat |> names()
      )
    ,
      #var_with_labels <-
      tibble::tibble(
        Var   = labelled::var_label(dat) |> unlist() |> names()
      , Label = labelled::var_label(dat) |> unlist() |> as.character()
      )
    , by = dplyr::join_by(Var)
    ) |>
    # When no Label, assign the Var name to Label
    dplyr::mutate(
      Label =
        dplyr::case_when(
          !is.na(Label) ~ Label
        , is.na(Label)  ~ Var
        )
    )

  return(var_labels)
} # e_data_var_labels_table_from_data

