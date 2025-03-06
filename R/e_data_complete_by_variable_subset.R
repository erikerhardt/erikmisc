#' For missing data, determine which sets of variables result in the most number of complete observations
#'
#' @param dat       data data.frame or tibble
#' @param var_list  list of variables, \code{NULL} for all
#' @param var_resp  \code{NULL} or one variable name to always be included (filters to keep only observations with this variable)
#'
#' @return out      a tibble with the \code{n_complete}, \code{n_var}, \code{var_names_print}, and a list of variable names in \code{var_names}
#' @importFrom stringr str_replace_all
#' @importFrom finalfit missing_pattern
#' @importFrom stats complete.cases
#' @import dplyr
#' @import tidyselect
#' @import tibble
#' @export
#'
#' @examples
#' # Generate missing values
#' dat_mtcars_miss_e <- dat_mtcars_e
#' prop_missing <- 0.10
#' n_missing <-
#'   sample.int(
#'     n    = prod(dim(dat_mtcars_miss_e))
#'   , size = round( prop_missing * prod(dim(dat_mtcars_miss_e)))
#'   )
#' ind_missing <- expand.grid(1:dim(dat_mtcars_miss_e)[1], 1:dim(dat_mtcars_miss_e)[2])[n_missing, ]
#' for (i_row in seq_along(n_missing)) {
#'   dat_mtcars_miss_e[ind_missing[i_row, 1], ind_missing[i_row, 2] ] <- NA
#' }
#'
#' # Plot missing data
#' dat_mtcars_miss_e |> e_plot_missing()
#'
#' out <- dat_mtcars_miss_e |> e_data_complete_by_variable_subset()
#' # Print table
#' out |> print(n = Inf, width = Inf)
#' # Print variable names from first row
#' out$var_names[1] |> unlist()
e_data_complete_by_variable_subset <-
  function(
    dat
  , var_list  = NULL
  , var_resp  = NULL
  ) {
  ## dat = dat_mtcars_miss_e
  ## var_list  = NULL
  ## var_resp = "mpg"

  if (is.null(var_list)) {
    var_list <- dat |> names()
  }

  # move response variable to first position
  if (!is.null(var_resp)) {
    var_list <- c(var_list[which(var_list == var_resp)], var_list[-which(var_list == var_resp)])
  }

  var_list_blank <-
    var_list |>
    stringr::str_replace_all(
      pattern = "."
    , replacement = " "
    )

  dat_miss <-
    dat |>
    dplyr::select(
      tidyselect::all_of(var_list)
    )

  # restrict to where response is not missing
  if (!is.null(var_resp)) {
    dat_miss <-
      dat_miss |>
      dplyr::filter(
        !is.na(!!sym(var_resp))
      )
  }

  dat_miss_pattern <-
    dat_miss |>
    finalfit::missing_pattern(
      plot = FALSE
    )

  dim_dat_miss_pattern <-
    dat_miss_pattern |>
    dim()

  # reorder columns to match data
  dat_miss_pattern <-
    dat_miss_pattern[, var_list]

  # remove summary last row and last column
  dat_miss_pattern <-
    dat_miss_pattern[-dim_dat_miss_pattern[1], -dim_dat_miss_pattern[2]]

  dim_dat_miss_pattern <-
    dat_miss_pattern |>
    dim()

  dat_var_complete <-
    tibble::tibble(
      n_complete  = rep(NA, dim_dat_miss_pattern[1]) |> as.numeric()
    , n_var       = rep(NA, dim_dat_miss_pattern[1]) |> as.numeric()
    , var_names_print   = rep(NA, dim_dat_miss_pattern[1]) |> as.character()
    , var_names   = rep(list(NA |> as.character()), dim_dat_miss_pattern[1])
    )
  for (i_row in seq_len(dim_dat_miss_pattern[1])) {
    ## i_row = 7

    var_names_this <-
      names(dat_miss)[dat_miss_pattern[i_row,] == 1]

    dat_var_complete$n_complete[i_row] <-
      dat_miss |>
      dplyr::select(
        tidyselect::all_of(var_names_this)
      ) |>
      stats::complete.cases() |>
      sum()
    dat_var_complete$n_var[i_row] <-
      var_names_this |>
      length()
    dat_var_complete$var_names[i_row] <-
      list(var_names_this)


    ind_var_1 <- which(dat_miss_pattern[i_row,] == 1)
    ind_var_0 <- which(dat_miss_pattern[i_row,] == 0)
    names(ind_var_0) <- var_list_blank[var_list %in% names(ind_var_0)]

    dat_var_complete$var_names_print[i_row] <-
      tibble::tibble(
        var =
          c(
            ind_var_1
          , ind_var_0
          ) |>
          names()
      , order =
          c(
            ind_var_1
          , ind_var_0
          )
      ) |>
      dplyr::arrange(
        order
      ) |>
      dplyr::pull(
        var
      ) |>
      paste(
        collapse = " "
      )
  }

  dat_var_complete <-
    dat_var_complete |>
    dplyr::arrange(
      dplyr::desc(n_complete)
    , dplyr::desc(n_var)
    )

  return(dat_var_complete)
} # e_data_complete_by_variable_subset
