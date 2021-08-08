#' Aggregate a set of variables by another set of variables with a specific function
#'
#' @param dat                 data.frame or tibble
#' @param var_names_by        list of variable names to group by
#' @param var_names_aggregate list of variable names to aggregate
#' @param func_aggregate      a function (e.g., \code{sum}) to aggregate all variables by, or a list of functions to aggregate each variable by
#' @param .groups             drops or keeps groups
#'
#' @return aggregated data
#' @import dplyr
#' @export
#'
#' @examples
#' e_table_aggregate(
#'     dat                 = dat_mtcars_e
#'   , var_names_by        = c("cyl")
#'   , var_names_aggregate = c("disp", "hp", "drat")
#'   , func_aggregate      = mean
#'   )
#' e_table_aggregate(
#'     dat                 = dat_mtcars_e
#'   , var_names_by        = c("cyl", "am")
#'   , var_names_aggregate = c("disp", "hp", "drat", "wt", "qsec")
#'   , func_aggregate      = list(sum, sum, mean, mean, sum)
#'   )
#'
#' # assign variables and functions in a list
#' list_aggregate_var_func <-
#'   list(
#'     `disp` = sum
#'   , `hp`   = sum
#'   , `drat` = mean
#'   , `wt`   = mean
#'   , `qsec` = sum
#'   )
#' e_table_aggregate(
#'     dat                 = dat_mtcars_e
#'   , var_names_by        = c("cyl", "am")
#'   , var_names_aggregate = list_aggregate_var_func %>% names()
#'   , func_aggregate      = list_aggregate_var_func
#'   )
e_table_aggregate <-
  function(
    dat
  , var_names_by
  , var_names_aggregate
  , func_aggregate        = list(sum)
  , .groups = c("drop", "keep")[1]
  ) {

  # ## DEBUG
  # dat                 = dat_mtcars_e
  # var_names_by        = c("cyl", "am")
  # var_names_aggregate = c("disp", "hp", "drat", "wt", "qsec")
  # func_aggregate      = list(sum, sum, mean, mean, sum)

  # if length 1 and not a list, make a list
  if(!is.list(func_aggregate)) {
    func_aggregate <- list(func_aggregate)
  }

  if (!(length(func_aggregate) == length(var_names_aggregate)) & !(length(func_aggregate) == 1)) {
    warning("e_table_aggregate: lengths of func_aggregate and var_names_aggregate do not agree, using func_aggregate[1] for all")
    func_aggregate <- func_aggregate[1]
  }

  # if only one function, then use for all
  if (length(func_aggregate) == 1) {
    func_aggregate <-
      rep(
        func_aggregate
      , length(var_names_aggregate)
      )
  }


  # summarize by group
  dat_group <-
    dat %>%
    dplyr::group_by(
      dplyr::across(
        .cols = all_of(var_names_by)
      )
    )


  # full data
  dat_aggregate <- NULL

  for (i_var in seq_along(var_names_aggregate)) {
    ## i_var = 1

    # assign function
    func_this <- func_aggregate[[i_var]]

    dat_aggregate_indy <-
      dat_group %>%
      dplyr::summarize(
        value =
          .data[[ var_names_aggregate[i_var] ]] %>%
          #sum(na.rm = TRUE)
          func_this(na.rm = TRUE)
      , .groups = "keep"
      ) %>%
      dplyr::rename(
        !!var_names_aggregate[i_var] := value
      )

    if (i_var == 1) {
      dat_aggregate <-
        dat_aggregate_indy
    } else {
      dat_aggregate <-
        dat_aggregate %>%
        dplyr::full_join(
          dat_aggregate_indy
        , by = var_names_by
        )
    }
  }

  if (.groups == c("drop", "keep")[1]) {
    dat_aggregate <-
      dat_aggregate %>%
      dplyr::ungroup()
  }

  return(dat_aggregate)
} # e_table_aggregate

