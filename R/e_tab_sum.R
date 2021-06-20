#' e_tab_sum create a frequency/proportion table by a set of variables
#'
#' @param dat           data.frame or tibble
#' @param var_names     list of variable names
#' @param sw_sort_prop  TRUE/FALSE to sort the last variable descending by proportion within the other variables
#'
#' @return tab_summary a summary table
#' @export
#'
#' @examples
#' e_tab_sum(
#'     dat           = ggplot2::mpg
#'   , var_names     = c("drv", "cyl", "class")
#'   , sw_sort_prop  = TRUE
#'   ) %>%
#'   print(n = Inf)
e_tab_sum <-
  function(
    dat
  , var_names
  , sw_sort_prop = TRUE
  ) {

  # summarize by group
  tab_summary <-
    dat %>%
    dplyr::group_by(
      dplyr::across(
        .cols = var_names
      )
    ) %>%
    dplyr::summarize(
      n = n()
    , .groups = "drop_last"
    ) %>%
    dplyr::mutate(
      prop = round(n / sum(n), 3)
    )

  # sort descending by group
  if (sw_sort_prop) {
    tab_summary <-
      tab_summary %>%
      dplyr::arrange(
        dplyr::desc(prop)
      , .by_group = TRUE
      )
  }

  # ungroup before return
  tab_summary <-
    tab_summary %>%
    dplyr::ungroup()

  return(tab_summary)
} # e_tab_sum

