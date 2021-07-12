#' Create a frequency/proportion summary table by a set of variables
#'
#' @param dat           data.frame or tibble
#' @param var_names     list of variable names
#' @param sw_sort_prop  TRUE/FALSE to sort the last variable descending by proportion within the other variables
#'
#' @return tab_summary a summary table
#' @import dplyr
#' @export
#'
#' @examples
#' e_table_sum_freq_prop(
#'     dat           = ggplot2::mpg
#'   , var_names     = c("drv", "cyl", "class")
#'   , sw_sort_prop  = TRUE
#'   ) %>%
#'   print(n = Inf)
e_table_sum_freq_prop <-
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
} # e_table_sum_freq_prop




# XXX update and test.  From Erhardt_ECURE_Analysis_Spring2021_20210702.Rmd
#' summarize text responses
e_table_sum_text <-
  function(
    dat_sum
  , var_names
  , text_blank = c(NA, "none", "n/a", "na")
  ) {
  ## f_tab_sum(dat_all, "Gender") %>% print(n=Inf)

  # dat_all_dup
  # , c("Treatment", "Group", var_class_tbl$Var[i_col])

  dat_sum[[ var_names[length(var_names)] ]] <-
    ifelse(
      !(
        tolower(
          dat_sum[[ var_names[length(var_names)] ]]
        ) %in%
        text_blank
      )
    , TRUE
    , NA
    )

  tab_dat_summary_temp <-
    dat_sum %>%
    group_by_(
      .dots = var_names
    ) %>%
    drop_na(var_names[length(var_names)]) %>%
    summarize(
      n = n()
    , .groups = "drop_last"
    ) %>%
    #mutate(
    #  prop = round(n / sum(n), 3)
    #) %>%
    #arrange(
    #  desc(n)
    #) %>%
    ungroup()

  return(tab_dat_summary_temp)
}

