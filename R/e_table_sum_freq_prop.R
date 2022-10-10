#' Create a frequency/proportion summary table by a set of variables
#'
#' @param dat           data.frame or tibble
#' @param var_names     list of variable names
#' @param sw_sort_prop  TRUE/FALSE to sort the last variable descending by proportion within the other variables
#' @param sw_drop_NA    TRUE/FALSE to drop NAs in \code{var_names} variables before calculating proportions
#' @param sw_totals     TRUE/FALSE to include totals
#'
#' @return tab_summary a summary table
#' @import dplyr
#' @importFrom tidyr drop_na
#' @importFrom tidyselect all_of
#' @export
#'
#' @examples
#' # Create data with missing values for examples
#' dat_miss = dat_mtcars_e
#' prop_missing = 0.10
#' n_missing = sample.int(n = prod(dim(dat_miss)), size = round( prop_missing * prod(dim(dat_miss))))
#' ind_missing = expand.grid(1:dim(dat_miss)[1], 1:dim(dat_miss)[2])[n_missing, ]
#' for (i_row in seq_along(n_missing)) {
#'   dat_miss[ind_missing[i_row,1], ind_missing[i_row,2] ] <- NA
#' }
#'
#' # do not sort, with NAs, no totals
#' e_table_sum_freq_prop(
#'     dat           = dat_miss
#'   , var_names     = c("vs", "am", "cyl")
#'   , sw_sort_prop  = FALSE
#'   , sw_drop_NA    = FALSE
#'   , sw_totals     = FALSE
#'   ) %>%
#'   print(n = Inf)
#'
#' # sorted by proportion, with NAs, no totals
#' e_table_sum_freq_prop(
#'     dat           = dat_miss
#'   , var_names     = c("vs", "am", "cyl")
#'   , sw_sort_prop  = TRUE
#'   , sw_drop_NA    = FALSE
#'   , sw_totals     = FALSE
#'   ) %>%
#'   print(n = Inf)
#'
#' # sorted by proportion, no NAs, no totals
#' e_table_sum_freq_prop(
#'     dat           = dat_miss
#'   , var_names     = c("vs", "am", "cyl")
#'   , sw_sort_prop  = TRUE
#'   , sw_drop_NA    = TRUE
#'   , sw_totals     = FALSE
#'   ) %>%
#'   print(n = Inf)
#'
#' # sorted by proportion, no NAs, with totals
#' e_table_sum_freq_prop(
#'     dat           = dat_miss
#'   , var_names     = c("vs", "am", "cyl")
#'   , sw_sort_prop  = TRUE
#'   , sw_drop_NA    = TRUE
#'   , sw_totals     = TRUE
#'   ) %>%
#'   print(n = Inf)
#'
e_table_sum_freq_prop <-
  function(
    dat
  , var_names
  , sw_sort_prop = TRUE
  , sw_drop_NA   = FALSE
  , sw_totals    = FALSE
  ) {

  # drop NAs
  if (sw_drop_NA) {
    dat <-
      dat %>%
      tidyr::drop_na(
        tidyselect::all_of(var_names)
      )
  }

  tab_summary <- list()

  i_list <- length(var_names) + 2

  for (i_var in 0:(length(var_names))) {
    ## i_var = 1

    i_list <- i_list - 1

    if (i_var == 0) {
      # summarize by group
      tab_summary[[ i_list ]] <-
        dat %>%
        dplyr::summarize(
          n = n()
        , .groups = "drop_last"
        ) %>%
        dplyr::mutate(
          prop = round(n / sum(n), 3)
        )
    } else {
      # summarize by group
      tab_summary[[ i_list ]] <-
        dat %>%
        dplyr::group_by(
          dplyr::across(
            .cols = var_names[1:i_var]
          )
        ) %>%
        dplyr::summarize(
          n = n()
        , .groups = "drop_last"
        ) %>%
        dplyr::mutate(
          prop = round(n / sum(n), 3)
        )
    }

    # sort descending by group
    if (sw_sort_prop) {
      tab_summary[[ i_list ]] <-
        tab_summary[[ i_list ]] %>%
        dplyr::arrange(
          dplyr::desc(prop)
        , .by_group = TRUE
        )
    }

    # ungroup before return
    tab_summary[[ i_list ]] <-
      tab_summary[[ i_list ]] %>%
      dplyr::ungroup()

    # add total labels
    if (sw_totals) {
      if (i_var < length(var_names)) {
        for (i_total in (i_var + 1):length(var_names)) {
          tab_summary[[ i_list ]][ var_names[i_total] ] <-
            "_TOTAL_"
        }
      }
    }

  } # i_var

  if (!sw_totals) {
    return(tab_summary[[ 1 ]])
  }

  # add total labels
  if (sw_totals) {
    tab_summary <-
      tab_summary %>%
      dplyr::bind_rows()
  }

  return(tab_summary)
} # e_table_sum_freq_prop

